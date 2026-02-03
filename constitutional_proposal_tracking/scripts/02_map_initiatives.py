
import os
import glob
import json
import re
import time
import google.generativeai as genai
from typing import Dict, List, Any

# --- Configuration ---
# API Key should be set in environment variable
# Improved logic: Check GEMINI_API_KEY first, then GOOGLE_API_KEY (used in other project scripts)
API_KEY = os.environ.get("GEMINI_API_KEY") 
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

# Paths
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MEMBERS_PATH = os.path.join(BASE_DIR, "convention_members.json")
INITIATIVES_DIR = os.path.join(BASE_DIR, "submitted_initiatives")
# Specific file mention by user for Comision 6
PDF_PATH = os.path.join(BASE_DIR, "comision-6", "PDFs", "texto-sistematizado-01-25.pdf")
OUTPUT_PATH = os.path.join(BASE_DIR, "proposals", "draft_0_mapping.json")

def load_json(path):
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)

def load_initiatives(directory: str) -> Dict[str, Any]:
    """
    Loads all initiative JSON files and creates a map of InitiativeID -> Data.
    Extracts ID from the key in the JSON (e.g., "98-6-..." -> "98-6").
    """
    initiatives_map = {}
    pattern = os.path.join(directory, "api_extracted_*.json")
    files = glob.glob(pattern)
    
    print(f"Found {len(files)} initiative files in {directory}")
    
    for file_path in files:
        data = load_json(file_path)
        for key, value in data.items():
            # Extract ID from the filename-like key
            # Example: "98-6-Iniciativa..." -> "98-6"
            # It seems the format is NUMBER-NUMBER-Title...
            match = re.match(r"^(\d+-\d+)", key)
            if match:
                init_id = match.group(1)
                initiatives_map[init_id] = value
                # Also store under the full key just in case
                initiatives_map[key] = value
    
    print(f"Loaded {len(initiatives_map)} initiatives.")
    return initiatives_map

def get_gemini_mapping(pdf_path: str, members_list: List[str]) -> List[Dict[str, Any]]:
    """
    Uses Gemini to parse the PDF and extract Article -> Initiative ID mapping.
    """
    if not API_KEY:
        raise ValueError("GEMINI_API_KEY environment variable not set.")
    
    genai.configure(api_key=API_KEY)
    
    # Upload the file
    print(f"Uploading {pdf_path} to Gemini...")
    sample_file = genai.upload_file(path=pdf_path, display_name="Texto Sistematizado")
    
    # Wait for file to differ processing state (usually immediate for small docs but good practice)
    print(f"Uploaded file '{sample_file.display_name}' as: {sample_file.uri}")
    
    # Model configuration
    # Using gemini-3-flash as requested by user.
    model = genai.GenerativeModel('gemini-3-flash-preview') 
    
    members_str = ", ".join(members_list)
    
    prompt = f"""
    You are an expert data extraction assistant.
    
    Attached is a PDF document "Texto Sistematizado" from the Chilean Constitutional Convention.
    This document links specific "Articles" (Artículos) of a proposed text to the "Initiative Numbers" (Boletines or N° de Iniciativa) that originated them.
    
    Your task is to extract this mapping for ALL articles found in the document.
    
    The output must get a JSON list of objects. Each object should have:
    - "article": The article identifier (e.g., "Artículo 1", "Artículo 15").
    - "initiative_ids": A list of initiative IDs identified as the source (e.g., ["41-6", "88-6"]).
    - "notes": Any specific notes regarding paragraphs (incisos) if mentioned (e.g., "Only first 3 paragraphs").
    
    The format of the initiative usually looks like "XX-6" or "XXX-6" (since this is commission 6).
    
    Here is a list of standardized convention member names for context, although primarily you are looking for initiative numbers:
    {members_str}
    
    Return ONLY valid JSON.
    """
    
    print("Generating content...")
    response = model.generate_content([prompt, sample_file])
    
    # Parse the response
    try:
        # cleanup markdown code blocks if present
        text = response.text.strip()
        if text.startswith("```json"):
            text = text[7:]
        if text.endswith("```"):
            text = text[:-3]
        
        extracted_data = json.loads(text)
        return extracted_data
    except Exception as e:
        print(f"Error parsing Gemini response: {e}")
        print("Raw response:")
        print(response.text)
        return []

def main():
    print("Starting mapping process...")
    
    # 1. Load Members
    if os.path.exists(MEMBERS_PATH):
        members = load_json(MEMBERS_PATH)
        print(f"Loaded {len(members)} members.")
    else:
        print("Warning: convention_members.json not found.")
        members = []

    # 2. Load Initiatives
    initiatives_data = load_initiatives(INITIATIVES_DIR)
    
    # 3. Extract Mapping from PDF
    print("Extracting mapping from PDF via Gemini...")
    try:
        mapping_list = get_gemini_mapping(PDF_PATH, members)
    except Exception as e:
        print(f"Critical error during Gemini extraction: {e}")
        return

    # 4. Merge Data
    final_output = []
    
    print("Merging extracted mapping with initiative data...")
    for item in mapping_list:
        merged_item = {
            "article": item.get("article"),
            "notes": item.get("notes"),
            "sources": []
        }
        
        for init_id in item.get("initiative_ids", []):
            source_data = {
                "initiative_id": init_id,
                "found_in_database": False,
                "authors_matched": []
            }
            
            # Try to find the initiative data
            if init_id in initiatives_data:
                init_info = initiatives_data[init_id]
                source_data["found_in_database"] = True
                source_data["title"] = init_info.get("propuesta_norma", "")[:50] + "..." # Snippet
                source_data["authors_matched"] = init_info.get("firmantes_matched", [])
            else:
                # Try simple fuzzy match or variation? 
                # Sometimes IDs might differ slightly (e.g. leading zeros)
                pass
            
            merged_item["sources"].append(source_data)
        
        final_output.append(merged_item)
        
    # 5. Save Output
    os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(final_output, f, ensure_ascii=False, indent=2)
        
    print(f"Mapping complete. Saved to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
