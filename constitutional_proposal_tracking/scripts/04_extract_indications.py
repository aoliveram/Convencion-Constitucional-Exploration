
import os
import json
import glob
import time
import google.generativeai as genai
from typing import List, Dict, Any

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") 
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MEMBERS_PATH = os.path.join(BASE_DIR, "convention_members.json")
COMISION_DIR = os.path.join(BASE_DIR, "comision-6", "PDFs")
OUTPUT_DIR = os.path.join(BASE_DIR, "comision-6", "indicaciones-api-extracted")

def load_json(path):
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)

def extract_indications_gemini(pdf_path: str, members_list: List[str]) -> List[Dict[str, Any]]:
    if not API_KEY:
        raise ValueError("API Key not found (GEMINI_API_KEY or GOOGLE_API_KEY).")
        
    genai.configure(api_key=API_KEY)
    
    # Model - Flash is efficient for this bulk task
    model = genai.GenerativeModel('gemini-3-flash-preview')
    
    print(f"Uploading {os.path.basename(pdf_path)}...")
    sample_file = genai.upload_file(path=pdf_path)
    
    members_str = ", ".join(members_list)
    
    prompt = f"""
    You are an expert legal data extractor for the Chilean Constitutional Convention.
    
    Attached is a "Voting Report" (Informe de Votación) containing many "Indications" (amendments).
    Your task is to extract ONLY the indications that were **APPROVED** (Aprobada). Ignore any that were Rejected, Withdrawn, or Postponed.
    
    For each APPROVED indication, extract interpretation of the following fields into a JSON object:
    
    1. "number": The indication number (e.g., "1", "15").
    2. "target_article": The article being modified (e.g., "Artículo 1", "Artículo 12").
    3. "action": The type of change. Use "ADD", "DELETE", or "MODIFY".
    4. "content": The EXACT text content of the indication or the resulting article text if it's a substitution.
    5. "authors_raw": The list of authors as written.
    6. "authors_matched": Match the authors against the Official List and return standardized names ("Surname, Name"). If NO specific names are found or it only says "del convencional", use ["UNKNOWN_AUTHOR"].
    
    **Official Member List for Matching:**
    {members_str}
    
    Return a JSON List of objects. If no approved indications are found, return [].
    """
    
    print(f"Generating content for {os.path.basename(pdf_path)}...")
    response = model.generate_content([prompt, sample_file])
    
    # Clean up file from Google Cloud to be polite (and avoid limits eventually)
    try:
        genai.delete_file(name=sample_file.name)
    except:
        pass

    try:
        text = response.text.replace('```json', '').replace('```', '').strip()
        return json.loads(text)
    except Exception as e:
        print(f"Error parsing response for {pdf_path}: {e}")
        return []

def main():
    print("--- Starting Batch Indication Extraction (Comisión 6) ---")
    
    # 1. Load Members
    members = load_json(MEMBERS_PATH) if os.path.exists(MEMBERS_PATH) else []
    
    # 2. Detect Files
    pdf_files = sorted(glob.glob(os.path.join(COMISION_DIR, "informe-indicaciones-*.pdf")))
    print(f"Found {len(pdf_files)} PDF files to process.")

    results_summary = []

    # 3. Process Loop
    for pdf_path in pdf_files:
        base_name = os.path.basename(pdf_path).replace(".pdf", "")
        out_path = os.path.join(OUTPUT_DIR, f"extracted_{base_name}.json")
        
        # Check if already processed to save time/cost
        if os.path.exists(out_path):
            print(f"File {base_name} already processed. Skipping.")
            continue
            
        try:
            indications = extract_indications_gemini(pdf_path, members)
            print(f"Successfully extracted {len(indications)} indications from {base_name}.")
            
            with open(out_path, 'w', encoding='utf-8') as f:
                json.dump(indications, f, ensure_ascii=False, indent=2)
            
            results_summary.append({"file": base_name, "count": len(indications)})
            
            # Brief pause to avoid rate limits if needed (Flash has high limits though)
            time.sleep(2)
            
        except Exception as e:
            print(f"Error processing {pdf_path}: {e}")

    print("\n--- Batch Processing Complete ---")
    for res in results_summary:
        print(f"- {res['file']}: {res['count']} indications")

if __name__ == "__main__":
    main()
