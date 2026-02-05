
import os
import json
import time
import google.generativeai as genai
import re

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") 
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PDF_PATH = os.path.join(BASE_DIR, "comision-2", "PDFs", "C2_COMPLEX_informe-4-04-08-comparado.pdf")
OUTPUT_DIR = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted")
OUTPUT_PATH = os.path.join(OUTPUT_DIR, "candidates_com2.json")
MEMBERS_PATH = os.path.join(BASE_DIR, "convention_members.json")

def load_json(path):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    return []

def get_pdf_page_count(model, sample_file):
    """Asks the model for the page count to plan the batching."""
    prompt = "How many pages are in this PDF document? Return ONLY the integer number."
    try:
        response = model.generate_content([prompt, sample_file])
        text = response.text.strip()
        # Find first number
        match = re.search(r'\d+', text)
        if match:
            return int(match.group(0))
    except Exception as e:
        print(f"Error getting page count: {e}")
    return 50 # Default fallback

def extract_candidates_full(model, sample_file, members_str):
    print(f"  > Processing full document with hierarchical structural analysis...")
    
    prompt = f"""
    ACT AS: Senior Legislative Data Architect.
    TASK: Perform a HIERARCHICAL EXTRACTION of the Comparative Table.
    
    VISUAL STRUCTURE ANALYSIS:
    - This table has distinct rows/blocks.
    - Column 1 ("Texto Sistematizado") defines the **PARENT ARTICLE** context. It usually contains text like "Artículo 14.- Title...".
    - Column 2 ("INDICACIONES") contains a list of **CHILD INDICATIONS** that apply *specifically* to that Parent Article.
    - Often, one big cell in Column 1 spans across many small rows in Column 2. This means ALL those indications belong to that single Parent Article.
    
    INSTRUCTIONS:
    1. Identify every unique PARENT ARTICLE in Column 1.
    2. For each Parent Article, extract ALL the indications (Column 2) physically associated with it or located immediately next to it until the next Parent Article begins.
    3. Extract the Indication Number, Text, and Authors.
    
    OUTPUT JSON FORMAT (Hierarchical):
    [
      {{
        "parent_article_ref": "Artículo 14",
        "parent_context_snippet": "Texto del artículo 14...",
        "indications": [
           {{ "number": "115", "text": "...", "authors": ["Name 1"] }},
           {{ "number": "116", "text": "...", "authors": ["Name 2"] }}
        ]
      }},
      {{
        "parent_article_ref": "Artículo 15",
        ...
      }}
    ]
    
    OFFICIAL MEMBER LIST (For matching):
    {members_str}
    
    Return ONLY JSON. Ensure you capture the correct Parent scope for every indication.
    """
    
    try:
        response = model.generate_content([prompt, sample_file])
        text = response.text
        match = re.search(r'\[.*\]', text, re.DOTALL)
        if match:
            return json.loads(match.group(0))
        else:
            return json.loads(text.replace('```json', '').replace('```', '').strip())
            
    except Exception as e:
        print(f"  Error in hierarchical extraction: {e}")
        return []

def main():
    print("--- Phase 2: Structural Candidate Extraction (Commission 2) ---")
    
    if not os.path.exists(PDF_PATH):
        print("Error: Comparado PDF not found.")
        return
        
    genai.configure(api_key=API_KEY)
    # Using Pro for reasoning capabilities and long context
    model = genai.GenerativeModel('gemini-3-pro-preview') 
    
    print(f"Uploading {os.path.basename(PDF_PATH)}...")
    sample_file = genai.upload_file(path=PDF_PATH)
    
    while sample_file.state.name == "PROCESSING":
        print(".", end="", flush=True)
        time.sleep(1)
        sample_file = genai.get_file(sample_file.name)
        
    print(f"\\nDocument ready.")
    
    members = load_json(MEMBERS_PATH)
    members_str = ", ".join(members)
    
    all_hierarchical_data = extract_candidates_full(model, sample_file, members_str)
    
    # Flatten for downstream compatibility
    flattened_candidates = []
    
    for block in all_hierarchical_data:
        parent_art = block.get("parent_article_ref", "Unknown")
        for ind in block.get("indications", []):
            flattened_candidates.append({
                "number": ind.get("number"),
                "raw_text": ind.get("text"),
                "target_article_guess": parent_art, # Now strictly derived from structure!
                "authors_matched": ind.get("authors", [])
            })
            
    # Save
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(flattened_candidates, f, ensure_ascii=False, indent=2)
        
    print(f"Saved {len(flattened_candidates)} total candidates to {OUTPUT_PATH}")
    
    # Cleanup
    genai.delete_file(sample_file.name)

if __name__ == "__main__":
    main()
