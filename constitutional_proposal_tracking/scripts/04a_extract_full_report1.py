import os
import json
import glob
import time
import re
import google.generativeai as genai

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") 
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PDF_DIR = os.path.join(BASE_DIR, "comision-2", "PDFs")
OUTPUT_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "C2_COMPLEX_informe-reemplazo-1-03-02.json")
MEMBERS_PATH = os.path.join(BASE_DIR, "convention_members.json")

def load_json(path):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    return []

def extract_from_pdf_chunk(model, pdf_path, members_str):
    print(f"  Uploading {os.path.basename(pdf_path)}...")
    sample_file = genai.upload_file(path=pdf_path)
    
    # Wait for processing
    while sample_file.state.name == "PROCESSING":
        time.sleep(2)
        sample_file = genai.get_file(sample_file.name)
        
    prompt = f"""
    ACT AS: Legal Data Specialist.
    DOC: "Informe de Reemplazo" (Replacement Report).
    FORMAT: 3-Column Table.
    1. [TEXTO SISTEMATIZADO] (The Base Article Text being debated).
    2. [INDICACIONES] (Proposed changes).
    3. [RESULTADO] (Voting Result).
    
    TASK: Extract EFFICIENTLY the structure of the debate.
    
    INSTRUCTIONS:
    1. Iterate through every ROW of the table. capture the "Base Article" context.
    2. Ideally, group by ARTICLE. 
       - If Column 1 has "Artículo 1.- ...", that starts a new Article Block.
       - Extract the FULL text of Column 1 as "base_text". DO NOT TRUNCATE. COPY EVERY SINGLE WORD FROM COLUMN 1. IF YOU TRUNCATE YOU FAIL THE TASK.
    3. List ALL indications for that article block found in Column 2.
       - Capture "number", "content", "result" (from Col 3).
       - IMPORTANT: Capture the "result" exactly (Aprobado, Rechazado, Retirado).
    
    OUTPUT JSON FORMAT (List of Articles):
    [
      {{
        "article_ref": "Artículo 1",
        "base_text": "Full text of article 1 from column 1. ",
        "indications": [
           {{ "number": "1", "content": "...", "result": "Aprobado", "authors": ["Roa"] }},
           {{ "number": "2", "content": "...", "result": "Rechazado", "authors": ["Squella"] }}
        ]
      }},
      ...
    ]
    
    MEMBER LIST (for author extraction):
    {members_str}
    
    Skipt the article '18 I' completely. Skip only that article. I'll fill the information later manually. 
    Return ONLY JSON.
    """
    
    try:
        response = model.generate_content([prompt, sample_file])
        text = response.text
        
        # Clean JSON
        match = re.search(r'\[.*\]', text, re.DOTALL)
        if match:
             json_str = match.group(0)
        else:
             json_str = text.replace('```json', '').replace('```', '').strip()
             
        data = json.loads(json_str)
        return data

    except Exception as e:
        print(f"    Error parsing {os.path.basename(pdf_path)}: {e}")
        return []
    finally:
        # Cleanup
        try:
             genai.delete_file(sample_file.name)
        except:
             pass

def main():
    print("--- Extracting FULL Report 1 Structure (Columns 1, 2, 3) ---")
    
    if not API_KEY:
        print("Error: API Key not found.")
        return

    genai.configure(api_key=API_KEY)
    # Using Pro 3 as requested for maximum table reasoning capability
    model = genai.GenerativeModel('gemini-3-pro-preview')
    
    members = load_json(MEMBERS_PATH)
    members_str = ", ".join(members) 
    
    # Find all 5 parts
    pdf_pattern = os.path.join(PDF_DIR, "C2_COMPLEX_informe-reemplazo-1-03-02_*.pdf")
    pdf_files = sorted(glob.glob(pdf_pattern))
    
    if not pdf_files:
        print(f"No PDF files found matching {pdf_pattern}")
        return
        
    full_report_structure = []
    
    for pdf_path in pdf_files:
        print(f"Processing chunk: {pdf_path}")
        chunk_data = extract_from_pdf_chunk(model, pdf_path, members_str)
        print(f"  -> Found {len(chunk_data)} grouped articles.")
        full_report_structure.extend(chunk_data)
        time.sleep(2) 
        
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(full_report_structure, f, ensure_ascii=False, indent=2)
        
    print(f"\nSaved full report structure ({len(full_report_structure)} blocks) to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
