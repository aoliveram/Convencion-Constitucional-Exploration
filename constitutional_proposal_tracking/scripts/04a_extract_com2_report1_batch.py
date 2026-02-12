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
# Output name requested by user
OUTPUT_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "C2_COMPLEX_informe-reemplazo-1-03-02.json") 
MEMBERS_PATH = os.path.join(BASE_DIR, "convention_members.json")

def load_json(path):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    return []

def extract_full_rows_from_pdf_chunk(model, pdf_path, members_str):
    print(f"  Uploading {os.path.basename(pdf_path)}...")
    sample_file = genai.upload_file(path=pdf_path)
    
    # Wait for processing
    while sample_file.state.name == "PROCESSING":
        time.sleep(2)
        sample_file = genai.get_file(sample_file.name)
        
    prompt = f"""
    You are an expert legal data extractor.
    Attached is a section of the "Informe de Reemplazo" (Replacement Report).
    It is a 3-COLUMN TABLE: [TEXTO SISTEMATIZADO] | [INDICACIONES] | [RESULTADO]
    
    TASK: Extract valid semantic rows from this table.
    
    A "Row" is defined by a distinct "TEXTO SISTEMATIZADO" block in Column 1 (usually an Article) and its associated Indications in Column 2 and Results in Column 3.
    However, sometimes multiple indications apply to the same Article text.
    
    OUTPUT JSON LIST Structure:
    [
      {{
        "article_ref_col1": "Article Number detected in Col 1 (e.g., 'Artículo 1' or 'Artículo 1.- (Artículo 2)')",
        "text_content_col1": "Full text content of Column 1",
        "indications": [
           {{
             "number": "Indication ID of the approved INDICATION (e.g. '1', '116')",
             "content": "The full text of the approved INDICATION (Column 2)",
             "action": "ADD", "MODIFY", or "DELETE" based on the content (e.g. "Suprimir", "Reemplazar", "Agregar").
             "authors_matched": List of known authors found in the approved INDICATION.
           }},
           ...
        ]
      }},
      ...
    ]
    
    **Official Member List:**
    {members_str}

    IMPORTANT:
    1. Capture ALL rows, even if no indication was approved. We need the Col 1 text to reconstruct the base draft.
    2. Be precise with the "result" field. If Col 3 says "Indicación 1: 12 a favor... Aprobada", the result is "Aprobado".
    
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
        try:
             genai.delete_file(sample_file.name)
        except:
             pass

def main():
    print("--- Extracting FULL DATA (Text + Indications) from Report 1 ---")
    
    if not API_KEY:
        print("Error: API Key not found.")
        return

    genai.configure(api_key=API_KEY)
    # Pro model required for high fidelity full-text extraction
    model = genai.GenerativeModel('gemini-1.5-pro') 
    
    # Find all 5 parts
    pdf_pattern = os.path.join(PDF_DIR, "C2_COMPLEX_informe-reemplazo-1-03-02_*.pdf")
    pdf_files = sorted(glob.glob(pdf_pattern))
    
    if not pdf_files:
        print(f"No PDF files found matching {pdf_pattern}")
        return
        
    full_dataset = []
    
    for pdf_path in pdf_files:
        print(f"Processing chunk: {pdf_path}")
        print("  (This may take longer due to full-text extraction...)")
        chunk_data = extract_full_rows_from_pdf_chunk(model, pdf_path, "")
        print(f"  -> Extracted {len(chunk_data)} Article Blocks.")
        full_dataset.extend(chunk_data)
        time.sleep(5) # Rate limit politeness
        
    # Save
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(full_dataset, f, ensure_ascii=False, indent=2)
        
    print(f"\nSuccess! Saved {len(full_dataset)} total article blocks to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
