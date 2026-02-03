
import os
import json
import google.generativeai as genai

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") 
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PDF_PATH = os.path.join(BASE_DIR, "comision-2", "PDFs", "informe-reemplazo-1-03-02.pdf")
OUTPUT_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "indications_report_1.json")
MEMBERS_PATH = os.path.join(BASE_DIR, "convention_members.json")

def load_json(path):
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)

def extract_from_table(pdf_path: str, members_list: list):
    genai.configure(api_key=API_KEY)
    # Using Flash for efficient table parsing. 
    # If table is very complex, Pro represents relationships better, but Flash is capable.
    model = genai.GenerativeModel('gemini-3-flash-preview')
    
    print(f"Uploading {os.path.basename(pdf_path)}...")
    sample_file = genai.upload_file(path=pdf_path)
    
    members_str = ", ".join(members_list)
    
    prompt = f"""
    You are an expert legal data extractor.
    Attached is the "Informe de Reemplazo" (Replacement Report) which contains a 3-COLUMN TABLE:
    1. TEXTO SISTEMATIZADO (Original Text)
    2. INDICACIONES (Proposed Amendments)
    3. RESULTADO (Result)
    
    Task:
    - Scrape ONLY the rows where the "RESULTADO" column contains "Aprobado" (Approved).
    - For each approved row, extract the Indication details into a JSON object:
      
      1. "number": Indication number/ID found in the 'INDICACIONES' column (e.g., "1", "116").
      2. "target_article": The article being modified (from the 'TEXTO SISTEMATIZADO' or the indication itself).
      3. "content": The text of the approved indication.
      4. "authors_matched": Match authors mentioned in 'INDICACIONES' against the Official List attached below.
      
    **Official Member List:**
    {members_str}
    
    Return a JSON List of objects.
    """
    
    print("Generating indication extraction...")
    response = model.generate_content([prompt, sample_file])
    
    try:
        text = response.text.replace('```json', '').replace('```', '').strip()
        data = json.loads(text)
        # Add basic action type based on content keywords if missing
        for item in data:
            content = item.get("content", "").lower()
            if "suprimir" in content or "eliminar" in content:
                item["action"] = "DELETE"
            elif "sustituir" in content or "reemplazar" in content:
                item["action"] = "MODIFY"
            elif "agregar" in content or "incorporar" in content:
                item["action"] = "ADD"
            else:
                item["action"] = "MODIFY" # Default
        return data

    except Exception as e:
        print(f"Error parsing response: {e}")
        # Debug
        with open(OUTPUT_PATH + ".raw.txt", "w", encoding='utf-8') as f:
            f.write(response.text)
        return []

def main():
    print("--- Starting Extract Step 2 (Comision 2 - Report 1) ---")
    
    if not os.path.exists(PDF_PATH):
        print(f"Error: File not found at {PDF_PATH}")
        return

    members = load_json(MEMBERS_PATH) if os.path.exists(MEMBERS_PATH) else []
    
    results = extract_from_table(PDF_PATH, members)
    print(f"Extracted {len(results)} approved indications.")
    
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(results, f, ensure_ascii=False, indent=2)
    print(f"Saved to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
