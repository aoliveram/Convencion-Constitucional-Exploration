
import os
import json
import re
import google.generativeai as genai

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") 
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PDF_PATH = os.path.join(BASE_DIR, "comision-2", "PDFs", "texto-sistematizado-02-16.pdf")
OUTPUT_PATH = os.path.join(BASE_DIR, "proposals", "draft_0_genesis_com2.json")

def process_genesis_gemini(pdf_path: str):
    if not API_KEY:
        raise ValueError("API Key not found.")
        
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel('gemini-3-flash-preview') # Back to Flash as Pro is failing
    
    print(f"Uploading {os.path.basename(pdf_path)}...")
    sample_file = genai.upload_file(path=pdf_path)
    
    prompt = """
    You are an expert legal data extractor.
    Attached is the "Texto Sistematizado" for Commission 2.
    
    Structure:
    - The document contains Articles (e.g., "Artículo 1.-", "Artículo 1 D.-").
    - At the VERY END of the article text, inside parentheses, is the 'Initiative ID' that generated it (e.g., "(72-2)", "(54-2)").
    
    Task:
    - Extract ALL Articles.
    - Output a JSON list of objects with:
      - "article_id": The clean number/letter (e.g., "1", "1 D").
      - "text": The full content of the article.
      - "initiative_id": The ID found in parentheses at the end. If multiple, list them.
      
    Example Output:
    [
        {"article_id": "1", "text": "...", "initiative_id": "72-2"},
        {"article_id": "2", "text": "...", "initiative_id": "55-2"}
    ]
    """
    
    print("Generating genesis map...")
    response = model.generate_content([prompt, sample_file])
    
    try:
        text = response.text.replace('```json', '').replace('```', '').strip()
        return json.loads(text)
    except Exception as e:
        print(f"Error parsing response: {e}")
        return []

def main():
    print("--- Starting Genesis Extraction (Commission 2) ---")
    
    if not os.path.exists(PDF_PATH):
        print(f"Error: File not found at {PDF_PATH}")
        return

    data = process_genesis_gemini(PDF_PATH)
    
    print(f"Extracted {len(data)} genesis articles.")
    
    # Transform to match our Historian 'Draft 0' format if needed?
    # Our Historian expects: {"article": "...", "sources": [{"initiative_id": "..."}]}
    
    formatted_data = []
    for item in data:
        formatted_data.append({
            "article": f"Artículo {item.get('article_id')}",
            "text": item.get("text"), # Com 2 HAS text in Draft 0!
            "sources": [{"initiative_id": item.get("initiative_id"), "match_details": {}}]
        })
        
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(formatted_data, f, ensure_ascii=False, indent=2)
        
    print(f"Saved to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
