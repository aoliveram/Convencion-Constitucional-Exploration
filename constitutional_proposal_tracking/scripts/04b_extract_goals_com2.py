
import os
import json
import google.generativeai as genai
import time

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") 
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PDF_DIR = os.path.join(BASE_DIR, "comision-2", "PDFs")
OUTPUT_DIR = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted")
OUTPUT_PATH = os.path.join(OUTPUT_DIR, "goals_com2.json")

# Goal Files (The "Correct Answers")
GOAL_FILES = [
    os.path.join(PDF_DIR, "C2_COMPLEX_informe-reemplazo-2-03-23-1.pdf"),
    os.path.join(PDF_DIR, "C2_COMPLEX_informe-reemplazo-3-03-23-2.pdf")
]

def extract_approved_articles(pdf_path):
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel('gemini-3-flash-preview')
    
    print(f"Uploading {os.path.basename(pdf_path)}...")
    sample_file = genai.upload_file(path=pdf_path)
    
    # Wait for processing
    while sample_file.state.name == "PROCESSING":
        print(".", end="", flush=True)
        time.sleep(1)
        sample_file = genai.get_file(sample_file.name)
        
    prompt = """
    ACT AS: Legal Data Extractor.
    DOCUMENT: This is a "Replacement Report" containing APPROVED Constitutional Articles.
    
    TASK:
    Extract all the ARTICLES that are presented as the "New Text" or "Proposal".
    Ignore the introductory text or voting tally details. Focus on the content of the articles themselves.
    
    OUTPUT JSON FORMAT:
    [
      {
        "article_number": "4",
        "article_title": "Plurinacionalidad", 
        "full_text": "Chile es un Estado plurinacional..."
      }
    ]
    
    If an article generally appears as "Artículo X.- Title. Content", split it correctly.
    """
    
    print(f"\nGenerando extracción para {os.path.basename(pdf_path)}...")
    try:
        response = model.generate_content([prompt, sample_file])
        text = response.text.replace('```json', '').replace('```', '').strip()
        data = json.loads(text)
        
        # Add metadata
        for item in data:
            item["source_pdf"] = os.path.basename(pdf_path)
            
        # Clean file
        genai.delete_file(sample_file.name)
        
        return data
        
    except Exception as e:
        print(f"Error processing {pdf_path}: {e}")
        return []

def main():
    print("--- Phase 1: Goal Extraction (Commission 2) ---")
    
    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)
        
    all_goals = []
    
    for pdf in GOAL_FILES:
        if os.path.exists(pdf):
            goals = extract_approved_articles(pdf)
            all_goals.extend(goals)
            print(f"Extracted {len(goals)} articles from {os.path.basename(pdf)}")
            time.sleep(2) # Rate limit hygiene
        else:
            print(f"Warning: File not found {pdf}")
            
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(all_goals, f, ensure_ascii=False, indent=2)
        
    print(f"Saved total {len(all_goals)} goals to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
