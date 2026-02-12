import os
import time
import json
import re
import google.generativeai as genai

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") or os.environ.get("GOOGLE_API_KEY")
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PDF_PATH = os.path.join(BASE_DIR, "comision-2", "PDFs", "C2_COMPLEX_BORRADOR-CONSTITUCIONAL-14-05-22.pdf")
OUTPUT_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "final_draft_com2.json")

def extract_final_articles(model, pdf_path):
    print(f"Uploading PDF: {os.path.basename(pdf_path)}...")
    sample_file = genai.upload_file(path=pdf_path)
    
    print("Waiting for file processing...")
    while sample_file.state.name == "PROCESSING":
        time.sleep(2)
        sample_file = genai.get_file(sample_file.name)
        
    print(f"File ready. State: {sample_file.state.name}")
    
    prompt = """
    ACT AS: Legislative Data Specialist.
    TASK: Extract all Constitutional Articles from this Final Draft document.
    
    DOCUMENT STRUCTURE:
    The document lists articles in the format:
    "101.- Artículo 1.- Estado. Chile es un Estado..."
    
    Where:
    - "101" is the Consecutive Number (Global ID).
    - "Artículo 1" is the Article Number within the commission/chapter.
    - "Estado" is the Title.
    - The rest is the Content.
    
    INSTRUCTIONS:
    1. Extract every article found in the text.
    2. Ignore headers like "CAPÍTULO (COM 2)..." or page numbers.
    3. Capture the FULL text of the article preserving paragraphs.
    
    OUTPUT JSON FORMAT:
    [
      {
        "global_id": "101",
        "article_ref": "Artículo 1",
        "title": "Estado",
        "text": "Chile es un Estado social y democrático..."
      },
      ...
    ]
    
    Return ONLY JSON.
    """
    
    try:
        print("Sending request to Gemini Flash...")
        response = model.generate_content([prompt, sample_file])
        text = response.text
        
        # Clean markdown formatting if present
        text = re.sub(r'```json', '', text)
        text = re.sub(r'```', '', text)
        text = text.strip()
        
        data = json.loads(text)
        return data
            
    except Exception as e:
        print(f"Error extracting data: {e}")
        return []
    finally:
        print("Cleaning up uploaded file...")
        genai.delete_file(sample_file.name)

def main():
    if not API_KEY:
        print("Error: API Key not found.")
        return

    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel('gemini-3-flash-preview')
    
    articles = extract_final_articles(model, PDF_PATH)
    
    if articles:
        os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
        with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
            json.dump(articles, f, ensure_ascii=False, indent=2)
        print(f"Success! Extracted {len(articles)} articles to {OUTPUT_PATH}")
    else:
        print("Extraction failed or returned empty.")

if __name__ == "__main__":
    main()
