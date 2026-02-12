
import os
import json
import time
import google.generativeai as genai

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") or os.environ.get("GOOGLE_API_KEY")
if not API_KEY:
    raise ValueError("GEMINI_API_KEY or GOOGLE_API_KEY not found in environment variables.")

genai.configure(api_key=API_KEY)

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
COM4_DIR = os.path.join(BASE_DIR, "comision-4")
PDF_DIR = os.path.join(COM4_DIR, "PDFs")

# Files to process
PDF_FILES = [
    "C4_COMPLEX_informe-1-03-07-votacion-general_1.pdf",
    "C4_COMPLEX_informe-1-03-07-votacion-general_2.pdf"
]

OUTPUT_FILE = os.path.join(COM4_DIR, "genesis-extracted", "C4_ICC_POOL.json")

def extract_approved_iccs(pdf_filename):
    pdf_path = os.path.join(PDF_DIR, pdf_filename)
    if not os.path.exists(pdf_path):
        print(f"Error: File not found {pdf_path}")
        return []

    print(f"Uploading {pdf_filename} to Gemini...")
    try:
        # Upload file using the File API
        myfile = genai.upload_file(pdf_path)
        print(f"Uploaded: {myfile.name}")
    except Exception as e:
        print(f"Error uploading file: {e}")
        return []

    # Wait for processing if necessary (images/videos usually need it, PDFs might be quick)
    while myfile.state.name == "PROCESSING":
        print("Processing file...")
        time.sleep(2)
        myfile = genai.get_file(myfile.name)

    # Use the requested model
    model_name = "gemini-3-pro-preview" 
    try:
        model = genai.GenerativeModel(model_name)
    except Exception as e:
        print(f"Error initializing model {model_name}: {e}")
        return []

    prompt = """
    ROLE: Expert Legal Data Extractor.
    TASK: Extract all "Iniciativas Convencionales Constituyentes" (ICC) that were APPROVED in general voting.

    INSTRUCTIONS:
    1. Scan the whole document.
    2. Identify blocks starting with "ICC N°" (e.g., "ICC N° 11", "ICC N° 89-4").
    3. Read the text of the proposed Articles or Incisos under that ICC.
    4. SYSTEMATICALLY CHECK the voting result immediately below each text block.
       - Look for: "Sometida a votación se aprobó", "aprobada", "Aprobado".
       - IGNORE if "rechazó", "rechazada".
    5. For every APPROVED block, extract:
       - "icc_id": The number of the ICC (e.g. "11", "89-4").
       - "text": The full text of the approved article/paragraph.
       - "voting_result": The text confirming approval (e.g. "aprobó 25 votos...").

    OUTPUT FORMAT:
    Return a valid JSON List of objects.
    [
      { "icc_id": "11", "text": "Art. 1... contenido...", "voting_result": "aprobó (25 votos...)" },
      ...
    ]
    """

    print(f"Generating content with {model_name}...")
    try:
        response = model.generate_content([myfile, prompt])
        print("Response received.")
        
        # Clean markdown
        text = response.text
        if "```json" in text:
            text = text.split("```json")[1].split("```")[0]
        elif "```" in text:
            text = text.split("```")[1].split("```")[0]
        
        data = json.loads(text.strip())
        return data

    except Exception as e:
        print(f"Error generating/parsing: {e}")
        return []

def main():
    all_iccs = []
    
    for pdf in PDF_FILES:
        print(f"\nProcessing {pdf}...")
        results = extract_approved_iccs(pdf)
        print(f"Found {len(results)} approved items in {pdf}")
        all_iccs.extend(results)

    # Save consolidated
    os.makedirs(os.path.dirname(OUTPUT_FILE), exist_ok=True)
    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        json.dump(all_iccs, f, ensure_ascii=False, indent=2)
    
    print(f"\nTotal Approved ICC blocks extracted: {len(all_iccs)}")
    print(f"Saved to {OUTPUT_FILE}")

if __name__ == "__main__":
    main()
