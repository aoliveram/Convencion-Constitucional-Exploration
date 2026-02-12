import google.generativeai as genai
import json
import os
import time

# Configuration
API_KEY = os.environ.get("GEMINI_API_KEY")
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

PDF_PATH = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking/comision-2/PDFs/C2_COMPLEX_informe-4-04-08-comparado.pdf"
OUTPUT_SISTEMATIZADO = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking/comision-2/reconstructed/C2_GENESIS_texto-sistematizado-03-08_Preview.json"
OUTPUT_INDICATIONS = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking/comision-2/reconstructed/C2_INDICATIONS_04_08_candidates.json"

MODEL_NAME = "gemini-3-pro-preview" # Using the pro model as requested

def upload_file(path):
    print(f"Uploading file: {path}")
    file = genai.upload_file(path)
    print(f"File uploaded: {file.name}")
    return file

def wait_for_files_active(files):
    print("Waiting for file processing...")
    for file in files: # Fix: iterate over file objects directly or names, but keep consistency
        file_obj = genai.get_file(file.name)
        while file_obj.state.name == "PROCESSING":
            print(".", end="", flush=True)
            time.sleep(2)
            file_obj = genai.get_file(file.name)
        if file_obj.state.name != "ACTIVE":
            raise Exception(f"File {file_obj.name} failed to process")
    print("...all files active")

def extract_sistematizado(pdf_file):
    print("Extracting 'Sistematizado' column...")
    model = genai.GenerativeModel(model_name=MODEL_NAME)
    
    prompt = """
    Analiza este PDF ("Comparado"). Tu objetivo es extraer SOLAMENTE el contenido de la columna izquierda llamada "SISTEMATIZADO".
    
    Ignora las columnas "INDICACIONES" y "RESULTADO".
    Extrae el texto COMPLETO de cada artículo que aparezca en la columna "SISTEMATIZADO".
    
    Formato de salida JSON:
    [
      {
        "article": "Nombre del Artículo (ej: Artículo 1)",
        "text": "Texto completo del artículo..."
      },
      ...
    ]
    
    Si el texto de un artículo está cortado entre páginas, únelo coherentemente.
    Asegúrate de capturar TODOS los artículos (deberían ser alrededor de 96, revisa bien).
    """
    
    # Passing prompt first, then file content
    response = model.generate_content([prompt, pdf_file], generation_config={"response_mime_type": "application/json"})
    return response.text

def extract_indications(pdf_file):
    print("Extracting 'Indicaciones' column...")
    model = genai.GenerativeModel(model_name=MODEL_NAME)
    
    prompt = """
    Analiza este PDF ("Comparado"). Tu objetivo es extraer SOLAMENTE el contenido de la columna central llamada "INDICACIONES".
    
    Ignora la columna "SISTEMATIZADO" y la columna "RESULTADO" (que está vacía).
    
    Quiero una lista de todas las indicaciones propuestas. Agrupa las indicaciones por el artículo al que hacen referencia (basándote en la fila o contexto cercano).
    
    Formato de salida JSON:
    [
      {
        "article_ref": "Referencia al artículo (ej: Artículo 1)",
        "indications": [
          {
            "number": "Número de indicación si existe (o 's/n')",
            "content": "Texto completo de la indicación...",
            "authors": ["Lista", "de", "autores", "inferida"]
          },
          ...
        ]
      },
      ...
    ]
    
    Presta atención a indicaciones como "Para sustituir el artículo...", "Para agregar...", "Para suprimir...".
    """
    
    response = model.generate_content([prompt, pdf_file], generation_config={"response_mime_type": "application/json"})
    return response.text

def main():
    # 0. Configure API
    if not API_KEY:
        print("Error: GEMINI_API_KEY or GOOGLE_API_KEY not found in environment.")
        return
    genai.configure(api_key=API_KEY)

    # 1. Upload File
    if not os.path.exists(PDF_PATH):
        print(f"Error: File not found at {PDF_PATH}")
        return

    pdf_file = upload_file(PDF_PATH)
    wait_for_files_active([pdf_file])

    # 2. Call 1: Sistematizado
    try:
        sistematizado_raw = extract_sistematizado(pdf_file)
        sistematizado_json = json.loads(sistematizado_raw)
        
        os.makedirs(os.path.dirname(OUTPUT_SISTEMATIZADO), exist_ok=True)
        with open(OUTPUT_SISTEMATIZADO, 'w', encoding='utf-8') as f:
            json.dump(sistematizado_json, f, indent=2, ensure_ascii=False)
        print(f"Saved Sistematizado to {OUTPUT_SISTEMATIZADO}")
        
    except Exception as e:
        print(f"Error extracting Sistematizado: {e}")

    # 3. Call 2: Indicaciones
    try:
        indications_raw = extract_indications(pdf_file)
        indications_json = json.loads(indications_raw)
        
        os.makedirs(os.path.dirname(OUTPUT_INDICATIONS), exist_ok=True)
        with open(OUTPUT_INDICATIONS, 'w', encoding='utf-8') as f:
            json.dump(indications_json, f, indent=2, ensure_ascii=False)
        print(f"Saved Indications to {OUTPUT_INDICATIONS}")
        
    except Exception as e:
        print(f"Error extracting Indications: {e}")

if __name__ == "__main__":
    main()
