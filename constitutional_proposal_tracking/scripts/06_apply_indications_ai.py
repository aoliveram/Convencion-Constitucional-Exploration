
import os
import json
import glob
import re
import time
import google.generativeai as genai
from datetime import datetime

# --- CONFIGURATION ---
BASE_DIR = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking"
MODEL_NAME = "gemini-3-pro-preview"

# Target Commissions (All except 2)
TARGET_COMISSIONS = [1, 3, 4, 5, 6, 7]

def setup_gemini():
    """Confirms API Key is present."""
    api_key = os.environ.get("GOOGLE_API_KEY")
    if not api_key:
        print("ERROR: GOOGLE_API_KEY environment variable not set.")
        return None
    genai.configure(api_key=api_key)
    return True

def get_files_for_commission(com_n):
    """
    Returns a tuple: (genesis_enriched_path, [list_of_indication_files_sorted])
    """
    com_dir = os.path.join(BASE_DIR, f"comision-{com_n}")
    
    # 1. Find Genesis Enriched
    genesis_dir = os.path.join(com_dir, "genesis-extracted")
    # Search for patterns like C{n}_GENESIS_*_enriched.json
    g_pattern = os.path.join(genesis_dir, f"C{com_n}_GENESIS_*_enriched.json")
    genesis_files = glob.glob(g_pattern)
    if not genesis_files:
        print(f"[C{com_n}] No enriched genesis file found.")
        return None, []
    
    # Take the first one found (should be unique)
    genesis_file = genesis_files[0]
    
    # 2. Find Indication Files
    indic_dir = os.path.join(com_dir, "indicaciones-universal-extracted")
    # Pattern: C{n}_VOTACION_*indicaciones*.json
    i_pattern = os.path.join(indic_dir, f"C{com_n}_VOTACION_*indicaciones*.json")
    indication_files = glob.glob(i_pattern)
    
    # Sort files to ensure chronological order. 
    # Usually filenames have dates like '03-07', '04-01'.
    # A simple string sort usually works for standardized names, but let's be careful.
    indication_files.sort()
    
    return genesis_file, indication_files

def construct_prompt(current_draft_json, indications_json):
    """
    Builds the massive context prompt for the AI.
    """
    prompt = f"""
ROL: Eres el Secretario Técnico de la Convención Constitucional de Chile. Tu tarea es aplicar con precisión jurídica absoluta un conjunto de indicaciones (enmiendas) aprobadas a un texto base de artículos constitucionales.

ENTRADA 1: TEXTO BASE (JSON)
{json.dumps(current_draft_json, ensure_ascii=False, indent=2)}

ENTRADA 2: INDICACIONES APROBADAS (JSON)
{json.dumps(indications_json, ensure_ascii=False, indent=2)}

INSTRUCCIONES CRÍTICAS:
1. **Iteración**: Revisa cada artículo del TEXTO BASE. Busca si tiene indicaciones asociadas en la lista de INDICACIONES (usando 'target_article' o 'article_number' como referencia).
2. **Aplicación**:
   - Si la acción es **DELETE/SUPRIMIR**: Elimina el contenido del artículo o el inciso específico según indique 'target_scope'. Si es eliminación total, el artículo desaparece del borrador o queda vacío.
   - Si la acción es **SUBSTITUTE/SUSTITUIR**: Reemplaza el texto exacto. Si el alcance es 'TOTAL', reemplaza todo el artículo. Si es 'INCISO', busca el inciso correspondiente (ej: inciso 2) y reemplázalo.
   - Si la acción es **ADD/AGREGAR**: Inserta el nuevo texto en la posición indicada (ej: "después del inciso 1", "como inciso final").
3. **Manejo de Errores**: Si una indicación hace referencia a un artículo que NO existe en el texto base, IGNORELA y continua con las siguientes.
4. **Formato de Salida**: Debes devolver un ÚNICO objeto JSON válido que contenga la lista actualizada de artículos. La estructura debe ser idéntica a la del TEXTO BASE (lista de objetos con claves 'article', 'content', 'link').
5. **No Alucinar**: No inventes texto. Solo usa el texto provisto en las indicaciones.

SALIDA ESPERADA (JSON Puro):
```json
[
  {{
    "article": "1",
    "content": "Texto actualizado..."
  }},
  ...
]
```
¡Genera SOLO el JSON, sin bloques de código markdown ni explicaciones previas!
"""
    return prompt

def process_commission_workflow(com_n, model):
    print(f"\n--- PROCESANDO COMISIÓN {com_n} ---")
    
    # 1. Get paths
    genesis_path, indication_files = get_files_for_commission(com_n)
    if not genesis_path:
        return
    
    if not indication_files:
        print(f"[C{com_n}] No indication files found. Skipping.")
        return

    # 2. Setup Output Directory
    output_dir = os.path.join(BASE_DIR, f"comision-{com_n}", "draft-after-indications")
    os.makedirs(output_dir, exist_ok=True)
    
    # 3. Load Initial State (Genesis)
    print(f"[C{com_n}] Cargando Génesis: {os.path.basename(genesis_path)}")
    with open(genesis_path, 'r', encoding='utf-8') as f:
        current_state = json.load(f)
    
    # 4. Sequential Processing
    for i, indic_file_path in enumerate(indication_files):
        indic_filename = os.path.basename(indic_file_path)
        print(f"[C{com_n}] Aplicando {indic_filename} (Paso {i+1}/{len(indication_files)})...")
        
        with open(indic_file_path, 'r', encoding='utf-8') as f:
            indications_data = json.load(f)
            
        # Build prompt
        prompt = construct_prompt(current_state, indications_data)
        
        # Call Gemini
        try:
            # Using the specific model requested
            response = model.generate_content(prompt)
            
            # Extract JSON from response
            response_text = response.text
            # Remove markdown code blocks if present
            clean_text = re.sub(r'```json\n|```', '', response_text).strip()
            
            new_state = json.loads(clean_text)
            
            # Validation: Check if it's a list
            if not isinstance(new_state, list):
                raise ValueError("La respuesta del modelo no es una lista de artículos.")
                
            # Update Current State
            current_state = new_state
            
            # Save Checkpoint
            output_filename = f"draft_after_{indic_filename}"
            output_path = os.path.join(output_dir, output_filename)
            
            with open(output_path, 'w', encoding='utf-8') as f:
                json.dump(current_state, f, ensure_ascii=False, indent=2)
                
            print(f"[C{com_n}] Éxito. Guardado en {output_filename}")
            
            # Sleep to avoid rate limits
            time.sleep(5)
            
        except Exception as e:
            print(f"[C{com_n}] ERROR FATAL al aplicar {indic_filename}: {e}")
            # Dump the raw response text for debugging
            debug_file = os.path.join(output_dir, f"ERROR_RESPONSE_{indic_filename}.txt")
            with open(debug_file, 'w', encoding='utf-8') as f:
                try:
                    f.write(response.text)
                except:
                    f.write(str(e))
            print(f"   -> Ver debug en {debug_file}")
            break # Stop this commission chain on error

def main():
    if not setup_gemini():
        return

    # Instantiate Model
    # Note: 'gemini-3-pro-preview' is likely a placeholder or future model name.
    # If the API rejects it, the user will see the error and can adjust.
    try:
        model = genai.GenerativeModel(MODEL_NAME)
        print(f"Inicializando modelo: {MODEL_NAME}")
        model = genai.GenerativeModel(MODEL_NAME)
    except Exception as e:
        print(f"Error preventivo inicializando modelo: {e}")
        return

    for com_n in TARGET_COMISSIONS:
        process_commission_workflow(com_n, model)
        
    print("\n--- PROCESO TERMINADO ---")

if __name__ == "__main__":
    main()
