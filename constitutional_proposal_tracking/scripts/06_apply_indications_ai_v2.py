
import os
import json
import glob
import re
import time
import google.generativeai as genai
from google.generativeai.types import GenerationConfig
from datetime import datetime

# --- CONFIGURATION ---
BASE_DIR = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking"
MODEL_NAME = "gemini-3-pro-preview" 

#TARGET_COMISSIONS = [1, 3, 4, 5, 6, 7]
TARGET_COMISSIONS = [7]
MAX_RETRIES = 5

def setup_gemini():
    api_key = os.environ.get("GEMINI_API_KEY") or os.environ.get("GOOGLE_API_KEY")
    if not api_key:
        print("ERROR: API Key not found.")
        return False
    genai.configure(api_key=api_key)
    return True

def extract_time_slice(filename):
    """
    Extracts the 'N' from 'informe-N' or 'informe-indicaciones-N'.
    Examples:
      C3_VOTACION_informe-indicaciones-1-02-14_1.json -> 1
      C4_VOTACION_informe-1-03-07-indicaciones_1.json -> 1
      C1_VOTACION_informe-indicaciones-3-04-01-2_1.json -> 3
    """
    # Strong prior: look for "informe-" followed immediately by a digit, 
    # OR "informe-indicaciones-" followed by a digit.
    
    # Pattern 1: informe-indicaciones-(\d+)
    m1 = re.search(r'informe-indicaciones-(\d+)', filename)
    if m1:
        return int(m1.group(1))
        
    # Pattern 2: informe-(\d+)
    m2 = re.search(r'informe-(\d+)', filename)
    if m2:
        return int(m2.group(1))
        
    # Fallback: Just return 'unknown' or log warning
    return "unknown"

def get_files_ordered(com_n):
    com_dir = os.path.join(BASE_DIR, f"comision-{com_n}")
    
    # Genesis Enriched
    g_pattern = os.path.join(com_dir, "genesis-extracted", f"C{com_n}_GENESIS_*_enriched.json")
    g_files = glob.glob(g_pattern)
    genesis_file = g_files[0] if g_files else None
    
    # Indications
    i_pattern = os.path.join(com_dir, "indicaciones-universal-extracted", f"C{com_n}_VOTACION_*indicaciones*.json")
    indic_files = glob.glob(i_pattern)
    indic_files.sort() # Alphabetical sort usually works for chronological dates in filenames
    
    return genesis_file, indic_files

def build_schema():
    """
    Defines the JSON schema for Gemini response.
    """
    return {
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "original_id": { "type": "string", "description": "ID único del artículo original (invariant)." },
                "current_number": { "type": "string", "description": "Nuevo número del artículo (ej: '5', '5 bis')." },
                "content": { "type": "string", "description": "Texto jurídico completo. Vacío si 'deleted'." },
                "status": { "type": "string", "enum": ["active", "deleted", "merged"], "description": "Estado del artículo." },
                "applied_indication_ids": {
                    "type": "array",
                    "items": { "type": "string" },
                    "description": "Lista de IDs de indicaciones ('number' field in source) aplicadas en ESTA vuelta."
                }
            },
            "required": ["original_id", "content", "status", "applied_indication_ids"]
        }
    }

def process_commission(com_n, model):
    print(f"\n=== COMISIÓN {com_n} ===")
    genesis_path, indic_files = get_files_ordered(com_n)
    
    # Helper compatible with previous script structure:
    if not genesis_path:
        # Retry with pattern matching from previous success
        com_dir = os.path.join(BASE_DIR, f"comision-{com_n}")
        g_pattern = os.path.join(com_dir, "genesis-extracted", f"C{com_n}_GENESIS_*_enriched.json")
        files = glob.glob(g_pattern)
        if not files:
            print(f"[C{com_n}] No genesis file found.")
            return
        genesis_path = files[0]
        
        i_pattern = os.path.join(com_dir, "indicaciones-universal-extracted", f"C{com_n}_VOTACION_*indicaciones*.json")
        indic_files = glob.glob(i_pattern)
        indic_files.sort()

    # SETUP OUTPUT
    out_dir = os.path.join(BASE_DIR, f"comision-{com_n}", "draft-after-indications")
    os.makedirs(out_dir, exist_ok=True)
    
    # LOAD GENESIS & INITIALIZE
    with open(genesis_path, 'r', encoding='utf-8') as f:
        draft = json.load(f)
        
    # PRE-PROCESS GENESIS: Rename 'authors' -> 'authors_genesis' and ensuring 'original_id'
    print(f"[C{com_n}] Inicializando {len(draft)} artículos base...")
    for idx, art in enumerate(draft):
        # ID robustness
        if 'original_id' not in art:
            # Use 'article' field or 'id' or purely index if missing
            art['original_id'] = art.get('article', str(idx+1))
        
        # Authors robustness
        if 'authors' in art:
            art['authors_genesis'] = art.pop('authors')
        elif 'authors_genesis' not in art:
            art['authors_genesis'] = []
            
        # Initial fields
        art['status'] = 'active'
        art['history_log'] = ["Genesis"]

    # SAVE GENESIS STANDARDIZED
    # (Optional, but good for debugging)
    
    # ITERATE INDICATIONS
    for step_idx, indic_path in enumerate(indic_files):
        fname = os.path.basename(indic_path)
        time_slice = extract_time_slice(fname)
        print(f"[C{com_n}] Paso {step_idx+1}/{len(indic_files)}: {fname} (Slice {time_slice})")
        
        # Load Indications
        with open(indic_path, 'r', encoding='utf-8') as f:
            indications_data = json.load(f)
            
        # Build Map: Indication ID -> Authors
        # Indication file structure: [{"number": "1", "authors_matched": [...], ...}, ...]
        indic_author_map = {}
        for ind in indications_data:
            i_id = str(ind.get('number', ''))
            auths = ind.get('authors_matched', [])
            indic_author_map[i_id] = auths
            
        # PREPARE PROMPT
        # We send the current draft state + indications
        prompt = f"""
ROL: Secretario Técnico.
TAREA: Aplica las INDICACIONES al BORRADOR.
REGLAS:
1. Retorna un JSON Array con objetos 'ArticleUpdate'.
2. Si una indicación SUPRIME un artículo, marca status="deleted" y content="".
3. MANTÉN el 'original_id' de cada artículo.
4. En 'applied_indication_ids', lista SOLO los números de las indicaciones que causaron un cambio en ESTE paso.

BORRADOR ACTUAL:
{json.dumps(draft, ensure_ascii=False, indent=2)}

INDICACIONES:
{json.dumps(indications_data, ensure_ascii=False, indent=2)}
"""

        # CALL GEMINI WITH RETRY
        response_json = None
        for attempt in range(MAX_RETRIES):
            try:
                response = model.generate_content(
                    prompt,
                    generation_config=GenerationConfig(
                        response_mime_type="application/json",
                        response_schema=build_schema()
                    )
                )
                response_json = json.loads(response.text)
                break
            except Exception as e:
                wait_time = 2 ** attempt
                print(f"   [Intento {attempt+1}] Error: {str(e)[:100]}... Reintentando en {wait_time}s")
                time.sleep(wait_time)
        
        if not response_json:
            print(f"FATAL: Fallaron todos los intentos para {fname}. Saltando comisión.")
            break
            
        # POST-PROCESS (The Hybrid Logic)
        # Update 'draft' with new content AND inject authors
        
        new_draft_map = {item['original_id']: item for item in response_json}
        
        # Reconstruct strict list preserving order or handling insertions?
        # The AI returns a list. Use that as the new draft order (it might have reordered/inserted).
        
        updated_draft_list = []
        for item in response_json:
            # Get authors for the applied indications
            applied_ids = item.get('applied_indication_ids', [])
            new_authors = set()
            for aid in applied_ids:
                if aid in indic_author_map:
                    new_authors.update(indic_author_map[aid])
            
            # Find previous state to preserve genesis authors
            # We look in the 'draft' (previous step) list
            # We must find by original_id
            prev_art = next((x for x in draft if x.get('original_id') == item['original_id']), None)
            
            # Create final object
            final_obj = item.copy() # Contains content, status, current_number
            
            # Carry over Genesis Authors
            if prev_art:
                final_obj['authors_genesis'] = prev_art.get('authors_genesis', [])
                # Carry over previous indications authors (history)
                # We iterate keys to find existing 'authors_indications_X'
                for k, v in prev_art.items():
                    if k.startswith('authors_indications_'):
                        final_obj[k] = v
            else:
                # New article inserted by indication?
                final_obj['authors_genesis'] = []

            # Add NEW authors for THIS time slice
            if new_authors:
                key = f"authors_indications_{time_slice}"
                # If key exists (rare, same slice multiple files?), merge. Else set.
                if key in final_obj:
                     existing = set(final_obj[key])
                     final_obj[key] = list(existing.union(new_authors))
                else:
                    final_obj[key] = list(new_authors)
            
            updated_draft_list.append(final_obj)
            
        # Update Main Loop variable
        draft = updated_draft_list
        
        # Save Checkpoint
        out_name = f"draft_after_{fname}"
        with open(os.path.join(out_dir, out_name), 'w', encoding='utf-8') as f:
            json.dump(draft, f, ensure_ascii=False, indent=2)
            
        print(f"   -> Guardado {out_name}")
        time.sleep(2) # Politeness

# ... (Main entry point setup) ...

def main():
    if setup_gemini():
        try:
            model = genai.GenerativeModel("gemini-3-pro-preview") 
            print(f"Modelo: gemini-3-pro-preview (Configurado para JSON Output)")
        except:
             return
             
        for c in TARGET_COMISSIONS:
            process_commission(c, model)

if __name__ == "__main__":
    main()
