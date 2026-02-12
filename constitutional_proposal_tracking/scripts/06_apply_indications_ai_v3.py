
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

# Target Commissions: Start with 7 as requested for testing, then expand.
TARGET_COMISSIONS = [7] 
# TARGET_COMISSIONS = [1, 3, 4, 5, 6, 7]

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
    Returns int or string representing the chronological step.
    """
    m1 = re.search(r'informe-indicaciones-(\d+)', filename)
    if m1: return int(m1.group(1))
    
    m2 = re.search(r'informe-(\d+)', filename)
    if m2: return int(m2.group(1))
    
    return "unknown"

def get_files_ordered(com_n):
    """
    Returns (genesis_path, list_of_indication_paths)
    """
    com_dir = os.path.join(BASE_DIR, f"comision-{com_n}")
    
    # 1. Find Genesis Enriched
    g_pattern = os.path.join(com_dir, "genesis-extracted", f"C{com_n}_GENESIS_*_enriched.json")
    g_files = glob.glob(g_pattern)
    genesis_file = g_files[0] if g_files else None
    
    # 2. Find Indication Files
    i_pattern = os.path.join(com_dir, "indicaciones-universal-extracted", f"C{com_n}_VOTACION_*indicaciones*.json")
    indic_files = glob.glob(i_pattern)
    indic_files.sort()
    
    return genesis_file, indic_files

def build_schema():
    """
    Defines the Output JSON schema for Gemini.
    We ask for a FLAT list of updates.
    """
    return {
        "type": "array",
        "items": {
            "type": "object",
            "properties": {
                "original_id": { "type": "string", "description": "El ID original (G-X) del artículo modificado." },
                "current_number": { "type": "string", "description": "Nuevo número del artículo (ej: '5', '5 bis', 'S/N')." },
                "content": { "type": "string", "description": "Nuevo contenido jurídico tras aplicar indicación. Vacío si deleted." },
                "status": { "type": "string", "enum": ["active", "deleted", "merged"], "description": "Estado resultante." },
                "applied_indication_ids": {
                    "type": "array",
                    "items": { "type": "string" },
                    "description": "Lista de 'number' de las indicaciones que causaron este cambio."
                }
            },
            "required": ["original_id", "content", "status", "applied_indication_ids", "current_number"]
        }
    }

def initialize_genesis_with_history(genesis_path):
    """
    Loads genesis JSON and transforms it into the Robust History Structure.
    """
    with open(genesis_path, 'r', encoding='utf-8') as f:
        raw_data = json.load(f)
        
    structured_draft = []
    
    for idx, item in enumerate(raw_data):
        # Create robust ID
        gid = f"G-{idx+1}"
        
        # Extract initial data
        content = item.get('content', '') or item.get('text', '')
        # Genesis authors might be in 'authors', 'authors_genesis', or 'sources'
        gen_authors = item.get('authors_genesis', item.get('authors', []))
        sources = item.get('sources', [])
        
        # Build the Robust Object
        article_obj = {
            "original_id": gid,
            "current_number": item.get('article', str(idx+1)),
            "status": "active",
            "final_content": content,
            "accumulated_authors": list(set(gen_authors)), # Start with genesis authors
            
            # The History Log
            "history": [
                {
                    "step": "Genesis",
                    "timestamp": "initial",
                    "action": "CREATE",
                    "content_snapshot": content,
                    "authors_involved": gen_authors,
                    "source_ids": sources
                }
            ]
        }
        structured_draft.append(article_obj)
        
    return structured_draft

def create_sparse_draft(full_draft):
    """
    Creates a lightweight version of the draft for the Prompt context.
    Excludes history logs and authorship metadata to save tokens.
    """
    sparse = []
    for art in full_draft:
        # Only include active articles in context? 
        # Ideally YES, but if we need to revive a deleted one, we might need it.
        # However, for simplicity and token limits, usually context includes currently active + maybe recently deleted.
        # Strategy: Include ALL, but mark deleted clearly so AI knows they exist but are gone.
        
        sparse.append({
            "original_id": art['original_id'],
            "current_number": art['current_number'],
            "status": art['status'],
            "content": art['final_content']
        })
    return sparse

def process_commission(com_n, model):
    print(f"\n=== COMISIÓN {com_n} (Estrategia Historial Incrustado) ===")
    
    genesis_path, indic_files = get_files_ordered(com_n)
    if not genesis_path:
        print(f"[C{com_n}] No enriched genesis file found.")
        return

    # OUTPUT DIRECTORY
    out_dir = os.path.join(BASE_DIR, f"comision-{com_n}", "draft-after-indications")
    os.makedirs(out_dir, exist_ok=True)
    
    # 1. INITIALIZE MASTER DRAFT
    print(f"[C{com_n}] Inicializando Master Draft desde Génesis...")
    master_draft = initialize_genesis_with_history(genesis_path)
    
    # Save Step 0
    with open(os.path.join(out_dir, "draft_00_genesis_master.json"), 'w', encoding='utf-8') as f:
        json.dump(master_draft, f, ensure_ascii=False, indent=2)

    # 2. ITERATE INDICATIONS
    for step_idx, indic_path in enumerate(indic_files):
        fname = os.path.basename(indic_path)
        time_slice = extract_time_slice(fname)
        step_label = f"Informe-{time_slice}"
        
        print(f"\n[C{com_n}] Procesando {fname} (Slice {time_slice})...")
        
        # Load Indications
        with open(indic_path, 'r', encoding='utf-8') as f:
            indications_data = json.load(f)
            
        # Map Indication Authors per ID
        indic_author_map = {}
        for ind in indications_data:
            num = str(ind.get('number', ''))
            auths = ind.get('authors_matched', [])
            indic_author_map[num] = auths
            
        # Prepare Prompt Context (Sparse)
        sparse_context = create_sparse_draft(master_draft)
        
        prompt = f"""
ROL: Secretario Técnico Convención Constitucional.
TAREA: Aplica las INDICACIONES al BORRADOR y genera la lista de actualizaciones.

INPUT:
1. BORRADOR ACTUAL (Lista simplificada para contexto):
{json.dumps(sparse_context, ensure_ascii=False, indent=2)}

2. INDICACIONES APROBADAS:
{json.dumps(indications_data, ensure_ascii=False, indent=2)}

INSTRUCCIONES CRÍTICAS:
1. Analiza CADA indicación y encuentra su artículo objetivo en el Borrador (por número o contenido).
2. Genera un objeto 'ArticleUpdate' SOLO para los artículos que sufren cambios (contenido, estado o numeración).
3. Si un artículo NO cambia, NO lo incluyas en la respuesta (para ahorrar output).
4. Si la indicación SUPRIME un artículo: retorna status="deleted", content="" y mantén su 'original_id'.
5. Si la indicación AGREGA un artículo NUEVO que no existía: inventa un 'original_id' único (ej: "NEW-1"), pon status="active".
6. 'applied_indication_ids': Lista estricta de los números de indicación usados.

SALIDA ESPERADA: JSON Array de actualizaciones únicamente.
"""
        
        # Call Gemini
        updates_received = []
        for attempt in range(MAX_RETRIES):
            try:
                response = model.generate_content(
                    prompt,
                    generation_config=GenerationConfig(
                        response_mime_type="application/json",
                        response_schema=build_schema()
                    )
                )
                updates_received = json.loads(response.text)
                break
            except Exception as e:
                wait_time = 2 ** attempt
                print(f"   [Intento {attempt+1}] Error: {str(e)[:100]}... Esperando {wait_time}s")
                time.sleep(wait_time)
        
        if updates_received is None: # None indicates failure after all retries
             print(f"FATAL: Fallo en {fname}. Abortando cadena de esta comisión.")
             break

        # 3. MERGE / UPDATE MASTER DRAFT (The Python Logic)
        print(f"   -> Recibidos {len(updates_received)} cambios desde AI.")
        
        # Index updates by ID for fast lookup
        update_map = {u['original_id']: u for u in updates_received}
        
        # A. Update Existing Articles
        for article in master_draft:
            gid = article['original_id']
            if gid in update_map:
                upd = update_map[gid]
                
                # Check for Authors
                img_ids = upd.get('applied_indication_ids', [])
                new_authors = set()
                for iid in img_ids:
                    if iid in indic_author_map:
                        new_authors.update(indic_author_map[iid])
                
                # Update Root Fields
                article['status'] = upd['status']
                article['current_number'] = upd['current_number']
                article['final_content'] = upd['content']
                
                # Update Accumulators
                current_acc = set(article.get('accumulated_authors', []))
                current_acc.update(new_authors)
                article['accumulated_authors'] = list(current_acc)
                
                # Append History Log
                log_entry = {
                    "step": step_label,
                    "filename": fname,
                    "action": "UPDATE" if article['status'] == 'active' else "DELETE",
                    "content_snapshot": upd['content'],
                    "applied_indications": img_ids,
                    "authors_involved": list(new_authors),
                    "timestamp": datetime.now().isoformat()
                }
                article['history'].append(log_entry)
                
                # Mark as processed in map to detect New Articles later
                del update_map[gid]
                
            else:
                # No change reported by AI -> Implicitly "Keep Previous State"
                # We do NOT add a history entry for "No Change" to keep log clean.
                pass
                
        # B. Handle New Articles (Additions)
        # Any items left in update_map are NEW insertions created by AI (IDs like "NEW-X")
        for new_id, upd in update_map.items():
            # Authors logic
            img_ids = upd.get('applied_indication_ids', [])
            new_authors = set()
            for iid in img_ids:
                if iid in indic_author_map:
                    new_authors.update(indic_author_map[iid])
            
            # Create New Object
            new_obj = {
                "original_id": new_id, # Keep the ID assigned by AI or generate one
                "current_number": upd['current_number'],
                "status": upd['status'],
                "final_content": upd['content'],
                "accumulated_authors": list(new_authors),
                "history": [
                    {
                        "step": step_label,
                        "filename": fname,
                        "action": "CREATE_NEW",
                        "content_snapshot": upd['content'],
                        "applied_indications": img_ids,
                        "authors_involved": list(new_authors),
                        "timestamp": datetime.now().isoformat()
                    }
                ]
            }
            master_draft.append(new_obj)
            print(f"   -> Insertado NUEVO artículo: {new_id} ({upd['current_number']})")

        # 4. SAVE CHECKPOINT
        out_name = f"draft_after_{fname}"
        with open(os.path.join(out_dir, out_name), 'w', encoding='utf-8') as f:
            json.dump(master_draft, f, ensure_ascii=False, indent=2)
            
        print(f"   -> Checkpoint guardado: {out_name}")
        time.sleep(2)

def main():
    if setup_gemini():
        print(f"Modelo Configurado: {MODEL_NAME}")
        try:
            model = genai.GenerativeModel(MODEL_NAME)
        except Exception as e:
            print(f"Error inicializando modelo: {e}")
            return

        for c in TARGET_COMISSIONS:
            process_commission(c, model)

if __name__ == "__main__":
    main()
