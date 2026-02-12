import os
import json
import re
from difflib import SequenceMatcher

# --- Configuration ---
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
GENESIS_PATH = os.path.join(BASE_DIR, "comision-2", "genesis-extracted", "C2_GENESIS_texto-sistematizado-02-16.json")
INDICATIONS_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "indications_report_1_full.json")
OUTPUT_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "C2_DRAFT_texto-sistematizado-03-02.json")

def load_json(path):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    print(f"Warning: {path} not found.")
    return []

def normalize_article_num(text):
    if not text: return "Unknown"
    match = re.search(r'\d+[A-Za-z]?', str(text)) # Matches "14", "14A", "1"
    if match:
        return match.group(0)
    return "Unknown"

def main():
    print("--- Generating Intermediate Draft (C2_DRAFT - Mar 02) ---")
    
    # 1. Load Baseline (Genesis)
    genesis_data = load_json(GENESIS_PATH) # List of dicts
    print(f"Loaded {len(genesis_data)} articles from Genesis.")
    
    # Convert to Dict for easy access
    draft_map = {}
    for item in genesis_data:
        # Key: "14", "1A"
        key = normalize_article_num(item.get("article"))
        draft_map[key] = item
        
    # 2. Load Changes (Indications)
    indications = load_json(INDICATIONS_PATH)
    print(f"Loaded {len(indications)} indications to apply.")
    
    applied_count = 0
    errors = 0
    
    for ind in indications:
        target_ref = ind.get("target_article_ref")
        action = ind.get("action", "MODIFY").upper()
        content = ind.get("content", "")
        ind_num = ind.get("number")
        
        target_key = normalize_article_num(target_ref)
        
        # Patch Logic
        if action == "DELETE":
            if target_key in draft_map:
                print(f"  [DELETE] Ind {ind_num}: Removing Article {target_key}")
                del draft_map[target_key]
                applied_count += 1
            else:
                # Fuzzy search? Or just log
                print(f"  [WARN] Ind {ind_num} tried to DELETE {target_key} but not found.")
                errors += 1
                
        elif action == "ADD":
            # Heuristic: If key exists, it might be an ADDITION aka inserting a paragraph, 
            # OR a collision. Usually ADD means 'New Article'.
            if target_key in draft_map:
                # If content is short, maybe it's a paragraph addition?
                # For simplicity in this 'draft' construction, we will APPEND content.
                print(f"  [ADD/APPEND] Ind {ind_num}: Appending to Article {target_key}")
                draft_map[target_key]["text"] += "\n" + content
                applied_count += 1
            else:
                 print(f"  [NEW] Ind {ind_num}: Creating Article {target_key}")
                 draft_map[target_key] = {
                     "article": f"Artículo {target_key}",
                     "title": "New Article (From Report 1)",
                     "text": content,
                     "source_indication": ind_num
                 }
                 applied_count += 1
                 
        elif action == "MODIFY":
            if target_key in draft_map:
                print(f"  [MODIFY] Ind {ind_num}: Updating Article {target_key}")
                draft_map[target_key]["text"] = content # Full replacement usually
                applied_count += 1
            else:
                 print(f"  [WARN] Ind {ind_num} tried to MODIFY {target_key} but not found.")
                 errors +=1
                 
    # 3. Export
    # Sort keys numerically if possible for cleanness
    sorted_keys = sorted(draft_map.keys(), key=lambda x: int(re.search(r'\d+', x).group()) if re.search(r'\d+', x) else 9999)
    
    final_draft_list = [draft_map[k] for k in sorted_keys]
    
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(final_draft_list, f, ensure_ascii=False, indent=2)
        
    print(f"\nApplied {applied_count} changes. Warning/Skips: {errors}")
    print(f"Saved Intermediate Draft with {len(final_draft_list)} articles to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
