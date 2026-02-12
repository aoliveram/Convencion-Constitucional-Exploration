import os
import json
import re
from difflib import SequenceMatcher

# --- Configuration ---
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
GENESIS_PATH = os.path.join(BASE_DIR, "comision-2", "genesis-extracted", "C2_GENESIS_texto-sistematizado-02-16.json")
REPORT_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "indications_report_1_full.json")

def load_json(path):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    print(f"File not found: {path}")
    return []

def normalize_article_num(text):
    if not text: return "Unknown"
    match = re.search(r'\d+[A-Za-z]?', str(text))
    if match: return match.group(0)
    return "Unknown"

def main():
    print("--- Auditing Alignment: Genesis (Feb 16) vs Report 1 (Mar 02) ---")
    
    genesis_data = load_json(GENESIS_PATH)
    indications = load_json(REPORT_PATH)
    
    # Map Genesis by Article Number
    genesis_map = {}
    for item in genesis_data:
        key = normalize_article_num(item.get("article"))
        genesis_map[key] = item.get("text", "")
        
    print(f"Genesis Articles: {len(genesis_map)} keys loaded.")
    
    matches = 0
    mismatches = 0
    missing_keys = 0
    
    print("\n--- Checking Sample Alignments ---")
    
    for i, ind in enumerate(indications):
        target_ref = ind.get("target_article_ref")
        target_key = normalize_article_num(target_ref)
        pdf_snippet = ind.get("target_column_1_text", "")
        
        # Try to detect Renumbering: Look for "(Artículo X)" pattern in the snippets
        # Pattern: "Artículo 23.- (Artículo 12) ..."
        remapped_key = None
        renum_match = re.search(r'\(Art[ií]culo\s+(\d+)\)', pdf_snippet, re.IGNORECASE)
        
        lookup_key = target_key
        if renum_match:
            remapped_key = renum_match.group(1)
            lookup_key = remapped_key
            
        gen_text = genesis_map.get(lookup_key)
        
        if gen_text:
            # Compare snippet vs start of genesis text
            # Normalize strings roughly
            s1 = pdf_snippet.lower().replace('\n', ' ')
            # Remove the header part "Artículo 23.- (Artículo 12)" from comparison to focus on content
            s1_content = re.sub(r'^.*?\)','', s1).strip()[:50]
            if not s1_content: s1_content = s1[:50] # Fallback
            
            s2 = gen_text.lower().replace('\n', ' ')[:50]
            
            ratio = SequenceMatcher(None, s1_content, s2).ratio()
            
            if ratio > 0.5: # Slightly lower threshold for fuzzy content match
                matches += 1
                if remapped_key:
                    print(f"[OK-REMAP] Ind {ind.get('number')} (Target {target_key}) -> Remapped to Genesis Art {remapped_key}")
                else:
                    pass # Silent OK for direct matches
            else:
                mismatches += 1
                print(f"[MISMATCH] Ind {ind.get('number')} targets Art {target_key} (Lookup {lookup_key}).")
                print(f"  PDF Content: '{s1_content}...'")
                print(f"  Genesis Says: '{s2}...'")
                print("-" * 40)
        else:
            missing_keys += 1
            print(f"[MISSING] Ind {ind.get('number')} targets Art {target_key} (Lookup {lookup_key}) NOT FOUND in Genesis.")
            
    print("\n--- Audit Summary ---")
    print(f"Total Indications Checkable: {matches + mismatches + missing_keys}")
    print(f"Passed (Aligned): {matches}")
    print(f"Failed (Text Mismatch): {mismatches}")
    print(f"Failed (Article Not Found): {missing_keys}")
    
    if mismatches > 5:
        print("\nCONCLUSION: High mismatch rate. The 'Genesis' baseline does NOT align with the Report 1 'Systematized Text'.")
        print("Possible causes: Renumbering, different source versions, or extraction errors.")
    else:
        print("\nCONCLUSION: Alignment looks GOOD. Proceed with patching.")

if __name__ == "__main__":
    main()
