import os
import json
import re
from difflib import SequenceMatcher

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OLD_GENESIS_PATH = os.path.join(BASE_DIR, "comision-2", "genesis-extracted", "C2_GENESIS_texto-sistematizado-02-16.json")
NEW_REPORT_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "C2_COMPLEX_informe-reemplazo-1-03-02_COMPLETE.json")

def load_json(path):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    return []

def normalize_article_num(text):
    if not text: return "Unknown"
    match = re.search(r'\d+[A-Za-z]?', str(text))
    if match: return match.group(0)
    return "Unknown"

def main():
    print("--- Comparing Old Genesis (Feb 16) vs New Extracted Base (Mar 02) ---")
    
    old_gen = load_json(OLD_GENESIS_PATH)
    new_rep = load_json(NEW_REPORT_PATH)
    
    # 1. Map Old Genesis
    old_map = {}
    for item in old_gen:
        k = normalize_article_num(item.get("article"))
        old_map[k] = item.get("text", "")
        
    print(f"Old Genesis: {len(old_map)} articles.")
    print(f"New Report: {len(new_rep)} blocks found.")
    
    matches = 0
    mismatches = 0
    new_articles = 0
    
    print("\n--- Differences ---")
    
    # Iterate through New Report Blocks
    for item in new_rep:
        ref = item.get("article_ref")
        num = normalize_article_num(ref)
        base_text = item.get("base_text", "")
        
        if not base_text: continue
        
        if num in old_map:
            old_text = old_map[num]
            
            # Compare
            s1 = base_text[:100].lower().replace('\n', ' ')
            s2 = old_text[:100].lower().replace('\n', ' ')
            
            ratio = SequenceMatcher(None, s1, s2).ratio()
            
            if ratio > 0.6:
                matches += 1
            else:
                mismatches += 1
                if mismatches <= 10: # Limit log
                    print(f"[MISMATCH] Art {num}")
                    print(f"  New (PDF): '{s1}...'")
                    print(f"  Old (Gen): '{s2}...'")
        else:
            new_articles += 1
            # print(f"[NEW] Art {num} found in Report 1 but not in Old Genesis.")
            
    print("\n--- Summary ---")
    print(f"Aligned Articles: {matches}")
    print(f"Mismatched Text: {mismatches}")
    print(f"New/Unknown Articles: {new_articles}")
    
    if mismatches > 10:
        print("\nCONCLUSION: Significant divergence. The New Report Base should replaces the Old Genesis.")

if __name__ == "__main__":
    main()
