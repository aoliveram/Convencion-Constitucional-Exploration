import os
import json

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
COMPLETE_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "C2_COMPLEX_informe-reemplazo-1-03-02_COMPLETE.json")
PART6_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "indications_report_1_part6_manual.json")

def load_json(path):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    return []

def main():
    print("--- Final Merge: Adding Part 6 to COMPLETE Report ---")
    
    current_complete = load_json(COMPLETE_PATH)
    part6_data = load_json(PART6_PATH)
    
    merged = current_complete + part6_data
    
    # Deduplicate by base_text hash to avoid overlaps
    seen = set()
    final_list = []
    for item in merged:
        # Key: first 100 chars of base text + ref
        key = (item.get("article_ref", "") + item.get("base_text", ""))[:150]
        if key not in seen:
            seen.add(key)
            final_list.append(item)
            
    print(f"Current blocks: {len(current_complete)}")
    print(f"New blocks from Part 6: {len(part6_data)}")
    print(f"Final Total Unique Blocks: {len(final_list)}")
    
    with open(COMPLETE_PATH, 'w', encoding='utf-8') as f:
        json.dump(final_list, f, ensure_ascii=False, indent=2)
        
    print(f"Saved update to {COMPLETE_PATH}")

if __name__ == "__main__":
    main()
