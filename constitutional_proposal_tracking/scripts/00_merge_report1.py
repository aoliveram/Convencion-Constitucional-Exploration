import os
import json

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PART1_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "C2_COMPLEX_informe-reemplazo-1-03-02.json")
PART2_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "indications_report_1_part5_6.json")
OUTPUT_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "C2_COMPLEX_informe-reemplazo-1-03-02_COMPLETE.json")

def load_json(path):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    print(f"Warning: {path} not found.")
    return []

def main():
    print("--- Merging Extracted Report Parts ---")
    
    data1 = load_json(PART1_PATH)
    data2 = load_json(PART2_PATH)
    
    # Simple list concatenation
    merged = data1 + data2
    
    # Deduplicate? Sometimes chunks overlap or user logic repeats.
    # Let's deduplicate by content hash roughly just in case
    seen = set()
    unique_merged = []
    
    for item in merged:
        # Create a unique key based on base text + number of indications
        key = item.get("base_text", "")[:50] + str(len(item.get("indications", [])))
        if key not in seen:
            seen.add(key)
            unique_merged.append(item)
    
    print(f"Part 1: {len(data1)} blocks")
    print(f"Part 2: {len(data2)} blocks")
    print(f"Total Unique: {len(unique_merged)} blocks")
    
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(unique_merged, f, ensure_ascii=False, indent=2)
        
    print(f"Saved merged report to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
