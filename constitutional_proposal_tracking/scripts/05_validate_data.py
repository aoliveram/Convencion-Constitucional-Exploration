
import os
import json
import glob
import re
from typing import List, Dict, Any

# --- Configuration ---
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROPOSALS_DIR = os.path.join(BASE_DIR, "proposals")
FINAL_TEXT_PATH = os.path.join(BASE_DIR, "proposals", "draft_final_text.json")

def load_json(path):
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)

def normalize_article_id(art_str: str) -> str:
    # Extracts number from "Artículo 12", "Art 12", etc.
    match = re.search(r'(\d+)', str(art_str))
    return match.group(1) if match else str(art_str)

def main():
    print("--- Starting Data Validation & Sanity Check ---")
    
    # 1. Load Reference (Final Draft)
    if not os.path.exists(FINAL_TEXT_PATH):
        print("Error: draft_final_text.json not found.")
        return
        
    final_draft = load_json(FINAL_TEXT_PATH)
    valid_article_ids = {str(a.get("article_id")) for a in final_draft}
    
    # 2. Find all extracted indication files
    files = glob.glob(os.path.join(PROPOSALS_DIR, "extracted_informe-indicaciones-*.json"))
    print(f"Validating {len(files)} extraction files...")
    
    total_indications = 0
    issues = []

    for file_path in sorted(files):
        fname = os.path.basename(file_path)
        data = load_json(file_path)
        
        seen_numbers = set()
        
        for idx, entry in enumerate(data):
            total_indications += 1
            ind_num = entry.get("number", "Unknown")
            art_raw = entry.get("target_article", "Missing")
            art_id = normalize_article_id(art_raw)
            action = entry.get("action")
            authors = entry.get("authors_matched", [])
            content = entry.get("content", "")

            # Issue: Duplicate Number in same file
            if ind_num in seen_numbers and ind_num != "Unknown":
                issues.append({
                    "file": fname,
                    "ind": ind_num,
                    "type": "DUPLICATE_NUMBER",
                    "msg": f"Indication {ind_num} appearing multiple times in file."
                })
            seen_numbers.add(ind_num)

            # Issue: Non-existent Article
            if art_id not in valid_article_ids and action != "ADD":
                issues.append({
                    "file": fname,
                    "ind": ind_num,
                    "type": "GHOST_ARTICLE",
                    "msg": f"Refers to '{art_raw}' (ID:{art_id}) which is not in final draft."
                })

            # Issue: Empty Authors
            if not authors:
                issues.append({
                    "file": fname,
                    "ind": ind_num,
                    "type": "EMPTY_AUTHORS",
                    "msg": f"No authors matched for indication {ind_num}."
                })
                
            # Issue: Unusual Action
            if action not in ["ADD", "DELETE", "MODIFY"]:
                issues.append({
                    "file": fname,
                    "ind": ind_num,
                    "type": "WEIRD_ACTION",
                    "msg": f"Action '{action}' is not one of ADD|DELETE|MODIFY."
                })

            # Issue: Empty Content
            if not content or len(str(content)) < 5:
                issues.append({
                    "file": fname,
                    "ind": ind_num,
                    "type": "EMPTY_CONTENT",
                    "msg": f"Content for indication {ind_num} is suspiciously short."
                })

    # 3. Summary Report
    print(f"\nScan complete. Total Indications Checked: {total_indications}")
    print(f"Total Issues Found: {len(issues)}\n")
    
    if issues:
        # Group by type for better reading
        by_type = {}
        for i in issues:
            t = i["type"]
            if t not in by_type: by_type[t] = []
            by_type[t].append(i)
            
        for t, items in by_type.items():
            print(f"--- {t} ({len(items)}) ---")
            for item in items[:10]: # Limit to first 10 per type to avoid spam
                print(f"  [{item['file']}] Ind {item['ind']}: {item['msg']}")
            if len(items) > 10:
                print(f"  ... and {len(items)-10} more.")
    else:
        print("Everything looks consistent! No issues found using current rules.")

    # 4. Save validation report
    report_path = os.path.join(BASE_DIR, "reports", "validation_report.json")
    with open(report_path, 'w', encoding='utf-8') as f:
        json.dump(issues, f, indent=2)
    print(f"\nDetailed report saved to {report_path}")

if __name__ == "__main__":
    main()
