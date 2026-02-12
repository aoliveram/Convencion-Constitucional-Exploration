import json
import os
import difflib

# Paths
BASE_DIR = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking/comision-2"
GENESIS_FILE = os.path.join(BASE_DIR, "reconstructed/C2_GENESIS_texto-sistematizado-03-02.json")
TARGET_FILE = os.path.join(BASE_DIR, "reconstructed/C2_DRAFT_texto-sistematizado-04-08.json")
TEMPLATE_FILE = os.path.join(BASE_DIR, "reconstructed/C2_MANUAL_MAPPING_template.json")
RANKING_FILE = os.path.join(BASE_DIR, "reconstructed/C2_MAPPING_candidates_rankings.json")
INDICATIONS_FILE = os.path.join(BASE_DIR, "reconstructed/C2_INDICATIONS_04_08_candidates.json")

OUTPUT_FILE = os.path.join(BASE_DIR, "reconstructed/C2_GENESIS_texto_sistematizado-04-08.json")
DELETED_REPORT_FILE = os.path.join(BASE_DIR, "reconstructed/C2_DELETED_ARTICLES_analysis.json")

def load_json(path):
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)

def find_best_match_in_genesis(target_article, genesis_articles):
    """
    Tries to find the genesis article that matches the target.
    Logic:
    1. Exact Title Match
    2. High Text Similarity
    """
    target_title = target_article['article']
    target_text = target_article['text']
    
    # 1. Exact Title Match
    for g_idx, g in enumerate(genesis_articles):
        if g['article'].strip() == target_title.strip():
            return g_idx, g
            
    # 2. Fuzzy Text Match? (Optional, but 99 implies we should look harder)
    # Using python's difflib for a quick check
    best_score = 0
    best_idx = -1
    
    for g_idx, g in enumerate(genesis_articles):
        score = difflib.SequenceMatcher(None, target_text, g['text']).ratio()
        if score > best_score:
            best_score = score
            best_idx = g_idx
            
    if best_score > 0.6: # Threshold
        return best_idx, genesis_articles[best_idx]
        
    return None, None

def main():
    print("Loading files...")
    genesis_articles = load_json(GENESIS_FILE)
    target_articles = load_json(TARGET_FILE)
    mapping_data = load_json(TEMPLATE_FILE)
    rankings_data = load_json(RANKING_FILE)
    
    print(f"Loaded {len(genesis_articles)} genesis articles.")
    print(f"Loaded {len(target_articles)} target articles.")
    
    # Track mapped genesis IDs
    mapped_genesis_ids = set()
    
    validated_articles = []
    
    # Iterate through manual mapping
    # Note: mapping_data has 'index' 1..41, assuming it aligns with target_articles list order 0..40
    
    for i, map_entry in enumerate(mapping_data):
        target = target_articles[i] # Current target article
        
        # Verify alignment just in case
        # map_entry['target_article'] should closely match target['article']
        # Normalized check
        if map_entry['target_article'].split('.-')[0] != target['article'].split('.-')[0]:
            print(f"Warning: Alignment mismatch at index {i}. Template: {map_entry['target_article']} vs Target: {target['article']}")
        
        mapped_ids = map_entry.get('mapped_source_article_ids', [])
        
        provenance = {
            "type": "New Article",
            "source_ids": [],
            "source_titles": [],
            "notes": "Generated during 04-08 consolidation"
        }
        
        if mapped_ids:
            provenance['type'] = "Mapped from Genesis (03-02)"
            
            for code in mapped_ids:
                mapped_genesis_idx = None
                mapped_genesis_obj = None
                
                if code == 99:
                    # Search globally in Genesis
                    print(f"Processing manual code [99] for target: {target['article']}")
                    idx, obj = find_best_match_in_genesis(target, genesis_articles)
                    if obj:
                        print(f"  -> Found match: ID {idx} | {obj['article']}")
                        mapped_genesis_idx = idx
                        mapped_genesis_obj = obj
                    else:
                        print(f"  -> NO MATCH FOUND for [99]. Marking as Unresolved.")
                        provenance['notes'] += "; Unresolved [99] code"
                        
                elif 1 <= code <= 5:
                    # Get from Ranking Candidates
                    # Get the ranking entry for this target (assuming 1:1 order)
                    ranking_entry = rankings_data[i]
                    candidates_list = ranking_entry.get('top_candidates', [])
                    
                    if len(candidates_list) > (code - 1):
                        selected_cand_data = candidates_list[code - 1]
                        cand_title = selected_cand_data['candidate_article']
                        cand_text = selected_cand_data['candidate_text_snippet'] 
                        
                        # Find this candidate in the REAL Genesis file to getting its ID
                        # Match by Title AND Text (robust) or just Title
                        found = False
                        for g_idx, g in enumerate(genesis_articles):
                            # Clean titles for comparison
                            if g['article'].strip() == cand_title.strip():
                                # Check text snippet overlap to be sure (first 50 chars)
                                if g['text'].strip()[:50] in cand_text.strip()[:60] or cand_text.strip()[:50] in g['text'].strip()[:60]:
                                    mapped_genesis_idx = g_idx
                                    mapped_genesis_obj = g
                                    found = True
                                    break
                                    
                        if not found:
                            # Fallback: title only
                             for g_idx, g in enumerate(genesis_articles):
                                if g['article'].strip() == cand_title.strip():
                                    mapped_genesis_idx = g_idx
                                    mapped_genesis_obj = g
                                    found = True
                                    print(f"  -> Warn: Match found by Title Only for {cand_title}")
                                    break
                                    
                        if not found:
                             print(f"  -> Error: Could not locate candidate '{cand_title}' in Genesis file.")
                             
                    else:
                         print(f"  -> Error: Index {code} out of bounds for ranking candidates list.")

                if mapped_genesis_idx is not None:
                    mapped_genesis_ids.add(mapped_genesis_idx)
                    provenance['source_ids'].append(mapped_genesis_idx)
                    provenance['source_titles'].append(mapped_genesis_obj['article'])
        
        # Construct final article object
        final_obj = target.copy()
        
        # Append to history
        if 'history' not in final_obj:
            final_obj['history'] = []
            
        final_obj['history'].append({
            "step": "Consolidation 04-08",
            "provenance": provenance
        })
        
        # Also add easy-access fields
        final_obj['genesis_source_ids'] = provenance['source_ids']
        
        validated_articles.append(final_obj)

    # Save Main File
    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        json.dump(validated_articles, f, indent=2, ensure_ascii=False)
    print(f"Saved consolidated articles to {OUTPUT_FILE}")
    
    # ---------------------------------------------------------
    # PART 2: Deleted Articles Analysis
    # ---------------------------------------------------------
    
    # Load Indications if available
    try:
        indications = load_json(INDICATIONS_FILE)
    except:
        indications = []
        print("Warning: Indications file not found or empty.")

    deleted_report = []
    
    for idx, g in enumerate(genesis_articles):
        if idx not in mapped_genesis_ids:
            # It's unmapped. Potentially deleted.
            status = "Unmapped"
            evidence = []
            
            # Check Indications for "Suprimir" + Article Title match
            for ind in indications:
                # Naive matching: "suprimir" in indication text AND article title in description
                # Note: Indications JSON structure needs to be checked. Assuming standard list.
                desc = str(ind).lower() # Convert whole obj to string for broad search or check specific fields
                # More precise: check specific text field if known
                ind_text = ind.get('indication_text', '') or ind.get('text', '')
                ind_type = ind.get('type', '')
                
                if 'suprimir' in ind_type.lower() or 'suprimir' in ind_text.lower():
                     # Does it mention this article?
                     # g['article'] is like "Artículo 5.- ..."
                     # Extract number "Artículo 5"
                     short_title = g['article'].split('.-')[0] if '.-' in g['article'] else g['article'].split('.')[0]
                     
                     if short_title.lower() in ind_text.lower():
                         evidence.append(f"Indication match: {ind_text[:100]}...")
            
            if evidence:
                status = "Deleted (Evidence Found)"
            else:
                status = "Deleted (Inferred - No Mapping)"
                
            deleted_report.append({
                "genesis_id": idx,
                "genesis_article": g['article'],
                "genesis_text": g['text'],
                "status": status,
                "evidence": evidence
            })
            
    # Save Deleted Report
    with open(DELETED_REPORT_FILE, 'w', encoding='utf-8') as f:
        json.dump(deleted_report, f, indent=2, ensure_ascii=False)
    print(f"Saved deleted articles report to {DELETED_REPORT_FILE} ({len(deleted_report)} entries)")

if __name__ == "__main__":
    main()
