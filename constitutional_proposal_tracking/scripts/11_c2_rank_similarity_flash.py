import json
import os
import google.generativeai as genai
import time

# Configuration
API_KEY = os.environ.get("GEMINI_API_KEY")
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

BASE_DIR = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking"
INPUT_TARGET_04_08 = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_DRAFT_texto-sistematizado-04-08.json")
INPUT_CANDIDATES_03_02 = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_GENESIS_texto-sistematizado-03-02.json")
OUTPUT_RANKINGS = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_MAPPING_candidates_rankings.json")

MODEL_NAME = "gemini-3-pro-preview"

def load_json(path):
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)

def rank_candidates_batch(model, targets_batch, all_candidates):
    # Construct candidates string once
    candidates_str = ""
    for i, cand in enumerate(all_candidates):
        candidates_str += f"ID {i} | TITLE: {cand['article']} | TEXT: {cand['text']}\n"
        
    # Construct targets string with IDs
    targets_str = ""
    for idx, t in enumerate(targets_batch):
        # We use a temporary Batch ID (0 to batch_size-1) for matching
        targets_str += f"TARGET_ID: {idx}\nTITLE: {t['article']}\nTEXT: {t['text']}\n---\n"

    prompt = f"""
    TASK: For EACH of the Target Articles listed below (identified by TARGET_ID), identify the Top 5 candidates from the Candidates List that are most semantically similar.
    Focus on CONTENT overlap.
    
    CANDIDATES LIST:
    {candidates_str}
    
    TARGETS LIST:
    {targets_str}
    
    OUTPUT FORMAT: JSON List of Objects. 
    IMPORTANT: You must include "target_id" corresponding to the TARGET_ID in the list above.
    [
      {{
        "target_id": 0,
        "target_article_title": "...",
        "top_candidates": [
           {{ "candidate_index": 0, "similarity_score": 0.75, "reason": "..." }},
           ...
        ]
      }},
      ...
    ]
    Return ONLY JSON.
    """
    
    try:
        response = model.generate_content(prompt, generation_config={"response_mime_type": "application/json"})
        return json.loads(response.text)
    except Exception as e:
        print(f"Error ranking batch: {e}")
        return []

def main():
    if not API_KEY:
        print("API Key missing.")
        return
        
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel(MODEL_NAME)
    
    targets = load_json(INPUT_TARGET_04_08)
    candidates = load_json(INPUT_CANDIDATES_03_02)
    
    results = []
    batch_size = 5
    
    print(f"Ranking {len(targets)} Targets in batches of {batch_size}...")
    
    for i in range(0, len(targets), batch_size):
        batch = targets[i:i+batch_size]
        print(f"Processing Batch {i//batch_size + 1} ({len(batch)} articles)...")
        
        batch_results = rank_candidates_batch(model, batch, candidates)
        
        # Map results by target_id
        result_map = {res.get('target_id'): res for res in batch_results}
        
        for idx, t in enumerate(batch):
            # idx matches the TARGET_ID sent in prompt
            res = result_map.get(idx)
            
            enriched_candidates = []
            if res:
                for cand_res in res.get('top_candidates', []):
                    c_idx = cand_res.get('candidate_index')
                    if c_idx is not None and 0 <= c_idx < len(candidates):
                        c = candidates[c_idx]
                        enriched_candidates.append({
                            "candidate_article": c['article'],
                            "candidate_text_snippet": c['text'], 
                            "similarity_score": cand_res.get('similarity_score'),
                            "reason": cand_res.get('reason')
                        })
            else:
                print(f"Warning: No ranking returned for Target ID {idx} ({t['article']})")

            results.append({
                "target_article": t['article'],
                "target_text_snippet": t['text'], 
                "top_candidates": enriched_candidates
            })
            
        time.sleep(2) # Rate limit
        
    # Save
    with open(OUTPUT_RANKINGS, 'w', encoding='utf-8') as f:
        json.dump(results, f, indent=2, ensure_ascii=False)
        
    print(f"Rankings saved to {OUTPUT_RANKINGS}")

if __name__ == "__main__":
    main()
