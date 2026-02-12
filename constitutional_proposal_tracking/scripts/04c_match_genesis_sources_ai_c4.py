
import os
import json
import numpy as np
import google.generativeai as genai
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
import re
import time

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") or os.environ.get("GOOGLE_API_KEY")
genai.configure(api_key=API_KEY)

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
COM4_DIR = os.path.join(BASE_DIR, "comision-4")

# Input/Output Paths
GENESIS_SOURCE_FILE = os.path.join(COM4_DIR, "genesis-extracted", "C4_GENESIS_texto-sistematizado-03-07.json")
ICC_POOL_FILE = os.path.join(COM4_DIR, "genesis-extracted", "C4_ICC_POOL.json")

# Intermediate Debug File (Requested by User)
CANDIDATES_DEBUG_FILE = os.path.join(COM4_DIR, "genesis-extracted", "C4_INDICATIONS_03_07_candidates.json")

# Final Output (Preview first)
OUTPUT_FILE = os.path.join(COM4_DIR, "genesis-extracted", "C4_GENESIS_texto-sistematizado-1-03-07_PREVIEW.json")

def load_json(path):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    print(f"Warning: File not found {path}")
    return []

# --- 1. TF-IDF Ranking ---

def rank_candidates(targets, candidates, top_k=10):
    """
    Returns a list of targets, each enriched with 'top_candidates' list.
    """
    target_texts = [t.get("text", "") for t in targets]
    candidate_texts = [c.get("text", "") for c in candidates]
    
    # Check if empty
    if not candidate_texts:
        print("No candidates found to rank.")
        return targets

    vectorizer = TfidfVectorizer().fit(target_texts + candidate_texts)
    
    target_vecs = vectorizer.transform(target_texts)
    candidate_vecs = vectorizer.transform(candidate_texts)
    
    # Compute similarity matrix (Targets x Candidates)
    similarity_matrix = cosine_similarity(target_vecs, candidate_vecs)
    
    enriched_targets = []
    
    for i, target in enumerate(targets):
        # Get similarities for this target
        sim_scores = similarity_matrix[i]
        # Get indices of top_k
        top_indices = sim_scores.argsort()[-top_k:][::-1]
        
        top_candidates = []
        for idx in top_indices:
            cand = candidates[idx].copy()
            cand["similarity_score"] = float(sim_scores[idx])
            # Keep only necessary fields to save tokens
            clean_cand = {
                "icc_id": cand.get("icc_id"),
                "text": cand.get("text")[:500] + "..." if len(cand.get("text", "")) > 500 else cand.get("text"),
                "score": round(cand["similarity_score"], 3)
            }
            top_candidates.append(clean_cand)
            
        target["candidates"] = top_candidates
        enriched_targets.append(target)
        
    return enriched_targets

# --- 2. Gemini Judge ---

def batch_judge(model, batch_targets):
    """
    Sends a batch of articles (with their candidates) to Gemini.
    """
    
    # Construct a clean input structure for the prompt
    prompt_input = []
    for t in batch_targets:
        prompt_input.append({
            "article_id": t.get("article"), # e.g. "Artículo 1"
            "article_text": t.get("text"),
            "candidates": t.get("candidates") # List of {icc_id, text}
        })
        
    prompt_str = json.dumps(prompt_input, ensure_ascii=False, indent=2)
    
    prompt = f"""
    ROLE: Expert Legal Data Judge.
    TASK: Identify the source "ICC" (Iniciativa Constituyente) for each Approved Article.
    
    INPUT DATA: A list of "Articles" (Approved Text) and "Candidates" (Source Proposals).
    
    INSTRUCTIONS:
    1. For each article in the list, read its "article_text".
    2. Compare it with the provided "candidates".
    3. Determine which candidate is the **origin** of the article.
       - The text might not be 100% identical due to editing, but semantic meaning and key phrases should match heavily.
       - Look for the highest similarity.
    4. If none of the candidates match reasonably well, return "No Match".
    
    OUTPUT FORMAT:
    Return a JSON List of results:
    [
      {{
        "article_id": "Artículo 1",
        "selected_icc_id": "11",  // The ID of the best match
        "confidence": 0.95,       // Use 0.0 to 1.0
        "reasoning": "Candidate 11 contains the exact same second paragraph about 'naturaleza'."
      }},
      ...
    ]
    
    DATA TO PROCESS:
    {prompt_str}
    """
    
    try:
        response = model.generate_content(prompt)
        text = response.text
        
        # Clean JSON markdown
        if "```json" in text:
            text = text.split("```json")[1].split("```")[0]
        elif "```" in text:
            text = text.split("```")[1].split("```")[0]
            
        return json.loads(text.strip())
        
    except Exception as e:
        print(f"Batch Error: {e}")
        return []

def main():
    # Load Data
    print("Loading data...")
    targets = load_json(GENESIS_SOURCE_FILE) # 50 articles
    candidates = load_json(ICC_POOL_FILE)    # Extracts from PDFs
    
    print(f"Loaded {len(targets)} targets and {len(candidates)} candidates.")
    
    if not candidates:
        print("CRITICAL: No candidates found. Run script 02b first.")
        return

    # Step 1: Pre-filter with TF-IDF
    print("Ranking candidates with TF-IDF...")
    enriched_targets = rank_candidates(targets, candidates, top_k=10)
    
    # SAVE CANDIDATES for review (User Requirement)
    print(f"Saving candidates debug file to {CANDIDATES_DEBUG_FILE}...")
    with open(CANDIDATES_DEBUG_FILE, 'w', encoding='utf-8') as f:
        json.dump(enriched_targets, f, ensure_ascii=False, indent=2)
    
    # Step 2: Batch Processing with Gemini
    BATCH_SIZE = 10 
    model_name = "gemini-3-pro-preview"
    
    try:
        model = genai.GenerativeModel(model_name)
    except Exception as e:
        print(f"Error init model {model_name}: {e}")
        return

    final_output = []
    
    total_batches = (len(enriched_targets) + BATCH_SIZE - 1) // BATCH_SIZE
    
    print(f"Starting semantic matching with {model_name}... ({total_batches} batches)")
    
    for i in range(0, len(enriched_targets), BATCH_SIZE):
        batch = enriched_targets[i : i + BATCH_SIZE]
        print(f"Processing Batch {i//BATCH_SIZE + 1}/{total_batches} (Arts {i}-{i+len(batch)})...")
        
        results = batch_judge(model, batch)
        
        # Merge results back into target objects
        result_map = {r.get("article_id"): r for r in results}
        
        for target in batch:
            art_id = target.get("article")
            decision = result_map.get(art_id)
            
            # Create final object structure
            final_obj = {
                "article": target.get("article"),
                "text": target.get("text"),
                "sources": [], # To be filled
                "match_meta": {}
            }
            
            if decision and decision.get("selected_icc_id") and str(decision.get("selected_icc_id")) != "No Match":
                final_obj["sources"] = [str(decision.get("selected_icc_id"))]
                final_obj["match_meta"] = {
                    "confidence": decision.get("confidence"),
                    "reasoning": decision.get("reasoning")
                }
            else:
                print(f"  Warning: No confident match for {art_id}")
                
            final_output.append(final_obj)
            
        time.sleep(2) # Modest rate limiting

    # Step 3: Save Final
    print(f"Saving final genesis file to {OUTPUT_FILE}...")
    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        json.dump(final_output, f, ensure_ascii=False, indent=2)
        
    print("Done.")

if __name__ == "__main__":
    main()
