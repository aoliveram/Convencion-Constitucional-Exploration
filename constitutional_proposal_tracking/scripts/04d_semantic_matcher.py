
import os
import json
import re
import time
import google.generativeai as genai
from difflib import SequenceMatcher

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") 
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INPUT_DIR = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted")
GOALS_PATH = os.path.join(INPUT_DIR, "goals_com2.json")
CANDIDATES_PATH = os.path.join(INPUT_DIR, "candidates_com2.json")
OUTPUT_PATH = os.path.join(INPUT_DIR, "indications_com2_final_matched.json")

def load_json(path):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    print(f"Warning: File not found {path}")
    return []

def normalize_article_num(text):
    """Extracts '4' from 'Artículo 4', 'Art. 4', '4', etc."""
    if not text: return "Unknown"
    match = re.search(r'\d+', str(text))
    if match:
        return match.group(0)
    return "Unknown"

# --- Phase 3: Semantic Matcher (The Judge) - Direct Comparison Strategy ---

def load_genesis_data(path):
    """Loads genesis data as a list of dicts."""
    return load_json(path)

def find_genesis_match(final_art, genesis_list):
    """
    Finds the corresponding article in Genesis based on Title (primary) and Reference/Text (secondary).
    This handles cases where Article numbers repeat across chapters.
    """
    final_title = final_art.get("title", "").lower().strip()
    final_ref = normalize_article_num(final_art.get("article_ref"))
    final_text_start = final_art.get("text", "")[:50].lower()

    # 1. Try Title Match + Number Match (Strongest)
    if final_title:
        for gen in genesis_list:
            gen_title = gen.get("article", "").lower() # Genesis puts title in unrelated fields sometimes, but 'article' usually holds Ref. 
            # Note: Genesis JSON structure provided earlier showed "article": "Artículo 1" and no separate title field in some entries, 
            # but let's check text content for title-like matches if needed. 
            # Actually, looking at Genesis file: "article": "Artículo 1", "text": "..."
            # It doesn't have a structured title field. It's implicit in the text or metadata.
            
            gen_ref = normalize_article_num(gen.get("article"))
            
            # Simple heuristic: If same number AND text starts similarly
            gen_text_start = gen.get("text", "")[:50].lower()
            
            if gen_ref == final_ref:
                # Calculate text similarity ratio for the start
                ratio = SequenceMatcher(None, gen_text_start, final_text_start).ratio()
                if ratio > 0.6: # Good match
                    return gen
    
    # 2. Try just Text Similarity for same number (Fallback)
    for gen in genesis_list:
        gen_ref = normalize_article_num(gen.get("article"))
        if gen_ref == final_ref:
             gen_text = gen.get("text", "")
             # If high overlap
             ratio = SequenceMatcher(None, gen_text, final_art.get("text", "")).ratio()
             if ratio > 0.5:
                 return gen
                 
    return None

def semantic_judge(model, genesis_text, final_art, candidates):
    """
    Asks the LLM to explain the transition from Genesis -> Final using Candidates.
    """
    candidates_str = json.dumps(candidates, ensure_ascii=False, indent=1)
    
    genesis_section = f'ORIGINAL TEXT (Genesis):\n"{genesis_text}"' if genesis_text else 'ORIGINAL TEXT (Genesis):\n(New Article / No equivalent found)'
    
    prompt = f"""
    ACT AS: Legal Historian & Judge.
    OBJECTIVE: Explain how the text evolved from Original to Final by selecting the responsible Indication(s).
    
    CONTEXT:
    Article Title: "{final_art.get("title")}"
    Article Number: "{final_art.get("article_ref")}"
    
    {genesis_section}
    
    FINAL APPROVED TEXT (Draft 14-May):
    "{final_art.get("text")}"
    
    CANDIDATE INDICATIONS (Proposals):
    {candidates_str}
    
    TASK:
    1. Compare Original vs Final. IF they are identical, return match_found: false (No indication changed it).
    2. IF different, find which Candidate Indication proposed the change (Modification, Addition, Suppression).
    3. Be precise. If the Final Text is a specific paragraph proposed by X, select X.
    
    OUTPUT JSON:
    {{
        "match_found": true,
        "selected_indication_numbers": ["12", "14"],
        "change_type": "Modification/Addition/Suppression",
        "reasoning": "Indication 12 introduced the phrase X which appears in the final text.",
        "confidence": "HIGH"
    }}
    """
    
    try:
        response = model.generate_content(prompt)
        text = response.text
        match = re.search(r'\{.*\}', text, re.DOTALL)
        if match:
             return json.loads(match.group(0))
        else:
             return json.loads(text.replace('```json', '').replace('```', '').strip())
    except Exception as e:
        print(f"  Judge Error: {e}")
        return {"match_found": False, "error": str(e)}

def main():
    print("--- Phase 3: Semantic Matcher (Direct Comparison Strategy) ---")
    
    # Paths
    FINAL_DRAFT_PATH = os.path.join(INPUT_DIR, "final_draft_com2.json")
    # NEW: Load Intermediate Draft Data (Refined Baseline)
    GENESIS_PATH = os.path.join(INPUT_DIR, "C2_DRAFT_texto-sistematizado-03-02.json")
    
    # Load Data
    final_articles = load_json(FINAL_DRAFT_PATH)
    genesis_articles = load_json(GENESIS_PATH)
    candidates = load_json(CANDIDATES_PATH)
    
    if not final_articles or not candidates:
        print("Missing input keys.")
        return
        
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel('gemini-2.0-flash-exp') # Fast and smart enough for extraction
    
    # Group candidates by number for quick access
    candidates_by_num = {}
    for c in candidates:
        n = normalize_article_num(c.get("target_article_guess"))
        if n not in candidates_by_num: candidates_by_num[n] = []
        candidates_by_num[n].append(c)
        
    final_matches = []
    
    print(f"Processing {len(final_articles)} final articles...")
    
    for final_art in final_articles:
        art_ref = final_art.get("article_ref")
        art_num = normalize_article_num(art_ref)
        
        print(f"\nAnalyzing: {art_ref} - {final_art.get('title')}")
        
        # 1. Find Genesis Ancestor
        genesis_match = find_genesis_match(final_art, genesis_articles)
        genesis_text = genesis_match.get("text") if genesis_match else None
        
        if genesis_text:
            print(f"  -> Mapped to Genesis Article {genesis_match.get('article')}")
        else:
            print("  -> No Genesis match found (Likely New Article)")
            
        # 2. Select Relevant Candidates
        # We start with candidates that share the same number
        relevant_candidates = candidates_by_num.get(art_num, [])
        
        if not relevant_candidates:
            print("  -> No candidates with this number. Evaluating all candidates (fallback)...")
            # If crucial, we could use all candidates, but let's try strict first to reduce noise
            # relevant_candidates = candidates 
        
        # 3. Judge
        if relevant_candidates:
            decision = semantic_judge(model, genesis_text, final_art, relevant_candidates)
            
            if decision.get("match_found"):
                sel_ids = decision.get("selected_indication_numbers", [])
                print(f"  -> MATCH: {sel_ids} ({decision.get('change_type')})")
                
                # Fetch Authors
                authors = set()
                for sid in sel_ids:
                    for c in candidates:
                         if str(c.get("number")) == str(sid):
                             authors.update(c.get("authors_matched", []))
                
                final_matches.append({
                    "final_article": f"{art_ref} - {final_art.get('title')}",
                    "final_text": final_art.get("text"),
                    "genesis_source": genesis_match.get("article") if genesis_match else "NEW",
                    "matched_indications": sel_ids,
                    "authors": list(authors),
                    "reasoning": decision.get("reasoning")
                })
            else:
                 print("  -> No indication match found.")
        else:
            print("  -> No relevant candidates to judge.")
            
        time.sleep(0.5)
        
    # Save
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(final_matches, f, ensure_ascii=False, indent=2)
    print(f"\nSaved {len(final_matches)} matches to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
