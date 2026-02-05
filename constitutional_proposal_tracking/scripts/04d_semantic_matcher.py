
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

def semantic_judge(model, goal_text, candidates):
    """
    Asks the LLM to decide which indication(s) produced the goal text.
    """
    candidates_str = json.dumps(candidates, ensure_ascii=False, indent=1)
    
    prompt = f"""
    ACT AS: Legal Historian & Judge.
    OBJECTIVE: Identify which legislative indication(s) resulted in the FINAL TEXT.
    
    FINAL APPROVED TEXT (The Goal):
    "{goal_text}"
    
    CANDIDATE INDICATIONS (The Proposals):
    {candidates_str}
    
    TASK:
    Compare the Final Text with the Candidates. 
    1. Identify which indication(s) match the Final Text content.
    2. If the Final Text is a combination of multiple indications, list all.
    3. If the Final Text is identical to "Base Text" (no change) or no indication matches, return null.
    
    OUTPUT JSON FORMAT:
    {{
        "match_found": true,
        "selected_indication_numbers": ["34", "35"],
        "reasoning": "Indication 34 proposed the first paragraph and 35 the second.",
        "confidence": "HIGH"
    }}
    Or if none:
    {{ "match_found": false, "selected_indication_numbers": [], "reasoning": "No semantic match found." }}
    
    Return ONLY JSON.
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
    print("--- Phase 3: Semantic Matcher (The Judge) ---")
    
    goals = load_json(GOALS_PATH)
    candidates = load_json(CANDIDATES_PATH)
    
    if not goals or not candidates:
        print("Missing input data. Run Phase 1 and 2 first.")
        return
        
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel('gemini-3-flash-preview') # Efficient Judge
    
    # 1. Group Candidates by Article for Efficiency
    candidates_by_art = {}
    for cand in candidates:
        norm_art = normalize_article_num(cand.get("target_article_guess"))
        if norm_art not in candidates_by_art:
            candidates_by_art[norm_art] = []
        candidates_by_art[norm_art].append(cand)
        
    final_matches = []
    
    for goal in goals:
        art_num = normalize_article_num(goal.get("article_number", goal.get("article_title")))
        full_text = goal.get("full_text", "")
        
        print(f"\nProcessing Goal: Article {art_num}...")
        
        # 1. Try candidates for the same article number
        relevant_candidates = candidates_by_art.get(art_num, [])
        
        decision = None
        if relevant_candidates:
            print(f"  Judging against {len(relevant_candidates)} candidates matching article number...")
            decision = semantic_judge(model, full_text, relevant_candidates)
        
        # 2. If no match, try ALL candidates (Renumbering safeguard)
        if not decision or not decision.get("match_found"):
            print("  No match found with strict article numbering. Trying ALL candidates (Renumbering check)...")
            # We don't send *all* 134 to Gemini Flash in one prompt if it's too big, 
            # but 134 is actually small enough.
            decision = semantic_judge(model, full_text, candidates)
            
        if decision.get("match_found"):
            selected_ids = decision.get("selected_indication_numbers", [])
            print(f"  MATCH FOUND! Indications: {selected_ids}")
            
            # Retrieve Author info from the matched candidates
            matched_authors = set()
            for sel_id in selected_ids:
                # Search in all candidates to be sure
                for cand in candidates:
                    if str(cand.get("number")) == str(sel_id):
                        matched_authors.update(cand.get("authors_matched", []))
            
            final_matches.append({
                "article": f"Artículo {art_num}",
                "text": full_text,
                "source_indications": selected_ids,
                "authors": list(matched_authors),
                "confidence": decision.get("confidence"),
                "judge_reasoning": decision.get("reasoning")
            })
        else:
            print("  No semantic match found after checking all candidates.")
            
        time.sleep(1) 
        
    # Save
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(final_matches, f, ensure_ascii=False, indent=2)
        
    print(f"\nDone. Saved {len(final_matches)} verified matches to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
