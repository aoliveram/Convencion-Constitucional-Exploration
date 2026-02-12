import json
import os
import google.generativeai as genai
import difflib

# Configuration
API_KEY = os.environ.get("GEMINI_API_KEY")
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

BASE_DIR = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking"
PDF_FINAL_DRAFT = os.path.join(BASE_DIR, "comision-2/PDFs/C2_COMPLEX_BORRADOR-CONSTITUCIONAL-14-05-22.pdf")
INPUT_BASE_03_08 = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_GENESIS_texto-sistematizado-03-08_Preview_2.json")
INPUT_INDICATIONS = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_INDICATIONS_04_08_candidates.json")
OUTPUT_FINAL = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_DRAFT_texto-sistematizado-04-08.json")
LOG_FILE = os.path.join(BASE_DIR, "comision-2/reconstructed/deduction_log.txt")

MODEL_NAME = "gemini-3-pro-preview"

def extract_ground_truth(pdf_path):
    print("Extracting Ground Truth from Final Draft...")
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel(model_name=MODEL_NAME)
    
    file_ref = genai.upload_file(pdf_path)
    
    # Simple wait
    import time
    while file_ref.state.name == "PROCESSING":
        time.sleep(1)
        file_ref = genai.get_file(file_ref.name)
        
    prompt = """
    You are a legal expert or stenographer. Extract all articles from this "Borrador Constitucional" VERBATIM.
    Target sections (Commission 2):
    1. "PRINCIPIOS CONSTITUCIONALES"
    2. "DEMOCRACIA PARTICIPATIVA Y DIRECTA"
    3. "NACIONALIDAD Y CIUDADANÍA"
    4. "DERECHOS ESPECÍFICOS Y PROTECCIÓN" (Older people, women, disability, asylum, children)

    There should be approximately 41 articles.
    
    CRITICAL INSTRUCTION:
    - The "article" field must contain the EXACT Article Number and Title as it appears in the PDF. Do not invent or summarize titles.
    - Example: If text says "Artículo 9.- Derecho al asilo.", the "article" field must be "Artículo 9.- Derecho al asilo".
    - The "text" field must contain the full body of the article.

    Return JSON list:
    [
      { "article": "Artículo X.- Title Verbatim", "text": "Full text..." }
    ]
    """
    
    response = model.generate_content([prompt, file_ref], generation_config={"response_mime_type": "application/json"})
    return json.loads(response.text)

def semantic_match(ground_truth, candidate_text):
    # Use simple ratio for now, or LLM if needed. 
    # User asked for "Semantic Deduction" and "Likert Scale".
    # Let's use difflib for speed and mapping to Likert.
    
    ratio = difflib.SequenceMatcher(None, ground_truth, candidate_text).ratio()
    
    # Map ratio to 1-7 Likert
    # 7: Exact match (1.0)
    # 6: Very high (0.9+)
    # 5: High (0.8+)
    # 4: Moderate (0.6+)
    # 1: No match
    
    if ratio > 0.98: score = 7
    elif ratio > 0.90: score = 6
    elif ratio > 0.80: score = 5
    elif ratio > 0.60: score = 4
    elif ratio > 0.40: score = 3
    elif ratio > 0.20: score = 2
    else: score = 1
    
    return score, ratio

def main():
    if not API_KEY:
        print("API Key missing.")
        return

    # 1. Load Data
    base_articles = json.load(open(INPUT_BASE_03_08, 'r'))
    all_indications = json.load(open(INPUT_INDICATIONS, 'r'))
    
    # 2. Extract Ground Truth
    ground_truth_list = extract_ground_truth(PDF_FINAL_DRAFT)
    print(f"Extracted {len(ground_truth_list)} Ground Truth Articles.")
    
    # Index Indication Groups
    ind_map = {item['article_ref']: item['indications'] for item in all_indications}
    
    final_articles = []
    log_entries = []
    
    # 3. Deduction Loop
    # We iterate over the GROUND TRUTH because that is the defining set of the new draft.
    # We try to find where it came from (Base + Indication).
    
    for gt_art in ground_truth_list:
        gt_title = gt_art['article']
        gt_text = gt_art['text']
        
        print(f"Analyzing {gt_title}...")
        
        # Find corresponding Base Article (from 03-08)
        # Try finding by title first
        base_match = None
        for b in base_articles:
            if b['article'] in gt_title or gt_title in b['article']: 
                base_match = b
                break
        
        current_history = []
        if base_match:
            current_history = list(base_match.get('history', []))
            base_text = base_match['text']
            
            # Check if Base Text matches GT directly (No Change)
            score_base, ratio_base = semantic_match(gt_text, base_text)
            
            if score_base >= 6:
                # No change
                log_entries.append(f"REQ: {gt_title} | SOURCE: Base (No Change) | CONFIDENCE: {score_base}/7")
                
                final_obj = base_match.copy()
                final_obj['text'] = gt_text # Use official text
                final_articles.append(final_obj)
                continue
                
            # If not direct match, check Indications
            # Get indications for this article ref
            candidates = ind_map.get(base_match['article'], [])
            
            best_ind = None
            best_score = 0
            
            for ind in candidates:
                cand_content = ind['content']
                # Clean content (remove "Para sustituir...")? 
                # Ideally yes, but let's compare raw for now or minimal clean
                score, ratio = semantic_match(gt_text, cand_content)
                if score > best_score:
                    best_score = score
                    best_ind = ind
            
            if best_ind and best_score >= 4: # Moderate match
                log_entries.append(f"REQ: {gt_title} | SOURCE: Indication {best_ind.get('number')} | CONFIDENCE: {best_score}/7")
                
                # Update History
                current_history.append({
                    "step": "Report 2 (04-08)",
                    "action": "Indication Approved",
                    "indication_number": best_ind.get('number'),
                    "authors": best_ind.get('authors', []),
                    "confidence_likert": best_score
                })
                
                final_obj = base_match.copy()
                final_obj['text'] = gt_text
                final_obj['history'] = current_history
                final_articles.append(final_obj)
                
            else:
                 # No good indication match. Maybe it's a new draft writing?
                 log_entries.append(f"REQ: {gt_title} | SOURCE: Unknown (No match > 4/7) | CLOSEST: {best_score}/7")
                 final_obj = base_match.copy()
                 final_obj['text'] = gt_text # Assume GT is correct
                 final_articles.append(final_obj)
        else:
             # No base match? New article?
             log_entries.append(f"REQ: {gt_title} | SOURCE: NEW (No Base found)")
             final_articles.append({
                 "article": gt_title,
                 "text": gt_text,
                 "history": [{"step": "04-08", "action": "New Article (PDF 14-05)"}]
             })

    # Save Output
    with open(OUTPUT_FINAL, 'w', encoding='utf-8') as f:
        json.dump(final_articles, f, indent=2, ensure_ascii=False)
        
    with open(LOG_FILE, 'w', encoding='utf-8') as f:
        f.write("\n".join(log_entries))
        
    print(f"Reconstruction Complete. Saved to {OUTPUT_FINAL}")
    print(f"Log saved to {LOG_FILE}")

if __name__ == "__main__":
    main()
