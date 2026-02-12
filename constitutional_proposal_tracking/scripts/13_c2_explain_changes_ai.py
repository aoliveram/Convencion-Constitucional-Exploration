
import json
import os
import google.generativeai as genai
import time

# Configuration
API_KEY = os.environ.get("GEMINI_API_KEY")
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

BASE_DIR = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking"
TARGET_FILE = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_GENESIS_texto_sistematizado-04-08.json")
INDICATIONS_FILE = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_INDICATIONS_04_08_candidates.json")

# Specific articles to analyze (The last 6)
TARGET_ARTICLES_TITLES = [
    "Artículo 1.- Derechos de las personas mayores",
    "Artículo 3",
    "Artículo 6",
    "Artículo 9.- Derecho al asilo.",
    "Artículo 10.- Principio de no devolución.",
    "Artículo 11.- Derechos de niñas, niños y adolescentes."
]

MODEL_NAME = "gemini-3-pro-preview" 

def get_last_6_articles(all_articles):
    # Filter by specific titles to be sure, or just take the last 6 if the user is certain.
    # The user said "llamados artículos 1, 3, 6, 9, 10 y 11". matching strictly.
    target_articles = []
    for art in all_articles:
        # Check if title matches one of the known ones.
        # Note: "Artículo 3" might match "Artículo 30" via 'in', so be careful with exact string match or startswith
        # The titles in JSON are specific.
        if art['article'] in TARGET_ARTICLES_TITLES:
             target_articles.append(art)
    
    # Sort them to match the order 1, 3, 6, 9, 10, 11 just in case
    # Actually, let's just return what we found.
    return target_articles

def analyze_with_ai(target_articles, indications_data):
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel(model_name="gemini-3-pro-preview")
    
    # Add index to the articles sent to AI for unambiguous matching
    articles_with_index = []
    for i, art in enumerate(target_articles):
        # We need to carry the original index from full_target_data if possible
        # But analyze_with_ai receives a subset.
        # Let's just add a temporary "analysis_id": i to the prompt
        art_copy = art.copy()
        art_copy['analysis_id'] = i
        articles_with_index.append(art_copy)
    
    prompt = f"""
    You are a legal expert analyzing the legislative history of the Chilean Constitutional Convention.
    
    TASK:
    Identify which specific "Indication" (amendment) is responsible for the text of the following Final Articles.
    You have a list of TARGET ARTICLES (the final approved text) and a list of CANDIDATE INDICATIONS (proposed amendments).
    
    For each Target Article, find the Indication that matches its text most closely (either virtually identical or clearly the source).
    
    INPUT DATA:
    
    --- TARGET ARTICLES (Final Text) ---
    {json.dumps(articles_with_index, indent=2, ensure_ascii=False)}
    
    --- CANDIDATE INDICATIONS ---
    {json.dumps(indications_data, indent=2, ensure_ascii=False)}
    
    INSTRUCTIONS:
    1. Compare the text of each Target Article with the content of the Indications.
    2. Note that "Artículo 1" in indications might refer to "Derechos personas mayores" OR "Estado". Use the CONTENT to disambiguate.
    3. If an indication says "Sustituir artículo X por...", check if the replacement text matches the Target.
    4. If an indication says "Agregar nuevo artículo...", check if the added text matches the Target.
    
    OUTPUT FORMAT (JSON ONLY):
    Returns a list of objects, one for each Target Article found:
    [
      {{
        "analysis_id": 0, // The ID provided in the input Target Articles
        "article_title": "Title of the target article",
        "winning_indication_number": "Number of the indication (e.g. '4', '91', '102')",
        "winning_indication_authors": ["Author 1", "Author 2"],
        "explanation": "Brief explanation of why this match is correct (e.g. 'Text is identical to Indication 4').",
        "match_type": "Substitution" or "New Article" or "Partial Composition"
      }}
    ]
    """
    
    try:
        response = model.generate_content(prompt, generation_config={"response_mime_type": "application/json"})
        return json.loads(response.text)
    except Exception as e:
        print(f"Error calling AI: {e}")
        return []

def main():
    if not API_KEY:
        print("Error: API Key not found.")
        return

    print("Loading data...")
    with open(TARGET_FILE, 'r', encoding='utf-8') as f:
        full_target_data = json.load(f)
    
    with open(INDICATIONS_FILE, 'r', encoding='utf-8') as f:
        indications_data = json.load(f)
        
    # Extract the relevant articles AND their original index
    relevant_articles = []
    target_indices = []
    
    # We want ONLY the last occurrences if there are duplicates, or specific ones. 
    # The user said "los últimos 6 artículos".
    # Let's literally take the last 6 entries of the file if their titles match the pattern?
    # Or just iterate and capture the ones that match our titles, keeping track of index.
    # Given the duplication "Artículo 3" (early) and "Artículo 3" (late), we must be careful.
    
    # Let's filter by title, but if we find duplicates, we need to distinguish.
    # The 'last 6' hint is strong. Let's look at the END of the list.
    
    # Let's find matches in reverse order
    for match_title in TARGET_ARTICLES_TITLES:
        # Find the LAST occurrence of this title in full_target_data
        found_idx = -1
        for i in range(len(full_target_data) - 1, -1, -1):
            if full_target_data[i]['article'] == match_title:
                found_idx = i
                break
        
        if found_idx != -1:
            # Check if this index is already added (to avoid double adding if titles duplicate in list)
            if found_idx not in target_indices:
                target_indices.append(found_idx)
                relevant_articles.append(full_target_data[found_idx])
    
    if not relevant_articles:
        print("No matching articles found to analyze.")
        return
        
    print(f"Analyzing {len(relevant_articles)} articles (Indices: {target_indices})...")
    for art in relevant_articles:
        print(f" - {art['article']}")

    # Call AI
    print("\nCalling Gemini for analysis...")
    analysis_results = analyze_with_ai(relevant_articles, indications_data)
    
    if not analysis_results:
        print("Failed to get analysis results.")
        return

    print(f"\nReceived analysis for {len(analysis_results)} articles.")
    
    # Update the main data using INDEX
    print("Updating article history...")
    
    # Create a lookup map by analysis_id
    results_map = {res['analysis_id']: res for res in analysis_results if 'analysis_id' in res}
    
    updated_count = 0
    # relevant_articles has the same order as passed to AI (0, 1, 2...)
    # target_indices has the corresponding true indices
    
    for i in range(len(relevant_articles)):
        if i in results_map:
            result = results_map[i]
            true_idx = target_indices[i]
            article = full_target_data[true_idx]
            
            # Construct the history entry
            history_entry = {
                "step": "Indications Check (04-08)",
                "match_found": True,
                "winning_indication": {
                    "number": result.get("winning_indication_number"),
                    "authors": result.get("winning_indication_authors"),
                    "type": result.get("match_type"),
                    "explanation": result.get("explanation")
                }
            }
            
            # Check if entry already exists to avoid duplication
            exists = False
            if "history" not in article:
                article["history"] = []
                
            for h in article["history"]:
                if h.get("step") == "Indications Check (04-08)":
                    h.update(history_entry)
                    exists = True
                    break
            
            if not exists:
                article["history"].append(history_entry)
            
            updated_count += 1
            print(f"Updated: {article['article']} -> Indication {result.get('winning_indication_number')}")
        else:
            print(f"Warning: No result for analysis_id {i}")

    # Save
    if updated_count > 0:
        with open(TARGET_FILE, 'w', encoding='utf-8') as f:
            json.dump(full_target_data, f, indent=2, ensure_ascii=False)
        print(f"\nSuccessfully updated {updated_count} articles in {TARGET_FILE}")
    else:
        print("\nNo articles were updated.")

if __name__ == "__main__":
    main()
