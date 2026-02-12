import json
import difflib
import os

# Configuration Paths
BASE_DIR = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking"
INPUT_03_02 = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_GENESIS_texto-sistematizado-03-02.json")
INPUT_03_08 = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_GENESIS_texto-sistematizado-03-08_Preview.json")
OUTPUT_MAPPED = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_GENESIS_texto-sistematizado-03-08_Preview_2.json")

def load_json(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        return json.load(f)

def clean_text(text):
    return text.lower().replace('\n', ' ').strip()

def main():
    print("--- Mapping 03-08 Articles to 03-02 History ---")
    
    # Load Data
    data_03_02 = load_json(INPUT_03_02) # List of ~96 articles
    data_03_08 = load_json(INPUT_03_08) # List of ~14 articles
    
    print(f"Loaded {len(data_03_02)} Base Articles (03-02)")
    print(f"Loaded {len(data_03_08)} Target Articles (03-08)")
    
    mapped_articles = []
    
    # Create a simple index of 03-02 texts for matching
    base_texts = [clean_text(a['text']) for a in data_03_02]
    
    for target_idx, target_art in enumerate(data_03_08):
        target_text_clean = clean_text(target_art['text'])
        target_title = target_art['article']
        
        print(f"\nProcessing: {target_title}")
        
        # Find best match
        # Using SequenceMatcher is slow for large lists, but 96 is small.
        best_match_idx = -1
        best_ratio = 0.0
        
        for base_idx, base_text_clean in enumerate(base_texts):
            # Optimization: Check if titles match first? 
            # Titles might have changed (renumbering). Content is king.
            
            # Quick overlap check before expensive SequenceMatcher?
            # Nah, 96 items is tiny.
            
            ratio = difflib.SequenceMatcher(None, target_text_clean, base_text_clean).ratio()
            if ratio > best_ratio:
                best_ratio = ratio
                best_match_idx = base_idx
        
        # Threshold
        if best_ratio > 0.4: # Fairly loose because text might have changed
            base_art = data_03_02[best_match_idx]
            match_title = base_art['article']
            print(f"  -> Match Found: {match_title} (Similarity: {best_ratio:.2f})")
            
            # Link History
            # We take the 03-02 history and append a "Mapping" step
            new_history = list(base_art.get('history', []))
            new_history.append({
                "step": "Mapping (03-02 -> 03-08)",
                "action": "Map/Consolidate",
                "original_article_ref": match_title,
                "similarity_score": best_ratio
            })
            
            # Create new object preserving 03-02 metadata + 03-08 text
            mapped_art = {
                "article": target_title, # Use new title
                "text": target_art['text'], # Use new text
                "sources": base_art.get('sources', []), # Keep original sources
                "history": new_history
            }
            mapped_articles.append(mapped_art)
            
        else:
            print(f"  -> NO MATCH FOUND (Best: {best_ratio:.2f}). Treating as New Article.")
            mapped_art = target_art.copy()
            mapped_art['history'] = [{
                "step": "Mapping (03-02 -> 03-08)",
                "action": "New/Unmapped",
                "similarity_score": best_ratio
            }]
            mapped_articles.append(mapped_art)

    # Save Output
    os.makedirs(os.path.dirname(OUTPUT_MAPPED), exist_ok=True)
    with open(OUTPUT_MAPPED, 'w', encoding='utf-8') as f:
        json.dump(mapped_articles, f, indent=2, ensure_ascii=False)
        
    print(f"\nSaved {len(mapped_articles)} mapped articles to {OUTPUT_MAPPED}")

if __name__ == "__main__":
    main()
