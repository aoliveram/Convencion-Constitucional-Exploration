
import os
import json
import glob
import re
import difflib
import plotly.graph_objects as go
import plotly.express as px
import pandas as pd
from typing import Dict, Any, List

# --- Configuration ---
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FINAL_TEXT_PATH = os.path.join(BASE_DIR, "proposals", "draft_final_text.json")
MAPPING_PATH = os.path.join(BASE_DIR, "proposals", "draft_0_mapping.json")
INITIATIVES_DIR = os.path.join(BASE_DIR, "submitted_initiatives")
PLOTS_DIR = os.path.join(BASE_DIR, "plots")

def load_json(path):
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)

def load_initiatives(directory: str) -> Dict[str, str]:
    """
    Loads initiative texts. Returns map: InitiativeID -> Text content (propuesta_norma)
    """
    initiatives_text = {}
    pattern = os.path.join(directory, "api_extracted_*.json")
    files = glob.glob(pattern)
    
    print(f"Loading initiative texts from {len(files)} files...")
    
    for file_path in files:
        data = load_json(file_path)
        for key, value in data.items():
            # Extract ID usually found at start "98-6-..."
            match = re.match(r"^(\d+-\d+)", key)
            if match:
                init_id = match.group(1)
                text = value.get("propuesta_norma", "")
                if text:
                    initiatives_text[init_id] = text
    
    return initiatives_text

def normalize_article_key(key: str) -> str:
    """
    Tries to convert "Artículo 1" or "1" to a standard string "1".
    """
    # Remove "Artículo" case insensitive
    clean = re.sub(r'art[ií]culo\s*', '', key, flags=re.IGNORECASE).strip()
    # Remove likely trailing "°" or similar
    clean = clean.replace('°', '')
    return clean

def calculate_similarity(text1, text2):
    """
    Calculates a similarity ratio between 0 and 1.
    """
    if not text1 or not text2:
        return 0.0
    return difflib.SequenceMatcher(None, text1, text2).ratio()

def main():
    print("Starting comparison...")
    
    # 1. Load Data
    final_articles = load_json(FINAL_TEXT_PATH) # List of {"article_id": "...", "text": "..."}
    mapping_data = load_json(MAPPING_PATH) # List of {"article": "Artículo X", "sources": [{"initiative_id": "..."}]}
    initiatives_map = load_initiatives(INITIATIVES_DIR)
    
    # 2. Build Comparison Table
    comparison_data = []
    
    # Create a lookup for mapping data by normalized ID
    mapping_lookup = {}
    for item in mapping_data:
        key = normalize_article_key(item.get("article", ""))
        mapping_lookup[key] = item

    print("Comparing articles...")
    
    for final_art in final_articles:
        art_id = str(final_art.get("article_id", ""))
        final_text = final_art.get("text", "")
        
        # Try to find the corresponding mapping
        # Ideally the IDs match (1 -> 1). 
        # draft_final_text.json has IDs like "1", "2"... 
        # draft_1_mapping.json has "Artículo 1"...
        
        mapping_entry = mapping_lookup.get(art_id)
        
        best_similarity = 0.0
        source_summary = "None"
        
        if mapping_entry:
            source_ids = [s.get("initiative_id") for s in mapping_entry.get("sources", [])]
            source_texts = []
            
            for sid in source_ids:
                if sid in initiatives_map:
                    source_texts.append(initiatives_map[sid])
            
            # Combine all source texts to find best match overlap
            full_source_text = "\n".join(source_texts)
            
            if full_source_text:
                # We compare the final text against the combined source text
                # Note: Initiatives are long documents. Final text is short.
                # We want to see if Final Text is *contained* or *similar* to parts of Initiative.
                # Standard sequence matcher might be low if lengths differ vastly.
                # Let's check inclusion or "clean" similarity.
                
                # Option A: Simple Ratio (might be low due to extra text in initiatives)
                # Option B: Check if final text sentences exist in initiative.
                
                # Let's use Ratio for visual simplicity for V1, but truncate initiative if too long?
                # or just accept that low matching means high processing.
                best_similarity = calculate_similarity(final_text, full_source_text)
                source_summary = ", ".join(source_ids)
            else:
                source_summary = f"Missing Text ({', '.join(source_ids)})"
        else:
            source_summary = "No Mapping Found"

        comparison_data.append({
            "Article ID": art_id,
            "Final Length": len(final_text),
            "Similarity": best_similarity,
            "Source Initiatives": source_summary,
            "Status": "Mapped" if mapping_entry else "Unmapped"
        })
        
    # 3. Visualization
    df = pd.DataFrame(comparison_data)
    
    # Sort by Article ID numerically if possible
    try:
        df["SortKey"] = pd.to_numeric(df["Article ID"])
    except:
        df["SortKey"] = df["Article ID"]
        
    df = df.sort_values("SortKey")
    
    # Plot 1: Similarity Bar Chart
    fig = px.bar(
        df, 
        x='Article ID', 
        y='Similarity',
        color='Similarity',
        color_continuous_scale='Redor', # Red (Low) to Orange to... or 'RdYlGn'
        title='Index of Mutation: Initiative Text vs. Final Draft Text',
        hover_data=['Source Initiatives', 'Final Length'],
        labels={'Similarity': 'Similarity Score (0-1)'}
    )
    
    fig.update_layout(
        xaxis_title="Article ID (Final Draft)",
        yaxis_title="Similarity to Original Initiative",
        template="plotly_white"
    )
    
    # Add a baseline annotation
    fig.add_hline(y=df['Similarity'].mean(), line_dash="dot", annotation_text="Average Similarity", annotation_position="bottom right")

    output_file = os.path.join(PLOTS_DIR, "stability_comparison_v1.html")
    fig.write_html(output_file)
    print(f"Plot saved to {output_file}")
    
    # 4. Save detailed JSON data for inspection
    output_json = os.path.join(BASE_DIR, "proposals", "comparison_metrics_v1.json")
    with open(output_json, 'w', encoding='utf-8') as f:
        json.dump(comparison_data, f, indent=2)

if __name__ == "__main__":
    main()
