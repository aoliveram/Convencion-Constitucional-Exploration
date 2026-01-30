
import pyreadr
import pandas as pd
import json
import os

# Define paths
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RDS_PATH = os.path.join(BASE_DIR, "proposals", "11-sentences_borrador.rds")
OUTPUT_PATH = os.path.join(BASE_DIR, "proposals", "draft_1_text.json")

def main():
    print(f"Loading data from {RDS_PATH}...")
    try:
        # Load the RDS file
        # pyreadr.read_r returns a dictionary associated with the object names
        # Usually the object inside is None (if saved with saveRDS) or the name (save)
        # For a single object RDS, pyreadr puts it in key None
        result = pyreadr.read_r(RDS_PATH)
        df_key = list(result.keys())[0] # Get the first key
        df = result[df_key]
        
        print(f"Data loaded. Columns: {df.columns.tolist()}")
        
        # Verify columns exist
        required_columns = ["id_articulo_borrador", "oracion", "id_oracion_borrador", "oracion_limpia"]
        missing_cols = [col for col in required_columns if col not in df.columns]
        if missing_cols:
            print(f"Error: Missing columns {missing_cols}")
            return

        # Structure the data
        articles = []
        
        # Group by article ID
        # Assuming id_articulo_borrador preserves order or we want to sort by it? 
        # Usually pandas groupby preserves order of appearance if sort=False, dependent on implementation
        # Let's ensure we process them in a logical order if possible. 
        # If the IDs are integers, we can sort. If strings, maybe alphanumeric.
        # Let's trust the file order for now via simple iteration or groupby without sort if order matters.
        
        # Using groupby
        grouped = df.groupby("id_articulo_borrador", sort=False)
        
        for art_id, group in grouped:
            # Construct article text by joining sentences
            # Using 'oracion' for the full text, assuming it has punctuation/spacing
            # or 'oracion_limpia' depending on user preference? 
            # User mentioned "oracion" and "oracion_limpia". "oracion" likely has the original format.
            full_text = " ".join(group["oracion"].astype(str).tolist())
            
            article_data = {
                "article_id": str(art_id),
                "text": full_text,
                "sentences": []
            }
            
            for _, row in group.iterrows():
                article_data["sentences"].append({
                    "sentence_id": str(row["id_oracion_borrador"]),
                    "text": str(row["oracion"]),
                    "clean_text": str(row["oracion_limpia"])
                })
            
            articles.append(article_data)
            
        # Save to JSON
        print(f"Constructed {len(articles)} articles.")
        with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
            json.dump(articles, f, ensure_ascii=False, indent=2)
            
        print(f"Saved structured draft to {OUTPUT_PATH}")

    except Exception as e:
        print(f"An error occurred: {e}")

if __name__ == "__main__":
    main()
