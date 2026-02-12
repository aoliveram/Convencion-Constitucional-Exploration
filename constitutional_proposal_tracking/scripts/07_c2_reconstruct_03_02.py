import json
import re
import os

# Configuration Paths
BASE_DIR = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking"
GENESIS_FILE = os.path.join(BASE_DIR, "comision-2/genesis-extracted/C2_GENESIS_texto-sistematizado-02-16.json")
INDICATIONS_FILE = os.path.join(BASE_DIR, "comision-2/indicaciones-api-extracted/C2_COMPLEX_informe-reemplazo-1-03-02.json")
OUTPUT_FILE = os.path.join(BASE_DIR, "comision-2/reconstructed/C2_GENESIS_texto-sistematizado-03-02.json")

def load_json(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        return json.load(f)

def clean_indication_text(content):
    """
    Extracts the actual legislative text from the wrapper language of the indication.
    Examples:
    - "Indicación de X para sustituir el artículo 1 por el siguiente: 'El Estado...'" -> "El Estado..."
    - "Indicación de Y para agregar... lo siguiente: “El Estado...”" -> "El Estado..."
    """
    # 1. Look for text inside quotes (smart quotes or straight quotes) after a colon
    # This covers: ... por el siguiente: “TEXTO”
    match_quotes = re.search(r':\s*[“"«]([\s\S]*?)[”"»]\.?\s*$', content)
    if match_quotes:
        return match_quotes.group(1).strip()

    # 2. Look for text after specific phrases like "por el siguiente:" or "lo siguiente:"
    # This covers cases where quotes might be missing
    match_following = re.search(r'(?:por el siguiente|lo siguiente|siguiente tenor):\s*([\s\S]*)', content, re.IGNORECASE)
    if match_following:
        raw_text = match_following.group(1).strip()
        # Clean potential trailing period or quote residue
        raw_text = raw_text.strip('“"«”"»').strip()
        if raw_text.endswith("."):
            pass # Keep period if it looks like sentence end
        return raw_text

    # Default: Return content as is (maybe it's a small modification without standard wrapper)
    # But usually for "Reemplazo" tables, it's the whole cell.
    # If the content is short and doesn't look like wrapper text, return it.
    if len(content) < 50: 
        return content
    
    # Fallback for complex unquoted replacements: try to identify the split manually
    # If we can't find a clean split, return the whole thing but mark it effectively
    return content

def normalize_article_key(key):
    """Normalizes 'Artículo 1 A' to 'Artículo 1A' for better matching if needed, or keeps consistent."""
    return key.strip().replace("  ", " ")

def main():
    print(f"Loading Genesis: {GENESIS_FILE}")
    genesis_data = load_json(GENESIS_FILE)
    
    print(f"Loading Indications: {INDICATIONS_FILE}")
    indications_data = load_json(INDICATIONS_FILE)

    # Index Base Articles for fast access
    # Structure: { "Artículo 1": { "text": "...", "sources": [...], "history": [] } }
    article_map = {}
    for item in genesis_data:
        key = normalize_article_key(item['article'])
        # Initialize history with origin
        item['history'] = [{
            "step": "Genesis (02-16)",
            "action": "Origin",
            "authors": item.get('sources', []), # These are source IDs (Boletines)
            "text_snapshot": item['text']
        }]
        article_map[key] = item

    # Process Indications
    articles_to_delete = []
    
    for group in indications_data:
        article_ref = normalize_article_key(group['article_ref'])
        base_text_in_indication = group.get('base_text', '')
        
        # Check if this article exists in our base
        if article_ref not in article_map:
            # Maybe it's a NEW article proposed?
            # Or a mismatch in naming?
            # For now, we will track it but warn.
            print(f"WARNING: Indication targets '{article_ref}' which is not in Genesis Base.")
            # Create a placeholder if it's a constructive indication
            article_map[article_ref] = {
                "article": article_ref,
                "text": "",
                "sources": [],
                "history": []
            }

        target_article = article_map[article_ref]
        
        # Iterate through indications for this article
        # We need to sequence them? usually only one is approved per article in this "Replacement" logic.
        approved_indications = [ind for ind in group['indications'] if (ind.get('result') or '').lower() == 'aprobado']
        
        for ind in approved_indications:
            ind_number = ind.get('number', '?')
            ind_content = ind.get('content', '')
            ind_authors = ind.get('authors', [])
            
            # Determine Action Type
            action = "Modify"
            new_text = target_article['text'] # Default to current
            
            is_deletion = any(k in ind_content.lower() for k in ['suprimir', 'eliminar', 'derogar'])
            is_replacement = any(k in ind_content.lower() for k in ['sustituir', 'reemplazar'])
            is_addition = 'agregar' in ind_content.lower()
            
            if is_deletion:
                action = "Delete"
                articles_to_delete.append(article_ref)
                new_text = "[ELIMINADO]"
            elif is_replacement:
                action = "Replace"
                extracted_text = clean_indication_text(ind_content)
                if extracted_text and extracted_text != ind_content:
                    new_text = extracted_text
                else:
                     # Fallback: if we can't extract cleanly, use the whole content but warn?
                     # Actually for "Replace", usually the whole content is the text if it's from a table cell.
                     # But here we have "Indicación...".
                     # If extraction failed (returned same content or None), we assume failure.
                     pass 
            elif is_addition:
                action = "Add"
                extracted_text = clean_indication_text(ind_content)
                if extracted_text:
                    # Append with newline
                    new_text += "\n" + extracted_text
            
            # Record History
            history_entry = {
                "step": "Report 1 (03-02)",
                "indication_number": ind_number,
                "action": action,
                "authors": ind_authors, # These are Convencional Names
                "raw_content": ind_content,
                "text_snapshot": new_text
            }
            target_article['history'].append(history_entry)
            
            # Apply Change
            target_article['text'] = new_text

    # Final Compilation
    # Filter out deleted articles
    final_articles = []
    
    # We want to preserve order. The original Genesis list order is good.
    # But we might have new keys added to article_map.
    # Let's rebuild list from article_map, but try to respect original order + appended news.
    
    # Original keys order
    original_keys = [normalize_article_key(item['article']) for item in genesis_data]
    
    for key in original_keys:
        if key in articles_to_delete:
            continue
        final_articles.append(article_map[key])
        
    # Check for any NEW articles that were added to map but weren't in original keys
    for key, val in article_map.items():
        if key not in original_keys and key not in articles_to_delete:
            final_articles.append(val)

    # Write Output
    os.makedirs(os.path.dirname(OUTPUT_FILE), exist_ok=True)
    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        json.dump(final_articles, f, indent=2, ensure_ascii=False)
        
    print(f"Success! Reconstructed {len(final_articles)} articles.")
    print(f"Deleted {len(articles_to_delete)} articles: {articles_to_delete}")
    print(f"Output saved to: {OUTPUT_FILE}")

if __name__ == "__main__":
    main()
