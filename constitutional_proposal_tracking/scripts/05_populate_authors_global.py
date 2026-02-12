import json
import os
import re
import glob

# --- CONFIGURATION ---
BASE_DIR = "/Users/anibaloliveramorales/Desktop/Doctorado/-Projects-/B - Convención Constitucional - Data/constitutional_proposal_tracking"
SUBMITTED_INITIATIVES_DIR = os.path.join(BASE_DIR, "submitted_initiatives")

# Exclude Comision 2 as requested
TARGET_COMISSIONS = [1, 3, 4, 5, 6, 7] 

def normalize_icc_id(icc_string):
    """
    Normalizes ICC ID by removing "ICC N°" prefix and stripping suffixes like "-3".
    Example: "ICC N° 514-3" -> "514"
             "514-3" -> "514"
             "514" -> "514"
    """
    s = str(icc_string).strip()
    # Remove "ICC N°" prefix types
    s = re.sub(r'ICC\s*N°\s*', '', s, flags=re.IGNORECASE)
    # Remove anything after a hyphen if it looks like a suffix number
    # (Checking if it fits the pattern "DIGITS-DIGIT")
    if '-' in s:
        parts = s.split('-')
        if len(parts) >= 2 and parts[0].isdigit():
             return parts[0]
    return s

def load_authors_map():
    """
    Loads all authors from submitted_initiatives JSONs into a map.
    Returns: { "514": ["Tammy Pustilnick", ...], ... }
    """
    authors_map = {}
    
    # Find all api_extracted files
    pattern = os.path.join(SUBMITTED_INITIATIVES_DIR, "api_extracted_*_corrected_4.json")
    files = glob.glob(pattern)
    
    print(f"Loading authors from {len(files)} files...")
    
    for file_path in files:
        with open(file_path, 'r', encoding='utf-8') as f:
            try:
                data = json.load(f)
                # Data structure is: { "filename.pdf": { "propuesta_norma": ..., "firmantes_matched": [...] }, ... }
                
                for key, val in data.items():
                    # Extract ID from key or content? 
                    # The keys in these files are filenames like "99-3-c-Iniciativa...pdf".
                    # The ID usually is the first number BEFORE the first hyphen.
                    match = re.match(r'^(\d+)-', key)
                    if match:
                        raw_id = match.group(1)
                        norm_id = normalize_icc_id(raw_id)
                        
                        firmantes = val.get('firmantes_matched', [])
                        if firmantes:
                            # If ID exists, we extend/merge just in case, though usually IDs are unique per file
                            if norm_id in authors_map:
                                # Merge and dedup
                                existing = set(authors_map[norm_id])
                                new_ones = set(firmantes)
                                authors_map[norm_id] = list(existing.union(new_ones))
                            else:
                                authors_map[norm_id] = firmantes
            except Exception as e:
                print(f"Error reading {file_path}: {e}")
                
    print(f"Loaded author map with {len(authors_map)} unique initiative IDs.")
    return authors_map

def process_commission(com_n, authors_map):
    com_dir = os.path.join(BASE_DIR, f"comision-{com_n}", "genesis-extracted")
    
    # Updated pattern to be more flexible:
    # Match C{n}_GENESIS_... .json
    # This covers 'C5_GENESIS_informe-1-03-01-texto-sistematizado.json' as well
    pattern = os.path.join(com_dir, f"C{com_n}_GENESIS_*.json")
    files = glob.glob(pattern)
    
    # Filter out _PREVIEW and _enriched and _candidates, but ensure it contains 'texto-sistematizado'
    # or at least 'GENESIS' which is already in the glob.
    genesis_files = [
        f for f in files 
        if "_PREVIEW" not in f 
        and "_enriched" not in f 
        and "_candidates" not in f
        and "texto-sistematizado" in f
    ]
    
    if not genesis_files:
        print(f"No base GENESIS file found for Comision {com_n}")
        return

    # Assuming one main genesis file per commission usually
    target_file = genesis_files[0]
    print(f"Processing Comision {com_n}: {os.path.basename(target_file)}")
    
    with open(target_file, 'r', encoding='utf-8') as f:
        articles = json.load(f)
        
    updated_count = 0
    
    for article in articles:
        # Get source IDs
        sources = article.get('sources', [])
        # Iterate and find authors
        article_authors = set()
        
        # sources might be a string "514" or list ["514"]
        if isinstance(sources, str):
            sources = [sources]
            
        for src in sources:
            norm_src = normalize_icc_id(src)
            if norm_src in authors_map:
                article_authors.update(authors_map[norm_src])
            else:
                # Debug: only print if source is not empty
                if src:
                    # print(f"  Warning: Source ID '{src}' (norm: {norm_src}) not found in map.")
                    pass
        
        # Add to article
        article['authors'] = list(article_authors)
        if article_authors:
            updated_count += 1
            
    # Save as _enriched
    base_name = os.path.basename(target_file)
    name_part, ext = os.path.splitext(base_name)
    new_name = f"{name_part}_enriched{ext}"
    output_path = os.path.join(com_dir, new_name)
    
    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(articles, f, ensure_ascii=False, indent=2)
        
    print(f"Saved {new_name}. Updated {updated_count}/{len(articles)} articles with authors.")

def main():
    print("Starting Author Population Script...")
    auth_map = load_authors_map()
    
    for com_n in TARGET_COMISSIONS:
        process_commission(com_n, auth_map)
        
    print("Done.")

if __name__ == "__main__":
    main()
