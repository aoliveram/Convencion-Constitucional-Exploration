
import os
import json
import glob
import re
from typing import Dict, List, Any, Optional

# --- Configuration ---
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROPOSALS_DIR = os.path.join(BASE_DIR, "proposals")
DRAFT_0_PATH = os.path.join(PROPOSALS_DIR, "draft_0_mapping.json")
OUTPUT_PATH = os.path.join(PROPOSALS_DIR, "draft_reconstructed_com6.json")

def load_json(path):
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)

# --- Classes for Evolution ---

class ArticleHistory:
    def __init__(self, article_id: str, initial_text: str, initial_source: str, initial_authors: List[str]):
        self.id = article_id
        # The 'current_text' is strictly the TEXT content.
        # In a more advanced version, this could be a list of paragraphs (Analysis unit).
        self.current_text = initial_text
        
        # 'authors' is a SET of all contributors to this article's current state
        self.authors = set(initial_authors)
        
        # 'history' logs every event that touched this article
        self.history = [{
            "step": 0,
            "event": "GENESIS",
            "source": initial_source,
            "description": "Initial creation from initiative mapping",
            "authors_added": initial_authors
        }]
        
        self.is_deleted = False

    def apply_modification(self, step_id: str, source_file: str, action: str, 
                           content: str, new_authors: List[str], indication_num: str):
        
        if self.is_deleted and action != "ADD":
            # Can't modify a deleted article unless it's a re-addition (rare)
            return

        event_desc = ""
        
        if action == "DELETE":
            self.is_deleted = True
            self.current_text = ""
            event_desc = f"Deleted by Indication {indication_num}"
            
        elif action == "MODIFY":
            # Replace text
            # ideally we would diff, but for now we replace
            self.current_text = content
            event_desc = f"Modified by Indication {indication_num}"
            self.authors.update(new_authors)
            
        elif action == "ADD":
            # Cases where ADD targets an existing article usually mean 
            # "Add paragraph to Art X". 
            # If content implies a full replacement or new text, append or replace?
            # For simplicity in V1: specific "ADD to Art X" appends text.
            self.current_text += "\n" + content
            event_desc = f"Content added by Indication {indication_num}"
            self.authors.update(new_authors)

        self.history.append({
            "step": step_id,
            "event": action,
            "source": source_file,
            "description": event_desc,
            "authors_added": new_authors,
            "indication_number": indication_num
        })

    def to_dict(self):
        return {
            "article_id": self.id,
            "text": self.current_text,
            "status": "DELETED" if self.is_deleted else "ACTIVE",
            "authors": list(self.authors),
            "history": self.history
        }

class HistorianEngine:
    def __init__(self):
        self.articles: Dict[str, ArticleHistory] = {}
        self.orphans: List[Dict] = [] # Track indications that didn't find a target

    def normalize_id(self, key):
        # Extract purely numeric ID if possible for matching
        match = re.search(r'(\d+)', str(key))
        return match.group(1) if match else str(key)

    def load_genesis(self, draft_0_data: List[Dict]):
        print("Loading Genesis (Draft 0)...")
        for entry in draft_0_data:
            # Entry: {"article": "Artículo 1", "text": "...", "sources": [...]}
            # We need to extract the ID and initial authors
            
            raw_id = entry.get("article", "Unknown")
            norm_id = self.normalize_id(raw_id)
            
            # Initial text logic:
            # The draft_0_mapping currently links Art -> Initiative
            # It DOES NOT usually contain the full text of the article unless we fetched it.
            # *Correction*: In previous steps we created 'draft_final_text.json' (text) 
            # AND 'draft_0_mapping.json' (mapping).
            # We strictly need the "Text 0". 
            # As per user instruction, draft_0_mapping is the key. 
            # BUT draft_0_mapping might lack the text. 
            
            # CRITICAL ASSUMPTION for V1:
            # We will initialize with EMPTY text if not found, or use the mapping structure.
            # Let's inspect draft_0 structure in run-time if needed.
            # Assuming draft_0 has 'sources' -> 'authors'.
            
            initial_authors = []
            sources_list = entry.get("sources", [])
            source_labels = []
            
            for src in sources_list:
                # src: {"initiative_id": "...", "authors_matched": [...]}
                # Note: draft_0_mapping structure from step 2 might vary.
                # Adapting to likely structure:
                m_authors = src.get("match_details", {}).get("authors_matched", []) 
                # OR src.get("authors", []) check step 02 output format.
                # Let's try flexible extraction
                
                if "firmantes_matched" in src:
                     initial_authors.extend(src["firmantes_matched"])
                elif "authors_matched" in src:
                    initial_authors.extend(src["authors_matched"])
                
                source_labels.append(src.get("initiative_id", "Unknown"))

            art = ArticleHistory(
                article_id=norm_id,
                initial_text="", # Text 0 is mostly unknown/placeholder in this mapping approach
                initial_source=f"Initiatives {', '.join(source_labels)}",
                initial_authors=list(set(initial_authors))
            )
            self.articles[norm_id] = art

    def process_indications(self, files: List[str]):
        print(f"Processing {len(files)} indication files...")
        
        # Sort files to ensure chronological order (very important)
        # Using filename sorting might correspond to date (1-02-08, 2-02-23...)
        sorted_files = sorted(files)

        for filepath in sorted_files:
            fname = os.path.basename(filepath)
            data = load_json(filepath)
            print(f"Applying patches from {fname} ({len(data)} modifications)...")
            
            for ind in data:
                # Indication Fields: number, target_article, action, content, authors_matched
                target_raw = ind.get("target_article", "")
                target_id = self.normalize_id(target_raw)
                action = ind.get("action", "MODIFY")
                content = ind.get("content", "")
                authors = ind.get("authors_matched", [])
                ind_num = ind.get("number", "?")

                # --- 1. Renumbering Logic Detection ---
                # Check if target says "pasa a ser X"
                # Not implemented in V1 for simplicity, assuming target_id is the OLD id.
                
                # --- 2. Find Target ---
                if target_id in self.articles:
                    self.articles[target_id].apply_modification(
                        step_id=fname,
                        source_file=fname,
                        action=action,
                        content=content,
                        new_authors=authors,
                        indication_num=ind_num
                    )
                elif action == "ADD":
                    # Create NEW article
                    # If target is "Artículo Nuevo" or "Artículo 20 bis", we create it.
                    new_art = ArticleHistory(
                        article_id=target_id,
                        initial_text=content,
                        initial_source=f"Indication {ind_num} ({fname})",
                        initial_authors=authors
                    )
                    self.articles[target_id] = new_art
                    new_art.history[0]["event"] = "CREATION_BY_INDICATION"
                else:
                    # Modify/Delete on non-existent article
                    self.orphans.append({
                        "file": fname,
                        "indication": ind_num,
                        "target": target_raw,
                        "reason": "Target ID not found in genesis or created yet"
                    })

    def export(self):
        output = []
        for aid, art_obj in self.articles.items():
            output.append(art_obj.to_dict())
        return output

def main():
    print("--- Starting Reconstruction Engine (The Historian) ---")
    
    engine = HistorianEngine()
    
    # 1. Load Draft 0
    if os.path.exists(DRAFT_0_PATH):
        draft_0 = load_json(DRAFT_0_PATH)
        engine.load_genesis(draft_0)
    else:
        print("Error: Draft 0 mapping not found.")
        return

    # 2. Get Indication Files (Searching across all comisions)
    files = glob.glob(os.path.join(BASE_DIR, "comision-*", "indicaciones-api-extracted", "extracted_informe-indicaciones-*.json"))
    
    # Also include the pilot batch if it's there
    pilot_files = glob.glob(os.path.join(BASE_DIR, "comision-*", "indicaciones-api-extracted", "indications_batch_*.json"))
    files.extend(pilot_files)
    
    # 3. Process
    engine.process_indications(files)
    
    # 4. Save
    result = engine.export()
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(result, f, ensure_ascii=False, indent=2)
        
    print(f"Reconstruction complete. Saved to {OUTPUT_PATH}")
    print(f"Total Articles Tracked: {len(result)}")
    print(f"Orphan Indications (Failed to apply): {len(engine.orphans)}")
    
    # Save orphans for debug
    if engine.orphans:
        orphan_path = OUTPUT_PATH.replace(".json", "_orphans.json")
        with open(orphan_path, 'w', encoding='utf-8') as f:
            json.dump(engine.orphans, f, indent=2)
        print(f"Orphan log saved to {orphan_path}")

if __name__ == "__main__":
    main()
