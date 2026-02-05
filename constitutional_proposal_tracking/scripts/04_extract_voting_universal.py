
import os
import sys
import json
import re
import glob
import time
import google.generativeai as genai

# --- Setup Imports ---
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(current_dir)
sys.path.append(project_root)

# Try importing config
try:
    from constitutional_proposal_tracking.config.commission_profiles import PROMPTS, COMMISSION_MAP
except ImportError:
    sys.path.append(os.path.dirname(project_root))
    from constitutional_proposal_tracking.constitutional_proposal_tracking.config.commission_profiles import PROMPTS, COMMISSION_MAP

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") or os.environ.get("GOOGLE_API_KEY")
BASE_DIR = os.path.dirname(current_dir)
MEMBERS_PATH = os.path.join(BASE_DIR, "convention_members.json")

def load_members():
    if os.path.exists(MEMBERS_PATH):
        with open(MEMBERS_PATH, 'r', encoding='utf-8') as f:
            return json.load(f)
    return []

def extract_voting(pdf_path, commission_id, members_list):
    if not API_KEY:
        raise ValueError("API Key not found.")
        
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel('gemini-3-flash-preview')
    
    # 1. Check Profile
    profile = COMMISSION_MAP.get(commission_id, {})
    voting_strategy = profile.get("voting", "NARRATIVE_VOTING")
    
    if voting_strategy == "CUSTOM_COMPLEX":
        print(f"  [SKIP] Complex voting strategy required (e.g. Com 2). Use specialized script.")
        return None
        
    print(f"  Strategy: {voting_strategy}")
    prompt_template = PROMPTS.get(voting_strategy)
    
    # Inject Members list into prompt context for better matching
    members_str = ", ".join(members_list)
    full_prompt = f"{prompt_template}\n\nOfficial Member List for Matching:\n{members_str}"
    
    print(f"  Uploading {os.path.basename(pdf_path)}...")
    sample_file = genai.upload_file(path=pdf_path)
    
    # 2. Generate
    response = model.generate_content([full_prompt, sample_file])
    
    # Cleanup
    try:
        genai.delete_file(sample_file.name)
    except:
        pass
        
    # 3. Parse
    try:
        text = response.text
        # Look for the first '[' and last ']' to extract the JSON array
        match = re.search(r'\[.*\]', text, re.DOTALL)
        if match:
            json_str = match.group(0)
            data = json.loads(json_str)
            return data
        else:
            # Fallback to previous method if no brackets found
            clean_text = text.replace('```json', '').replace('```', '').strip()
            return json.loads(clean_text)
    except Exception as e:
        print(f"  Error parsing Gemini response: {e}")
        return []

def main():
    print("--- Universal Voting Extraction ---")
    
    members = load_members()
    print(f"Loaded {len(members)} convention members.")
    
    found_files = []
    for i in range(1, 8):
        pdf_dir = os.path.join(BASE_DIR, f"comision-{i}", "PDFs")
        if not os.path.exists(pdf_dir): continue
        
        # Look for VOTACION files
        files = glob.glob(os.path.join(pdf_dir, f"C{i}_VOTACION_*.pdf"))
        found_files.extend([(f, i) for f in files])
        
    print(f"Found {len(found_files)} voting files to process.")
    
    for pdf_path, com_id in found_files:
        print(f"\nProcessing {os.path.basename(pdf_path)} (Com {com_id})...")
        
        out_dir = os.path.join(BASE_DIR, f"comision-{com_id}", "indicaciones-universal-extracted")
        if not os.path.exists(out_dir): os.makedirs(out_dir)
        
        out_name = os.path.basename(pdf_path).replace(".pdf", ".json")
        out_path = os.path.join(out_dir, out_name)
        
        if os.path.exists(out_path):
            print("  Skipping (Already Exists)")
            continue
            
        try:
            results = extract_voting(pdf_path, com_id, members)
            
            if results is None:
                continue # Skipped complex
                
            with open(out_path, 'w', encoding='utf-8') as f:
                json.dump(results, f, ensure_ascii=False, indent=2)
            
            print(f"  Saved {len(results)} approved indications.")
            time.sleep(1) # Rate limit hygiene
            
        except Exception as e:
            print(f"  FAILED: {e}")

if __name__ == "__main__":
    main()
