
import os
import sys
import json
import re
import glob
import google.generativeai as genai

# --- Setup Imports ---
# Add project root to path to import config
current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(current_dir) # constitutional_proposal_tracking/
sys.path.append(project_root)

# Try importing config
try:
    from constitutional_proposal_tracking.config.commission_profiles import PROMPTS, COMMISSION_MAP
except ImportError:
    # Fallback if structure is slightly different or running from different cwd
    # Try direct import if we are deeper
    sys.path.append(os.path.dirname(project_root))
    from constitutional_proposal_tracking.constitutional_proposal_tracking.config.commission_profiles import PROMPTS, COMMISSION_MAP

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") or os.environ.get("GOOGLE_API_KEY")
BASE_DIR = os.path.dirname(current_dir)

def extract_genesis(pdf_path, commission_id):
    if not API_KEY:
        raise ValueError("API Key not found.")
        
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel('gemini-3-flash-preview')
    
    print(f"Uploading {os.path.basename(pdf_path)}...")
    sample_file = genai.upload_file(path=pdf_path)
    
    # 1. Get Profile
    profile = COMMISSION_MAP.get(commission_id, {})
    prompt_key = profile.get("genesis", "NARRATIVE_GENESIS") # Default
    
    if prompt_key == "CUSTOM_COMPLEX":
        print(f"  [SKIP] Complex genesis strategy required for Commission {commission_id}.")
        return []
        
    prompt_text = PROMPTS.get(prompt_key)
    
    print(f"Strategy: {prompt_key}")
    
    # 2. Generate
    response = model.generate_content([prompt_text, sample_file])
    
    # 3. Clean and Parse
    try:
        text = response.text.replace('```json', '').replace('```', '').strip()
        data = json.loads(text)
        return data
    except Exception as e:
        print(f"Error parsing Gemini response: {e}")
        return []

def post_process_data(data, commission_id):
    """
    Apply specific cleaning rules per commission.
    """
    cleaned = []
    for item in data:
        art = item.get("article", "Unknown")
        sources = item.get("sources", [])
        
        # Rule: Com 2 & 7 often return IDs like "(89-2)". We want "89-2".
        if commission_id in [2, 7]:
            new_sources = []
            for s in sources:
                # Remove parens
                clean_s = s.replace("(", "").replace(")", "").strip()
                new_sources.append(clean_s)
            sources = new_sources
            
        item["sources"] = sources
        cleaned.append(item)
    return cleaned

def main():
    print("--- Universal Genesis Extraction ---")
    
    # Iterate all commissions looking for GENESIS files
    found_files = []
    for i in range(1, 8):
        # Look for standard "GENESIS" files
        pdf_dir = os.path.join(BASE_DIR, f"comision-{i}", "PDFs")
        if not os.path.exists(pdf_dir): continue
        
        files = glob.glob(os.path.join(pdf_dir, f"C{i}_GENESIS_*.pdf"))
        found_files.extend([(f, i) for f in files])

    print(f"Found {len(found_files)} genesis files to process.")
    
    for pdf_path, com_id in found_files:
        print(f"\nProcessing {os.path.basename(pdf_path)} (Com {com_id})...")
        
        # Check if already done?
        out_dir = os.path.join(BASE_DIR, f"comision-{com_id}", "genesis-extracted")
        if not os.path.exists(out_dir): os.makedirs(out_dir)
        
        out_name = os.path.basename(pdf_path).replace(".pdf", ".json")
        out_path = os.path.join(out_dir, out_name)
        
        if os.path.exists(out_path):
            print("  Skipping (Already Exists)")
            continue
            
        try:
            raw_data = extract_genesis(pdf_path, com_id)
            final_data = post_process_data(raw_data, com_id)
            
            with open(out_path, 'w', encoding='utf-8') as f:
                json.dump(final_data, f, ensure_ascii=False, indent=2)
                
            print(f"  Saved {len(final_data)} articles to {out_name}")
            
        except Exception as e:
            print(f"  FAILED: {e}")

if __name__ == "__main__":
    main()
