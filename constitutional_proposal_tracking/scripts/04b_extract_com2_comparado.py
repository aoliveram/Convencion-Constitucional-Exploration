
import os
import json
import glob
import google.generativeai as genai
import difflib

# --- Configuration ---
API_KEY = os.environ.get("GEMINI_API_KEY") 
if not API_KEY:
    API_KEY = os.environ.get("GOOGLE_API_KEY")

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PDF_DIR = os.path.join(BASE_DIR, "comision-2", "PDFs")

# Files defining the "Goal" (Approved Text)
GOAL_FILES = [
    os.path.join(PDF_DIR, "informe-reemplazo-2-03-23-1.pdf"),
    os.path.join(PDF_DIR, "informe-reemplazo-3-03-23-2.pdf")
]

# File defining the "Candidates" (Indications)
CANDIDATE_FILE = os.path.join(PDF_DIR, "informe-4-04-08-comparado.pdf")

OUTPUT_PATH = os.path.join(BASE_DIR, "comision-2", "indicaciones-api-extracted", "indications_comparado_matched.json")
MEMBERS_PATH = os.path.join(BASE_DIR, "convention_members.json")

def load_json(path):
    with open(path, 'r', encoding='utf-8') as f:
        return json.load(f)

def extract_goal_text(files: list):
    """
    Extracts the full text of articles that were approved in the replacement reports.
    Returns: Dict { "Article 1": "Full text...", "Article 2": "Full text..." }
    """
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel('gemini-3-flash-preview')
    
    combined_goals = {}
    
    for pdf in files:
        print(f"Extracting Goal Text from {os.path.basename(pdf)}...")
        sample_file = genai.upload_file(path=pdf)
        
        prompt = """
        Extract all Articles found in this document. 
        This document represents the "Approved New Articles".
        Return a JSON object where keys are the Article Name (e.g., "Artículo 1", "Artículo 12") and values are the Full Text content.
        Do NOT summarize. Extract exact text.
        """
        
        response = model.generate_content([prompt, sample_file])
        try:
            text = response.text.replace('```json', '').replace('```', '').strip()
            data = json.loads(text)
            combined_goals.update(data)
        except Exception as e:
            print(f"Error parsing goal text: {e}")
            
    return combined_goals

def extract_candidates(pdf_path: str):
    """
    Extracts all indications from the Comparado file.
    Returns: List of { "number": "1", "content": "...", "authors": ... }
    """
    genai.configure(api_key=API_KEY)
    model = genai.GenerativeModel('gemini-3-flash-preview')
    
    print(f"Extracting Candidates from {os.path.basename(pdf_path)}...")
    sample_file = genai.upload_file(path=pdf_path)
    
    # We load members for matching
    members_list = load_json(MEMBERS_PATH) if os.path.exists(MEMBERS_PATH) else []
    members_str = ", ".join(members_list)

    prompt = f"""
    You are analyzing a "Comparado" document (Comparative Table).
    Column 2 contains "INDICACIONES".
    
    Task:
    - Extract ALL INDICATIONS present in the table.
    - Ignore the "Text Systematized" (Col 1) and "Result" (Col 3, often empty).
    - For each indication, extract:
      1. "number": The indication ID (e.g., "115", "116", "2-7").
      2. "content": The text of the proposal.
      3. "target_article_guess": Try to guess which article this refers to (e.g. from context "suprimir el artículo 14").
      4. "authors_matched": Match authors against the list below.
      
    **Official Member List:**
    {members_str}
    
    Return a JSON List.
    """
    
    response = model.generate_content([prompt, sample_file])
    try:
        text = response.text.replace('```json', '').replace('```', '').strip()
        data = json.loads(text)
        return data
    except Exception as e:
        print(f"Error parsing candidates: {e}")
        return []


def clean_candidate_content(text):
    # Try to find content inside smart quotes or normal quotes
    import re
    # Pattern for “...” matching longest possible or finding multiple?
    # Using non-greedy but we iterate to find a "substantial" one
    matches = re.finditer(r'[“"](.+?)[”"]', text, re.DOTALL)
    best_candidate = ""
    for m in matches:
        val = m.group(1).strip()
        if len(val) > len(best_candidate):
            best_candidate = val
            
    if len(best_candidate) > 10: # Minimum substantial length
        return best_candidate
        
    return text # Fallback to full text if no substantial quote found

def clean_goal_content(text):
    import re
    # Remove "Artículo X.- Title."
    text = re.sub(r'^Artículo \d+.*?-', '', text).strip()
    # Remove parenthetical notes referencing approvals e.g. (Inciso primero aprobado...)
    text = re.sub(r'\(.*?aprobado.*?\)', '', text, flags=re.IGNORECASE).strip()
    return text

def match_logic(goals: dict, candidates: list):
    """
    The Puzzle Solver: Finds which candidate matches the goal text.
    """
    matched_indications = []
    
    print(f"Matching {len(goals)} goals against {len(candidates)} candidates...")
    
    for art_name, goal_text in goals.items():
        # Normalize Goal: remove newlines, extra spaces, and headers
        goal_curr = clean_goal_content(goal_text)
        goal_clean = " ".join(goal_curr.split())
        best_ratio = 0.0
        best_candidate = None
        
        for cand in candidates:
            cand_raw = cand.get("content", "")
            
            # Skip withdrawals or suppressions (they don't produce text)
            if "Retirada" in cand_raw or "suprimir" in cand_raw.lower():
                continue
                
            cand_clean = clean_candidate_content(cand_raw)
            cand_clean = " ".join(cand_clean.split())
            
            # 1. Direct Containment (Strongest Signal)
            # If the goal text is fully inside the candidate (quoted part), or vice versa
            if len(goal_clean) > 20 and (goal_clean in cand_clean or cand_clean in goal_clean):
                ratio = 1.0
            else:
                ratio = difflib.SequenceMatcher(None, goal_clean, cand_clean).ratio()
            
            if ratio > best_ratio:
                best_ratio = ratio
                best_candidate = cand
        
        # Threshold
        if best_ratio > 0.6: # Lowered threshold slightly
            print(f"Match found for {art_name}: Indication {best_candidate['number']} (Score: {best_ratio:.2f})")
            
            # Create the approved indication object
            matched_indications.append({
                "number": best_candidate.get("number"),
                "target_article": art_name,
                "action": "MODIFY", 
                "content": goal_text, 
                "authors_matched": best_candidate.get("authors_matched", []),
                "match_score": best_ratio,
                "original_candidate_text": best_candidate.get("content"),
                "note": "Matched via Comparado logic"
            })
        else:
            print(f"No good match for {art_name} (Best score: {best_ratio:.2f})")

            
    return matched_indications


def main():
    print("--- Starting Complex Matching Step 3 (Comision 2) ---")
    
    if not os.path.exists(CANDIDATE_FILE):
        print("Candidate file missing.")
        return

    # 1. Get the Goals (The 'Answer Key')
    goals = extract_goal_text(GOAL_FILES)
    print(f"DEBUG: Found {len(goals)} goal articles.")
    
    # 2. Get the Candidates (The 'Possibilities')
    candidates = extract_candidates(CANDIDATE_FILE)
    print(f"DEBUG: Found {len(candidates)} candidate indications.")
    
    # 3. Solve
    final_matches = match_logic(goals, candidates)
    
    with open(OUTPUT_PATH, 'w', encoding='utf-8') as f:
        json.dump(final_matches, f, ensure_ascii=False, indent=2)
        
    print(f"Saved {len(final_matches)} matched indications to {OUTPUT_PATH}")

if __name__ == "__main__":
    main()
