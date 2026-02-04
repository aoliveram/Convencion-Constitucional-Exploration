
# --- Prompt Templates ---

PROMPTS = {
    "NARRATIVE_GENESIS": """
    You are an expert data extractor. Attached is a "Texto Sistematizado".
    
    Task:
    - Extract ALL Articles.
    - Structure: Articles usually start with "Artículo X".
    - Source Extraction: Search for the "Initiative ID" or "Boletín" that generated it.
      - In narrative documents, this is often found in parentheses at the end of the article (e.g., "(89-6)") or in a footnote/header reference.
    - Output JSON: [{"article": "Artículo 1", "text": "...", "sources": ["89-6"]}]
    """,

    "TABULAR_GENESIS": """
    You are an expert data extractor analyzing a TABLE.
    
    Task:
    - Analyze the visual table structure.
    - Column 1 usually contains the "Boletín" or "Iniciativa" ID (e.g., 45-3).
    - Column 2 (or adjacent) contains the "Propuesta" or "Texto".
    - Extract the mapping: Article Text -> Initiative Source.
    - Output JSON: [{"article": "Article found in text", "text": "...", "sources": ["initiative_id_from_col_1"]}]
    """,

    "NARRATIVE_VOTING": """
    You are an expert legal data extractor. Attached is a Voting Report.
    
    Task:
    - Extract ONLY "Indicaciones Aprobadas" (Approved).
    - Scan the text for keywords like "Se aprueba", "Aprobada", "Votación Separada: Aprobada".
    - For each approved item, extract:
      1. Number: Indication number (e.g. "12", "1-J").
      2. Target: Structure being modified (Article 1, Title 2).
      3. Content: The specific text approved.
      4. Authors: List of specific names mentioned (e.g. "De los convencionales A, B y C").
    - Output JSON list.
    """,

    "TABULAR_VOTING": """
    You are an expert legal data extractor analyzing a VOTING TABLE.
    
    Structure:
    - Look for a column labeled "Resultado" or "Votación".
    - Process ONLY rows where the result is "Aprobada" or "Afirmativa".
    - Column "Indicación" or "Texto": Contains the amendment.
    - Column "Autores": Contains the sponsors.
    
    Task:
    - Extract approved indications.
    - Output JSON: [{"number": "...", "target_article": "...", "action": "ADD/MOD/DEL", "content": "...", "authors_raw": "..."}]
    """
}

# --- Commission Profiles ---

# Maps Commission Number (Int) -> Profile Dict
# Profiles define which Prompt Key to use for Genesis and Voting
COMMISSION_MAP = {
    1: {"genesis": "NARRATIVE_GENESIS", "voting": "NARRATIVE_VOTING"},
    2: {"genesis": "NARRATIVE_GENESIS", "voting": "CUSTOM_COMPLEX"}, # Com 2 is special
    3: {"genesis": "TABULAR_GENESIS",   "voting": "TABULAR_VOTING"},
    4: {"genesis": "CUSTOM_COMPLEX",  "voting": "NARRATIVE_VOTING"},
    5: {"genesis": "TABULAR_GENESIS",   "voting": "TABULAR_VOTING"},
    6: {"genesis": "NARRATIVE_GENESIS", "voting": "NARRATIVE_VOTING"},
    7: {"genesis": "NARRATIVE_GENESIS", "voting": "NARRATIVE_VOTING"}
}
