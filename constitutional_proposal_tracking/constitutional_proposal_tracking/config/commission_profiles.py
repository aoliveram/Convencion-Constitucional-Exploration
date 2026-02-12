
# --- Prompt Templates ---

PROMPTS = {
    "NARRATIVE_GENESIS": """
    ACT AS: Expert Legal Data Extractor.
    INPUT: PDF Document "Texto Sistematizado" (Constitutional Draft Genesis).
    
    VISUAL STRUCTURE:
    - The document is a continuous text. 
    - Articles start with keywords like "Artículo 1", "Artículo 12", "Artículo 15 bis".
    - The **Source Initiative ID** (Boletín) is usually found **at the end of the article text**, enclosed in parentheses.
      - Example Com 7: "...descentralizada. (ICC N° 24-7 Artículo XX1...)" -> Source: "24-7"
      - Example Com 2: "...y pro persona. (7-2)" -> Source: "7-2"
    
    TASK:
    1. Extract every Article found in the text.
    2. For each article, extract:
       - "article": The numbering title (e.g., "Artículo 1").
       - "text": The full content of the article.
       - "sources": A list of Initiative IDs found in the parentheses associated with that article.
         - CLEANING: Remove "ICC", "N°", "Boletín", or "Iniciativa" from the ID. Just keep the numbers (e.g., "24-7").
    
    OUTPUT: A valid JSON List of objects.
    """,

    "TABULAR_GENESIS": """
    ACT AS: Expert Legal Data Extractor.
    INPUT: PDF Document "Texto Sistematizado" (Constitutional Draft Genesis).
    
    VISUAL STRUCTURE:
    - The document is organized in a **TWO-COLUMN TABLE** layout.
    - **Left Column**: Contains the Source Initiative IDs (e.g., "41-6", "ICC 951-5").
    - **Right Column**: Contains the Article Text (e.g., "Artículo 1.- La función jurisdiccional...").
    
    TASK:
    1. Analyze the document row by row, associating the Left Column (Source) with the Right Column (Text).
    2. For each row:
       - "article": Extract the Article Numbering from the start of the text (e.g., "Artículo 1").
       - "text": The full content of the article.
       - "sources": Extract the ID from the Left Column.
         - CLEANING: Remove "ICC", "N°", "Boletín". Keep the format "NUMBER-NUMBER" (e.g., "41-6", "951-5").
         - If multiple IDs appear (e.g. "41-6 / 97-6"), split them into a list.
    
    OUTPUT: A valid JSON List of objects.
    """,

    "NARRATIVE_VOTING": """
    ACT AS: Expert Constitutional Legislative Secretary.

    GOAL: Extract data from the attached "Voting Report" (Informe de Votación) to reconstruct the exact final text of the Constitutional Draft.

    INPUT: A PDF containing "Indicaciones" (Amendments) and voting results.
    REFERENCE: List of Convention Members: {members_str}

    INSTRUCTIONS:
    1. Scan the document for "Indicaciones" (Amendments).
    2. FILTER: Process ONLY indications that are explicitely marked as **"APROBADA"** (Approved) in the voting text (e.g., "resultando aprobada", "se aprueba", "aprobada por unanimidad"). 
      - IGNORE rejected, withdrawn (retirada), or incompatible indications.

    3. FOR EACH APPROVED INDICATION, EXTRACT A JSON OBJECT WITH THESE FIELDS:

      - "number": The integer number of the indication (e.g., "3", "15"). Remove any "N°" prefix.
      
      - "authors_matched": Identify the authors from the text and match them against the Reference List. Output a list of standardized names.
      
      - "target_article": The integer number of the main article being affected (e.g., "4" if the text says "Artículo 4").
      
      - "target_scope": The specific part of the article being affected. 
        * Values: "TOTAL" (Whole article), "INCISO" (Specific paragraph), "WORDING" (Specific words/phrases).
        * Rule: If text says "para sustituir el artículo X", scope is "TOTAL". If it says "en el inciso segundo", scope is "INCISO 2".

      - "action": The operation to perform. Map Spanish verbs to these keywords:
        * "SUBSTITUTE": (reemplazar, sustituir). Replaces the whole target with new text.
        * "DELETE": (suprimir, eliminar). Removes the target completely.
        * "ADD": (agregar, incorporar, añadir). Inserts new text.
        * "MODIFY_PHRASE": (sustituir la frase, reemplazar la expresión). Swaps specific words within a paragraph.

      - "content": The EXACT text content involved.
        * If action is DELETE: Leave empty string "".
        * If action is SUBSTITUTE: The full new text of the article/paragraph.
        * If action is ADD: The text being added.
        * If action is MODIFY_PHRASE: The *NEW* text that replaces the old phrase.

      - "content_to_remove": (Only for MODIFY_PHRASE)
        * The specific phrase being replaced or deleted. (e.g. if replacing "hombres" with "personas", this field is "hombres").

      - "placement_instructions": (Mandatory for ADD)
        * Extract the exact Spanish instruction of where to place the text.
        * Example: "luego del punto seguido", "como inciso final", "entre la palabra X y la palabra Y".

    EXAMPLES OF LOGIC:

    Case A: "Indicación N° 3 para reemplazar el artículo 4 por el siguiente: 'Texto completo...'"
    -> action: "SUBSTITUTE", target_scope: "TOTAL", content: "Texto completo..."

    Case B: "Indicación N° 8 para agregar el siguiente nuevo inciso: 'Texto del inciso...'"
    -> action: "ADD", target_scope: "INCISO", content: "Texto del inciso...", placement_instructions: "nuevo inciso"

    Case C: "Indicación N° 1 para suprimir el artículo 1"
    -> action: "DELETE", target_scope: "TOTAL", content: ""

    Case D: "Indicación N° 20 para sustituir la frase 'de los hombres' por 'de las personas'"
    -> action: "MODIFY_PHRASE", target_scope: "WORDING", content: "de las personas", content_to_remove: "de los hombres"

    OUTPUT FORMAT:
    Return a valid JSON List. No markdown, no explanations outside the JSON.
    """,

    "TABULAR_VOTING": """
    ACT AS: Expert Legal Data Extractor for the Chilean Constitutional Convention.
    
    VISUAL STRUCTURE ANALYSIS:
    You are processing documents (Commission 3 style) where the layout is:
    1. **The Indication Text**: A paragraph starting with "- IND [Number] ([Authors]) ...".
    2. **The Voting Table**: Immediately below the text, a grid with columns "A favor", "En contra", "Abstención", "No vota", "Total", "Resultado".
    
    TASK:
    1. Scan the document for **Voting Tables**.
    2. Check the "Resultado" column. Process ONLY blocks where the result is **"APROBADA"**.
    3. If a table indicates "APROBADA", look at the **text block immediately preceding that table** to extract the data.
    
    EXTRACTION FIELDS (JSON):
    
   - "number": The integer number of the indication (e.g., "401", "30"). Remove "IND" prefix.
   
   - "authors_matched": Identify authors from the text block above the table (usually inside parentheses like "(07 Mella, Y. Gómez...)"). Match against: {members_str}
   
   - "target_article": The integer number of the article being modified (e.g. "49"). Look for context lines like "En votación: Artículo 49".
   
   - "action": Map the Spanish verbs in the text block to:
     * "SUBSTITUTE": (reemplazar, sustituir).
     * "DELETE": (suprimir, eliminar).
     * "ADD": (agregar, incorporar, añadir).
     * "MODIFY_PHRASE": (sustituir la frase, eliminar la expresión).

   - "target_scope": 
     * "TOTAL": (Whole article).
     * "INCISO": (Specific paragraph/number).
     * "WORDING": (Specific words).

   - "content": The text content involved.
     * If DELETE: "".
     * If SUBSTITUTE/ADD: The text usually inside quotation marks or following the colon.
     * If MODIFY_PHRASE: The new text.

   - "content_to_remove": (For MODIFY_PHRASE) The text being replaced.

   - "placement_instructions": (For ADD) Exact Spanish instruction (e.g., "luego del inciso 1").

    RETURN: A JSON List. No markdown, no explanations outside the JSON.
    """
}

# --- Commission Profiles ---

# Maps Commission Number (Int) -> Profile Dict
# Profiles define which Prompt Key to use for Genesis and Voting
COMMISSION_MAP = {
    1: {"genesis": "NARRATIVE_GENESIS", "voting": "NARRATIVE_VOTING"},
    2: {"genesis": "NARRATIVE_GENESIS", "voting": "CUSTOM_COMPLEX"}, # Com 2 voting is special
    3: {"genesis": "TABULAR_GENESIS",   "voting": "TABULAR_VOTING"},
    4: {"genesis": "CUSTOM_COMPLEX",  "voting": "NARRATIVE_VOTING"}, # Com 4 genesis is special
    5: {"genesis": "TABULAR_GENESIS",   "voting": "NARRATIVE_VOTING"},
    6: {"genesis": "TABULAR_GENESIS", "voting": "NARRATIVE_VOTING"},
    7: {"genesis": "NARRATIVE_GENESIS", "voting": "NARRATIVE_VOTING"}
}
