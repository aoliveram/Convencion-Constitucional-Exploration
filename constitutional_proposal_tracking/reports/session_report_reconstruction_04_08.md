# Progress Report: Commission 2 Reconstruction (Draft 04-08)
**Date:** February 6, 2026

## 1. Executive Summary
The objective of this session was to reconstruct the legislative history of Commission 2 (Principios Constitucionales) as of April 8th, 2022 (Draft 04-08). This draft serves as the bridge between the initial reports (Draft 03-02) and the Final Harmonized Proposal (05-14). 

**Status:** Ready for Manual Mapping.
**Key Artifact:** `C2_MAPPING_candidates_rankings.json` (Contains Top 5 matches for 41 Target Articles).

## 2. Key Challenges Encountered
1.  **Title Drift / Mismatches**:
    - The source PDF for 04-08 ("Borrador Constitucional") often used different numbering or titles than the previous draft (03-02).
    - As a result, automated fuzzy matching failed, marking most articles as "New" instead of linking them to their predecessors.

2.  **Extraction Hallucinations (Script 10)**:
    - Initially, the `gemini-3-pro-preview` model hallucinated article titles (e.g., mislabeling "Derecho al asilo" as "Derecho a migrar") or truncated text.
    - **Fix**: The script was updated to enforce **Verbatim Title Extraction**, ensuring the JSON strictly mirrors the PDF.

3.  **Ranking Script Performance (Script 11)**:
    - The initial ranking script was too slow (sequential processing).
    - **Fix**: Implemented **Batch Processing** (5 articles per request).
    - **Fix**: Switched from Title-based matching to **ID-based matching** to handle duplicate titles (e.g., multiple "Artículo 2" entries) correctly.

## 3. Work Completed
### A. Extraction (Script 10)
- **Input**: `C2_COMPLEX_BORRADOR-CONSTITUCIONAL-14-05-22.pdf`
- **Logic**: Extraction of all 41 articles belonging to Commission 2 across 4 thematic blocks.
- **Output**: `C2_DRAFT_texto-sistematizado-04-08.json` (Ground Truth).

### B. Similarity Ranking (Script 11)
- **Input**: Target Articles (04-08) vs Candidate Articles (03-02).
- **Model**: `gemini-3-pro-preview`.
- **Logic**: Generates a Top 5 list of similar articles based on semantic content overlap.
- **Output**: `C2_MAPPING_candidates_rankings.json` (Includes full text and reasoning).

### C. Manual Mapping Preparation
- Created **Manual Mapping Template** (`C2_MANUAL_MAPPING_template.json`).
- Verified that **Deletion Analysis** is feasible: Unmapped articles from 03-02 will be cross-referenced with "Suprimir" indications found in `C2_INDICATIONS_04_08_candidates.json`.

## 4. Next Steps
1.  **Manual Mapping**: User fills in `C2_MANUAL_MAPPING_template.json` to definitively link 04-08 articles to their 03-02 sources.
2.  **Deletion Analysis**: Identify 03-02 articles that were *not* mapped. Search for their "Suprimir" indications to finalize the history tracking.
3.  **Final Reconstruction**: Generate the final Linked JSON.
