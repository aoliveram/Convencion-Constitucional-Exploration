# Scalable Data Extraction Guide

## Phase 1: Hygiene (Renaming)
Run this script to standardize file names in all `comision-*/PDFs` folders.
It defaults to `dry_run=True` (safe mode).

```bash
# Preview changes
python3 constitutional_proposal_tracking/scripts/00_organize_files.py

# Execute changes (EDIT SCRIPT FIRST to set dry_run=False)
python3 constitutional_proposal_tracking/scripts/00_organize_files.py
```

## Phase 2 & 3: Universal Extraction
Ensure you have `GEMINI_API_KEY` set.

### Step A: Extract Genesis (Draft 0)
This will scan all commissions for files starting with `C{N}_GENESIS_` and extract the mappings.

```bash
python3 constitutional_proposal_tracking/scripts/02_extract_genesis_universal.py
```

Check output in: `constitutional_proposal_tracking/comision-*/genesis-extracted/`

### Step B: Extract Voting (Evolution)
This will scan all commissions for `C{N}_VOTACION_` files. It uses the strategy defined in `config/commission_profiles.py`.

```bash
python3 constitutional_proposal_tracking/scripts/04_extract_voting_universal.py
```

Check output in: `constitutional_proposal_tracking/comision-*/indicaciones-universal-extracted/`

## Configuration
To change prompt strategies or add new commission types, edit:
`constitutional_proposal_tracking/constitutional_proposal_tracking/config/commission_profiles.py`
