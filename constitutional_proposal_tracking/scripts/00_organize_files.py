
import os
import re

# Dictionary of keywords mapping to standardized prefixes
TYPE_MAP = {
    "sistematizado": "GENESIS",
    "indicaciones": "VOTACION",
    "comparado": "COMPLEX",
    "reemplazo": "COMPLEX",
    "votacion-general": "COMPLEX"
}

# Regex to ignore generic reports "informe-DD-MM-AA.pdf" or "informe-N-DD-MM.pdf"
# We want to SKIP files that are purely dates or just number-date without "indicaciones/sistematizado"
# Pattern: informe- followed by digits and dashes, ending in .pdf
IGNORE_PATTERN = re.compile(r"^informe-[\d\-r]+(_\d+)?\.pdf$")

# Specific filename overrides: { "filename": "PREFIX" or None to skip }
SPECIFIC_OVERRIDES = {
    "texto-sistematizado-04-01.pdf": "COMPLEX",
    "texto-sistematizado-02-16.pdf": "GENESIS",
    "informe-1-03-07-texto-sistematizado.pdf": "COMPLEX",
    "votacion-general-informe-1-02-08.pdf": None
}

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def organize_files(dry_run=True):
    print(f"--- Starting Cleanup (DRY RUN: {dry_run}) ---")
    
    # Iterate Comisiones 1 to 7
    for i in range(1, 8):
        com_dir = os.path.join(BASE_DIR, f"comision-{i}", "PDFs")
        if not os.path.exists(com_dir):
            continue
            
        print(f"\nChecking Commission {i} ({com_dir})...")
        files = os.listdir(com_dir)
        
        for fname in files:
            if not fname.endswith(".pdf"):
                continue
                
            # 1. Check Specific Overrides first
            prefix = "UNKNOWN"
            if fname in SPECIFIC_OVERRIDES:
                prefix = SPECIFIC_OVERRIDES[fname]
                if prefix is None:
                    # print(f"  [SKIP] Overridden: {fname}")
                    continue
            
            # 2. If not overridden, apply standard classification
            if prefix == "UNKNOWN":
                # Skip if already standardized (starts with correct prefix)
                if fname.startswith("GENESIS_") or fname.startswith("VOTACION_") or fname.startswith("COMPLEX_"):
                    continue

                # Check ignore pattern (Basic Reports)
                if IGNORE_PATTERN.match(fname):
                    # Double check it doesn't contain our keywords
                    if not any(keyword in fname for keyword in TYPE_MAP.keys()):
                        continue

                # Determine Type based on keywords
                for keyword, p in TYPE_MAP.items():
                    if keyword in fname:
                        prefix = p
                        break
            
            if prefix == "UNKNOWN":
                print(f"  [SKIP] Could not classify: {fname}")
                continue
                
            # Construct new name
            # Format: C{Num}_{PREFIX}_{Original}
            new_name = f"C{i}_{prefix}_{fname}"
            
            old_path = os.path.join(com_dir, fname)
            new_path = os.path.join(com_dir, new_name)
            
            print(f"  RENAME: {fname} -> {new_name}")
            
            if not dry_run:
                try:
                    os.rename(old_path, new_path)
                except Exception as e:
                    print(f"    ERROR: {e}")

if __name__ == "__main__":
    # Default is SAFETY FIRST
    organize_files(dry_run=False)
    print("\nTo execute changes, set dry_run=False in the script execution.")
