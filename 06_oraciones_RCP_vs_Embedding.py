# 06_oraciones_RCP_vs_Embedding.py
# Compara, por artículo, los apellidos del .xlsx del profesor vs.
# los apellidos provenientes de firmantes de iniciativas top-1 (Embedding)
# asociadas a las oraciones de cada artículo del borrador.

import pandas as pd
import json
import glob
import re
import os
import unicodedata
from typing import List, Set, Dict

# -------------------------------
# 0) RUTAS / ENTRADAS
# -------------------------------
BORRADOR_JSON_PATH = "patrocinantes_identificacion/borrador_constitucional_estructurado.json"
COMPARACION_CSV_PATH = "scripts - files/analizar_procedencia_borrador/comparacion_tfidf_embeddings.csv"
INICIATIVAS_ORACIONES_CSV_PATH = "scripts - files/analizar_procedencia_borrador/oraciones_iniciativas_df.csv"

API_DIR_GLOB = "patrocinantes_identificacion/api_extracted_*_corrected_4.json"

PROF_XLSX_PATH = "rcp_convencion/autores_indicaciones_aprobadas.xlsx"
PROF_SHEET = "Hoja3"   # ajusta si la hoja cambia

# Salidas
OUT_CSV = "scripts - files/analizar_procedencia_borrador/score_similitud_prof_vs_pipeline.csv"
OUT_JSON = "scripts - files/analizar_procedencia_borrador/score_similitud_prof_vs_pipeline.json"

# -------------------------------
# 1) UTILIDADES
# -------------------------------
def strip_suffix(col: str) -> str:
    """Elimina sufijos '.1', '.2', etc., típicos de Excel cuando hay columnas duplicadas."""
    return re.sub(r"\.\d+$", "", col)

def norm_text(s: str) -> str:
    """Normaliza: lowercase, sin acentos, colapsa espacios, recorta."""
    if not isinstance(s, str):
        return ""
    s = s.strip()
    s = unicodedata.normalize('NFD', s)
    s = ''.join(c for c in s if unicodedata.category(c) != 'Mn')  # quita acentos
    s = s.lower()
    s = re.sub(r"\s+", " ", s)
    return s

def surname_from_matched(fullname: str) -> str:
    """
    Extrae el apellido desde un string tipo 'Apellido, Nombre'.
    Mantiene apellidos compuestos (p.ej., 'de la maza').
    """
    if not isinstance(fullname, str) or "," not in fullname:
        return ""
    apellido = fullname.split(",", 1)[0].strip()
    return norm_text(apellido)

def texto_starts_with_art(texto: str) -> bool:
    """
    Determina si 'Texto' comienza con 'Art'/'Artículo' ignorando comillas y espacios iniciales,
    y siendo insensible a acentos/mayúsculas.
    """
    if not isinstance(texto, str):
        return False
    t = texto.lstrip(" '\"")  # quita espacios y comillas iniciales
    t_norm = norm_text(t)
    # Acepta 'art', 'articulo', 'artículo'
    return t_norm.startswith("art")  # lo suficientemente amplio para 'articulo', 'artículo'

# -------------------------------
# 2) CARGA DE DATOS BASE
# -------------------------------
# 2.1 Borrador por artículo y tokenización en oraciones
df_borrador = pd.read_json(BORRADOR_JSON_PATH)

# Tokeniza por punto + espacios; propaga id_articulo_borrador a cada oración
df_borrador['texto_articulo_limpio'] = df_borrador['texto_articulo_limpio'].str.split(r'\.\s*')
df_oraciones_borrador = (
    df_borrador
    .explode('texto_articulo_limpio')
    .rename(columns={'texto_articulo_limpio': 'oracion'})
)
# Filtra fragmentos muy cortos
df_oraciones_borrador = df_oraciones_borrador[
    df_oraciones_borrador['oracion'].astype(str).str.strip().str.len() > 15
].reset_index(drop=True)
# Asigna ID secuencial de oración
df_oraciones_borrador['id_oracion_borrador'] = df_oraciones_borrador.index + 1

# 2.2 Comparaciones (TF-IDF + Embeddings) y oraciones de iniciativas (para filename)
df_comp = pd.read_csv(COMPARACION_CSV_PATH)
df_inic = pd.read_csv(INICIATIVAS_ORACIONES_CSV_PATH)

# -------------------------------
# 3) TOP-1 POR EMBEDDING Y MAPEOS
# -------------------------------
# Asumimos que 'rank == 1' es la mejor coincidencia por Embedding
df_top1 = df_comp[df_comp['rank'] == 1].copy()

# Adjunta texto y artículo de esa oración del borrador
df_top1 = df_top1.merge(
    df_oraciones_borrador[['id_oracion_borrador', 'id_articulo_borrador', 'oracion']],
    on='id_oracion_borrador',
    how='left'
)

# Adjunta filename de la iniciativa del match embedding (via id_oracion_iniciativa_match_emb)
df_top1 = df_top1.merge(
    df_inic[['id_oracion_iniciativa', 'filename']],
    left_on='id_oracion_iniciativa_match_emb',
    right_on='id_oracion_iniciativa',
    how='left'
).rename(columns={'filename': 'filename_top_emb'})

# -------------------------------
# 4) CARGA METADATOS API (FIRMANTES POR INICIATIVA)
# -------------------------------
# meta_por_filename: dict[filename_pdf] -> dict(con 'firmantes_matched', etc.)
meta_por_filename: Dict[str, dict] = {}
for path in glob.glob(API_DIR_GLOB):
    try:
        with open(path, 'r', encoding='utf-8') as fh:
            data = json.load(fh)
            meta_por_filename.update(data)
    except Exception as e:
        print(f"[ADVERTENCIA] No pude leer {path}: {e}")

def firmantes_apellidos_from_filename(filename: str) -> Set[str]:
    """
    Dado el nombre de archivo de iniciativa, retorna el set de APELLIDOS normalizados
    de 'firmantes_matched'.
    """
    if not isinstance(filename, str):
        return set()
    meta = meta_por_filename.get(filename)
    if not meta:
        return set()
    firmantes = meta.get("firmantes_matched", []) or []
    apellidos = {surname_from_matched(f) for f in firmantes if surname_from_matched(f)}
    return apellidos

# -------------------------------
# 5) AGRUPAR POR ARTÍCULO (TU PIPELINE)
# -------------------------------
# Para cada artículo: unir TODOS los apellidos de firmantes de las iniciativas top-1
# de sus oraciones.
apellidos_por_articulo_pipeline: Dict[int, Set[str]] = {}

for art_id, grupo in df_top1.groupby('id_articulo_borrador'):
    files = grupo['filename_top_emb'].dropna().unique().tolist()
    acc: Set[str] = set()
    for fn in files:
        acc |= firmantes_apellidos_from_filename(fn)
    apellidos_por_articulo_pipeline[art_id] = acc

# -------------------------------
# 6) LECTURA XLSX DEL PROFESOR Y EXTRACCIÓN APELLIDOS
# -------------------------------
# Estructura de la hoja (ejemplo observado): columnas 'id', 'id_borrador', 'Texto', ...,
# y columnas de apellidos desde 'Arauna' hasta 'Videla' (con posibles duplicados .1)
df_prof = pd.read_excel(PROF_XLSX_PATH, sheet_name=PROF_SHEET)

# Filtrar SOLO filas cuyo 'Texto' comience con 'Art'
mask_art = df_prof['Texto'].apply(texto_starts_with_art)
df_prof_art = df_prof[mask_art].copy()

# Detectar el rango de columnas de apellidos (Arauna -> Videla)
cols = df_prof_art.columns.tolist()
try:
    i_start = cols.index("Arauna")
    i_end = cols.index("Videla") + 1
except ValueError:
    raise RuntimeError("No se encontraron columnas 'Arauna' y/o 'Videla' en el XLSX del profesor.")

apellido_cols = cols[i_start:i_end]

# Mapeo normalizado de nombre de columna -> apellido normalizado (sin sufijos)
apellido_col_to_key = {c: norm_text(strip_suffix(c)) for c in apellido_cols}

# Para cada fila-Artículo del profesor, recoger apellidos con valor 'presente'
# Asumimos 'presente' si la celda es no nula y no es cero.
def row_apellidos_prof(row) -> Set[str]:
    s: Set[str] = set()
    for c in apellido_cols:
        val = row.get(c)
        if pd.notna(val) and str(val).strip() not in ("", "0", "0.0"):
            s.add(apellido_col_to_key[c])  # usar apellido normalizado de la col
    return s

# id_borrador del profesor debe mapear contra id_articulo_borrador del pipeline
if 'id_borrador' not in df_prof_art.columns:
    raise RuntimeError("No se encontró columna 'id_borrador' en el XLSX del profesor.")

apellidos_por_articulo_prof: Dict[int, Set[str]] = {}
for _, r in df_prof_art.iterrows():
    art_id = r['id_borrador']
    if pd.isna(art_id):
        continue
    art_id = int(art_id)
    apellidos_por_articulo_prof[art_id] = row_apellidos_prof(r)

# -------------------------------
# 7) COMPARACIÓN Y SCORE
# -------------------------------
records = []
all_art_ids = sorted(set(apellidos_por_articulo_prof.keys()) | set(apellidos_por_articulo_pipeline.keys()))

for art_id in all_art_ids:
    set_prof = apellidos_por_articulo_prof.get(art_id, set())
    set_pipe = apellidos_por_articulo_pipeline.get(art_id, set())

    inter = set_prof & set_pipe
    missing = set_prof - set_pipe    # en el profesor, pero no en mi pipeline
    extra = set_pipe - set_prof      # en mi pipeline, pero no en el profesor

    n_prof = len(set_prof)
    n_pipe = len(set_pipe)
    n_inter = len(inter)
    score = (n_inter / n_prof) if n_prof > 0 else None  # None si el profe no listó apellidos

    records.append({
        "id_articulo_borrador": art_id,
        "n_prof": n_prof,
        "n_pipeline": n_pipe,
        "n_interseccion": n_inter,
        "score_contenido": score,  # |A∩B| / |A|  (A = profesor)
        "apellidos_profesor": sorted(set_prof),
        "apellidos_pipeline": sorted(set_pipe),
        "interseccion": sorted(inter),
        "faltantes_en_pipeline": sorted(missing),
        "solo_en_pipeline": sorted(extra),
    })

df_out = pd.DataFrame(records).sort_values("id_articulo_borrador")

# -------------------------------
# 8) GUARDAR RESULTADOS
# -------------------------------
os.makedirs(os.path.dirname(OUT_CSV), exist_ok=True)
df_out.to_csv(OUT_CSV, index=False, encoding="utf-8")

with open(OUT_JSON, "w", encoding="utf-8") as f:
    json.dump(records, f, ensure_ascii=False, indent=2)

print(f"[OK] Guardado CSV:  {OUT_CSV}")
print(f"[OK] Guardado JSON: {OUT_JSON}")