# 06_oraciones_RCP_vs_Embedding_distr.py
# - Hist A: distribución de score_contenido (solo top-1 Embedding)
# - Hist B: distribución de score_contenido (top-1 + top-2 si delta Embedding está en el 25% más bajo)
# Mantiene mismo rango/bins para comparabilidad.

import pandas as pd
import numpy as np
import json
import glob
import re
import os
import unicodedata
import matplotlib.pyplot as plt
from typing import Dict, Set, List, Tuple

# -------------------------------
# RUTAS (ajústalas si difieren)
# -------------------------------
BORRADOR_JSON_PATH = "patrocinantes_identificacion/borrador_constitucional_estructurado.json"
COMPARACION_CSV_PATH = "ideological-scaling-files/analizar_procedencia_borrador/comparacion_tfidf_embeddings.csv"
INICIATIVAS_ORACIONES_CSV_PATH = "ideological-scaling-files/analizar_procedencia_borrador/oraciones_iniciativas_df.csv"
API_DIR_GLOB = "patrocinantes_identificacion/api_extracted_*_corrected_4.json"

PROF_XLSX_PATH = "rcp_convencion/autores_indicaciones_aprobadas.xlsx"
PROF_SHEET = "Hoja3"

# -------------------------------
# UTILIDADES
# -------------------------------
def strip_suffix(col: str) -> str:
    return re.sub(r"\.\d+$", "", str(col))

def norm_text(s: str) -> str:
    if not isinstance(s, str):
        return ""
    s = s.strip()
    s = unicodedata.normalize('NFD', s)
    s = ''.join(c for c in s if unicodedata.category(c) != 'Mn')  # quita acentos
    s = s.lower()
    s = re.sub(r"\s+", " ", s)
    return s

def surname_from_matched(fullname: str) -> str:
    # fullname tipo "Apellido, Nombre"
    if not isinstance(fullname, str) or "," not in fullname:
        return ""
    ap = fullname.split(",", 1)[0].strip()
    return norm_text(ap)

def texto_starts_with_art(texto: str) -> bool:
    if not isinstance(texto, str):
        return False
    t = texto.lstrip(" '\"")
    t_norm = norm_text(t)
    return t_norm.startswith("art")

# -------------------------------
# CARGA BÁSICA
# -------------------------------
# Borrador: tokenizar artículos en oraciones y heredar id_articulo_borrador
df_borrador = pd.read_json(BORRADOR_JSON_PATH)
df_borrador['texto_articulo_limpio'] = df_borrador['texto_articulo_limpio'].str.split(r'\.\s*')
df_orac = (
    df_borrador
    .explode('texto_articulo_limpio')
    .rename(columns={'texto_articulo_limpio': 'oracion'})
)
df_orac = df_orac[df_orac['oracion'].astype(str).str.strip().str.len() > 15].reset_index(drop=True)
# Asumimos que el CSV de comparaciones ya trae 'id_oracion_borrador'; lo respetamos:
df_orac['id_oracion_borrador_token'] = df_orac.index + 1  # por si se necesitara, pero NO se usa si ya viene en CSV

# Comparaciones: contiene múltiples filas por oración y por método con columna 'rank' y 'similitud_emb'
df_comp = pd.read_csv(COMPARACION_CSV_PATH)

# Oraciones de iniciativas: mapea id_oracion_iniciativa -> filename
df_inic = pd.read_csv(INICIATIVAS_ORACIONES_CSV_PATH)[['id_oracion_iniciativa','filename']]

# -------------------------------
# TOP1 / TOP2 POR EMBEDDING + DELTA
# -------------------------------
# Nos quedamos solo con columnas necesarias
need_cols = [
    'id_oracion_borrador',
    'rank',
    'id_oracion_iniciativa_match_emb',
    'similitud_emb'
]
missing = [c for c in need_cols if c not in df_comp.columns]
if missing:
    raise RuntimeError(f"Faltan columnas en comparacion_tfidf_embeddings.csv: {missing}")

df_e = df_comp[need_cols].dropna(subset=['id_oracion_borrador','rank','id_oracion_iniciativa_match_emb','similitud_emb']).copy()

# Top-1 y Top-2 por Embedding (menor rank = mejor)
top1 = df_e.sort_values(['id_oracion_borrador','rank']).groupby('id_oracion_borrador').nth(0).reset_index()
top2 = df_e.sort_values(['id_oracion_borrador','rank']).groupby('id_oracion_borrador').nth(1).reset_index()

top1 = top1.rename(columns={
    'id_oracion_iniciativa_match_emb': 'id_inic_top1',
    'similitud_emb': 'sim_top1'
})[['id_oracion_borrador','id_inic_top1','sim_top1']]

top2 = top2.rename(columns={
    'id_oracion_iniciativa_match_emb': 'id_inic_top2',
    'similitud_emb': 'sim_top2'
})[['id_oracion_borrador','id_inic_top2','sim_top2']]

df_rank = pd.merge(top1, top2, on='id_oracion_borrador', how='left')
df_rank['delta_emb'] = df_rank['sim_top1'] - df_rank['sim_top2']

# Percentil 25 de delta (cuanto más bajo, más “empate”)
q75 = df_rank['delta_emb'].quantile(0.75)
df_rank['usar_top2'] = df_rank['delta_emb'] <= q75

# Mapear a filenames
df_rank = df_rank.merge(df_inic, left_on='id_inic_top1', right_on='id_oracion_iniciativa', how='left').rename(columns={'filename':'file_top1'})
df_rank = df_rank.drop(columns=['id_oracion_iniciativa'])
df_rank = df_rank.merge(df_inic, left_on='id_inic_top2', right_on='id_oracion_iniciativa', how='left').rename(columns={'filename':'file_top2'})
df_rank = df_rank.drop(columns=['id_oracion_iniciativa'])

# Adjuntar id_articulo_borrador
df_rank = df_rank.merge(
    df_orac[['id_articulo_borrador']].assign(id_oracion_borrador=lambda d: d.index+1),
    on='id_oracion_borrador',
    how='left'
)

# -------------------------------
# METADATOS API_Extracted
# -------------------------------
meta_por_filename: Dict[str, dict] = {}
for path in glob.glob(API_DIR_GLOB):
    try:
        with open(path, 'r', encoding='utf-8') as fh:
            data = json.load(fh)
            meta_por_filename.update(data)
    except Exception as e:
        print(f"[WARN] No pude leer {path}: {e}")

def apellidos_firmantes(filename: str) -> Set[str]:
    if not isinstance(filename, str) or not filename:
        return set()
    meta = meta_por_filename.get(filename)
    if not meta:
        return set()
    firmantes = meta.get("firmantes_matched", []) or []
    return {surname_from_matched(x) for x in firmantes if surname_from_matched(x)}

# -------------------------------
# PIPELINES DE FIRMANTES POR ARTÍCULO
# -------------------------------
def apellidos_pipeline_por_articulo(df_sel: pd.DataFrame, relajado: bool) -> Dict[int, Set[str]]:
    """
    df_sel: dataframe con columnas:
      - id_articulo_borrador
      - file_top1
      - file_top2
      - usar_top2 (bool)
    relajado=False -> usa solo file_top1
    relajado=True  -> usa file_top1 (+ file_top2 si usar_top2==True)
    """
    out: Dict[int, Set[str]] = {}
    for art_id, g in df_sel.groupby('id_articulo_borrador'):
        acc: Set[str] = set()
        for _, row in g.iterrows():
            acc |= apellidos_firmantes(row.get('file_top1'))
            if relajado and bool(row.get('usar_top2')):
                acc |= apellidos_firmantes(row.get('file_top2'))
        out[int(art_id)] = acc
    return out

# -------------------------------
# XLSX DEL PROFESOR -> APELLIDOS POR ARTÍCULO
# -------------------------------
df_prof = pd.read_excel(PROF_XLSX_PATH, sheet_name=PROF_SHEET)
df_prof_art = df_prof[df_prof['Texto'].apply(texto_starts_with_art)].copy()

# Detectar columnas Arauna..Videla (incluyendo posibles duplicados con sufijo .1, .2)
cols = df_prof_art.columns.tolist()
try:
    i_start = cols.index("Arauna")
    i_end = cols.index("Videla") + 1
except ValueError:
    raise RuntimeError("No se encontraron columnas 'Arauna' y/o 'Videla' en el XLSX del profesor.")

apellido_cols = cols[i_start:i_end]
col2key = {c: norm_text(strip_suffix(c)) for c in apellido_cols}

def row_apellidos_prof(row) -> Set[str]:
    s: Set[str] = set()
    for c in apellido_cols:
        val = row.get(c)
        if pd.notna(val) and str(val).strip() not in ("", "0", "0.0"):
            s.add(col2key[c])
    return s

if 'id_borrador' not in df_prof_art.columns:
    raise RuntimeError("No se encontró columna 'id_borrador' en el XLSX del profesor.")

apellidos_prof_por_art: Dict[int, Set[str]] = {}
for _, r in df_prof_art.iterrows():
    art_id = r['id_borrador']
    if pd.isna(art_id):
        continue
    apellidos_prof_por_art[int(art_id)] = row_apellidos_prof(r)

# -------------------------------
# SCORES (BASE vs RELAJADO)
# -------------------------------
def scores_por_articulo(apell_prof: Dict[int, Set[str]], apell_pipe: Dict[int, Set[str]]) -> pd.DataFrame:
    arts = sorted(set(apell_prof.keys()) | set(apell_pipe.keys()))
    recs = []
    for art in arts:
        A = apell_prof.get(art, set())
        B = apell_pipe.get(art, set())
        if len(A) == 0:
            score = np.nan
        else:
            score = len(A & B) / len(A)
        recs.append({
            "id_articulo_borrador": art,
            "n_prof": len(A),
            "n_pipeline": len(B),
            "n_interseccion": len(A & B),
            "score_contenido": score
        })
    return pd.DataFrame(recs)

# Construir ambos pipelines
pipe_base = apellidos_pipeline_por_articulo(df_rank, relajado=False)
pipe_relax = apellidos_pipeline_por_articulo(df_rank, relajado=True)

df_score_base = scores_por_articulo(apellidos_prof_por_art, pipe_base)
df_score_relax = scores_por_articulo(apellidos_prof_por_art, pipe_relax)

# Filtrar solo válidos (como en el gráfico original)
valid_base = df_score_base.dropna(subset=['score_contenido']).copy()
valid_relax = df_score_relax.dropna(subset=['score_contenido']).copy()

# -------------------------------
# GRÁFICOS DE DIFERENCIAS ENTRE EMBEDDING Y TF-IDF
# -------------------------------
# Calcula diferencias entre rank1 y ranks 2–5 y grafica en una grilla 2x4
# Fila 1: Embedding, Fila 2: TF-IDF

ranks_to_compare = [2, 3, 4, 5]

# Prepara listas de diferencias
diffs_emb = []
diffs_tfidf = []
for r in ranks_to_compare:
    d_emb = (pivot_emb[1] - pivot_emb[r]).dropna()
    d_tfidf = (pivot_tfidf[1] - pivot_tfidf[r]).dropna()
    diffs_emb.append(d_emb)
    diffs_tfidf.append(d_tfidf)

# Calcular límites comunes por fila
# Embedding
max_freq_emb = 0
max_val_emb = 0
for s in diffs_emb:
    counts, bins = np.histogram(s, bins=50)
    if counts.max() > max_freq_emb:
        max_freq_emb = counts.max()
    if s.max() > max_val_emb:
        max_val_emb = s.max()

# TF-IDF
max_freq_tfidf = 0
max_val_tfidf = 0
for s in diffs_tfidf:
    counts, bins = np.histogram(s, bins=50)
    if counts.max() > max_freq_tfidf:
        max_freq_tfidf = counts.max()
    if s.max() > max_val_tfidf:
        max_val_tfidf = s.max()

# Crear figura 2x4
fig, axes = plt.subplots(2, 4, figsize=(20, 8), sharey=False)

# Fila 1: Embedding
for i, r in enumerate(ranks_to_compare):
    ax = axes[0, i]
    ax.hist(diffs_emb[i], bins=50, color='tab:blue')
    ax.set_ylim(0, max_freq_emb * 1.05)
    ax.set_xlim(0, max_val_emb * 1.05)
    ax.set_title(f"Emb. Rank1 - Rank{r}")
    if i == 0:
        ax.set_ylabel("Frecuencia")
    if i == 0 or i == len(ranks_to_compare) - 1:
        ax.set_xlabel("Diferencia similitud")

# Fila 2: TF-IDF
for i, r in enumerate(ranks_to_compare):
    ax = axes[1, i]
    ax.hist(diffs_tfidf[i], bins=50, color='tab:orange')
    ax.set_ylim(0, max_freq_tfidf * 1.05)
    ax.set_xlim(0, max_val_tfidf * 1.05)
    ax.set_title(f"TF-IDF Rank1 - Rank{r}")
    if i == 0:
        ax.set_ylabel("Frecuencia")
    if i == 0 or i == len(ranks_to_compare) - 1:
        ax.set_xlabel("Diferencia similitud")

plt.tight_layout()
plt.savefig("ideological-scaling-plots/diferencias_embedding_tfidf.pdf")
plt.show()

# -------------------------------
# HISTOGRAMAS COMPARABLES
# -------------------------------

# --- Stats para subtítulos ---
mean_base = valid_base['score_contenido'].mean()
sd_base   = valid_base['score_contenido'].std()
n_base    = len(valid_base)
subtitle_base = f"Mean={mean_base:.3f} · SD={sd_base:.3f} · N válidos={n_base}"

mean_relax = valid_relax['score_contenido'].mean()
sd_relax   = valid_relax['score_contenido'].std()
n_relax    = len(valid_relax)
subtitle_relax = f"Mean={mean_relax:.3f} · SD={sd_relax:.3f} · N válidos={n_relax}"

# Bins y rango idénticos: tomamos min/max de la serie base
xmin = min(valid_base['score_contenido'].min(), 0.0)
xmax = max(valid_base['score_contenido'].max(), 1.0)
bins = np.linspace(xmin, xmax, 21)  # 20 bins como referencia

plt.figure()
plt.hist(valid_base['score_contenido'].values, bins=bins)
plt.title("Distribución de score_contenido (BASE: solo top-1 Embedding)\n" + subtitle_base)
plt.xlabel("score_contenido")
plt.ylabel("frecuencia")
plt.ylim(0, 15)
plt.tight_layout()
plt.savefig("ideological-scaling-plots/score_contenido_base.pdf", bbox_inches="tight")
plt.show()

plt.figure()
plt.hist(valid_relax['score_contenido'].values, bins=bins)
plt.title("Distribución de score_contenido (RELAJADO: top-1 + top-2 si Δ Embedding ≤ P75)\n" + subtitle_relax)
plt.xlabel("score_contenido")
plt.ylabel("frecuencia")
plt.ylim(0, 15)
plt.tight_layout()
plt.savefig("ideological-scaling-plots/score_contenido_p75.pdf", bbox_inches="tight")
plt.show()

# -------------------------------
# RESUMEN COMPARATIVO
# -------------------------------
def resumen(dfv: pd.DataFrame, tag: str) -> dict:
    s = dfv['score_contenido']
    return {
        "config": tag,
        "n_valid": int(len(dfv)),
        "mean": float(s.mean()),
        "median": float(s.median()),
        "pct_ge_075": float((s >= 0.75).mean()*100),
        "pct_eq_1": float((s == 1.0).mean()*100),
        "pct_eq_0": float((s == 0.0).mean()*100)
    }

summary_base = resumen(valid_base, "BASE")
summary_relax = resumen(valid_relax, "RELAJADO")

print("[P25 delta Embedding]", round(q75, 6))
print("BASE:", summary_base)
print("RELAJADO:", summary_relax)
