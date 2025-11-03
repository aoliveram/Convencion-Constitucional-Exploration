# Creates 
# borrador_anotado.tex 
# From 
# borrador_constitucional_estructurado.json, 11-sentences_iniciativas.csv, 11-TFIDF-vs-Emb.csv
# It doesn't use 11-analisis_procedencia_oracion-patrocinante.json

import pandas as pd
import json
import re

# --- 1. Rutas a datos estructurados y comparaciones ---
BORRADOR_JSON_PATH = "co-sponsorship-analysis/borrador_constitucional_estructurado.json"
INICIATIVAS_ORACIONES_CSV_PATH = "co-sponsorship-analysis/analizar_procedencia_borrador/11-sentences_iniciativas.csv"
COMPARACION_CSV_PATH = "co-sponsorship-analysis/analizar_procedencia_borrador/11-TFIDF-vs-Emb.csv"

OUTPUT_LATEX_PATH = "co-sponsorship-analysis/analizar_procedencia_borrador/11-borrador_anotado.tex"


# --- 2. Función para Escapar Caracteres de LaTeX ---
def escape_latex(text):
    """Escapa caracteres especiales de LaTeX en una cadena de texto."""
    if not isinstance(text, str):
        return text
    replacements = {
        '&': r'\&', '%': r'\%', '$': r'\$', '#': r'\#', '_': r'\_',
        '{': r'\{', '}': r'\}', '~': r'\textasciitilde{}', '^': r'\textasciicircum{}',
        '\\': r'\textbackslash{}',
    }
    for char, escaped_char in replacements.items():
        text = text.replace(char, escaped_char)
    return text

# --- 3. Carga y Preparación de Datos ---

try:
    # Cargar el borrador estructurado
    df_borrador = pd.read_json(BORRADOR_JSON_PATH)

    # Cargar los datos de comparación
    df_comparacion = pd.read_csv(COMPARACION_CSV_PATH)
    
    # Cargar el CSV de oraciones de iniciativas
    df_oraciones_iniciativas = pd.read_csv(INICIATIVAS_ORACIONES_CSV_PATH)

except FileNotFoundError as e:
    print(f"\n[ERROR] Archivo no encontrado: {e.filename}")
    print("Por favor, verifica las rutas en la sección de configuración")
    exit()

# Tokenizar el Borrador en oraciones para tener un ID consistente
df_borrador['texto_articulo_limpio'] = df_borrador['texto_articulo_limpio'].str.split(r'\.\s*')
df_oraciones_borrador = df_borrador.explode('texto_articulo_limpio').rename(columns={'texto_articulo_limpio': 'oracion'})
df_oraciones_borrador = df_oraciones_borrador[df_oraciones_borrador['oracion'].str.strip().str.len() > 15].reset_index(drop=True)
df_oraciones_borrador['id_oracion_borrador'] = df_oraciones_borrador.index + 1

# --- 4. Unir los DataFrames para obtener la información completa ---

# Filtrar solo las 2 mejores coincidencias de embeddings
df_top_emb = df_comparacion[df_comparacion['rank'] <= 3].copy()

# Uniendo con el texto de las oraciones del borrador (Los IDs deben coincidir)
merged_df = pd.merge(
    df_top_emb,
    df_oraciones_borrador[['id_oracion_borrador', 'id_articulo_borrador', 'oracion']],
    on='id_oracion_borrador',
    how='left'
)

# Uniendo con los nombres de archivo de las iniciativas USANDO LOS IDs CORRECTOS
merged_df = pd.merge(
    merged_df,
    df_oraciones_iniciativas[['id_oracion_iniciativa', 'filename']],
    left_on='id_oracion_iniciativa_match_emb',
    right_on='id_oracion_iniciativa',
    how='left'
).rename(columns={'filename': 'origen_emb'})

# Join para los matches de tfidf
merged_df = pd.merge(
    merged_df,
    df_oraciones_iniciativas[['id_oracion_iniciativa', 'filename']],
    left_on='id_oracion_iniciativa_match_tfidf',
    right_on='id_oracion_iniciativa',
    how='left',
    suffixes=('', '_drop') # Evitar columnas duplicadas de id_oracion_iniciativa
).drop(columns=[col for col in merged_df.columns if '_drop' in col])


# Agrupar por oración del borrador para tener las 2 mejores coincidencias en una lista
results_grouped = merged_df.groupby(['id_articulo_borrador', 'id_oracion_borrador', 'oracion']).apply(
    lambda x: x.sort_values('rank').to_dict('records')
).reset_index(name='matches')

# Unir de nuevo con el df de todas las oraciones del borrador
final_data = pd.merge(
    df_oraciones_borrador,
    results_grouped,
    on=['id_articulo_borrador', 'id_oracion_borrador', 'oracion'],
    how='left'
)

# --- 5. Generación del Archivo LaTeX ---

with open(OUTPUT_LATEX_PATH, 'w', encoding='utf-8') as f:
    # Escribir el preámbulo de LaTeX
    f.write(r"""\documentclass[11pt, a4paper]{article}
\usepackage[utf8]{inputenc}
\usepackage{graphicx}
\usepackage[svgnames]{xcolor}
\usepackage[margin=1in]{geometry}
\usepackage{enumitem}

\title{Borrador Anotado - Convención Constitucional 2021-2022}
\author{Análisis de Procedencia}
\date{\today}

\begin{document}
\maketitle

""")

    f.write(r"\begin{enumerate}" + "\n\n")

    # Agrupar por artículo para la iteración (sort_values asegura que los artículos se procesen en orden numérico)
    articulos = final_data.sort_values('id_articulo_borrador').groupby('id_articulo_borrador', sort=False)
    
    for id_articulo, grupo_oraciones in articulos:
        # Iteracion por artículo        
        f.write(r"\item \textbf{Artículo} \newline" + "\n")
        
        for index, row in grupo_oraciones.iterrows():
            
            oracion_texto_escaped = escape_latex(row['oracion'])
            f.write(f"{oracion_texto_escaped}. \n")

            # Escribir las anotaciones de las mejores coincidencias
            matches = row['matches']
            if isinstance(matches, list):
                for match in matches:
                    origen_pdf = escape_latex(match.get('origen_emb', 'N/A'))
                    sim_emb = f"{match.get('similitud_emb', 0):.3f}"
                    sim_tfidf = f"{match.get('similitud_tfidf', 0):.3f}"
                    rank = match.get('rank', 0)
                    
                    # Línea 1: Rango y nombre de la iniciativa
                    anotacion_linea1 = f"\\newline {{\\color{{gray}} \\textbf{{{rank}º:}} {origen_pdf}}}"
                    f.write(anotacion_linea1 + "\n")
                    
                    # Línea 2: Puntuaciones de similitud
                    anotacion_linea2 = f"\\newline {{\\color{{gray}} (Emb: {sim_emb}, TF-IDF: {sim_tfidf})}}"
                    f.write(anotacion_linea2 + "\n")
            
            f.write("\n") # Espacio entre oraciones y sus anotaciones

        f.write("\n") # Espacio extra entre artículos

    f.write(r"\end{enumerate}" + "\n")
    f.write(r"\end{document}" + "\n")
