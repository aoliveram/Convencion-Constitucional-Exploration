# Creates
# analisis_procedencia_oracion-patrocinante.json

import pandas as pd
import json
import re
import os
from tqdm import tqdm  # barra de progreso

# --- 1. Configuración de Archivos ---

BASE_FOLDER = "co-sponsorship-analysis"
ANALYSIS_FOLDER = "co-sponsorship-analysis/analizar_procedencia_borrador"

# Rutas de los archivos de entrada
BORRADOR_JSON_PATH = os.path.join(BASE_FOLDER, "borrador_constitucional_estructurado.json")
COMPARACION_CSV_PATH = os.path.join(ANALYSIS_FOLDER, "11-TFIDF-vs-Emb.csv")
INICIATIVAS_ORACIONES_CSV_PATH = os.path.join(ANALYSIS_FOLDER, "11-sentences_iniciativas.csv")

# Carpeta que contiene los JSON originales con metadatos de firmantes
INICIATIVAS_METADATA_FOLDER = BASE_FOLDER
INICIATIVAS_METADATA_PATTERN = r"api_extracted_.*_corrected_4\.json$"

# Ruta del archivo de salida JSON
OUTPUT_JSON_PATH = "co-sponsorship-analysis/analizar_procedencia_borrador/11-analisis_procedencia_oracion-patrocinante.json"

# --- 2. Carga y Preparación de Datos ---

try:
    # Cargar el borrador estructurado
    df_borrador = pd.read_json(BORRADOR_JSON_PATH)

    # Cargar los datos de comparación
    df_comparacion = pd.read_csv(COMPARACION_CSV_PATH)

    # Cargar el CSV de oraciones de iniciativas
    df_oraciones_iniciativas = pd.read_csv(INICIATIVAS_ORACIONES_CSV_PATH)

    # Cargar los metadatos de todas las iniciativas (firmantes)
    all_metadata = {}
    for filename in os.listdir(INICIATIVAS_METADATA_FOLDER):
        if re.match(INICIATIVAS_METADATA_PATTERN, filename):
            path = os.path.join(INICIATIVAS_METADATA_FOLDER, filename)
            with open(path, 'r', encoding='utf-8') as f:
                data = json.load(f)
                for pdf_name, content in data.items():
                    # Guardar solo los firmantes matcheados, o una lista vacía si no existen
                    all_metadata[pdf_name] = content.get('firmantes_matched', [])

    df_metadata_iniciativas = pd.DataFrame(list(all_metadata.items()), columns=['filename', 'firmantes_matched'])

except FileNotFoundError as e:
    print(f"\n[ERROR] Archivo no encontrado: {e.filename}")
    print("Por favor, verifica las rutas en la sección de configuración.")
    exit()

# Tokenizar el Borrador en oraciones y crear IDs
df_borrador['texto_articulo_limpio'] = df_borrador['texto_articulo_limpio'].str.split(r'\.\s*')
df_oraciones_borrador = df_borrador.explode('texto_articulo_limpio').rename(columns={'texto_articulo_limpio': 'oracion'})
df_oraciones_borrador = df_oraciones_borrador[df_oraciones_borrador['oracion'].str.strip().str.len() > 15].reset_index(drop=True)
df_oraciones_borrador['id_oracion_borrador'] = df_oraciones_borrador.index + 1

# --- 3. Enriquecer los datos de comparación ---

# Cambiar el nombre de la columna 'filename' a 'nombre_pdf'
df_metadata_iniciativas.rename(columns={'filename': 'nombre_pdf'}, inplace=True)

# Unir los datos de comparación con el texto de las iniciativas y los firmantes
df_comparacion_enriquecida = pd.merge(
    df_comparacion,
    df_oraciones_iniciativas[['id_oracion_iniciativa', 'filename', 'oracion']],
    left_on='id_oracion_iniciativa_match_emb',
    right_on='id_oracion_iniciativa',
    how='left'
).rename(columns={'filename': 'nombre_pdf', 'oracion': 'txt_oracion_iniciativa'})

df_comparacion_enriquecida = pd.merge(
    df_comparacion_enriquecida,
    df_metadata_iniciativas,
    on='nombre_pdf',
    how='left'
)


def prepare_rank(df, rank_number, suffix):
    """Filtra por rank, elimina la columna de rank y agrega sufijos únicos."""
    rank_df = df[df['rank'] == rank_number].copy()
    rank_df = rank_df.drop(columns=['rank'])
    rename_map = {col: f"{col}_{suffix}" for col in rank_df.columns if col != 'id_oracion_borrador'}
    return rank_df.rename(columns=rename_map)


# --- 4. Pivotar los datos para tener Rank 1, Rank 2 y Rank 3 como columnas ---

df_rank1 = prepare_rank(df_comparacion_enriquecida, 1, 'r1')
df_rank2 = prepare_rank(df_comparacion_enriquecida, 2, 'r2')
df_rank3 = prepare_rank(df_comparacion_enriquecida, 3, 'r3')

# Unir todo junto, empezando por todas las oraciones del borrador
df_final_plano = pd.merge(
    df_oraciones_borrador,
    df_rank1,
    on='id_oracion_borrador',
    how='left'
)

df_final_plano = pd.merge(
    df_final_plano,
    df_rank2,
    on='id_oracion_borrador',
    how='left'
)

df_final_plano = pd.merge(
    df_final_plano,
    df_rank3,
    on='id_oracion_borrador',
    how='left'
)

# --- 5. Generar la Estructura JSON Anidada Final ---

output_list = []

coincidence_map = [
    ('1st_coincidencia', 'r1'),
    ('2nd_coincidencia', 'r2'),
    ('3rd_coincidencia', 'r3'),
]

# Usar tqdm para mostrar una barra de progreso
for _, row in tqdm(df_final_plano.iterrows(), total=df_final_plano.shape[0]):

    # Construir el objeto base para cada oración del borrador
    oracion_obj = {
        "id_art_borrador": row['id_articulo_borrador'],
        "id_oracion_borrador": f"{row['id_articulo_borrador']}-{row['id_oracion_borrador']}",
        "txt_oracion_borrador": row['oracion']
    }

    for label, suffix in coincidence_map:
        nombre_col = f"nombre_pdf_{suffix}"
        nombre_val = row.get(nombre_col)
        if pd.notna(nombre_val):
            oracion_obj[label] = {
                "nombre_pdf": nombre_val,
                "txt_oracion_iniciativa": row[f"txt_oracion_iniciativa_{suffix}"],
                "puntaje_embedding": row[f"similitud_emb_{suffix}"],
                "puntaje_tfidf": row[f"similitud_tfidf_{suffix}"],
                "firmantes_matched": row[f"firmantes_matched_{suffix}"]
            }
        else:
            oracion_obj[label] = None

    output_list.append(oracion_obj)

# --- 6. Guardar ---

try:
    with open(OUTPUT_JSON_PATH, 'w', encoding='utf-8') as f:
        # indent=2 para que el JSON sea legible
        json.dump(output_list, f, ensure_ascii=False, indent=2)
    print(f"\n¡Éxito! El análisis completo se ha guardado en:")
    print(f"-> {OUTPUT_JSON_PATH}")
except Exception as e:
    print(f"[ERROR] No se pudo guardar el archivo JSON: {e}")
