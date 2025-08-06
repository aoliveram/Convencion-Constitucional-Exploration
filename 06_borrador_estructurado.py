# Genera 'patrocinantes_identificacion/borrador_constitucional_estructurado.csv'
import pdfplumber
import pandas as pd
import re
import os

# --- Configuración ---
PDF_FILENAME = "PROPUESTA-DE-BORRADOR-CONSTITUCIONAL-14-05-22.pdf"
PDF_FOLDER = "patrocinantes_identificacion"
OUTPUT_CSV_FILENAME = "borrador_constitucional_estructurado.csv"

BORRADOR_PDF_PATH = os.path.join(PDF_FOLDER, PDF_FILENAME)
OUTPUT_CSV_PATH = os.path.join(PDF_FOLDER, OUTPUT_CSV_FILENAME)

print("--- Iniciando Extracción Estructurada (v2) del Borrador Constitucional ---")

# --- 1. Cargar y Pre-procesar el PDF ---
if not os.path.exists(BORRADOR_PDF_PATH):
    print(f"[ERROR] No se encontró el archivo PDF en: {BORRADOR_PDF_PATH}")
    exit()

print(f"Cargando texto desde: {BORRADOR_PDF_PATH}...")

try:
    with pdfplumber.open(BORRADOR_PDF_PATH) as pdf:
        full_text = "".join([page.extract_text(x_tolerance=2) or "" for page in pdf.pages])

    full_text = re.sub(r'\s*\n\s*', ' ', full_text)
    full_text = re.sub(r'\s+', ' ', full_text).strip()
    print("Texto del PDF cargado y pre-procesado exitosamente.")

except Exception as e:
    print(f"[ERROR] Ocurrió un error al leer el archivo PDF: {e}")
    exit()

# --- 2. Extraer Artículos Usando SPLIT ---

print("Buscando y extrayendo artículos con la nueva estrategia de división...")

# Regex para el DELIMITADOR. Capturamos el ID para usarlo después.
split_pattern = r"(\d{1,3})\.\-\s*Artículo"

# Dividir el texto completo usando la expresión regular. re.split con un grupo de captura
# MANTIENE el delimitador en la lista resultante.
parts = re.split(split_pattern, full_text)

if len(parts) <= 1:
    print("[ERROR] No se pudo dividir el documento por artículos. Revisa el patrón.")
    exit()

articles_data = []
# Iteramos sobre la lista de a 2, saltando el primer elemento.
# range(start, stop, step)
for i in range(1, len(parts), 2):
    article_id = parts[i].strip()
    article_body = parts[i+1].strip()
    
    # Limpiar el cuerpo del artículo para quitar el número/nombre del artículo y el título inicial
    # por ejemplo, quita "2°.- Democracia paritaria. " del inicio del cuerpo
    cleaned_body = re.sub(r"^\s*[\w°º\s]+\.\-\s*.*?\.\s", "", article_body, count=1, flags=re.IGNORECASE)
    
    # Limpieza adicional para casos donde no hay título
    if len(cleaned_body) == len(article_body): # si la limpieza no cambió nada
        cleaned_body = re.sub(r"^\s*[\w°º\s]+\.\-\s*", "", article_body, count=1, flags=re.IGNORECASE)
    
    # Quitar cualquier encabezado de capítulo al final
    cleaned_body = re.sub(r"CAPÍTULO\s*\(COM\s*\d+\).*$", "", cleaned_body, flags=re.IGNORECASE).strip()
    
    if article_id and cleaned_body: # Solo añadir si tenemos un ID y un cuerpo
        articles_data.append({
            "id_articulo_borrador": int(article_id),
            "texto_articulo_limpio": cleaned_body
        })

print(f"Se encontraron y procesaron {len(articles_data)} artículos.")

# --- 3. Crear el DataFrame Final ---
df_final = pd.DataFrame(articles_data)

if df_final.empty:
    print("[ERROR] El DataFrame final está vacío. La extracción falló.")
    exit()
    
# --- 4. Guardar Resultados en CSV ---
try:
    df_final.to_csv(OUTPUT_CSV_PATH, index=False, encoding='utf-8-sig')
    print(f"\n¡Éxito! Los artículos estructurados se han guardado en:")
    print(f"-> {OUTPUT_CSV_PATH}")

    # Mostrar las primeras y últimas filas del resultado para verificación
    print("\n--- Vista Previa de los Primeros 5 Artículos Extraídos ---")
    print(df_final.head())
    print("\n--- Vista Previa de los Últimos 5 Artículos Extraídos ---")
    print(df_final.tail())
    print("---------------------------------------------------------")

except Exception as e:
    print(f"[ERROR] No se pudo guardar el archivo CSV: {e}")