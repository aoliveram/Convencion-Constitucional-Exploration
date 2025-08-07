# -*- coding: utf-8 -*-
import google.generativeai as genai
import os
import json
import glob
import time
from collections import deque
import sys
import math

# --- 1. Configuración ---
INPUT_FOLDER = "patrocinantes_identificacion/"
OUTPUT_FOLDER = "patrocinantes_identificacion/"
PROMPT_FILE = os.path.join(INPUT_FOLDER, "prompt_cleaning_BAD_2.txt") 
INPUT_FILE_PATTERN = "api_extracted_*_corrected_3.json"
OUTPUT_FILE_SUFFIX = "corrected_4.json"

# --- Constantes de Límite de Frecuencia, Reintentos y Lotes ---
MAX_REQUESTS_PER_MINUTE = 10 
BATCH_SIZE = 20 # Número de iniciativas por llamada a la API
SECONDS_WINDOW = 60
MAX_RETRIES = 3
RETRY_DELAY = 10 # Aumentar el delay para lotes más grandes

# --- Configuración de la API de Gemini ---
try:
    GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
    if not GOOGLE_API_KEY:
        raise ValueError("Variable de entorno GOOGLE_API_KEY no configurada.")
    genai.configure(api_key=GOOGLE_API_KEY)
except Exception as e:
    print(f"[ERROR DE CONFIGURACIÓN] No se pudo configurar la API de Google: {e}")
    sys.exit(1)

# Modelo a utilizar
MODEL_NAME = 'gemini-2.5-flash' 
print(f"Usando el modelo de Gemini: {MODEL_NAME}")
model = genai.GenerativeModel(MODEL_NAME)

# --- 2. Cargar el Prompt Base ---
try:
    with open(PROMPT_FILE, 'r', encoding='utf-8') as f:
        prompt_base = f.read()
    print(f"Prompt para lotes cargado exitosamente desde '{PROMPT_FILE}'")
except FileNotFoundError:
    print(f"[ERROR FATAL] El archivo del prompt '{PROMPT_FILE}' no se encontró.")
    sys.exit(1)

# --- 3. Encontrar y Procesar Archivos JSON ---
input_files = glob.glob(os.path.join(INPUT_FOLDER, INPUT_FILE_PATTERN))
if not input_files:
    print(f"No se encontraron archivos con el patrón '{INPUT_FILE_PATTERN}'.")
    sys.exit(0)

print(f"\nSe encontraron {len(input_files)} archivos JSON para procesar.")
request_timestamps = deque()

# Bucle principal sobre cada archivo JSON
for input_file_path in sorted(input_files):
    filename_base = os.path.basename(input_file_path).replace("_corrected_3.json", "")
    output_file_path = os.path.join(OUTPUT_FOLDER, f"{filename_base}_{OUTPUT_FILE_SUFFIX}")
    
    print(f"\n--- Procesando Archivo: {os.path.basename(input_file_path)} ---")

    if os.path.exists(output_file_path):
        print(f"  -> El archivo de salida '{os.path.basename(output_file_path)}' ya existe. Saltando.")
        continue

    try:
        with open(input_file_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
    except Exception as e:
        print(f"  [ERROR] No se pudo leer o parsear el archivo. Saltando. Error: {e}")
        continue
    
    # Convertir el diccionario de iniciativas en una lista de items para poder hacer lotes
    iniciativas_items = list(data.items())
    total_iniciativas = len(iniciativas_items)
    cleaned_results = {}
    
    num_batches = math.ceil(total_iniciativas / BATCH_SIZE)
    
    # Bucle sobre los lotes (batches) de iniciativas
    for i in range(num_batches):
        start_index = i * BATCH_SIZE
        end_index = start_index + BATCH_SIZE
        batch_items = iniciativas_items[start_index:end_index]
        
        print(f"  -> Procesando lote {i + 1}/{num_batches} (iniciativas {start_index + 1} a {min(end_index, total_iniciativas)})...")
        
        # Crear el array JSON para el prompt, donde cada elemento es un objeto iniciativa
        input_json_for_prompt = [{key: value} for key, value in batch_items]
        input_json_str = json.dumps(input_json_for_prompt, ensure_ascii=False, indent=2)
        
        full_prompt = f"{prompt_base}\n\n{input_json_str}"
        
        retries = 0
        success = False
        while retries < MAX_RETRIES and not success:
            # Control de frecuencia
            current_time = time.monotonic()
            while request_timestamps and current_time - request_timestamps[0] > SECONDS_WINDOW:
                request_timestamps.popleft()
            if len(request_timestamps) >= MAX_REQUESTS_PER_MINUTE:
                wait_time = max(0, (SECONDS_WINDOW - (current_time - request_timestamps[0]))) + 0.2
                print(f"     -- Límite de frecuencia alcanzado. Esperando {wait_time:.1f}s... --", end='\r')
                time.sleep(wait_time)
                print(" " * 80, end='\r')

            try:
                response = model.generate_content(full_prompt)
                request_timestamps.append(time.monotonic())
                
                response_text = response.text.strip()
                if response_text.startswith("```json"):
                    response_text = response_text[7:]
                if response_text.endswith("```"):
                    response_text = response_text[:-3]
                
                cleaned_json_list = json.loads(response_text)
                
                # Asegurarse de que la respuesta es una lista y tiene el tamaño correcto
                if not isinstance(cleaned_json_list, list) or len(cleaned_json_list) != len(batch_items):
                    raise ValueError(f"La respuesta de la API no es una lista o su tamaño no coincide con el lote. Tamaño esperado: {len(batch_items)}, recibido: {len(cleaned_json_list)}")

                # Actualizar el diccionario de resultados con los datos del lote procesado
                for cleaned_item in cleaned_json_list:
                    cleaned_results.update(cleaned_item)
                
                success = True
                
            except Exception as api_error:
                retries += 1
                print(f"     [ERROR LOTE - Intento {retries}/{MAX_RETRIES}] {str(api_error)[:150]}...")
                if retries < MAX_RETRIES:
                    time.sleep(RETRY_DELAY)
                else:
                    # Si el lote falla, registrar el error para todas las iniciativas en ese lote
                    for key, value in batch_items:
                        cleaned_results[key] = value
                        cleaned_results[key]['error_limpieza_api'] = f"Fallo en lote tras {MAX_RETRIES} intentos: {str(api_error)}"

    # --- 4. Guardar el archivo procesado ---
    try:
        with open(output_file_path, 'w', encoding='utf-8') as f:
            json.dump(cleaned_results, f, ensure_ascii=False, indent=2)
        print(f"  -> ¡Éxito! Archivo limpio guardado en: {os.path.basename(output_file_path)}")
    except Exception as e:
        print(f"  [ERROR] No se pudo guardar el archivo de salida '{output_file_path}'. Error: {e}")

print("\n--- Proceso de Limpieza Completado ---")