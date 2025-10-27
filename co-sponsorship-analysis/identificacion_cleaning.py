# -*- coding: utf-8 -*-
import google.generativeai as genai
import os
import json
import glob
import time
from collections import deque
import sys

# --- 1. Configuración ---
INPUT_FOLDER = "patrocinantes_identificacion/"
OUTPUT_FOLDER = "patrocinantes_identificacion/"
PROMPT_FILE = os.path.join(INPUT_FOLDER, "prompt_cleaning.txt")
INPUT_FILE_PATTERN = "api_extracted_*_corrected_3.json"
OUTPUT_FILE_SUFFIX = "corrected_4.json"

# --- Constantes de Límite de Frecuencia y Reintentos ---
MAX_REQUESTS_PER_MINUTE = 15 # Para Gemini 2.5 Flash
SECONDS_WINDOW = 60
MAX_RETRIES = 3
RETRY_DELAY = 5 # segundos

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
MODEL_NAME = 'gemini-2.5-flash-lite'
print(f"Usando el modelo de Gemini: {MODEL_NAME}")
model = genai.GenerativeModel(MODEL_NAME)

# --- 2. Cargar el Prompt Base ---
try:
    with open(PROMPT_FILE, 'r', encoding='utf-8') as f:
        prompt_base = f.read()
    print(f"Prompt cargado exitosamente desde '{PROMPT_FILE}'")
except FileNotFoundError:
    print(f"[ERROR FATAL] El archivo del prompt '{PROMPT_FILE}' no se encontró.")
    sys.exit(1)
except Exception as e:
    print(f"[ERROR FATAL] No se pudo leer el archivo del prompt: {e}")
    sys.exit(1)

# --- 3. Encontrar y Procesar Archivos JSON ---
input_files = glob.glob(os.path.join(INPUT_FOLDER, INPUT_FILE_PATTERN))
if not input_files:
    print(f"No se encontraron archivos con el patrón '{INPUT_FILE_PATTERN}' en la carpeta '{INPUT_FOLDER}'.")
    sys.exit(0)

print(f"\nSe encontraron {len(input_files)} archivos JSON para procesar.")

# Cola para controlar la frecuencia de las solicitudes
request_timestamps = deque()

# Bucle principal sobre cada archivo JSON
for input_file_path in sorted(input_files):
    filename_base = os.path.basename(input_file_path).replace("_corrected_3.json", "")
    output_file_path = os.path.join(OUTPUT_FOLDER, f"{filename_base}_{OUTPUT_FILE_SUFFIX}")
    
    print(f"\n--- Procesando Archivo: {os.path.basename(input_file_path)} ---")

    # Saltar si el archivo de salida ya existe
    if os.path.exists(output_file_path):
        print(f"  -> El archivo de salida '{os.path.basename(output_file_path)}' ya existe. Saltando.")
        continue

    try:
        with open(input_file_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
    except Exception as e:
        print(f"  [ERROR] No se pudo leer o parsear el archivo JSON de entrada. Saltando. Error: {e}")
        continue
        
    cleaned_results = {}
    total_iniciativas = len(data)
    
    # Bucle sobre cada iniciativa dentro del archivo JSON
    for i, (iniciativa_key, iniciativa_data) in enumerate(data.items()):
        print(f"  -> Procesando iniciativa {i + 1}/{total_iniciativas}: {iniciativa_key[:50]}...")
        
        # Crear el objeto JSON de entrada para el prompt
        input_json_for_prompt = {iniciativa_key: iniciativa_data}
        
        # Convertir a string para pasarlo al prompt
        input_json_str = json.dumps(input_json_for_prompt, ensure_ascii=False, indent=2)
        
        full_prompt = f"{prompt_base}\n\n{input_json_str}"
        
        # --- Llamada a la API con Reintentos y Control de Frecuencia ---
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
                
                # Extraer y parsear la respuesta JSON
                response_text = response.text.strip()
                
                # Limpiar el bloque de código si Gemini lo devuelve
                if response_text.startswith("```json"):
                    response_text = response_text[7:]
                if response_text.endswith("```"):
                    response_text = response_text[:-3]
                
                cleaned_json = json.loads(response_text)
                
                # El prompt pide devolver el JSON completo. Lo extraemos.
                cleaned_key = list(cleaned_json.keys())[0]
                cleaned_data = cleaned_json[cleaned_key]
                
                # Guardar el resultado limpio
                cleaned_results[cleaned_key] = cleaned_data
                success = True
                
            except Exception as api_error:
                retries += 1
                print(f"     [ERROR API - Intento {retries}/{MAX_RETRIES}] {str(api_error)[:150]}...")
                if retries < MAX_RETRIES:
                    time.sleep(RETRY_DELAY)
                else:
                    # Registrar el error y continuar con la siguiente iniciativa
                    cleaned_results[iniciativa_key] = iniciativa_data # Guardar original
                    cleaned_results[iniciativa_key]['error_limpieza_api'] = f"Fallo tras {MAX_RETRIES} intentos: {str(api_error)}"

    # --- 4. Guardar el archivo procesado ---
    try:
        with open(output_file_path, 'w', encoding='utf-8') as f:
            json.dump(cleaned_results, f, ensure_ascii=False, indent=2)
        print(f"  -> ¡Éxito! Archivo limpio guardado en: {os.path.basename(output_file_path)}")
    except Exception as e:
        print(f"  [ERROR] No se pudo guardar el archivo de salida '{output_file_path}'. Error: {e}")

print("\n--- Proceso de Limpieza Completado ---")