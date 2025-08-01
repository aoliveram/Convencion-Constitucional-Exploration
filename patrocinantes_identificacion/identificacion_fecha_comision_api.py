# -*- coding: utf-8 -*-
import google.generativeai as genai
import os
import json
import re
import time
from collections import deque
import sys
import glob

# --- 1. Configuración ---
# Rutas relativas a la carpeta donde se ejecuta el script
PDF_FOLDER = "patrocinantes_identificacion/iniciativas_normas"
JSON_FOLDER = "patrocinantes_identificacion"
PROMPT_FILE = "patrocinantes_identificacion/prompt_augmentation.txt" # <<-- NUEVO PROMPT

# Sufijos para los archivos JSON de entrada y salida
INPUT_SUFFIX = "_corrected_2.json"
OUTPUT_SUFFIX = "_corrected_3.json"

# --- Constantes de Límite de Frecuencia ---
MAX_REQUESTS_PER_MINUTE = 15
SECONDS_WINDOW = 60
INITIAL_RETRY_DELAY = 5 # Delay fijo para reintentos
MAX_RETRY_DELAY = 60 # Segundos

# Carga tu API Key
GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
if not GOOGLE_API_KEY:
    raise ValueError("Variable de entorno GOOGLE_API_KEY no configurada.")

# Configura la biblioteca
try:
    genai.configure(api_key=GOOGLE_API_KEY)
except Exception as config_err:
     raise RuntimeError(f"Error configurando la API: {config_err}")

# --- 2. Modelo y Prompt ---
MODEL_NAME = 'gemini-2.0-flash'
print(f"Usando el modelo: {MODEL_NAME}")

# Cargar el prompt desde el archivo
try:
    with open(PROMPT_FILE, 'r', encoding='utf-8') as f_prompt:
        prompt_base = f_prompt.read()
    print(f"Prompt de aumento de datos cargado desde '{PROMPT_FILE}'")
except FileNotFoundError:
    raise FileNotFoundError(f"Error: El archivo del prompt '{PROMPT_FILE}' no se encontró.")
except Exception as e:
    raise RuntimeError(f"Error al leer el archivo del prompt '{PROMPT_FILE}': {e}")

# Añadir la instrucción final
prompt_template = prompt_base + "\n\n**Analiza el siguiente documento:**"

# --- 3. Procesamiento de Archivos JSON y PDFs ---
if not os.path.isdir(JSON_FOLDER):
    raise FileNotFoundError(f"La carpeta de JSONs no fue encontrada: {JSON_FOLDER}")
if not os.path.isdir(PDF_FOLDER):
    raise FileNotFoundError(f"La carpeta de PDFs no fue encontrada: {PDF_FOLDER}")

# Encontrar todos los archivos JSON de entrada
json_input_files = glob.glob(os.path.join(JSON_FOLDER, f"*{INPUT_SUFFIX}"))

if not json_input_files:
    print(f"No se encontraron archivos JSON con el sufijo '{INPUT_SUFFIX}' en la carpeta '{JSON_FOLDER}'.")
    exit()

print(f"Se encontraron {len(json_input_files)} archivos JSON para procesar.")

# Inicializa el modelo
try:
    model = genai.GenerativeModel(MODEL_NAME)
except Exception as model_err:
    raise RuntimeError(f"Error inicializando el modelo '{MODEL_NAME}': {model_err}")

request_timestamps = deque()

# Bucle principal por CADA ARCHIVO JSON
for json_input_path in json_input_files:
    json_output_path = json_input_path.replace(INPUT_SUFFIX, OUTPUT_SUFFIX)

    print(f"\n============================================================")
    print(f"Procesando archivo JSON: {os.path.basename(json_input_path)}")
    print(f"============================================================")

    # Reanudación: si el archivo de salida ya existe, saltar
    if os.path.exists(json_output_path):
        print(f"  -> El archivo de salida '{os.path.basename(json_output_path)}' ya existe. Saltando.")
        continue

    # Cargar el JSON base
    try:
        with open(json_input_path, 'r', encoding='utf-8') as f:
            base_data = json.load(f)
    except Exception as e:
        print(f"  !! Error al cargar o parsear '{json_input_path}': {e}. Saltando este archivo.")
        continue

    augmented_data = base_data.copy() # Trabajar sobre una copia
    filenames_to_process = list(base_data.keys())
    total_files_in_json = len(filenames_to_process)

    # Bucle secundario por CADA PDF listado en el JSON actual
    for index, filename in enumerate(filenames_to_process):
        print(f"\n--- Procesando PDF {index + 1}/{total_files_in_json}: {filename} ---")
        pdf_path = os.path.join(PDF_FOLDER, filename)

        # Si el PDF no existe, registrar error y continuar
        if not os.path.isfile(pdf_path):
            print(f"  !! Error: Archivo PDF no encontrado: {pdf_path}")
            # Marcar el error en la entrada existente
            augmented_data[filename]['error_aumento'] = "Archivo PDF no encontrado en la ruta esperada."
            continue

        uploaded_file_info = None
        attempt = 0
        max_attempts = 5
        api_call_successful = False

        # Bucle de reintentos para el PDF actual
        while attempt < max_attempts:
            attempt += 1
            # --- Control de Frecuencia ---
            try:
                current_time = time.monotonic()
                while request_timestamps and current_time - request_timestamps[0] > SECONDS_WINDOW: request_timestamps.popleft()
                if len(request_timestamps) >= MAX_REQUESTS_PER_MINUTE:
                    wait_time = max(0, (SECONDS_WINDOW - (current_time - request_timestamps[0]))) + 0.2
                    print(f"  -- Límite de peticiones. Esperando {wait_time:.1f}s... --", end='\r'); sys.stdout.flush()
                    time.sleep(wait_time)
                    print(" " * 80, end='\r'); sys.stdout.flush()
            except Exception as rate_limit_err: print(f"  !! Error control frecuencia: {rate_limit_err}")

            # --- Inicio Intento API ---
            try:
                # --- Cargar Archivo ---
                if not uploaded_file_info:
                    print(f"  Intento {attempt}/{max_attempts}: Cargando archivo...")
                    uploaded_file_info = genai.upload_file(path=pdf_path)
                    print(f"  Archivo cargado. API Name: {uploaded_file_info.name}")

                # --- Generar Contenido ---
                print(f"  Intento {attempt}/{max_attempts}: Enviando prompt...")
                generation_config = genai.types.GenerationConfig(temperature=0.0) # Temperatura baja para ser más determinista
                request_options = genai.types.RequestOptions(timeout=120)

                response = model.generate_content(
                    [prompt_template, uploaded_file_info],
                    generation_config=generation_config,
                    request_options=request_options
                )
                request_timestamps.append(time.monotonic())
                
                # --- Procesar Respuesta ---
                print(f"  Intento {attempt}/{max_attempts}: Procesando respuesta...")
                response_text = response.text.strip()
                if response_text.startswith("```json"): response_text = response_text[7:]
                if response_text.endswith("```"): response_text = response_text[:-3]
                response_text = response_text.strip()

                # Parsear JSON
                parsed_json = json.loads(response_text)
                required_keys = ["comision", "comision_n", "fecha"]
                if isinstance(parsed_json, dict) and all(key in parsed_json for key in required_keys):
                    print(f"  >> Éxito Intento {attempt}/{max_attempts}: Respuesta JSON válida recibida.")
                    
                    # --- Lógica para insertar los nuevos campos en el orden correcto ---
                    original_entry = augmented_data[filename]
                    new_entry = {}
                    inserted = False
                    
                    for key, value in original_entry.items():
                        new_entry[key] = value
                        if key == "autor_matched" and not inserted:
                            new_entry["comision"] = parsed_json.get("comision")
                            new_entry["comision_n"] = parsed_json.get("comision_n")
                            new_entry["fecha"] = parsed_json.get("fecha")
                            inserted = True
                    
                    # Si 'autor_matched' no existía, agregar al final (fallback)
                    if not inserted:
                        new_entry.update(parsed_json)

                    augmented_data[filename] = new_entry # Reemplazar la entrada con la nueva aumentada y ordenada
                    api_call_successful = True
                    break # Salir del bucle de reintentos
                else:
                    raise ValueError("JSON recibido no tiene las claves requeridas.")

            except Exception as e:
                error_message = f"Intento {attempt}/{max_attempts} fallido: {type(e).__name__} - {str(e)[:150]}"
                print(f"  !! {error_message}")
                augmented_data[filename]['error_aumento'] = error_message
                
                if attempt < max_attempts:
                    time.sleep(INITIAL_RETRY_DELAY)
                    if uploaded_file_info:
                        try:
                            genai.delete_file(name=uploaded_file_info.name)
                            uploaded_file_info = None
                        except Exception as del_err:
                            print(f"  !! Adv: No se pudo borrar {uploaded_file_info.name}: {del_err}")
                else:
                    print(f"  !! Máximo de intentos alcanzado para '{filename}'.")
        
        # --- Limpieza final del archivo cargado ---
        if uploaded_file_info and hasattr(uploaded_file_info, 'name'):
            try:
                time.sleep(0.5)
                genai.delete_file(name=uploaded_file_info.name)
            except Exception as delete_err:
                print(f"  !! Adv final: No se pudo eliminar {uploaded_file_info.name}: {delete_err}")


    # --- Guardar el archivo JSON aumentado ---
    print(f"\n--- Procesamiento del lote completado. Guardando resultados en '{os.path.basename(json_output_path)}' ---")
    try:
        with open(json_output_path, 'w', encoding='utf-8') as f:
            json.dump(augmented_data, f, ensure_ascii=False, indent=2)
        print("--- Guardado exitoso. ---")
    except Exception as write_e:
        print(f"!!!!!!!! ERROR escribiendo el archivo de salida '{json_output_path}': {write_e}")

print("\n--- Proceso Finalizado para todos los archivos JSON ---")