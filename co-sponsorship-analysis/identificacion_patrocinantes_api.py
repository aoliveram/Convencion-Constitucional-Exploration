import google.generativeai as genai
import os
import json
import re
import time
from collections import deque
import sys

# --- 1. Configuración ---
PDF_FOLDER = "co-sponsorship-analysis/iniciativas_normas" # Carpeta de los archivos de entrada
# BASE para los nombres de archivo de salida por rango (ahora incluye la ruta de destino)
OUTPUT_PATH_BASE = "co-sponsorship-analysis/api_extracted" # Correcto: destino fuera de la carpeta de PDFs
PROMPT_FILE = "co-sponsorship-analysis/prompt.txt" # Archivo que contiene el prompt

MAX_REQUESTS_PER_MINUTE = 15
SECONDS_WINDOW = 60
INITIAL_RETRY_DELAY = 5 # Delay fijo para reintentos
MAX_RETRY_DELAY = 60 # Segundos # Aunque se usa INITIAL_RETRY_DELAY fijo ahora, se mantiene por si se quiere cambiar

# API Key
GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
if not GOOGLE_API_KEY:
    raise ValueError("Variable de entorno GOOGLE_API_KEY no configurada.")

# Configuración de la biblioteca
try:
    genai.configure(api_key=GOOGLE_API_KEY)
except Exception as config_err:
     raise RuntimeError(f"Error configurando la API: {config_err}")

# --- 2. Modelo y Prompt ---
MODEL_NAME = 'gemini-2.0-flash' # Modelo consistente
print(f"Usando el modelo: {MODEL_NAME}")

# Cargar el prompt
try:
    with open(PROMPT_FILE, 'r', encoding='utf-8') as f_prompt:
        prompt_base = f_prompt.read()
    print(f"Prompt cargado desde '{PROMPT_FILE}'")
    # Verificar si claves esperadas del output están en el prompt (validación ligera)
    if "{" not in prompt_base or "}" not in prompt_base:
         print(f"ADVERTENCIA: El prompt en '{PROMPT_FILE}' parece no incluir una estructura JSON esperada.")
except FileNotFoundError:
    raise FileNotFoundError(f"Error: El archivo del prompt '{PROMPT_FILE}' no se encontró.")
except Exception as e:
    raise RuntimeError(f"Error al leer el archivo del prompt '{PROMPT_FILE}': {e}")

# añadimos instrucción final
prompt_template = prompt_base + "\n\n**Analiza el siguiente documento:**"


# --- 3. Preparar archivos para procesamiento (Extraer números, ordenar) ---
if not os.path.isdir(PDF_FOLDER):
    raise FileNotFoundError(f"Carpeta no encontrada: {PDF_FOLDER}")

all_pdf_filenames = [f for f in os.listdir(PDF_FOLDER) if f.lower().endswith(".pdf")]

processable_files_with_numbers = []
skipped_no_number = 0

for filename in all_pdf_filenames:
    match = re.match(r"^(\d+)", filename) # Busca uno o más dígitos al inicio
    if match:
        try:
            file_number = int(match.group(1))
            processable_files_with_numbers.append((file_number, filename))
        except ValueError:
            print(f"ADVERTENCIA: No se pudo convertir el número '{match.group(1)}' de '{filename}'. Saltando.")
            skipped_no_number += 1
    else:
        print(f"ADVERTENCIA: No se encontró un número inicial en '{filename}'. Saltando.")
        skipped_no_number += 1

# Ordenar archivos por número descendente
processable_files_with_numbers.sort(key=lambda item: item[0], reverse=True)

total_files_found = len(all_pdf_filenames)
total_files_with_numbers = len(processable_files_with_numbers)

if total_files_with_numbers == 0:
    print(f"No se encontraron archivos PDF con número inicial en '{PDF_FOLDER}'.")
    exit()

print(f"Hay {total_files_with_numbers} archivos con número inicial para procesar.")


# --- 4. Agrupar archivos por rangos de 100 y determinar el orden de procesamiento ---

# número máximo y mínimo de docs
max_number = processable_files_with_numbers[0][0] # Ya están ordenados descendentemente
min_number = processable_files_with_numbers[-1][0]

max_range_start = (max_number // 100) * 100
min_range_start = (min_number // 100) * 100

print(f"Rango numérico de archivos encontrados: {min_number} - {max_number}")
print(f"Se procesarán rangos desde {max_range_start} hacia abajo.")

# Inicializar el modelo
try:
    model = genai.GenerativeModel(MODEL_NAME)
except Exception as model_err:
    raise RuntimeError(f"Error inicializando el modelo '{MODEL_NAME}': {model_err}")

request_timestamps = deque()
global_file_counter = 0 # Contador de archivos procesados (intentados) en esta ejecución


# --- 5. Iterar sobre los rangos y procesar archivos dentro de cada rango ---

# ----------------------------------------------------------------------------------------------
# --- Bucle principal para procesar archivos en rangos de 100 ---
# ----------------------------------------------------------------------------------------------    



# Iterar desde el rango más alto HACIA ABAJO, en pasos de 100
for current_range_start in range(max_range_start, min_range_start - 100, -100):

    range_end_bound = current_range_start + 99 # Límite superior teórico del rango

    # archivos que caen en este rango numérico
    files_in_current_range = [(num, fname) for num, fname in processable_files_with_numbers
                              if current_range_start <= num <= range_end_bound]

    # Si no hay archivos en este rango, saltar al siguiente
    if not files_in_current_range:
        continue

    # número mínimo y máximo REAL dentro de este grupo de archivos
    actual_min_in_range = min(num for num, _ in files_in_current_range)
    actual_max_in_range = max(num for num, _ in files_in_current_range)

    # Determinar la RUTA completa del archivo OUTPUT para este rango
    range_output_path = f"{OUTPUT_PATH_BASE}_{actual_min_in_range}_{actual_max_in_range}.json"

    # Verificar si el archivo de salida existe ---
    if os.path.exists(range_output_path):
        print(f"\n--- Archivo de resultados para el rango {actual_min_in_range}-{actual_max_in_range} ya existe ('{range_output_path}'). Saltando este rango. ---")
        global_file_counter += len(files_in_current_range) # Contar como saltados en el total
        continue # Pasar al siguiente rango en el bucle for

    # Crear el directorio de destino si no existe ---
    output_directory = os.path.dirname(range_output_path)
    if output_directory: # Solo intentar crear si hay un directorio especificado
        try:
            os.makedirs(output_directory, exist_ok=True)
        except Exception as dir_err:
            print(f"!!!!!!!! ERROR creando directorio de salida '{output_directory}': {dir_err}. No se procesará este rango.")
            continue 

    # --- Procesar la ventana actual ---
    print(f"\n--- Procesando rango {actual_min_in_range}-{actual_max_in_range} ({len(files_in_current_range)} archivos). Guardando en '{range_output_path}' ---")
    
    # Creamos diccionario para almacenar resultados SOLO de este rango
    current_range_results = {}
    range_file_counter = 0 # Contador de archivos procesados DENTRO de este rango

    for file_number, filename in files_in_current_range:
        global_file_counter += 1
        range_file_counter += 1

        # ruta completa del archivo PDF de entrada usando PDF_FOLDER
        pdf_path = os.path.join(PDF_FOLDER, filename)

        print(f"\n--- Procesando archivo {global_file_counter}/{total_files_with_numbers} (Rango {range_file_counter}/{len(files_in_current_range)}): {filename} ---")

        # Estructura FINAL json
        final_entry_data = {
            "propuesta_norma": None, "error": None, "autor": None,
            "autor_matched": None, "firmantes": [], "firmantes_matched": [],
            "firmantes_not_matched": [], "n_firmantes": 0,
            "n_firmantes_matched": 0, "n_firmantes_not_matched": 0,
            "error_limpieza": None # Añadir campo para error de limpieza
        }

        # Esto no debería ocurrir aquí si el filtrado inicial es correcto, pero es una doble verificación
        if not os.path.isfile(pdf_path):
            print(f"  !! Error: Archivo de entrada no encontrado: {pdf_path}")
            final_entry_data["error"] = "Archivo de entrada no encontrado."
            current_range_results[filename] = final_entry_data
            continue

        uploaded_file_info = None
        attempt = 0
        max_attempts = 5 # Limitar intentos
        temp_api_result = None # Para guardar la respuesta JSON parseada exitosa

        # Bucle de REINTENTOS
        while attempt < max_attempts:
            attempt += 1
            # Control de Frecuencia ---
            try:
                current_time = time.monotonic()
                # Eliminar timestamps antiguos
                while request_timestamps and current_time - request_timestamps[0] > SECONDS_WINDOW:
                    request_timestamps.popleft()
                # Verificar si se alcanzó el límite
                if len(request_timestamps) >= MAX_REQUESTS_PER_MINUTE:
                    wait_time = max(0, (SECONDS_WINDOW - (current_time - request_timestamps[0]))) + 0.2
                    print(f"  -- Rate Limit ({len(request_timestamps)}/{MAX_REQUESTS_PER_MINUTE} RPM). Esperando {wait_time:.1f}s... --", end='\r'); sys.stdout.flush()
                    time.sleep(wait_time)
                    print(" " * 80, end='\r'); sys.stdout.flush() # Limpiar la línea de espera
                    current_time = time.monotonic() # Actualizar tiempo después de esperar
                    # Limpiar de nuevo por si acaso
                    while request_timestamps and current_time - request_timestamps[0] > SECONDS_WINDOW:
                         request_timestamps.popleft()
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
                generation_config = genai.types.GenerationConfig(temperature=0.15) # TEMPERATURA
                request_options = None
                try: request_options = genai.types.RequestOptions(timeout=300)
                except AttributeError: # Para compatibilidad si la versión no tiene RequestOptions
                     # print("  ADVERTENCIA: Su versión de google-generativeai no soporta RequestOptions (timeout).")
                     request_options = None

                # Request a GEMINI
                response = model.generate_content(
                    [prompt_template, uploaded_file_info],
                    generation_config=generation_config,
                    request_options=request_options
                )
                request_timestamps.append(time.monotonic()) # Registrar el timestamp de la llamada API exitosa o fallida (si no lanza excepción antes)


                # --- Procesar Respuesta ---
                print(f"  Intento {attempt}/{max_attempts}: Procesando respuesta...")
                response_text = ""
                try:
                    # Intentar acceder a la respuesta
                    response_text = response.text.strip()
                    # Limpiar bloques de código JSON (opcional, pero robustece el parseo)
                    if response_text.startswith("```json"): response_text = response_text[7:]
                    if response_text.endswith("```"): response_text = response_text[:-3]
                    response_text = response_text.strip()
                except Exception as text_access_error:
                     # Manejar posibles errores al acceder a response.text (ej: si fue bloqueado)
                     block_reason_msg = ""
                     try:
                         # Intentar obtener la razón del bloqueo si existe
                         if hasattr(response, 'prompt_feedback') and response.prompt_feedback.block_reason:
                             block_reason_msg = f"Bloqueado - Razón: {response.prompt_feedback.block_reason}"
                     except Exception: pass # Ignorar errores al intentar obtener la razón del bloqueo
                     error_detail = block_reason_msg or f"Error acceso a response.text: {text_access_error}"
                     final_entry_data["error"] = f"Intento {attempt}/{max_attempts}: {error_detail}" # Guardar error final para este archivo
                     print(f"  !! {error_detail}.")
                     temp_api_result = None # Asegurar que no se usa un resultado parcial/inválido
                     break # Salir del while de reintento para este archivo (error irrecuperable)

                # Parsear JSON
                # Buscar la primera y última llave para encontrar el bloque JSON
                json_match = re.search(r"(\{.*?\})", response_text, re.DOTALL | re.MULTILINE)
                if json_match:
                     json_string = json_match.group(1) # Obtener el contenido dentro de las primeras {}
                     try:
                         parsed_json = json.loads(json_string)
                         # Validación ESTRUCTURA FINAL ESPERADA (9 claves principales + counts)
                         required_keys = ["propuesta_norma", "autor", "autor_matched", "firmantes",
                                          "firmantes_matched", "firmantes_not_matched",
                                          "n_firmantes", "n_firmantes_matched", "n_firmantes_not_matched"]
                         if (isinstance(parsed_json, dict) and
                             all(key in parsed_json for key in required_keys) and # Verificar que las claves existen
                             isinstance(parsed_json.get("firmantes"), list) and # Verificar tipos esperados
                             isinstance(parsed_json.get("firmantes_matched"), list) and
                             isinstance(parsed_json.get("firmantes_not_matched"), list) and
                             isinstance(parsed_json.get("n_firmantes"), int) and
                             isinstance(parsed_json.get("n_firmantes_matched"), int) and
                             isinstance(parsed_json.get("n_firmantes_not_matched"), int)):

                             temp_api_result = parsed_json # Guardar resultado válido
                             print(f"  >> Éxito Intento {attempt}/{max_attempts}: Respuesta JSON válida recibida y validada.")
                             final_entry_data["error"] = None # Limpiar error si había de intento previo
                             break # ***** SALIR DEL BUCLE WHILE (ÉXITO API y VALIDACIÓN) *****
                         else:
                              # Error de validación de estructura o tipos después de parseo JSON
                              final_entry_data["error"] = f"Intento {attempt}/{max_attempts}: Error Validación - JSON recibido no tiene claves/tipos requeridos (9 claves esperadas)."
                              print(f"  !! {final_entry_data['error']}.")
                              temp_api_result = None # Asegurarse de no usar un resultado no válido
                              break # Salir del while de reintento para este archivo (error de formato esperado)
                     except json.JSONDecodeError as json_e:
                         # Error de parseo JSON
                         final_entry_data["error"] = f"Intento {attempt}/{max_attempts}: Error Parseo JSON: {json_e}"
                         print(f"  !! {final_entry_data['error']}.")
                         temp_api_result = None # Asegurarse de no usar un resultado no válido
                         break # Salir del while
                else:
                    final_entry_data["error"] = f"Intento {attempt}/{max_attempts}: No se encontró un bloque JSON {{...}} en la respuesta."
                    print(f"  !! {final_entry_data['error']}.")
                    temp_api_result = None # Asegurarse de no usar un resultado no válido
                    break # Salir del while

            except Exception as e:
                # Manejo de Errores de API/Red ---
                error_message = f"{type(e).__name__} - {str(e)[:150]}" # Limitar longitud del mensaje
                final_entry_data["error"] = f"Intento {attempt}/{max_attempts} fallido: {error_message}" # Guardar error temporal
                error_str = str(e).lower()
                is_retryable = ("deadlineexceeded" in error_str or "resourceexhausted" in error_str or
                                "unavailable" in error_str or "internal" in error_str or "overloaded" in error_str or
                                "429" in error_str or "500" in error_str or "503" in error_str or
                                isinstance(e, TimeoutError) or "transient" in error_str or "aborted" in error_str) # Añadir transient/aborted errors

                if is_retryable and attempt < max_attempts:
                     current_delay = INITIAL_RETRY_DELAY # Usar delay fijo
                     print(f"  >> Intento {attempt}/{max_attempts} fallido: {error_message}. Reintentando en {current_delay:.1f}s...")
                     time.sleep(current_delay)
                     # Limpiar archivo ANTES de reintentar (si se cargó)
                     if uploaded_file_info:
                         try:
                             print(f"  Limpiando archivo cargado ({uploaded_file_info.name}) antes de reintento...")
                             genai.delete_file(name=uploaded_file_info.name)
                             uploaded_file_info = None # Resetear para volver a cargar
                         except Exception as del_err_retry:
                              print(f"  !! Adv: No se pudo borrar {uploaded_file_info.name if uploaded_file_info else ''}: {del_err_retry}")
                     continue # Ir al siguiente intento del BUCLE WHILE para este archivo
                else:
                    # Si no es reintentable O se alcanzó el máximo de intentos
                    if attempt >= max_attempts:
                        final_entry_data["error"] = f"Máximo de {max_attempts} intentos alcanzado. Último error: {error_message}"
                        print(f"  !! Máximo de {max_attempts} intentos alcanzado para '{filename}'.")
                    else:
                         print(f"  !! Error no recuperable en Intento {attempt}/{max_attempts}: {error_message}.")
                    break # Salir del BUCLE WHILE para este archivo

        # --- Fin del Bucle While para el archivo actual ---

        # Asignar Resultado Final del Archivo ---
        if temp_api_result is not None and final_entry_data["error"] is None: # Si la API respondió y se parseó JSON válido
            # Rellenar final_entry_data con los valores del JSON recibido y validado
            final_entry_data.update(temp_api_result) # Copiar todas las claves/valores del JSON válido
            final_entry_data["error"] = None # Asegurar que no haya error de intentos previos
            final_entry_data["error_limpieza"] = None # Asegurar que no haya error de limpieza si la API tuvo éxito

        # Limpieza final del archivo cargado (fuera del while de reintento)
        if uploaded_file_info and hasattr(uploaded_file_info, 'name'):
            try:
                print(f"  Limpiando archivo cargado final: {uploaded_file_info.name}...")
                time.sleep(0.5) # Pequeño delay opcional antes de borrar
                genai.delete_file(name=uploaded_file_info.name)
                # No resetear uploaded_file_info = None aquí; se resetea en el while si es None.
                # La eliminación es lo importante.
            except Exception as delete_err:
                print(f"  !! Adv final: No se pudo eliminar {uploaded_file_info.name}: {delete_err}")
                # Registrar error de limpieza solo si no hubo otro error más grave de API/procesamiento
                if final_entry_data.get("error") is None:
                    final_entry_data["error_limpieza"] = f"Fallo al borrar {uploaded_file_info.name}: {delete_err}"
                else:
                     # Si ya hay un error principal, registrar el error de limpieza también si el campo no existe
                     if final_entry_data.get("error_limpieza") is None:
                         final_entry_data["error_limpieza"] = f"Fallo al borrar {uploaded_file_info.name}: {delete_err}"


        # Guardar el resultado para este archivo en el diccionario DEL RANGO actual
        current_range_results[filename] = final_entry_data


    # --- Fin del Bucle For para archivos dentro del rango actual ---

    # --- GUARDAR resultados para el rango actual ---
    print(f"\n--- Rango {actual_min_in_range}-{actual_max_in_range} completado. Guardando resultados en '{range_output_path}' ---")
    try:
        with open(range_output_path, 'w', encoding='utf-8') as f:
            json.dump(current_range_results, f, ensure_ascii=False, indent=2)
        print(f"--- Resultados para el rango {actual_min_in_range}-{actual_max_in_range} guardados exitosamente. ---")
    except Exception as write_e:
         print(f"!!!!!!!! ERROR escribiendo el archivo de salida para el rango {actual_min_in_range}-{actual_max_in_range} ('{range_output_path}'): {write_e}")
