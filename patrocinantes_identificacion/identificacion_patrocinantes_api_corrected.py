import google.generativeai as genai
import os
import json
import re
import time
import sys
import logging
import copy

# --- 1. Configuración API  y llamada Prompt---
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

LISTA_OFICIAL_NOMBRES = [
    "Abarca, Damaris", "Abarca, Jorge", "Achurra, Ignacio", "Aguilera, Tiare",
    "Alvarado, Gloria", "Alvarez, Julio", "Alvarez, Rodrigo", "Alvez, Amaya",
    "Ampuero, Adriana", "Andrade, Cristobal", "Antilef, Victorino", "Arancibia, Jorge",
    "Arauna, Francisca", "Arellano, Marco", "Arrau, Martin", "Atria, Fernando",
    "Bacian, Wilfredo", "Baradit, Jorge", "Baranda, Benito", "Barcelo, Luis",
    "Barraza, Marcos", "Bassa, Jaime", "Botto, Miguel Angel", "Bown, Carol",
    "Bravo, Daniel", "Caamano, Francisco", "Caiguan, Alexis", "Calvo, Carlos",
    "Cancino, Adriana", "Cantuarias, Rocio", "Carrillo, Alondra", "Castillo, Eduardo",
    "Castillo, Trinidad", "Castro, Claudia", "Catrileo, Rosa", "Celedon, Roberto",
    "Celis, Raul", "Chahin, Fuad", "Chinga, Eric", "Cozzi, Ruggero",
    "Cretton, Eduardo", "Cruz, Andres", "Cubillos, Marcela", "Cespedes, Lorena",
    "Daza, Mauricio", "De la Maza, Bernardo", "Delgado, Aurora", "Dominguez, Gaspar",
    "Dorador, Cristina", "Fernandez, Patricio", "Flores, Alejandra", "Fontaine, Bernardo",
    "Fuchslocher, Javier", "Gallardo, Bessy", "Galleguillos, Felix", "Garin, Renato",
    "Giustinianovich, Elisa", "Godoy, Isabel", "Gonzalez, Dayana", "Gonzalez, Lidia",
    "Grandon, Giovanna", "Grandon, Paola", "Gutierrez, Hugo", "Gomez, Claudio",
    "Gomez, Yarela", "Harboe, Felipe", "Henriquez, Natalia", "Hoppe, Vanessa",
    "Hube, Constanza", "Hurtado, Maximiliano", "Hurtado, Ruth", "Jimenez, Luis",
    "Jofre, Alvaro", "Jurgensen, Harry", "Labbe, Bastian", "Labra, Patricia",
    "Labrana, Elsa", "Laibe, Tomas", "Larrain, Hernan", "Letelier, Margarita",
    "Linconao, Francisca", "Llanquileo, Natividad", "Logan, Rodrigo", "Loncon, Elisa",
    "Madriaga, Tania", "Mamani, Isabella", "Marinovic, Teresa", "Martin, Juan Jose",
    "Martinez, Helmuth", "Mayol, Luis", "Mella, Jeniffer", "Mena, Felipe",
    "Meneses, Janis", "Millabur, Adolfo", "Miranda, Valentina", "Monckeberg, Cristian",
    "Montealegre, Katerine", "Montero, Ricardo", "Moreno, Alfredo", "Munoz, Pedro",
    "Namor, Guillermo", "Navarrete, Geoconda", "Neumann, Ricardo", "Nunez, Nicolas",
    "Olivares, Ivanna", "Orellana, Matias", "Ossandon, Manuel", "Oyarzun, Maria Jose",
    "Pinto, Malucha", "Politzer, Patricia", "Portilla, Ericka", "Pustilnick, Tammy",
    "Perez, Alejandra", "Quinteros, Maria Elisa", "Rebolledo, Barbara", "Reyes, Maria Ramona",
    "Rivera, Maria Magdalena", "Rivera, Pollyana", "Roa, Giovanna", "Royo, Manuela",
    "Saldana, Alvin", "Salinas, Fernando", "San Juan, Constanza", "Schonhaut, Constanza",
    "Sepulveda, Barbara", "Sepulveda, Carolina", "Serey, Mariela", "Silva, Luciano",
    "Squella, Agustin", "Stingo, Daniel", "Sanchez, Beatriz", "Tepper, Maria Angelica",
    "Tirado, Fernando", "Toloza, Pablo", "Ubilla, Maria Cecilia", "Uribe, Cesar",
    "Urrutia, Tatiana", "Valenzuela, Cesar", "Valenzuela, Paulina", "Vallejos, Loreto",
    "Vargas, Margarita", "Vargas, Mario", "Vega, Roberto", "Veloso, Paulina",
    "Velasquez, Hernan", "Vergara, Lisette", "Vidal, Rossana", "Videla, Carolina",
    "Viera, Christian", "Vilches, Carolina", "Villena, Ingrid", "Woldarsky, Manuel",
    "Zarate, Camila", "Zuniga, Luis Arturo"
]
LISTA_OFICIAL_SET = set(LISTA_OFICIAL_NOMBRES)

BASE_FOLDER = "patrocinantes_identificacion"
INPUT_FOLDER = os.path.join(BASE_FOLDER)
OUTPUT_FOLDER = os.path.join(BASE_FOLDER)
PROMPT_FILE = os.path.join(BASE_FOLDER, "prompt_matching.txt")

os.makedirs(OUTPUT_FOLDER, exist_ok=True)

# Parámetros ---
API_CALL_DELAY = 10 # Pausa (segs) cuando la API tira un error
MAX_RETRIES = 5 # Número máximo de reintentos por error
INITIAL_RETRY_DELAY = 5 # Pausa (segs) para el primer reintento

# Carga de API Key ---
GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
if not GOOGLE_API_KEY:
    raise ValueError("Variable de entorno GOOGLE_API_KEY no configurada.")

# Configuración de Gemini ---
try:
    genai.configure(api_key=GOOGLE_API_KEY)
    logging.info("API de Google Generative AI configurada correctamente.")
except Exception as config_err:
    raise RuntimeError(f"Error configurando la API: {config_err}")

# Modelo --- 

MODEL_NAME = 'gemini-2.0-flash'
logging.info(f"Usando el modelo: {MODEL_NAME}")

# Prompt --

try:
    with open(PROMPT_FILE, 'r', encoding='utf-8') as f_prompt:
        prompt_base = f_prompt.read()
    logging.info(f"Prompt base cargado desde '{PROMPT_FILE}'")
    if "firmantes_matched" not in prompt_base or "firmantes_not_matched" not in prompt_base:
        logging.warning(f"ADVERTENCIA: El prompt en '{PROMPT_FILE}' parece no incluir referencias a 'firmantes_matched' o 'firmantes_not_matched'.")
except FileNotFoundError:
    raise FileNotFoundError(f"Error: El archivo del prompt '{PROMPT_FILE}' no se encontró.")
except Exception as e:
    raise RuntimeError(f"Error al leer el archivo del prompt '{PROMPT_FILE}': {e}")


# --- 2. Funciones Auxiliares ---

# Llamda Gemini
def call_gemini_with_retry(prompt_text):
    """Llama a la API de Gemini con reintentos y un delay fijo después de cada intento."""
    retries = 0
    last_exception = None
    while retries < MAX_RETRIES:
        try:
            model = genai.GenerativeModel(MODEL_NAME)
            response = model.generate_content(prompt_text)

            # Pausa después del intento de llamada
            logging.debug(f"Esperando {API_CALL_DELAY} segundos después de la llamada API...")
            time.sleep(API_CALL_DELAY)

            if response and response.parts:
                try:
                    response_text = response.text
                    logging.debug(f"Respuesta cruda de la API recibida.")
                    return response_text
                except ValueError:
                    logging.error("La respuesta de la API fue bloqueada o no contiene texto útil.")
                    last_exception = ValueError("Respuesta bloqueada o sin texto útil")
                except AttributeError:
                     logging.error("La estructura de la respuesta de la API no contiene 'text'. Respuesta: %s", response)
                     last_exception = AttributeError("Respuesta sin atributo 'text'")
            else:
                logging.warning("Respuesta vacía o sin partes recibida de la API.")
                last_exception = ValueError("Respuesta vacía o sin partes")

        except genai.types.generation_types.StopCandidateException as e:
             logging.error(f"Generación detenida por la API: {e}. Respuesta parcial: {e.candidate}")
             last_exception = e
             # Pausa si la generación fue detenida
             logging.debug(f"Esperando {API_CALL_DELAY} segundos después de generación detenida...")
             time.sleep(API_CALL_DELAY)
        except Exception as e:
            logging.error(f"Error llamando a la API de Gemini: {e}")
            last_exception = e
            # Pausa después de un error general
            logging.debug(f"Esperando {API_CALL_DELAY} segundos después de error en llamada API...")
            time.sleep(API_CALL_DELAY)


        # Si se llega aquí, hubo un error o respuesta inválida
        retries += 1
        if retries < MAX_RETRIES:
             wait_time = INITIAL_RETRY_DELAY * (2 ** (retries - 1)) # retraso exponencial
             logging.warning(f"Reintento {retries}/{MAX_RETRIES} después de {wait_time} segundos...")
             time.sleep(wait_time)

    logging.error(f"Falló la llamada a la API después de {MAX_RETRIES} reintentos. Último error: {last_exception}")
    return None # fallo después de reintentos

# Función parse_gemini_response, para armar el json ---
def parse_gemini_response(response_text):
    """Intenta parsear la respuesta JSON de Gemini, extrayendo el JSON."""
    if not response_text:
        return None
    try:
        match = re.search(r'```json\s*(\{.*?\})\s*```', response_text, re.DOTALL)
        if match:
            json_str = match.group(1)
        else:
            start = response_text.find('{')
            end = response_text.rfind('}')
            if start != -1 and end != -1 and end > start:
                json_str = response_text[start:end+1]
            else:
                 logging.error(f"No se encontró un bloque JSON válido en la respuesta: {response_text}")
                 return None

        data = json.loads(json_str)
        if isinstance(data, dict) and \
           'firmantes_matched' in data and isinstance(data['firmantes_matched'], list) and \
           'firmantes_not_matched' in data and isinstance(data['firmantes_not_matched'], list):
            return data
        else:
            logging.error(f"La estructura JSON parseada no es la esperada: {data}")
            return None
    except json.JSONDecodeError as e:
        logging.error(f"Error decodificando JSON de la respuesta: {e}\nRespuesta recibida:\n{response_text}")
        return None
    except Exception as e:
        logging.error(f"Error inesperado parseando la respuesta: {e}\nRespuesta recibida:\n{response_text}")
        return None
    
# Función principal para procesar PDF's ---

def process_file(input_filepath):
    """Procesa un archivo JSON de entrada para corregir firmantes."""
    filename = os.path.basename(input_filepath)
    output_filename = filename.replace(".json", "_corrected.json")
    output_filepath = os.path.join(OUTPUT_FOLDER, output_filename)

    logging.info(f"--- Procesando archivo: {filename} ---")

    try:
        with open(input_filepath, 'r', encoding='utf-8') as f_in:
            data = json.load(f_in)
    except FileNotFoundError:
        logging.error(f"Archivo no encontrado: {input_filepath}")
        return
    except json.JSONDecodeError:
        logging.error(f"Error decodificando JSON en el archivo: {input_filepath}")
        return
    except Exception as e:
        logging.error(f"Error leyendo el archivo {input_filepath}: {e}")
        return

    corrected_count = 0
    entries_to_process = len(data)

    for key, entry in data.items():
        if not isinstance(entry, dict):
            logging.warning(f"Entrada {key} no es un diccionario, omitiendo.")
            continue
        if 'firmantes' not in entry or not isinstance(entry['firmantes'], list):
            logging.warning(f"Entrada {key} no tiene 'firmantes' o no es una lista, omitiendo.")
            continue
        if not entry['firmantes']:
            continue

        needs_correction = False
        if 'firmantes_not_matched' in entry and isinstance(entry['firmantes_not_matched'], list) and entry['firmantes_not_matched']:
            needs_correction = True
        elif 'firmantes_matched' not in entry or not entry.get('firmantes_matched'):
             needs_correction = True

        if needs_correction:
            firmantes_list = entry['firmantes']
            prompt_specific = prompt_base + "\n\n" + json.dumps({"firmantes": firmantes_list}, ensure_ascii=False, indent=2)

            logging.info(f"Intentando corrección para: {key}") 
            response_text = call_gemini_with_retry(prompt_specific)

            if response_text:
                corrected_data = parse_gemini_response(response_text)
                if corrected_data:
                    validated_matched = [name for name in corrected_data.get('firmantes_matched', []) if name in LISTA_OFICIAL_SET]
                    if len(validated_matched) != len(corrected_data.get('firmantes_matched', [])):
                        logging.warning(f"API devolvió nombres matcheados no oficiales para {key}. Se filtraron.")

                    original_firmantes_set = set(firmantes_list) # Para asegurarse de que se compare con la lista original de ESTA entrada
                    validated_not_matched = [name for name in corrected_data.get('firmantes_not_matched', []) if name in original_firmantes_set]
                    if len(validated_not_matched) != len(corrected_data.get('firmantes_not_matched', [])):
                         logging.warning(f"API devolvió nombres no matcheados que no estaban en la lista original para {key}. Se filtraron.")

                    entry['firmantes_matched'] = validated_matched
                    entry['firmantes_not_matched'] = validated_not_matched
                    entry['n_firmantes_matched'] = len(validated_matched)
                    entry['n_firmantes_not_matched'] = len(validated_not_matched)
                    entry['error_limpieza'] = None
                    corrected_count += 1
                    logging.info(f"Corrección exitosa para {key}. Matched: {len(validated_matched)}, Not Matched: {len(validated_not_matched)}") # Log mantenido
                else:
                    logging.error(f"No se pudo parsear o validar la respuesta de la API para {key}.")
                    entry['error_limpieza'] = "Error parseando respuesta API"
            else:
                logging.error(f"No se obtuvo respuesta válida de la API para {key} después de reintentos.")
                entry['error_limpieza'] = "Error llamando API"
        else:
            # Para asegurarse de que los contadores existan y sean correctos si no se corrigió
            if 'firmantes_matched' in entry and isinstance(entry['firmantes_matched'], list):
                 entry['n_firmantes_matched'] = len(entry['firmantes_matched'])
            else:
                 entry['firmantes_matched'] = []
                 entry['n_firmantes_matched'] = 0
            if 'firmantes_not_matched' in entry and isinstance(entry['firmantes_not_matched'], list):
                 entry['n_firmantes_not_matched'] = len(entry['firmantes_not_matched'])
            else:
                 entry['firmantes_not_matched'] = []
                 entry['n_firmantes_not_matched'] = 0


    try:
        with open(output_filepath, 'w', encoding='utf-8') as f_out:
            json.dump(data, f_out, ensure_ascii=False, indent=2)
        logging.info(f"Archivo corregido guardado en: {output_filepath}")
        logging.info(f"Se intentaron/realizaron correcciones en {corrected_count} entradas en {filename}.") # Ajustado mensaje
    except Exception as e:
        logging.error(f"Error guardando el archivo corregido {output_filepath}: {e}")

    logging.info(f"--- Fin del procesamiento para: {filename} ---")


# --- 4. EJECUCIÓN -----------

if __name__ == "__main__":
    files_processed_count = 0 # Contador para saber cuándo pausar entre archivos
    if len(sys.argv) > 1:
        # Procesar archivos pasados como argumentos
        num_args = len(sys.argv) - 1
        for i, input_arg in enumerate(sys.argv[1:]):
            # Pausar ANTES de procesar el siguiente archivo (si no es el primero)
            if files_processed_count > 0:
                logging.info(f"--- Pausa de 60 segundos antes de procesar '{input_arg}' para evitar límite de frecuencia entre archivos ---")
                time.sleep(60)

            input_path = os.path.join(INPUT_FOLDER, input_arg)
            if os.path.isfile(input_path) and input_path.endswith(".json"):
                process_file(input_path)
                files_processed_count += 1 # Incrementar después de procesar un archivo
            elif os.path.isdir(input_path):
                 logging.warning(f"Argumento es un directorio, procesando archivos .json dentro de: {input_path}")
                 inner_files_processed_in_dir = 0
                 files_in_dir = [f for f in os.listdir(input_path) if f.endswith(".json") and "corrected" not in f]
                 for filename in files_in_dir:
                      # La pausa de 10s ocurrirá DESPUÉS de cada llamada a la API dentro de process_file.
                      filepath = os.path.join(input_path, filename)
                      process_file(filepath) # Llama a process_file para cada archivo en el dir
                      inner_files_processed_in_dir += 1
                 files_processed_count += 1 # Incrementar DESPUÉS de procesar el directorio completo
            else:
                logging.warning(f"Argumento '{input_arg}' no es un archivo .json válido o directorio en {INPUT_FOLDER}, omitiendo.")
    else:
        # Procesar todos los archivos .json en la carpeta de entrada si no se pasan argumentos
        logging.info(f"No se especificaron archivos. Procesando todos los .json en {INPUT_FOLDER}")
        files_to_process = [f for f in os.listdir(INPUT_FOLDER) if f.endswith(".json") and "corrected" not in f]
        if not files_to_process:
             logging.info(f"No se encontraron archivos .json para procesar en {INPUT_FOLDER}")
        else:
            for i, filename in enumerate(files_to_process):
                 # Pausar ANTES de procesar el siguiente archivo (si no es el primero)
                if files_processed_count > 0:
                    logging.info(f"--- Pausa de 60 segundos antes de procesar '{filename}' para evitar límite de frecuencia entre archivos ---")
                    time.sleep(60)

                filepath = os.path.join(INPUT_FOLDER, filename)
                process_file(filepath)
                files_processed_count += 1 # Incrementar después de procesar

    logging.info("Proceso completado.")
