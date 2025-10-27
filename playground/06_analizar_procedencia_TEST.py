import pdfplumber
import re
from collections import defaultdict

# --- Configuración ---
BORRADOR_PDF_PATH = "patrocinantes_identificacion/PROPUESTA-DE-BORRADOR-CONSTITUCIONAL-14-05-22.pdf"
PAGES_TO_TEST = 1 # Analicemos solo la primera página a fondo por ahora

# ==============================================================================
# PARTE 1: DIAGNÓSTICO DE FUENTES
# Ejecuta esta parte primero para descubrir el nombre de la fuente en negrita.
# ==============================================================================
print("--- Iniciando Parte 1: Diagnóstico de Fuentes ---")
font_names = set()
try:
    with pdfplumber.open(BORRADOR_PDF_PATH) as pdf:
        first_page = pdf.pages[0]
        # Recolectar todos los nombres de fuentes únicos de los caracteres
        for char in first_page.chars:
            if 'fontname' in char:
                font_names.add(char['fontname'])

    print("Fuentes encontradas en la primera página:")
    for font in sorted(list(font_names)):
        print(f" - {font}")
    print("\n[ACCIÓN REQUERIDA] Identifica la fuente que parece ser la negrita (ej. 'TimesNewRomanPS-BoldMT')")
    print("y cópiala en la variable 'FONT_NAME_BOLD' en la Parte 2 del script.")

except Exception as e:
    print(f"[ERROR] No se pudo realizar el diagnóstico de fuentes: {e}")

# ==============================================================================
# PARTE 2: EXTRACCIÓN MEJORADA
# Después de completar la Parte 1, actualiza FONT_NAME_BOLD y ejecuta de nuevo.
# ==============================================================================
import pdfplumber
import re
from collections import defaultdict

# --- Configuración ---
BORRADOR_PDF_PATH = "patrocinantes_identificacion/PROPUESTA-DE-BORRADOR-CONSTITUCIONAL-14-05-22.pdf"
PAGES_TO_TEST = 5 # Puedes aumentar esto para probar más páginas

# --- Nombre de la Fuente en Negrita ---
# Basado en tu diagnóstico, esta es la fuente correcta.
FONT_NAME_BOLD = "TimesNewRomanPS-BoldMT"

print(f"--- Iniciando Test con Detección de Negritas Mejorada ---")
print(f"Archivo a analizar: {BORRADOR_PDF_PATH}")
print(f"Usando '{FONT_NAME_BOLD}' como fuente en negrita.")
print(f"Analizando las primeras {PAGES_TO_TEST} páginas...\n")


def process_page_by_words(page):
    """
    Extrae palabras en lugar de caracteres para un mejor orden y análisis.
    Esta versión es más robusta contra el error 'y0'.
    """
    # Extraer todas las palabras con su información detallada
    words = page.extract_words(x_tolerance=2, y_tolerance=2, keep_blank_chars=True)
    
    current_line_words = []
    reconstructed_lines = []
    
    if not words:
        return []

    # Usar el 'top' (coordenada y superior) de la primera palabra como referencia para la primera línea
    current_top = words[0].get('top', 0)
    
    for word in words:
        # CORRECCIÓN: Verificar si la palabra tiene la propiedad 'top' antes de usarla
        if 'top' not in word:
            continue # Ignorar elementos que no son palabras válidas (sin coordenadas)

        # Si la palabra está en una nueva línea (su coordenada 'top' es significativamente diferente)
        if abs(word['top'] - current_top) > 2: # Tolerancia vertical de 2px
            if current_line_words:
                reconstructed_lines.append(current_line_words)
            current_line_words = [word]
            current_top = word['top']
        else:
            current_line_words.append(word)
            
    # Añadir la última línea que estaba en proceso
    if current_line_words:
        reconstructed_lines.append(current_line_words)
        
    # Analizar cada línea reconstruida
    analyzed_lines = []
    for line_words in reconstructed_lines:
        # Ordenar palabras por su posición horizontal para asegurar el orden correcto
        line_words.sort(key=lambda w: w['x0'])
        line_text = " ".join([w['text'] for w in line_words])
        
        # Contar cuántas palabras tienen la fuente en negrita
        bold_words_count = sum(1 for w in line_words if w.get('fontname') == FONT_NAME_BOLD)
        
        # Considerar la línea en negrita si al menos la primera palabra lo es, o la mayoría
        is_line_bold = False
        if len(line_words) > 0:
            # Es negrita si la mayoría de las palabras son negrita
            if (bold_words_count / len(line_words)) > 0.5:
                is_line_bold = True
        
        analyzed_lines.append({'text': line_text, 'is_bold': is_line_bold})
        
    return analyzed_lines


try:
    with pdfplumber.open(BORRADOR_PDF_PATH) as pdf:
        # Iterar sobre el número de páginas especificado para el test
        for i, page in enumerate(pdf.pages):
            if i >= PAGES_TO_TEST:
                break
                
            print(f"\n--- Análisis Mejorado de la Página {i + 1} ---")
            
            # Usar el nuevo método basado en palabras
            analyzed_lines = process_page_by_words(page)
            
            for line_info in analyzed_lines:
                line_text = line_info['text'].strip()
                is_bold_flag = line_info['is_bold']
                
                # No imprimir líneas vacías
                if not line_text:
                    continue
                
                # Usar regex para identificar el patrón de encabezado de artículo
                match = re.search(r"^\s*(\d{1,3})\.\-\s*Artículo\s+([\d°ºA-Za-z\s]+)\.\-", line_text)
                
                if match:
                    if is_bold_flag:
                        print(f"[✔ ENCABEZADO EN NEGRITA] -> {line_text}")
                    else:
                        print(f"[❌ ENCABEZADO SIN NEGRITA] -> {line_text}")
                else:
                    # El resto del texto
                    if is_bold_flag:
                        print(f"[  Texto en Negrita  ] -> {line_text}")
                    else:
                        # Imprimir una porción del texto normal para ver el contexto
                        print(f"  (Texto normal) {line_text[:90]}...")

except FileNotFoundError:
    print(f"\n[ERROR] El archivo no fue encontrado en la ruta: '{BORRADOR_PDF_PATH}'")
except Exception as e:
    print(f"\n[ERROR] Ocurrió un error inesperado: {e}")

print("\n--- Test Finalizado ---")