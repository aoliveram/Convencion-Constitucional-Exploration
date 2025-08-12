library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# --- 1. Cargar los Datos ---

# Cargar el JSON final con el análisis de procedencia
ANALISIS_JSON_PATH <- "scripts - files/analisis_procedencia_oracion-patrocinante.json"
if (!file.exists(ANALISIS_JSON_PATH)) {
  stop("El archivo JSON no fue encontrado. Asegúrate de que esté en el directorio correcto.")
}
analisis_df <- fromJSON(ANALISIS_JSON_PATH, flatten = TRUE) %>% as_tibble()

# Objeto con las coaliciones de los convencionales (copiado de tu input)
coaliciones_convencionales <- tibble::tibble(nombre = c(
  "Antilef, Victorino", 
  "Bacian, Wilfredo", "Caiguan, Alexis", "Chinga, Eric", "Godoy, Isabel", "Linconao, Francisca",
  "Llanquileo, Natividad", "Vargas, Margarita", "Vergara, Lisette", "Arellano, Marco", 
  "Madriaga, Tania", "Perez, Alejandra", "Woldarsky, Manuel", "Grandon, Giovanna", 
  "Labrana, Elsa", "Catrileo, Rosa", "Loncon, Elisa", "Millabur, Adolfo", "Mamani, Isabella",
  "Jimenez, Luis", "Galleguillos, Felix", "Tirado, Fernando", "Aguilera, Tiare", 
  "Gonzalez, Lidia", "Mella, Jeniffer", "Serey, Mariela", "Oyarzun, Maria Jose", 
  "Bassa, Jaime", "Urrutia, Tatiana", "Stingo, Daniel", "Roa, Giovanna", "Atria, Fernando",
  "Schonhaut, Constanza", "Sanchez, Beatriz", "Achurra, Ignacio", "Abarca, Damaris", 
  "Viera, Christian", "Alvez, Amaya", "Delgado, Aurora", "Gomez, Yarela", "Hurtado, Maximiliano",
  "Calvo, Carlos", "Castillo, Maria Trinidad", "Gomez, Claudio", "Valenzuela, Cesar",
  "Baradit, Jorge", "Fernandez, Patricio", "Pinto, Malucha", "Orellana, Matias", 
  "Cancino, Adriana", "Montero, Ricardo", "Cruz, Andres", "Reyes, Maria Ramona", 
  "Munoz, Pedro", "Vargas, Mario", "Alvarez, Julio", "Laibe, Tomas", "Flores, Alejandra", 
  "Dorador, Cristina", "Meneses, Janis", "Vilches, Carolina", "Carrillo, Alondra", 
  "Saldana, Alvin", "Alvarado, Gloria", "Quinteros, Maria Elisa", "Labbe, Bastian", 
  "Hoppe, Vanessa", "Royo, Manuela", "Giustinianovich, Elisa", "Gonzalez, Dayana", 
  "Olivares, Ivanna", "Bravo, Daniel", "Zarate, Camila", "Rivera, Maria Magdalena", 
  "Henriquez, Natalia", "Villena, Ingrid", "Caamano, Francisco", "Vallejos, Loreto", 
  "Salinas, Fernando", "Arauna, Francisca", "Uribe, Cesar", "Abarca, Jorge", "Namor, Guillermo",
  "Politzer, Patricia", "Baranda, Benito", "Martin, Juan Jose", "Valenzuela, Paulina", 
  "Sepulveda, Carolina", "Pustilnick, Tammy", "Fuchslocher, Javier", "Cespedes, Lorena", 
  "Martinez, Helmuth", "Dominguez, Gaspar", "Daza, Mauricio", "Videla, Carolina", 
  "Gutierrez, Hugo", "Portilla, Ericka", "Velasquez, Hernan", "Gallardo, Bessy", 
  "Miranda, Valentina", "Sepulveda, Barbara", "Barraza, Marcos", "Nunez, Nicolas", 
  "Grandon, Paola", "Celedon, Roberto", "Botto, Miguel Angel", "Squella, Agustin", 
  "Logan, Rodrigo", "Harboe, Felipe", "Barcelo, Luis", "Chahin, Fuad", "Castillo, Eduardo", 
  "Rivera, Pollyana", "Jofre, Alvaro", "Toloza, Pablo", "Vega, Roberto", "Cozzi, Ruggero", 
  "Celis, Raul", "Arancibia, Jorge", "De la Maza, Bernardo", "Zuniga, Luis Arturo", 
  "Monckeberg, Cristian", "Marinovic, Teresa", "Letelier, Margarita", "Larrain, Hernan", 
  "Fontaine, Bernardo", "Hube, Constanza", "Cubillos, Marcela", "Ossandon, Manuel", 
  "Castro, Claudia", "Bown, Carol", "Neumann, Ricardo", "Rebolledo, Barbara", 
  "Moreno, Alfredo", "Labra, Patricia", "Arrau, Martin", "Cantuarias, Rocio", 
  "Silva, Luciano", "Veloso, Paulina", "Hurtado, Ruth", "Cretton, Eduardo", 
  "Tepper, Maria Angelica", "Mayol, Luis", "Mena, Felipe", "Jurgensen, Harry", 
  "Ubilla, Maria Cecilia", "Montealegre, Katerine", "Navarrete, Geoconda", "Alvarez, Rodrigo",
  "San Juan, Constanza", "Andrade, Cristobal", "Garin, Renato", "Vidal, Rossana", 
  "Ampuero, Adriana"), 
  coalicion = c(rep("Coordinadora Constituyente Plurinacional y Popular", 15), 
       rep("Escaños Reservados", 9), rep("Frente Amplio", 16), 
       rep("Colectivo Socialista", 17), rep("Movimientos Sociales Constituyentes", 12), 
       rep("Pueblo Constituyente", 12), rep("Independientes No Neutrales", 13), 
       rep("Chile Digno", 11), rep("Colectivo del Apruebo", 7), 
       rep("Vamos por Chile", 37), rep("Sin Grupo", 5)))

# --- 2. Procesamiento y Conteo de Aportes ---

# Seleccionar 1st_coincidencia y desanidar la lista de firmantes.
aportes_por_convencional <- analisis_df %>%
  # Mantener solo las filas que tienen una primera coincidencia válida
  filter(!is.na(`1st_coincidencia.nombre_pdf`)) %>%
  # La columna de firmantes es una lista de listas, necesitamos desanidarla
  unnest(`1st_coincidencia.firmantes_matched`) %>%
  # Renombrar la columna de firmantes para claridad
  rename(convencional = `1st_coincidencia.firmantes_matched`) %>%
  # Contar cuántas veces aparece cada convencional
  count(convencional, name = "oraciones_aportadas")

# --- 3. Unir Datos ---

# Unir el conteo de aportes con la información de las coaliciones
datos_plot <- aportes_por_convencional %>%
  left_join(coaliciones_convencionales, by = c("convencional" = "nombre")) %>%
  # Filtrar por si algún convencional no se encontró en la lista de coaliciones
  filter(!is.na(coalicion)) %>%
  # Ordenar los datos para el gráfico: primero por coalición, luego por n° de aportes
  arrange(coalicion, desc(oraciones_aportadas)) %>%
  # Crear un factor 'convencional' para que ggplot respete este orden en el eje X
  mutate(convencional = factor(convencional, levels = convencional))

# Calcular un resumen de aportes por coalición
resumen_coalicion <- datos_plot %>%
  group_by(coalicion) %>%
  summarise(aportes_totales = sum(oraciones_aportadas)) %>%
  arrange(desc(aportes_totales))

print(resumen_coalicion)

# --- 4. Gráfico ---

plot_aportes <- ggplot(datos_plot, aes(x = convencional, y = oraciones_aportadas, fill = coalicion)) +
  geom_col(show.legend = FALSE) + # Barras de colores por coalición, sin leyenda de fill
  geom_text(aes(label = oraciones_aportadas), vjust = -0.4, size = 2.5, color = "black") + # Etiqueta con el número de aportes
  
  # Usar facet_grid para agrupar por coalición en el eje X
  # scales = "free_x" permite que cada faceta tenga su propio conjunto de etiquetas en el eje X
  # space = "free_x" permite que el ancho de cada faceta sea proporcional al número de convencionales
  facet_grid(. ~ coalicion, scales = "free_x", space = "free_x", switch = "x") +
  
  # Ajustar la apariencia del gráfico
  labs(
    title = "Número de Oraciones Aportadas al Borrador Constitucional por Convencional",
    subtitle = "Agrupado por Coalición. 'Aporte' definido como patrocinio de la iniciativa con la mejor coincidencia (Embeddings).",
    x = "Convencional Constituyente",
    y = "Número de Oraciones Aportadas"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # Rotar etiquetas del eje X para mejor legibilidad
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    # Mover las etiquetas de las facetas (nombres de coaliciones) a la parte inferior
    strip.placement = "outside",
    # Estilo de las etiquetas de las facetas
    strip.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold", size = 10),
    # Posicionar título y subtítulo
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    # Eliminar el espacio entre facetas
    panel.spacing.x = unit(0, "lines"),
    # Ajustar márgenes
    plot.margin = margin(10, 10, 10, 10)
  )

# Imprimir el gráfico
print(plot_aportes)

# ggsave("aportes_por_convencional.png", plot = plot_aportes, width = 16, height = 9, dpi = 300)