# creates bump_chart_coaliciones.pdf"

library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(cowplot)
library(gridExtra)

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

# Reclasificar
coaliciones_convencionales <- coaliciones_convencionales %>%
  mutate(coalicion = case_when(
    # Bernardo de la Maza
    nombre == "De la Maza, Bernardo" ~ "Sin Grupo",
    # Bloque 1: Chile Libre (6 integrantes)
    nombre %in% c("Letelier, Margarita", "Marinovic, Teresa", "Arrau, Martin", 
                  "Cantuarias, Rocio", "Hurtado, Ruth", "Jurgensen, Harry") ~ "Chile Libre",
    # Bloque 2: Unidos por Chile (8 integrantes)
    nombre %in% c("Rivera, Pollyana", "Arancibia, Jorge", "Hube, Constanza", 
                  "Neumann, Ricardo", "Cretton, Eduardo", "Ubilla, Maria Cecilia", 
                  "Montealegre, Katerine", "Alvarez, Rodrigo") ~ "Unidos por Chile",
    # Bloque 3: Chile Unido (7 integrantes)
    nombre %in% c("Cubillos, Marcela", "Castro, Claudia", "Bown, Carol", 
                  "Moreno, Alfredo", "Zuniga, Luis Arturo", "Toloza, Pablo", 
                  "Mena, Felipe") ~ "Chile Unido",
    # Bloque 4: Independientes RN-Evópoli (ahora 15 integrantes)
    nombre %in% c("Rebolledo, Barbara", "Monckeberg, Cristian", "Tepper, Maria Angelica", 
                  "Mayol, Luis", "Navarrete, Geoconda", "Ossandon, Manuel", 
                  "Veloso, Paulina", "Silva, Luciano", "Labra, Patricia", "Larrain, Hernan", 
                  "Fontaine, Bernardo", "Vega, Roberto", "Jofre, Alvaro", "Cozzi, Ruggero", 
                  "Celis, Raul") ~ "Independientes RN-Evópoli",
    # El resto queda igual
    TRUE ~ coalicion
  ))

saveRDS(coaliciones_convencionales, "scripts - files/coaliciones_convencionales.rds")

# Cargar los datos de ordenamiento
ORDENAMIENTO_CSV_PATH <- "scripts - files/ordenamientos_pleno/ordenamiento_1D_WNOM_76-99.csv"
if (!file.exists(ORDENAMIENTO_CSV_PATH)) {
  stop(paste0("El archivo '", ORDENAMIENTO_CSV_PATH, "' no fue encontrado."))
}
ordenamiento_df <- readr::read_csv(ORDENAMIENTO_CSV_PATH)

nombre_map <- c(
  # Mapeo manual: "Nombre_en_ordenamiento_df" = "Nombre_estandarizado_en_coaliciones"
  "Gonz'alez Calder'on, Lidia" = "Gonzalez, Lidia",
  "Antilef ~Nanco, Victorino" = "Antilef, Victorino",
  "Godoy Monardez, Isabel" = "Godoy, Isabel",
  "Llanquileo Pilquim'an, Natividad" = "Llanquileo, Natividad",
  "Chinga Ferreira, Eric" = "Chinga, Eric",
  "Hoppe Espoz, Vanessa" = "Hoppe, Vanessa",
  "Henr'iquez Carre~no, Natalia" = "Henriquez, Natalia",
  "Catrileo Arias, Rosa" = "Catrileo, Rosa",
  "Salda~na Mu~noz, Alvin" = "Saldana, Alvin",
  "Rivera Iribarren, Mar'ia Magdalena" = "Rivera, Maria Magdalena",
  "Caiguan Ancapan, Alexis" = "Caiguan, Alexis",
  "San Juan Standen, Constanza" = "San Juan, Constanza",
  "Andrade Le'on, Crist'obal" = "Andrade, Cristobal",
  "Vilches Fuenzalida, Carolina" = "Vilches, Carolina",
  "Madriaga Flores, Tania" = "Madriaga, Tania",
  "Tirado Soto, Fernando" = "Tirado, Fernando",
  "Roa Cadin, Giovanna" = "Roa, Giovanna",
  "Labb'e Salazar, Basti'an" = "Labbe, Bastian",
  "Meneses Palma, Janis" = "Meneses, Janis",
  "Ampuero Barrientos, Adriana" = "Ampuero, Adriana",
  "Quinteros C'aceres, Mar'ia Elisa" = "Quinteros, Maria Elisa",
  "Mamani Mamani, Isabella" = "Mamani, Isabella",
  "Woldarsky Gonz'alez, Manuel" = "Woldarsky, Manuel",
  "Galleguillos Aymani, F'elix" = "Galleguillos, Felix",
  "Serey Jim'enez, Mariela" = "Serey, Mariela",
  "Carrillo Vidal, Alondra" = "Carrillo, Alondra",
  "Royo Letelier, Manuela" = "Royo, Manuela",
  "Bacian Delgado, Wilfredo" = "Bacian, Wilfredo",
  "Giustinianovich Campos, Elisa" = "Giustinianovich, Elisa",
  "Portilla Barrios, Ericka" = "Portilla, Ericka",
  "Dorador Ortiz, Cristina" = "Dorador, Cristina",
  "Uribe Araya, C'esar" = "Uribe, Cesar",
  "Grand'on Gonz'alez, Paola" = "Grandon, Paola",
  "Guti'errez G'alvez, Hugo" = "Gutierrez, Hugo",
  "Alvarado Jorquera, Gloria" = "Alvarado, Gloria",
  "Grand'on Caro, Giovanna" = "Grandon, Giovanna",
  "Bassa Mercado, Jaime" = "Bassa, Jaime",
  "Delgado Vergara, Aurora" = "Delgado, Aurora",
  "Alvez Mar'in, Amaya" = "Alvez, Amaya",
  "Barraza G'omez, Marcos" = "Barraza, Marcos",
  "Urrutia Herrera, Tatiana" = "Urrutia, Tatiana",
  "Miranda Arce, Valentina" = "Miranda, Valentina",
  "Vallejos D'avila, Loreto" = "Vallejos, Loreto",
  "Caama~no Rojas, Francisco" = "Caamano, Francisco",
  "Flores Carlos, Alejandra" = "Flores, Alejandra",
  "Vargas L'opez, Margarita" = "Vargas, Margarita",
  "Achurra D'iaz, Ignacio" = "Achurra, Ignacio",
  "Jim'enez C'aceres, Luis" = "Jimenez, Luis",
  "P'erez Espina, Alejandra" = "Perez, Alejandra",
  "Schonhaut Soto, Constanza" = "Schonhaut, Constanza",
  "Sep'ulveda Hales, B'arbara" = "Sepulveda, Barbara",
  "Millabur ~Nancuil, Adolfo" = "Millabur, Adolfo",
  "Z'arate Z'arate, Camila" = "Zarate, Camila",
  "Salinas Manfredini, Fernando" = "Salinas, Fernando",
  "G'omez S'anchez, Yarela" = "Gomez, Yarela",
  "Olivares Miranda, Ivanna" = "Olivares, Ivanna",
  "Bravo Silva, Daniel" = "Bravo, Daniel",
  "Atria Lemaitre, Fernando" = "Atria, Fernando",
  "Arauna Urrutia, Francisca" = "Arauna, Francisca",
  "Vel'asquez N'u~nez, Hern'an" = "Velasquez, Hernan",
  "Videla Osorio, Carolina" = "Videla, Carolina",
  "Gonz'alez Araya, Dayyana" = "Gonzalez, Dayana",
  "Villena Narbona, Ingrid" = "Villena, Ingrid",
  "Abarca Gonz'alez, Damaris" = "Abarca, Damaris",
  "N'u~nez Gangas, Nicol'as" = "Nunez, Nicolas",
  "Arellano Ortega, Marco" = "Arellano, Marco",
  "Viera 'Alvarez, Christian" = "Viera, Christian",
  "Stingo Camus, Daniel" = "Stingo, Daniel",
  "Labra~na Pino, Elsa" = "Labrana, Elsa",
  "Linconao Huircap'an, Francisca" = "Linconao, Francisca",
  "Vergara Riquelme, Lisette" = "Vergara, Lisette",
  "Oyarz'un Solis, Mar'ia Jos'e" = "Oyarzun, Maria Jose",
  "Mart'inez Llancapan, Helmuth" = "Martinez, Helmuth",
  "S'anchez Mu~noz, Beatriz" = "Sanchez, Beatriz",
  "Loncon Antileo, Elisa" = "Loncon, Elisa",
  "Vidal Hern'andez, Loreto" = "Vidal, Rossana", # !
  "Mella Escobar, Jeniffer" = "Mella, Jeniffer",
  "Aguilera Hey, Tiare" = "Aguilera, Tiare",
  "Celed'on Fern'andez, Roberto" = "Celedon, Roberto",
  "Gar'in Gonz'alez, Renato" = "Garin, Renato",
  "G'omez Castro, Claudio" = "Gomez, Claudio",
  "Baradit Morales, Jorge" = "Baradit, Jorge",
  "Cancino Meneses, Adriana" = "Cancino, Adriana",
  "Pinto Solari, Malucha" = "Pinto, Malucha",
  "Calvo Mu~noz, Carlos" = "Calvo, Carlos",
  "Mu~noz Leiva, Pedro" = "Munoz, Pedro",
  "Orellana Cuellar, Mat'ias" = "Orellana, Matias",
  "Daza Carrasco, Mauricio" = "Daza, Mauricio",
  "Abarca Riveros, Jorge" = "Abarca, Jorge",
  "Valenzuela Maass, C'esar" = "Valenzuela, Cesar",
  "Reyes Painequeo, Mar'ia Ramona" = "Reyes, Maria Ramona",
  "Montero Allende, Ricardo" = "Montero, Ricardo",
  "Alvarez Pinto, Julio" = "Alvarez, Julio",
  "Vargas Vidal, Mario" = "Vargas, Mario",
  "Martin Bravo, Juan Jos'e" = "Martin, Juan Jose",
  "Politzer Kerekes, Patricia" = "Politzer, Patricia",
  "Fuchslocher Baeza, Javier" = "Fuchslocher, Javier",
  "Valenzuela Rio, Paulina" = "Valenzuela, Paulina",
  "Namor Kong, Guillermo" = "Namor, Guillermo",
  "Baranda Ferr'an, Benito" = "Baranda, Benito",
  "C'espedes Fern'andez, Lorena" = "Cespedes, Lorena",
  "Gallardo Prado, Bessy" = "Gallardo, Bessy",
  "Castillo Boilet, Mar'ia Trinidad" = "Castillo, Maria Trinidad",
  "Hurtado Roco, Maximiliano" = "Hurtado, Maximiliano",
  "Laibe Saez, Tom'as" = "Laibe, Tomas",
  "Fern'andez Chadwick, Patricio" = "Fernandez, Patricio",
  "Pustilnick Arditi, Tammy" = "Pustilnick, Tammy",
  "Sep'ulveda Sep'ulveda, Carolina" = "Sepulveda, Carolina",
  "Dom'inguez Donoso, Gaspar" = "Dominguez, Gaspar",
  "Botto Salinas, Miguel 'Angel" = "Botto, Miguel Angel",
  "Castillo Vigouroux, Eduardo" = "Castillo, Eduardo",
  "Barcel'o Amado, Luis" = "Barcelo, Luis",
  "Chah'in Valenzuela, Fuad" = "Chahin, Fuad",
  "Squella Narducci, Agust'in" = "Squella, Agustin",
  "Harboe Bascu~n'an, Felipe" = "Harboe, Felipe",
  "Logan Soto, Rodrigo" = "Logan, Rodrigo",
  "Cruz Carrasco, Andr'es" = "Cruz, Andres",
  "De la Maza Ba~nados, Bernardo" = "De la Maza, Bernardo",
  "Monckeberg Bruner, Cristian" = "Monckeberg, Cristian",
  "Navarrete Arratia, Geoconda" = "Navarrete, Geoconda",
  "Larra'in Matte, Hern'an" = "Larrain, Hernan",
  "Cozzi Elzo, Ruggero" = "Cozzi, Ruggero",
  "Fontaine Talavera, Bernardo" = "Fontaine, Bernardo",
  "Veloso Mu~noz, Paulina" = "Veloso, Paulina",
  "Rebolledo Aguirre, B'arbara" = "Rebolledo, Barbara",
  "Ossand'on Lira, Manuel" = "Ossandon, Manuel",
  "Moreno Echeverr'ia, Alfredo" = "Moreno, Alfredo",
  "Rivera Bigas, Pollyana" = "Rivera, Pollyana",
  "Jofr'e C'aceres, Alvaro" = "Jofre, Alvaro",
  "Silva Mora, Luciano" = "Silva, Luciano",
  "Vega Campusano, Roberto" = "Vega, Roberto",
  "Labra Besserer, Patricia" = "Labra, Patricia",
  "Tepper Kolossa, Mar'ia Ang'elica" = "Tepper, Maria Angelica",
  "Toloza Fern'andez, Pablo" = "Toloza, Pablo",
  "Hurtado Olave, Ruth" = "Hurtado, Ruth",
  "Cretton Rebolledo, Eduardo" = "Cretton, Eduardo",
  "Mena Villar, Felipe" = "Mena, Felipe",
  "Castro Guti'errez, Claudia" = "Castro, Claudia",
  "Arancibia Reyes, Jorge" = "Arancibia, Jorge",
  "Z'u~niga Jory, Luis Arturo" = "Zuniga, Luis Arturo",
  "Alvarez Zenteno, Rodrigo" = "Alvarez, Rodrigo",
  "Celis Montt, Ra'ul" = "Celis, Raul",
  "Neumann Bertin, Ricardo" = "Neumann, Ricardo",
  "Jurgensen Caesar, Harry" = "Jurgensen, Harry",
  "Bown Sep'ulveda, Carol" = "Bown, Carol",
  "Ubilla P'erez, Mar'ia Cecilia" = "Ubilla, Maria Cecilia",
  "Letelier Cort'es, Margarita" = "Letelier, Margarita",
  "Mayol Bouchon, Luis" = "Mayol, Luis",
  "Arrau Garc'ia-Huidobro, Mart'in" = "Arrau, Martin",
  "Hube Portus, Constanza" = "Hube, Constanza",
  "Cantuarias Rubio, Roc'io" = "Cantuarias, Rocio",
  "Cubillos Sigall, Marcela" = "Cubillos, Marcela",
  "Montealegre Navarro, Katerine" = "Montealegre, Katerine",
  "Marinovic Vial, Teresa" = "Marinovic, Teresa"
)

# Cambiamos nombres a ordenamiento_df
ordenamiento_df <- ordenamiento_df %>%
  mutate(nombre_estandarizado = dplyr::recode(nombre_votante, !!!nombre_map))

# --- 2. Procesamiento y Conteo de Aportes ---

# Unir la información de coalición y posición ideológica
convencionales_full_info <- coaliciones_convencionales %>%
  left_join(ordenamiento_df %>% select(nombre_estandarizado, posicion_ideologica), 
            by = c("nombre" = "nombre_estandarizado"))

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

# posición promedio por coalición
posicion_coalicion <- convencionales_full_info %>%
  filter(!is.na(posicion_ideologica)) %>% # Ignorar los que no tienen datos
  group_by(coalicion) %>%
  summarise(posicion_promedio = mean(posicion_ideologica, na.rm = TRUE)) %>%
  arrange(posicion_promedio) # Ordenar (asumiendo negativo=izquierda)

posicion_coalicion

# Unimos el conteo de aportes con la información completa de los convencionales.
datos_plot <- aportes_por_convencional %>%
  left_join(convencionales_full_info, by = c("convencional" = "nombre")) %>%
  # crear un factor para 'coalicion' ORDENADO por la posición ideológica promedio.
  mutate(coalicion = factor(coalicion, levels = posicion_coalicion$coalicion)) %>%
  # ordenamos primero por el factor de coalición, luego por n° de aportes
  arrange(coalicion, desc(oraciones_aportadas)) %>%
  # Crear un factor 'convencional' para que ggplot respete este orden final
  mutate(convencional = factor(convencional, levels = unique(convencional)))

# --- 4. Plot (Ordenado por posición media) ---

plot_aportes_ordenado <- ggplot(datos_plot, aes(x = convencional, y = oraciones_aportadas, fill = coalicion)) +
  geom_col(show.legend = FALSE) + # Barras coloreadas por coalición, sin leyenda de color
  geom_text(aes(label = oraciones_aportadas), vjust = -0.4, size = 2.5, color = "black") +
  
  # Usar facet_grid para agrupar visualmente por coalición en el eje X
  facet_grid(. ~ coalicion, scales = "free_x", space = "free_x", switch = "x") +
  
  # apariencia del gráfico
  labs(
    title = "N° de Oraciones Aportadas al Borrador Constitucional por Convencional",
    subtitle = "Agrupado y Ordenado por Coalición",
    x = NULL,
    y = "N° de Oraciones Aportadas"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), # Rotar etiqueta
    strip.placement = "outside", # etiquetas a la parte inferior
    strip.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold", size = 10), # Estilo etiquetas
    plot.title = element_text(hjust = 0.5, face = "bold"), # título y subtítulo
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    panel.spacing.x = unit(0, "lines"), # Eliminar el espacio entre facetas
    plot.margin = margin(10, 10, 10, 10) # márgenes
  )

plot_aportes_ordenado

ggsave("scripts - plots/aportes_por_convencional.pdf", plot = plot_aportes_ordenado, width = 16, height = 9, dpi = 300)

# --- 5. Bump chart: posición ideológica promedio por coalición ---

# Cargar el ordenamiento por ventanas para todos los convencionales
ORDEN_T_PATH_RDS <- "scripts - files/03_orden_votantes_t.rds"
if (!file.exists(ORDEN_T_PATH_RDS)) {
  stop("No se encontró '03_orden_votantes_t.rds' en 'scripts - files/'.")
}
orden_t_df <- readRDS(ORDEN_T_PATH_RDS)

# Construir tabla larga con posiciones iniciales y finales (incluye 100-106 desde Periodo2)
orden_t_long <- dplyr::bind_rows(
  orden_t_df %>% dplyr::transmute(Votante, Periodo = Periodo1, posicion_ideologica = pos_ideol_inicial),
  orden_t_df %>% dplyr::transmute(Votante, Periodo = Periodo2, posicion_ideologica = pos_ideol_final)
)

# Quedarnos con una fila por (Votante, Periodo)
orden_t_slim <- orden_t_long %>%
  dplyr::group_by(Votante, Periodo) %>%
  dplyr::summarise(posicion_ideologica = mean(posicion_ideologica, na.rm = TRUE), .groups = "drop")

# Unir coaliciones a cada votante
orden_t_joined <- orden_t_slim %>%
  dplyr::left_join(coaliciones_convencionales, by = c("Votante" = "nombre"))

# Promedios por coalición y periodo
promedios_coal_periodo <- orden_t_joined %>%
  dplyr::filter(!is.na(coalicion)) %>%
  dplyr::group_by(coalicion, Periodo) %>%
  dplyr::summarise(
    posicion_media = mean(posicion_ideologica, na.rm = TRUE),
    sd_pos = stats::sd(posicion_ideologica, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  )

# Abreviaciones y paleta de colores por coalición
abbr_map <- c(
  "Coordinadora Constituyente Plurinacional y Popular" = "CCPP",
  "Escaños Reservados" = "Esc. Res.",
  "Frente Amplio" = "FA",
  "Colectivo Socialista" = "Col. Soc.",
  "Movimientos Sociales Constituyentes" = "Mov. SC",
  "Pueblo Constituyente" = "Pueblo. C.",
  "Independientes No Neutrales" = "INN",
  "Chile Digno" = "Chile Digno",
  "Colectivo del Apruebo" = "Col. A",
  "Sin Grupo" = "Sin. G.",
  "Chile Libre" = "Chile Libre",
  "Unidos por Chile" = "Un. x Ch.",
  "Chile Unido" = "Chile Unido",
  "Independientes RN-Evópoli" = "RN-Evo",
  "Vamos por Chile" = "V. x C."
)

cols_coal <- c(
  "CCPP"      = "#1f77b4",
  "Esc. Res." = "#ff7f0e",
  "FA"        = "#2ca02c",
  "Col. Soc." = "#d62728",
  "Mov. SC"   = "#9467bd",
  "Pueblo. C."= "#8c564b",
  "INN"       = "#e377c2",
  "Chile Digno" = "#7f7f7f",
  "Col. A"    = "#bcbd22",
  "Sin. G."   = "#17becf",
  "Chile Libre"= "#1b9e77",
  "Un. x Ch." = "#d95f02",
  "Chile Unido"= "#7570b3",
  "RN-Evo"    = "#e7298a",
  "V. x C."   = "#66a61e"
)

# Orden temporal de los bloques (usar sólo los presentes)
niveles_periodo <- c("01-15","16-21","22-37","56-75","76-99","100-106")
niveles_periodo <- niveles_periodo[niveles_periodo %in% promedios_coal_periodo$Periodo]

promedios_coal_periodo <- promedios_coal_periodo %>%
  dplyr::mutate(
    Periodo = factor(Periodo, levels = niveles_periodo),
    Periodo_num = as.numeric(Periodo),
    coal_abbr = dplyr::recode(coalicion, !!!abbr_map, .default = coalicion)
  ) %>%
  dplyr::arrange(Periodo)

RIBBON_SD_MULT <- 0.15  # puedes bajar a 0.10 si aún hay mucho solapamiento

# Bump chart de posición promedio por coalición con banda de desviación estándar (±k·DE)
bump_coaliciones <- ggplot(promedios_coal_periodo,
                           aes(x = Periodo_num, y = posicion_media, group = coal_abbr, color = coal_abbr)) +
  geom_ribbon(aes(ymin = posicion_media - RIBBON_SD_MULT * sd_pos,
                  ymax = posicion_media + RIBBON_SD_MULT * sd_pos,
                  fill = coal_abbr),
              alpha = 0.18, color = NA) +
  geom_line(size = 0.9) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq_along(niveles_periodo), labels = niveles_periodo) +
  scale_color_manual(values = cols_coal, guide = "none") +
  scale_fill_manual(values = cols_coal, guide = "none") +
  labs(
    title = "Evolución de la Posición Promedio por Coalición",
    subtitle = paste0("Curvas con banda ±", RIBBON_SD_MULT, "·SD"),
    x = NULL,
    y = "Posición ideológica promedio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 6))
  )

# Tabla/lista de valores finales (último periodo observado por coalición), usando abreviaciones y color
ultimos <- promedios_coal_periodo %>%
  dplyr::group_by(coal_abbr) %>%
  dplyr::slice_max(Periodo_num, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(posicion_media)) %>%
  dplyr::transmute(coal_abbr, posicion_media)

# Panel derecho como un ggplot con texto coloreado
etiquetas <- ultimos %>%
  dplyr::mutate(label = sprintf("%s    %.3f", coal_abbr, posicion_media))

tabla_derecha <- ggplot(etiquetas, aes(x = 0, y = reorder(coal_abbr, posicion_media))) +
  geom_text(aes(label = label, color = coal_abbr), hjust = 0, size = 3.2) +
  scale_color_manual(values = cols_coal, guide = "none") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))

# Combinar gráfico principal + tabla a la derecha
combinado <- cowplot::plot_grid(bump_coaliciones, tabla_derecha, ncol = 2, rel_widths = c(4, 1.4))



# Mostrar y guardar solo el combinado (sin diccionario)
show(combinado)

ggsave("scripts - plots/bump_chart_coaliciones.pdf", plot = combinado, width = 12, height = 10.2, dpi = 300)
