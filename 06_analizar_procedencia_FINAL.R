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