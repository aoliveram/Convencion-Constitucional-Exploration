library(readr)
ordenamiento_1D_MCMC_01_15 <- read_csv("scripts - files/ordenamientos_pleno/ordenamiento_1D_MCMC_01-15.csv")
ordenamiento_1D_MCMC_01_15$nombre_votante
str(ordenamiento_1D_MCMC_01_15)
min_val <- min(ordenamiento_1D_MCMC_01_15$posicion_ideologica, na.rm = TRUE)
max_val <- max(ordenamiento_1D_MCMC_01_15$posicion_ideologica, na.rm = TRUE)

ordenamiento_1D_MCMC_01_15$posicion_ideologica <- 
  -(2 * (ordenamiento_1D_MCMC_01_15$posicion_ideologica - min_val) / (max_val - min_val) - 1)

ordenamiento_1D_MCMC_01_15$nombre_votante_form <- ordenamiento_1D_MCMC_01_15$nombre_votante

ordenamiento_1D_WNOM_01_15 <- read_csv("scripts - files/ordenamientos_pleno/ordenamiento_1D_WNOM_01-15.csv")
str(ordenamiento_1D_WNOM_01_15)
ordenamiento_1D_WNOM_01_15$nombre_votante

nombres_oficiales_wnom_full <- c(
  "Calvo, Carlos", "Uribe, Cesar", "Henriquez, Natalia", "Woldarsky, Manuel",
  "Gonzalez, Lidia", "Rivera, Pollyana", "Ampuero, Adriana", "Andrade, Cristobal",
  "Gonzalez, Dayana", "Vergara, Lisette", "San Juan, Constanza", "Harboe, Felipe",
  "Giustinianovich, Elisa", "Caiguan, Alexis", "Celedon, Roberto", "Llanquileo, Natividad",
  "Hurtado, Ruth", "Madriaga, Tania", "Mamani, Isabella", "Caamano, Francisco",
  "Zarate, Camila", "Grandon, Paola", "Vallejos, Loreto", "Sepulveda, Barbara",
  "Saldana, Alvin", "Tirado, Fernando", "Bacian, Wilfredo", "Portilla, Ericka",
  "Labbe, Bastian", "Alvarado, Gloria", "Rojas, Rodrigo", "Garin, Renato", "Quinteros, Maria Elisa",
  "Grandon, Giovanna", "Royo, Manuela", "Miranda, Valentina", "Flores, Alejandra",
  "Perez, Alejandra", "Barraza, Marcos", "Meneses, Janis", "Arauna, Francisca",
  "Nunez, Nicolas", "Vargas, Margarita", "Bravo, Daniel", "Videla, Carolina",
  "Antilef, Victorino", "Velasquez, Hernan", "Castillo, Eduardo", "Villena, Ingrid",
  "Arellano, Marco", "Olivares, Ivanna", "Vilches, Carolina", "Salinas, Fernando",
  "Jimenez, Luis", "Dorador, Cristina", "Linconao, Francisca", "Gomez, Yarela",
  "Labrana, Elsa", "Millabur, Adolfo", "Martinez, Helmuth", "Loncon, Elisa",
  "Vidal, Rossana", "Aguilera, Tiare", "Galleguillos, Felix", "Celis, Raul",
  "Delgado, Aurora", "Roa, Giovanna", "Achurra, Ignacio", "Schonhaut, Constanza",
  "Gomez, Claudio", "Abarca, Damaris", "Serey, Mariela", "Bassa, Jaime",
  "Alvez, Amaya", "Urrutia, Tatiana", "Atria, Fernando", "Gallardo, Bessy",
  "Pinto, Malucha", "Stingo, Daniel", "Oyarzun, Maria Jose", "Daza, Mauricio",
  "Cantuarias, Rocio", "Sanchez, Beatriz", "Mella, Jeniffer", "Martin, Juan Jose",
  "Viera, Christian", "Baradit, Jorge", "Cancino, Adriana", "Munoz, Pedro",
  "Orellana, Matias", "Alvarez, Julio", "Godoy, Isabel", "Fuchslocher, Javier",
  "Abarca, Jorge", "Vargas, Mario", "Baranda, Benito", "Valenzuela, Cesar",
  "Valenzuela, Paulina", "Namor, Guillermo", "Politzer, Patricia", "Montero, Ricardo",
  "Fernandez, Patricio", "Hurtado, Maximiliano", "Reyes, Maria Ramona", "Chahin, Fuad",
  "Laibe, Tomas", "Pustilnick, Tammy", "Castillo, Maria Trinidad", "Sepulveda, Carolina",
  "Dominguez, Gaspar", "Castro, Claudia", "Barcelo, Luis", "Botto, Miguel Angel",
  "Logan, Rodrigo", "Chinga, Eric", "Squella, Agustin", "Gutierrez, Hugo",
  "Cruz, Andres", "De la Maza, Bernardo", "Monckeberg, Cristian", "Navarrete, Geoconda",
  "Larrain, Hernan", "Ossandon, Manuel", "Cozzi, Ruggero", "Silva, Luciano",
  "Rebolledo, Barbara", "Fontaine, Bernardo", "Veloso, Paulina", "Jofre, Alvaro",
  "Tepper, Maria Angelica", "Labra, Patricia", "Moreno, Alfredo", "Cespedes, Lorena",
  "Hube, Constanza", "Vega, Roberto", "Rivera, Maria Magdalena", "Toloza, Pablo",
  "Mena, Felipe", "Catrileo, Rosa", "Hoppe, Vanessa", "Cretton, Eduardo",
  "Alvarez, Rodrigo", "Jurgensen, Harry", "Marinovic, Teresa", "Neumann, Ricardo",
  "Ubilla, Maria Cecilia", "Bown, Carol", "Cubillos, Marcela", "Arrau, Martin",
  "Arancibia, Jorge", "Zuniga, Luis Arturo", "Mayol, Luis", "Carrillo, Alondra",
  "Letelier, Margarita", "Montealegre, Katerine"
)

#ordenamiento_1D_WNOM_01_15$nombre_votante_form <- nombres_oficiales_wnom
ordenamiento_1D_WNOM_01_15$nombre_votante_form <- ordenamiento_1D_WNOM_01_15$nombre_votante

ordenamiento_1D_rcp <- read_csv("rcp_convencion/ordenamiento_1D_rcp.csv")
str(ordenamiento_1D_rcp)
ordenamiento_1D_rcp$nombre_votante

nombres_oficiales_rcp <- c(
  "Cantuarias, Rocio", "Marinovic, Teresa", "Montealegre, Katerine", "Arrau, Martin",
  "Cubillos, Marcela", "Hube, Constanza", "Letelier, Margarita", "Ubilla, Maria Cecilia",
  "Zuniga, Luis Arturo", "Jurgensen, Harry", "Mayol, Luis", "Bown, Carol",
  "Neumann, Ricardo", "Labra, Patricia", "Castro, Claudia", "Celis, Raul",
  "Alvarez, Rodrigo", "Mena, Felipe", "Arancibia, Jorge", "Toloza, Pablo",
  "Hurtado, Ruth", "Cretton, Eduardo", "Moreno, Alfredo", "Tepper, Maria Angelica",
  "Jofre, Alvaro", "Rivera, Pollyana", "Vega, Roberto", "Ossandon, Manuel",
  "Veloso, Paulina", "Fontaine, Bernardo", "Rebolledo, Barbara", "Silva, Luciano",
  "Cozzi, Ruggero", "Larrain, Hernan", "Navarrete, Geoconda", "Monckeberg, Cristian",
  "Cruz, Andres", "De la Maza, Bernardo", "Harboe, Felipe", "Squella, Agustin",
  "Chahin, Fuad", "Logan, Rodrigo", "Botto, Miguel Angel", "Castillo, Eduardo",
  "Laibe, Tomas", "Castillo, Maria Trinidad", "Barcelo, Luis", "Sepulveda, Carolina",
  "Hurtado, Maximiliano", "Dominguez, Gaspar", "Fernandez, Patricio", "Cespedes, Lorena",
  "Montero, Ricardo", "Pustilnick, Tammy", "Reyes, Maria Ramona", "Valenzuela, Cesar",
  "Alvarez, Julio", "Baranda, Benito", "Orellana, Matias", "Vargas, Mario",
  "Calvo, Carlos", "Munoz, Pedro", "Valenzuela, Paulina", "Politzer, Patricia",
  "Gomez, Claudio", "Fuchslocher, Javier", "Baradit, Jorge", "Martin, Juan Jose",
  "Abarca, Jorge", "Gallardo, Bessy", "Namor, Guillermo", "Cancino, Adriana",
  "Daza, Mauricio", "Pinto, Malucha", "Mella, Jeniffer", "Sanchez, Beatriz",
  "Oyarzun, Maria Jose", "Viera, Christian", "Celedon, Roberto", "Stingo, Daniel",
  "Abarca, Damaris", "Atria, Fernando", "Garin, Renato", "Aguilera, Tiare",
  "Vidal, Rossana", "Achurra, Ignacio", "Gomez, Yarela", "Loncon, Elisa",
  "Martinez, Helmuth", "Schonhaut, Constanza", "Urrutia, Tatiana", "Alvez, Amaya",
  "Labrana, Elsa", "Bassa, Jaime", "Delgado, Aurora", "Serey, Mariela",
  "Linconao, Francisca", "Vergara, Lisette", "Roa, Giovanna", "Nunez, Nicolas",
  "Jimenez, Luis", "Arellano, Marco", "Velasquez, Hernan", "Salinas, Fernando",
  "Olivares, Ivanna", "Gonzalez, Dayana", "Bravo, Daniel", "Videla, Carolina",
  "Villena, Ingrid", "Arauna, Francisca", "Millabur, Adolfo", "Barraza, Marcos",
  "Miranda, Valentina", "Gutierrez, Hugo", "Sepulveda, Barbara", "Portilla, Ericka",
  "Perez, Alejandra", "Rojas, Rodrigo", "Zarate, Camila", "Carrillo, Alondra",
  "Flores, Alejandra", "Quinteros, Maria Elisa", "Caamano, Francisco", "Vargas, Margarita",
  "Galleguillos, Felix", "Grandon, Paola", "Vallejos, Loreto", "Grandon, Giovanna",
  "Alvarado, Gloria", "Giustinianovich, Elisa", "Royo, Manuela", "Dorador, Cristina",
  "Labbe, Bastian", "Woldarsky, Manuel", "Bacian, Wilfredo", "Mamani, Isabella",
  "Meneses, Janis", "Tirado, Fernando", "Vilches, Carolina", "Madriaga, Tania",
  "Saldana, Alvin", "Henriquez, Natalia", "Godoy, Isabel", "Uribe, Cesar",
  "Caiguan, Alexis", "Andrade, Cristobal", "Catrileo, Rosa", "San Juan, Constanza",
  "Rivera, Maria Magdalena", "Antilef, Victorino", "Chinga, Eric", "Ampuero, Adriana",
  "Hoppe, Vanessa", "Llanquileo, Natividad", "Gonzalez, Lidia"
)

ordenamiento_1D_rcp$nombre_votante_form <- nombres_oficiales_rcp

# --- Paquetes
library(dplyr)
library(tidyr)
library(ggplot2)

# Normalizo a un formato largo y los uno
rcp_long <- ordenamiento_1D_rcp %>%
  transmute(nombre_votante_form, posicion_ideologica, fuente = "RCP")

wnom_long <- ordenamiento_1D_WNOM_01_15 %>%
  transmute(nombre_votante_form, posicion_ideologica, fuente = "WNOM")

# Para incluir MCMC:
mcmc_long <- ordenamiento_1D_MCMC_01_15 %>%
  transmute(nombre_votante_form, posicion_ideologica, fuente = "MCMC")

datos <- bind_rows(rcp_long, wnom_long, mcmc_long)

# --- Orden según RCP
niveles_y <- rcp_long %>%
  arrange(posicion_ideologica) %>%
  pull(nombre_votante_form)

datos <- datos %>%
  mutate(nombre_votante_form = factor(nombre_votante_form, levels = niveles_y))

# --- Gráfico
p <- ggplot(datos, aes(x = posicion_ideologica,
                       y = nombre_votante_form,
                       color = fuente)) +
  geom_point(size = 2, alpha = 0.9) +
  scale_color_manual(values = c("RCP" = "#D62728", "WNOM" = "#2CA02C", "MCMC" = "#1F77B4"
  )) +
  labs(title = "Comparación de Ordenamiento Ideológico",
       x = "Posición Ideológica",
       y = "Nombre del votante",
       color = "Fuente") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_line(linewidth = 0.2, colour = "grey90"),
        panel.grid.minor = element_blank())

show(p)

ggsave("comparacion_ordenamiento_ideologico_RCP_WNOM_2.pdf",
       plot = p, width = 10, height = 16, units = "in", device = "pdf")

# --- Comparación de posiciones ideológicas entre RCP y WNOM

df_dif <- ordenamiento_1D_rcp %>%
  transmute(nombre_votante_form,
            pos_rcp = posicion_ideologica) %>%
  inner_join(
    ordenamiento_1D_WNOM_01_15 %>%
      transmute(nombre_votante_form,
                pos_wnom = posicion_ideologica),
    by = "nombre_votante_form"
  ) %>%
  mutate(
    dif = pos_wnom - pos_rcp,     # diferencia real (WNOM − RCP)
    dif_abs = abs(dif)            # diferencia absoluta
  ) %>%
  arrange(desc(dif_abs))

# opcional: guardar a disco y ver las mayores discrepancias
write_csv(df_dif, "diferencias_WNOM_vs_RCP_2.csv")
head(df_dif, 15)


# ============== Analisis Robustez WNOM 155 vs 154 =============================

library(dplyr)
library(readr)
library(ggplot2)

# --- Ya tenemos ordenamiento_1D_rcp

rcp <- ordenamiento_1D_rcp
rcp$pos_rcp <- rcp$posicion_ideologica

# --- Cargar WNOM full y drop (asegúrate de que lleven nombre_votante_form imputado también)

wnom_full <- read_csv("scripts - files/ordenamientos_pleno/ordenamiento_1D_WNOM_01_15_155.csv")

wnom_full$nombre_votante_form <- nombres_oficiales_wnom_full
wnom_full$pos_wnom_full <- wnom_full$posicion_ideologica

wnom_drop <- read_csv("scripts - files/ordenamientos_pleno/ordenamiento_1D_WNOM_01_15_154.csv")

wnom_drop$nombre_votante_form <- nombres_oficiales_wnom
wnom_drop$pos_wnom_drop <- wnom_drop$posicion_ideologica

# --- Unir todo
cmp <- rcp %>%
  inner_join(wnom_full, by = "nombre_votante_form") %>%
  inner_join(wnom_drop, by = "nombre_votante_form") %>%
  mutate(
    dif_full  = pos_wnom_full - pos_rcp,
    dif_drop  = pos_wnom_drop - pos_rcp,
    dif_abs_full = abs(dif_full),
    dif_abs_drop = abs(dif_drop),
    dif_between  = pos_wnom_drop - pos_wnom_full,
    dif_abs_between = abs(dif_between)
  )

# --- Métricas útiles
tau <- 0.75  # umbral de “desalineación"
summ <- list(
  n = nrow(cmp),
  mae_full = mean(cmp$dif_abs_full),
  mae_drop = mean(cmp$dif_abs_drop),
  rmse_full = sqrt(mean((cmp$dif_full)^2)),
  rmse_drop = sqrt(mean((cmp$dif_drop)^2)),
  spearman_full = suppressWarnings(cor(cmp$pos_wnom_full, cmp$pos_rcp, method = "spearman")),
  spearman_drop = suppressWarnings(cor(cmp$pos_wnom_drop, cmp$pos_rcp, method = "spearman")),
  count_big_full = sum(cmp$dif_abs_full > tau),
  count_big_drop = sum(cmp$dif_abs_drop > tau)
)
summ

# --- ¿Se concentra el error en los que están “después de Rojas”?
# Para esto necesitamos el ORDEN DE NOMBRES usado en el run FULL (con todos).
# Carga ese mismo orden (tal como entra a wnominate) en un vector `votantes_full`.
# Si no lo tienes guardado, puedes reconstruirlo desde el CSV si trae la columna original de nombres; 
# aquí suponemos que ya tienes votantes_full en R (mismo orden que se pasó a wnominate).

# EJEMPLO (ajusta al nombre real del vector si difiere):
# votantes_full <- readRDS("scripts - files/01_votantes.rds")  # con Rojas incluido, en el orden del run FULL

# Normaliza a nombre_votante_form (ya lo hiciste antes; aquí asumimos que coincide 1:1)
# Para marcar a los "posteriores a Rojas":
idx_rojas <- which(nombres_oficiales_wnom_full == "Rojas, Rodrigo")
cmp <- cmp %>%
  mutate(is_after_rojas = nombre_votante_form %in% nombres_oficiales_wnom_full[(idx_rojas+1):length(nombres_oficiales_wnom_full)])

# Resumen por bloque
by_block <- cmp %>%
  group_by(is_after_rojas) %>%
  summarise(
    n = n(),
    mae_full = mean(dif_abs_full),
    mae_drop = mean(dif_abs_drop),
    big_full = sum(dif_abs_full > tau),
    big_drop = sum(dif_abs_drop > tau),
    .groups = "drop"
  )
by_block

# --- Comparación directa entre WNOM drop y full (cambios por sacar a Rojas)
top_between <- cmp %>% arrange(desc(dif_abs_between)) %>%
  select(nombre_votante_form, pos_wnom_full, pos_wnom_drop, dif_between, dif_abs_between) %>%
  head(15)
top_between
#write_csv(top_between, "top15_cambios_drop_vs_full.csv")
