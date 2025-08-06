# Cargar la librería necesaria
library(dplyr)

# 1. Creamos el tibble original
coaliciones_convencionales <- tibble::tibble(nombre = c("Antilef, Victorino", 
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

# 2. Reclasificamos las coaliciones, asignando a De la Maza a "Sin Grupo"
coaliciones_actualizadas <- coaliciones_convencionales %>%
  mutate(coalicion = case_when(
    
    # Caso especial para Bernardo de la Maza
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
    
    # Para todos los demás casos, mantenemos el valor original
    TRUE ~ coalicion
  ))

# 3. Verificamos el nuevo conteo
print("Conteo de miembros por coalición en la tabla final actualizada:")
coaliciones_actualizadas %>%
  count(coalicion, sort = TRUE)
