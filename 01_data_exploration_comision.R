#install.packages('dplyr')
#install.packages('readxl')
#install.packages('tidyr')
#install.packages('ggplot2')

# Load necessary libraries
library(readxl)
library(dplyr)

# Read the Excel file
data <- read_excel('Comisión/25_sistema_politico.xls', col_names = FALSE)
#data <- read_excel('Pleno/sesion_103.xls', col_names = FALSE)

# Identify rows where the first column contains "VOTACIONID"
start_indices <- which(data[[1]] == "VOTACIONID")

# Create a list to store blocks of data
blocks <- list()

# Loop through the start indices to separate the blocks
for (i in seq_along(start_indices)) {
  start_idx <- start_indices[i]
  end_idx <- ifelse(i < length(start_indices), start_indices[i + 1] - 1, nrow(data))
  
  # Extract the block and store it in the list
  block <- data[start_idx:end_idx, ]
  blocks[[i]] <- block
}

# Check the number of blocks created
length(blocks)

# Optional:
str(blocks)
str(blocks[[1]])


#------------------------------------------------------------------------------

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Initialize an empty list to store processed data
votaciones_data <- list()

# Iterate over each block in the 'blocks' object
for (i in seq_along(blocks)) {
  block <- blocks[[i]]
  
  # Extract VOTACIONID and FECHA from the block
  votacion_id <- block[2, 1][[1]]  # VOTACIONID is in row 2, column 1
  fecha_raw <- block[2, 4][[1]]    # FECHA is in row 2, column 4
  
  # Extract only the date portion from FECHA
  #fecha <- as.Date(substr(fecha_raw, 1, 10), format = "%d-%m-%Y")
  fecha <- format(as.Date(substr(fecha_raw, 1, 10), format = "%d-%m-%Y"), "%m-%d")
  
  # Extract voting data starting from row 4 onwards
  voting_data <- block[-c(1:3), ]  # Remove metadata and header rows
  
  # Assign proper column names based on the structure
  colnames(voting_data) <- c("NA1","NOMBRE", "VOTACION", "NA2", "NA3", "NA4", "NA5", "NA6")
  
  # Select relevant columns: NOMBRE and VOTACION
  temp_data <- voting_data %>%
    select(NOMBRE, VOTACION) %>%
    mutate(
      VOTACIONID = votacion_id,
      FECHA = fecha,
      VOTO_NUM = case_when(
        VOTACION == "A FAVOR" ~ 1,
        VOTACION == "ABSTENCION" ~ 0,
        VOTACION == "EN CONTRA" ~ -1,
        VOTACION == "NO VOTA" ~ 9,
        TRUE ~ NA_real_  # Handle unexpected values
      )
    )
  
  # Append to the list
  votaciones_data[[i]] <- temp_data
}

# Combine all blocks into a single data frame
votaciones_df <- do.call(rbind, votaciones_data)
str(votaciones_df)


# Add a placeholder year (e.g., "2022") to the FECHA column
votaciones_df <- votaciones_df %>%
  mutate(FECHA = as.Date(paste0("2022-", FECHA), format = "%Y-%m-%d"))

# Filter rows with dates up to "03-08" (March 8)
#votaciones_filtered <- votaciones_df %>% filter(FECHA <= as.Date("2022-03-08"))
votaciones_filtered <- votaciones_df %>% filter(as.Date("2022-03-08") < FECHA)

# Check the filtered data
str(votaciones_filtered)

# Create a heatmap plot
ggplot(votaciones_filtered, aes(x = factor(VOTACIONID), y = NOMBRE, fill = factor(VOTO_NUM))) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("-1" = "red", "0" = "yellow", "1" = "green", "9" = "gray"),
    name = "Votación",
    labels = c("En Contra (-1)", "Abstención (0)", "A Favor (1)", "No Vota (9)")
  ) +
  labs(
    title = "Heatmap de Votaciones por Individuo",
    x = "VOTACIONID",
    y = "Nombre"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7)
  ) +
  facet_wrap(~ FECHA, scales = "free_x", nrow = 1) # Group by date on x-axis


#
unique(votaciones_df$NOMBRE)
