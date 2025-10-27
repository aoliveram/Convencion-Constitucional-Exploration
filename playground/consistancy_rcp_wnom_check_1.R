# Paquetes
library(readxl)
library(dplyr)
library(stringi)
library(purrr)

# ---- Helpers
norm_name <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    gsub("\\s+", " ", ., perl = TRUE) %>%
    trimws()
}

map_vote <- function(x) {
  if (is.na(x)) return(NA_real_)
  v <- tolower(norm_name(as.character(x)))
  if (v %in% c("1","0")) return(as.numeric(v))
  # afirmativos
  if (v %in% c("aprueba","apruebo","a favor","afavor","si","sí","por la afirmativa") ||
      startsWith(v, "aprueba")) return(1)
  # negativos
  if (v %in% c("rechaza","rechazo","en contra","encontra","por la negativa","no") ||
      startsWith(v, "rechaza")) return(0)
  # no computables (abstención/pareo/ausente/inhabilitado/presente/etc.)
  if (v %in% c("abstencion","abstención","pareo","pareado","pareada","ausente",
               "presente","inhabilitado","inhabilitada","excusa","no vota","novota",
               "sin voto","s/v","se abstiene","s/ voto","sin votar","")) return(NA_real_)
  return(NA_real_)
}

pick_name_col <- function(df) {
  cand <- names(df)
  targets <- c("NOMBRE","Nombre","Convencional","Convencionales","candidato","Nombre completo")
  for (t in targets) {
    hit <- cand[tolower(norm_name(cand)) == tolower(norm_name(t))]
    if (length(hit) > 0) return(hit[1])
  }
  cand[1]
}

# ---- Cargar .xls (todas las hojas)
path_xls <- "data - pleno/sesion_7.xls"
sheets <- readxl::excel_sheets(path_xls)

sublist <- map(sheets, function(sh){
  df <- readxl::read_excel(path_xls, sheet = sh)
  df <- df %>% select(where(~!all(is.na(.x)))) %>% filter(if_any(everything(), ~ !is.na(.)))
  if (ncol(df) < 2) return(NULL)
  name_col <- pick_name_col(df)
  df <- df %>% filter(!is.na(.data[[name_col]])) %>%
    mutate(NOMBRE_RAW = as.character(.data[[name_col]])) %>%
    select(NOMBRE_RAW, everything(), -all_of(name_col))
  # renombrar columnas de voto con prefijo de hoja para evitar choques
  vote_cols <- setdiff(names(df), "NOMBRE_RAW")
  names(df)[names(df) != "NOMBRE_RAW"] <- paste0(sh, "__", vote_cols)
  df
})

sublist <- compact(sublist)
stopifnot(length(sublist) > 0)

# ---- Unir por nombre (outer join)
mat_raw <- reduce(sublist, full_join, by = "NOMBRE_RAW") %>%
  mutate(NOMBRE_RAW = trimws(NOMBRE_RAW),
         NOMBRE_NORM = norm_name(NOMBRE_RAW)) %>%
  arrange(NOMBRE_RAW)

# ---- Versión numérica
vote_cols_all <- setdiff(names(mat_raw), c("NOMBRE_RAW","NOMBRE_NORM"))
mat_num <- mat_raw
mat_num[vote_cols_all] <- lapply(mat_num[vote_cols_all], map_vote)

# ---- Guardar
readr::write_csv(mat_raw, "sesion7_votos_raw.csv")
readr::write_csv(mat_num, "sesion7_votos_numeric.csv")

# Objetos en memoria:
#   - mat_raw: texto tal cual
#   - mat_num: codificada (1/0/NA)