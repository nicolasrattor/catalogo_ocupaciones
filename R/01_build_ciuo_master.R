# =============================================================================
# 01_build_ciuo_master.R
# Construye la tabla maestra de la taxonomía CIUO-08.CL (4 niveles) a partir
# de la estructura oficial publicada por el INE (Sección 5 del documento
# "CIUO 08.CL - Clasificador Chileno de Ocupaciones", INE, 2019):
# https://www.ine.gob.cl/docs/default-source/buenas-practicas/clasificaciones/ciuo/clasificador/ciuo-08-cl.pdf
#
# CIUO-08.CL es la adaptación nacional de CIUO-08 (OIT). Difiere del estándar
# internacional en varios puntos documentados por el INE (p. ej. paramédicos
# reclasificados del gran grupo 2 al 3, creación del subgrupo principal 36
# "Técnicos en educación", desagregaciones nacionales en salud como
# bioquímicos 2134 e ingenieros biomédicos 2147, etc.). Por eso se usa
# directamente el texto oficial chileno en vez de una fuente internacional.
# =============================================================================

library(dplyr)
library(stringr)
library(readr)

txt_path <- "data/reference/ciuo08cl_estructura_ine.txt"
lines <- read_lines(txt_path, locale = locale(encoding = "UTF-8")) |> str_trim()
lines <- lines[lines != ""]

# --- Detectar "GRAN GRUPO N" (encabezado) seguido del nombre en la línea sig.
gran_grupo_actual <- NA_character_
gran_grupo_nombre_actual <- NA_character_

registros <- list()

i <- 1
while (i <= length(lines)) {
  ln <- lines[i]

  if (str_detect(ln, "^GRAN GRUPO")) {
    gran_grupo_actual <- str_extract(ln, "[0-9]+$")
    gran_grupo_nombre_actual <- str_to_sentence(lines[i + 1]) |> str_remove("\\.$")
    i <- i + 2
    next
  }

  # Nivel 2: NN. Texto   (2 dígitos)
  m2 <- str_match(ln, "^([0-9]{2})\\.\\s*(.+?)\\.?$")
  # Nivel 3: NNN. Texto  (3 dígitos)
  m3 <- str_match(ln, "^([0-9]{3})\\.\\s*(.+?)\\.?$")
  # Nivel 4: NNNN. Texto (4 dígitos)
  m4 <- str_match(ln, "^([0-9]{4})\\.\\s*(.+?)\\.?$")

  if (!is.na(m4[1, 1])) {
    registros[[length(registros) + 1]] <- tibble(
      nivel = 4, codigo = m4[1, 2], nombre = m4[1, 3],
      gran_grupo = gran_grupo_actual, gran_grupo_nombre = gran_grupo_nombre_actual
    )
  } else if (!is.na(m3[1, 1])) {
    registros[[length(registros) + 1]] <- tibble(
      nivel = 3, codigo = m3[1, 2], nombre = m3[1, 3],
      gran_grupo = gran_grupo_actual, gran_grupo_nombre = gran_grupo_nombre_actual
    )
  } else if (!is.na(m2[1, 1])) {
    registros[[length(registros) + 1]] <- tibble(
      nivel = 2, codigo = m2[1, 2], nombre = m2[1, 3],
      gran_grupo = gran_grupo_actual, gran_grupo_nombre = gran_grupo_nombre_actual
    )
  }
  i <- i + 1
}

todos <- bind_rows(registros)

nivel2 <- todos |> filter(nivel == 2) |>
  transmute(subgrupo_principal = codigo, subgrupo_principal_nombre = str_squish(nombre))
nivel3 <- todos |> filter(nivel == 3) |>
  transmute(subgrupo = codigo, subgrupo_nombre = str_squish(nombre),
            subgrupo_principal = str_sub(codigo, 1, 2))
nivel4 <- todos |> filter(nivel == 4) |>
  transmute(codigo_ciuo08 = codigo, ocupacion_nombre = str_squish(nombre),
            subgrupo = str_sub(codigo, 1, 3),
            gran_grupo = gran_grupo, gran_grupo_nombre = gran_grupo_nombre)

# --- Ensamblar tabla maestra jerárquica -------------------------------------
ciuo_master <- nivel4 |>
  left_join(nivel3, by = "subgrupo") |>
  left_join(nivel2, by = "subgrupo_principal") |>
  mutate(codigo_ciuo08_num = as.numeric(codigo_ciuo08)) |>
  select(
    codigo_ciuo08, codigo_ciuo08_num, ocupacion_nombre,
    subgrupo, subgrupo_nombre,
    subgrupo_principal, subgrupo_principal_nombre,
    gran_grupo, gran_grupo_nombre
  ) |>
  arrange(codigo_ciuo08_num) |>
  distinct(codigo_ciuo08, .keep_all = TRUE)

stopifnot(nrow(ciuo_master) > 430)
stopifnot(!any(duplicated(ciuo_master$codigo_ciuo08)))

write_csv(ciuo_master, "data/reference/ciuo08_master.csv")

cat("Tabla maestra CIUO-08.CL construida:", nrow(ciuo_master), "ocupaciones (4 dígitos)\n")
cat("Gran grupos:", n_distinct(ciuo_master$gran_grupo), "\n")
cat("Subgrupos principales (2 dígitos):", n_distinct(ciuo_master$subgrupo_principal), "\n")
cat("Subgrupos (3 dígitos):", n_distinct(ciuo_master$subgrupo), "\n")
