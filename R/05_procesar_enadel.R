# =============================================================================
# 05_procesar_enadel.R
# Procesa la Encuesta de Demanda Laboral (ENADEL) 2025 y construye agregados
# de demanda, dificultad de contratación y requisitos por ocupación CIUO-08.CL.
#
# ESTRUCTURA DE LOS DATOS
# ---------------------------------------------------------------------------
# Una fila por empresa (5.576), con hasta 7 "bloques" de cargo/ocupación con
# vacante (pregunta C1 del cuestionario: "los 7 principales cargos/ocupaciones
# en los que tuvo VACANTES, en los que logró contratar o no personal"). Cada
# bloque i (1 a 7) tiene: c1_Codigo_CIUO_i (ocupación, CIUO-08.CL 4 dígitos),
# c1_c_i (nivel educacional requerido, 0-7), c1_d_i (años de experiencia
# requeridos), c1_e_i (¿requiere licencia/certificación? 1=Sí/2=No),
# c1_f_i (¿logró contratar? 1=Sí/2=No), c1_h_i (cantidad de vacantes
# contratadas), y c1_g_dif1_i a c1_g_dif5_i (hasta 5 razones de dificultad,
# solo si c1_f_i == 2, es decir, no logró contratar).
#
# Este script reestructura los datos de ancho (1 fila = 1 empresa) a largo
# (1 fila = 1 empresa x bloque de ocupación), y agrega por código CIUO-08.CL,
# ponderando siempre por `pond_final` (ponderador muestral final de ENADEL).
#
# Fuente de las categorías: Cuestionario ENADEL 2025 (Tarjetas 4, 5 y 6).
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)

enadel_wide <- read_csv("data/processed/enadel_2025_seleccion.csv", show_col_types = FALSE)

CODIGOS_NA_ENADEL <- c(-88, -99)

# --- Reestructurar de ancho a largo (1 fila = empresa x bloque 1-7) --------
extraer_bloque <- function(wide, i) {
  col <- function(nombre) {
    nombre_completo <- paste0(nombre, "_", i)
    if (nombre_completo %in% names(wide)) wide[[nombre_completo]] else NA_real_
  }
  tibble(
    idempresa_ficticio = wide$idempresa_ficticio,
    pond_final = wide$pond_final,
    codigo_ciuo = col("c1_Codigo_CIUO"),
    nivel_educ = col("c1_c"),
    anios_exp = col("c1_d"),
    requiere_cert = col("c1_e"),
    contrato = col("c1_f"),
    vacantes_contratadas = col("c1_h"),
    dif1 = col("c1_g_dif1"),
    dif2 = col("c1_g_dif2"),
    dif3 = col("c1_g_dif3"),
    dif4 = col("c1_g_dif4"),
    dif5 = col("c1_g_dif5")
  )
}

enadel_long <- map_dfr(1:7, ~ extraer_bloque(enadel_wide, .x)) |>
  filter(!is.na(codigo_ciuo)) |>
  mutate(
    codigo_ciuo = str_pad(as.character(as.integer(codigo_ciuo)), 4, pad = "0"),
    nivel_educ = if_else(nivel_educ %in% 0:7, nivel_educ, NA_real_),
    anios_exp = if_else(!anios_exp %in% CODIGOS_NA_ENADEL & anios_exp >= 0, anios_exp, NA_real_),
    requiere_cert = if_else(requiere_cert %in% c(1, 2), requiere_cert, NA_real_),
    contrato = if_else(contrato %in% c(1, 2), contrato, NA_real_)
  )

# --- Pregunta C3: cargos/ocupaciones con vacantes PROYECTADAS para 2025 ----
# (cargo, tareas, nivel educacional y años de experiencia; a diferencia de
# C1, el cuestionario NO releva una cantidad de personas por cargo en C3 —
# se usa entonces el número ponderado de EMPRESAS que proyectan una vacante
# en esa ocupación, no un total de personas).
extraer_bloque_c3 <- function(wide, i) {
  col <- function(nombre) {
    nombre_completo <- paste0(nombre, "_", i)
    if (nombre_completo %in% names(wide)) wide[[nombre_completo]] else NA_real_
  }
  tibble(
    idempresa_ficticio = wide$idempresa_ficticio,
    pond_final = wide$pond_final,
    codigo_ciuo = col("c3_Codigo_CIUO")
  )
}

enadel_c3_long <- map_dfr(1:3, ~ extraer_bloque_c3(enadel_wide, .x)) |>
  filter(!is.na(codigo_ciuo)) |>
  mutate(codigo_ciuo = str_pad(as.character(as.integer(codigo_ciuo)), 4, pad = "0"))

enadel_c3_agg <- enadel_c3_long |>
  group_by(codigo_ciuo) |>
  summarise(
    n_empresas_proyectan = n(),
    peso_empresas_proyectan = sum(pond_final, na.rm = TRUE),
    .groups = "drop"
  )

# --- Etiquetas oficiales (Cuestionario ENADEL 2025) -------------------------
niveles_educacionales <- c(
  "0" = "Sin exigencia de educación formal",
  "1" = "Educación básica",
  "2" = "Educación media científico humanista",
  "3" = "Educación media técnico profesional",
  "4" = "Técnico superior",
  "5" = "Profesional",
  "6" = "Profesional con Magíster",
  "7" = "Profesional con Doctorado"
)

razones_dificultad <- c(
  "1" = "Candidatos sin competencias o habilidades técnicas requeridas",
  "2" = "Candidatos sin habilidades blandas o socioemocionales requeridas",
  "3" = "Candidatos sin nivel educacional requerido",
  "4" = "Candidatos sin licencias, certificaciones o requisitos legales",
  "5" = "Candidatos sin la experiencia laboral mínima requerida",
  "6" = "Remuneración ofrecida no aceptada",
  "7" = "Condiciones laborales (horario y/o jornada) no aceptadas",
  "8" = "Falta de postulantes",
  "9" = "Ubicación geográfica",
  "10" = "Falta de oportunidades de desarrollo profesional",
  "11" = "Falta de prestaciones de la empresa",
  "12" = "Otra dificultad"
)

media_ponderada <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

moda_ponderada <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_character_)
  tab <- tapply(w[ok], x[ok], sum)
  names(tab)[which.max(tab)]
}

# % ponderado que representa la categoría más frecuente (la moda) sobre el
# total de respuestas válidas para esa ocupación.
pct_moda_ponderada <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  tab <- tapply(w[ok], x[ok], sum)
  max(tab) / sum(tab)
}

# --- Agregado principal por ocupación ---------------------------------------
enadel_agg <- enadel_long |>
  group_by(codigo_ciuo) |>
  summarise(
    n_empresas_muestra = n(),
    peso_empresas = sum(pond_final, na.rm = TRUE),
    personas_contratadas = sum(pond_final * vacantes_contratadas * (contrato == 1), na.rm = TRUE),
    pct_sin_contratar = media_ponderada(contrato == 2, pond_final),
    nivel_educ_moda = moda_ponderada(nivel_educ, pond_final),
    nivel_educ_pct = pct_moda_ponderada(nivel_educ, pond_final),
    anios_exp_prom = media_ponderada(anios_exp, pond_final),
    pct_requiere_cert = media_ponderada(requiere_cert == 1, pond_final),
    .groups = "drop"
  ) |>
  mutate(nivel_educ_glosa = unname(niveles_educacionales[nivel_educ_moda]))

# --- Razón principal de dificultad (solo entre quienes NO lograron contratar) ---
dificultades_long <- enadel_long |>
  filter(contrato == 2) |>
  select(codigo_ciuo, pond_final, dif1, dif2, dif3, dif4, dif5) |>
  pivot_longer(cols = c(dif1, dif2, dif3, dif4, dif5), values_to = "razon") |>
  filter(!is.na(razon), as.character(razon) %in% names(razones_dificultad))

razon_principal <- dificultades_long |>
  group_by(codigo_ciuo, razon) |>
  summarise(peso = sum(pond_final, na.rm = TRUE), .groups = "drop") |>
  arrange(codigo_ciuo, desc(peso)) |>
  group_by(codigo_ciuo) |>
  slice_head(n = 1) |>
  ungroup() |>
  transmute(codigo_ciuo, razon_dificultad_glosa = unname(razones_dificultad[as.character(razon)]))

enadel_agg <- enadel_agg |>
  left_join(razon_principal, by = "codigo_ciuo") |>
  left_join(enadel_c3_agg, by = "codigo_ciuo")

write_csv(enadel_agg, "data/processed/enadel_agregado_ocupacion.csv")

# --- Cargos específicos que reportan las empresas (pregunta C3) ------------
# Texto libre que cada empresa escribe para el cargo/ocupación con vacante
# proyectada, agregado y ponderado por pond_final. Se guardan los cargos
# distintos por ocupación, ordenados de mayor a menor peso, para mostrarlos
# como texto (no como cifra) en la ficha de cada ocupación.
extraer_bloque_c3_cargo <- function(wide, i) {
  col <- function(nombre) {
    nombre_completo <- paste0(nombre, "_", i)
    if (nombre_completo %in% names(wide)) wide[[nombre_completo]] else NA
  }
  tibble(
    pond_final = wide$pond_final,
    codigo_ciuo = col("c3_Codigo_CIUO"),
    cargo = col("c3_Puesto_Trabajo")
  )
}

enadel_cargos_long <- map_dfr(1:3, ~ extraer_bloque_c3_cargo(enadel_wide, .x)) |>
  filter(!is.na(codigo_ciuo), !is.na(cargo), str_trim(cargo) != "") |>
  mutate(
    codigo_ciuo = str_pad(as.character(as.integer(codigo_ciuo)), 4, pad = "0"),
    cargo = str_trim(cargo)
  )

enadel_cargos_agg <- enadel_cargos_long |>
  group_by(codigo_ciuo, cargo) |>
  summarise(peso = sum(pond_final, na.rm = TRUE), n_empresas = n(), .groups = "drop") |>
  arrange(codigo_ciuo, desc(peso))

write_csv(enadel_cargos_agg, "data/processed/enadel_cargos_ocupacion.csv")

write_csv(enadel_long, "data/processed/enadel_long.csv")

cat("ENADEL 2025 procesado.\n")
cat("Bloques empresa x ocupación con código válido:", nrow(enadel_long), "\n")
cat("Ocupaciones (códigos CIUO-08.CL distintos):", nrow(enadel_agg), "\n")
cat("Ocupaciones con muestra < 5 empresas:", sum(enadel_agg$n_empresas_muestra < 5), "\n")
