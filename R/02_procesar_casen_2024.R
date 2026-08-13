# =============================================================================
# 02_procesar_casen_2024.R
# Carga CASEN 2024 (ya filtrada a CSV), limpia códigos perdidos (-88/-99/-66),
# une con la tabla maestra CIUO-08 y construye:
#   1) una base "persona-ocupado" lista para análisis (RDS)
#   2) una tabla agregada por ocupación (4 dígitos) con indicadores de
#      mercado laboral, ponderada por el factor de expansión (expr)
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# --- Códigos de "missing" estándar de CASEN ---------------------------------
# -88 = no sabe / no aplica ; -99 = no responde ; -66 = no aplica (variantes)
CODIGOS_NA <- c(-88, -99, -66)

reemplazar_na_casen <- function(x) {
  x[x %in% CODIGOS_NA] <- NA
  x
}

# --- 1. Cargar datos ---------------------------------------------------------
casen_raw <- read_csv(
  "data/processed/casen_2024_seleccion.csv",
  guess_max = 220000,
  show_col_types = FALSE
)

ciuo_master <- read_csv("data/reference/ciuo08_master.csv", show_col_types = FALSE)

# --- 2. Limpieza --------------------------------------------------------------
vars_numericas <- c(
  "oficio1_08", "oficio4_08", "rama1", "rama4", "activ", "asalariado",
  "contrato", "cotiza", "depen_fun", "o10", "o15", "o21", "o22",
  "ytotcor", "ypchtotcor", "ytrabajocor", "pobreza", "pobreza_multi",
  "esc", "edad", "sexo", "region"
)

casen <- casen_raw |>
  mutate(across(all_of(vars_numericas), reemplazar_na_casen)) |>
  mutate(
    # código CIUO-08.CL a 4 dígitos, como texto con padding (ej: 110 -> "0110")
    codigo_ciuo08 = if_else(
      !is.na(oficio4_08),
      str_pad(as.character(as.integer(oficio4_08)), width = 4, pad = "0"),
      NA_character_
    ),
    ocupado = activ == 1,
    sexo_lbl = case_when(sexo == 1 ~ "Hombre", sexo == 2 ~ "Mujer", TRUE ~ NA_character_),
    categoria_ocupacional = case_when(
      o21 == 1 ~ "Patrón/empleador",
      o21 == 2 ~ "Trabajador por cuenta propia",
      o21 == 3 ~ "Asalariado/empleado",
      TRUE ~ NA_character_
    ),
    formal = case_when(
      cotiza == 1 ~ "Cotiza (formal)",
      cotiza == 0 ~ "No cotiza (informal)",
      TRUE ~ NA_character_
    ),
    horas_semanales = o10,
    region = as.integer(region)
  )

# --- 3. Base persona-ocupado (para exploración y futuras uniones) -----------
casen_ocupados <- casen |>
  filter(ocupado, !is.na(codigo_ciuo08)) |>
  left_join(ciuo_master, by = "codigo_ciuo08")

n_sin_match <- casen_ocupados |> filter(is.na(gran_grupo)) |> nrow()
cat("Ocupados sin match en tabla CIUO-08:", n_sin_match, "de", nrow(casen_ocupados), "\n")

saveRDS(casen_ocupados, "data/processed/casen_2024_ocupados.rds")

# --- 4. Agregación ponderada por ocupación (4 dígitos) ----------------------
media_ponderada <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

agregado_ocupacion <- casen_ocupados |>
  group_by(codigo_ciuo08, ocupacion_nombre, subgrupo_principal, subgrupo_principal_nombre,
            subgrupo, subgrupo_nombre, gran_grupo, gran_grupo_nombre) |>
  summarise(
    n_muestra = n(),
    personas_estimadas = sum(expr, na.rm = TRUE),
    ingreso_trabajo_prom = media_ponderada(ytrabajocor, expr),
    ingreso_trabajo_mediana = median(rep(ytrabajocor[!is.na(ytrabajocor)],
                                          pmax(1, round(expr[!is.na(ytrabajocor)] / 1000)))),
    horas_semanales_prom = media_ponderada(horas_semanales, expr),
    tasa_formalidad = media_ponderada(cotiza, expr),
    pct_asalariados = media_ponderada(o21 == 3, expr),
    pct_cuenta_propia = media_ponderada(o21 == 2, expr),
    pct_empleadores = media_ponderada(o21 == 1, expr),
    pct_mujeres = media_ponderada(sexo == 2, expr),
    edad_prom = media_ponderada(edad, expr),
    escolaridad_prom = media_ponderada(esc, expr),
    .groups = "drop"
  ) |>
  # Resguardo estadístico: ocultar/advertir estimaciones con muestra muy chica
  mutate(muestra_chica = n_muestra < 30) |>
  arrange(desc(personas_estimadas))

write_csv(agregado_ocupacion, "data/processed/casen_2024_agregado_ocupacion.csv")

# --- 5. Agregación a nivel de gran grupo (1 dígito), para vistas generales -
agregado_gran_grupo <- casen_ocupados |>
  group_by(gran_grupo, gran_grupo_nombre) |>
  summarise(
    n_muestra = n(),
    personas_estimadas = sum(expr, na.rm = TRUE),
    ingreso_trabajo_prom = media_ponderada(ytrabajocor, expr),
    horas_semanales_prom = media_ponderada(horas_semanales, expr),
    tasa_formalidad = media_ponderada(cotiza, expr),
    pct_mujeres = media_ponderada(sexo == 2, expr),
    .groups = "drop"
  ) |>
  arrange(desc(personas_estimadas))

write_csv(agregado_gran_grupo, "data/processed/casen_2024_agregado_gran_grupo.csv")

cat("\nListo.\n")
cat(" - Ocupaciones (4 dígitos) con datos:", nrow(agregado_ocupacion), "\n")
cat(" - Personas ocupadas estimadas (total país):", format(sum(agregado_ocupacion$personas_estimadas), big.mark = "."), "\n")
cat(" - Ocupaciones con muestra < 30 casos (usar con cautela):", sum(agregado_ocupacion$muestra_chica), "\n")
