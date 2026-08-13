# =============================================================================
# 04_procesar_piaac.R
# Procesa la Evaluación de Competencias de Adultos (PIAAC), Ciclo 2, Chile,
# y construye agregados de habilidades (numeracidad, literacidad, uso de
# habilidades en el trabajo) por ocupación, a nivel de:
#   - Gran grupo CIUO-08 (1 dígito, 10 categorías) — el más confiable
#   - Subgrupo principal CIUO-08 (2 dígitos, ~42 categorías observadas)
# El nivel de 4 dígitos (grupo primario) NO se usa: la mediana de casos por
# código a 4 dígitos es de solo 5 personas en la muestra chilena de PIAAC,
# insuficiente para estimaciones confiables.
#
# METODOLOGÍA — VALORES PLAUSIBLES (estándar PIAAC/PISA)
# ---------------------------------------------------------------------------
# La numeracidad y la literacidad no se miden con un solo puntaje por
# persona, sino con 10 "valores plausibles" (PVLIT1-10, PVNUM1-10) que
# representan la incertidumbre de la medición. El procedimiento estándar
# para estimar un promedio de grupo es: calcular el promedio ponderado
# (por el ponderador muestral SPFWT0) del grupo usando CADA uno de los 10
# valores plausibles por separado, y luego promediar esos 10 resultados.
# Este script sigue exactamente ese procedimiento para el punto estimado.
# (No se calculan errores estándar completos vía los 80 ponderadores de
# réplica SPFWT1-80 — ver nota en la ficha metodológica del dashboard.)
#
# USO DE HABILIDADES EN EL TRABAJO
# ---------------------------------------------------------------------------
# A diferencia de literacidad/numeracidad (habilidad medida con una prueba),
# las escalas NUMWORKC2, READWORKC2_T1, WRITWORKC2 e ICTWORKC2 miden la
# FRECUENCIA/COMPLEJIDAD con que la persona usa esas habilidades en su
# trabajo (autorreporte), en una escala WLE de PIAAC (no acotada 0-100,
# valores típicos entre 0 y 5 en la muestra chilena). Se promedian
# directamente (ponderadas por SPFWT0), sin metodología de valores
# plausibles ya que no son constructos con esa estructura.
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

piaac <- read_csv("data/processed/piaac_chile_seleccion.csv", show_col_types = FALSE)
ciuo_master <- read_csv("data/reference/ciuo08_master.csv", show_col_types = FALSE)

# --- Códigos de "no aplica / no ocupado" en ISCO08_C ------------------------
CODIGOS_NO_OCUPADO <- c("9996", "9997", "9998", "9999", ".", "")

piaac_ocupados <- piaac |>
  filter(!ISCO08_C %in% CODIGOS_NO_OCUPADO, !is.na(ISCO08_C)) |>
  mutate(
    gran_grupo = as.character(ISCO1C),
    subgrupo_principal = str_pad(as.character(ISCO2C), 2, pad = "0"),
    across(c(NUMWORKC2, READWORKC2_T1, WRITWORKC2, ICTWORKC2), ~ suppressWarnings(as.numeric(.x)))
  ) |>
  filter(!gran_grupo %in% c("995", "0"))  # excluir FF.AA. (muestra ínfima, n=2) y códigos residuales

media_ponderada <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

# --- Promedio sobre los 10 valores plausibles (metodología PIAAC/PISA) -----
promedio_pv <- function(df, prefijo, w) {
  cols <- paste0(prefijo, 1:10)
  estimaciones <- sapply(cols, function(col) media_ponderada(df[[col]], df[[w]]))
  mean(estimaciones, na.rm = TRUE)
}

agregar_piaac <- function(datos, var_grupo) {
  datos |>
    group_by(grupo = .data[[var_grupo]]) |>
    group_modify(~ {
      tibble(
        n_muestra = nrow(.x),
        numeracidad = promedio_pv(.x, "PVNUM", "SPFWT0"),
        literacidad = promedio_pv(.x, "PVLIT", "SPFWT0"),
        uso_numeracidad_trabajo = media_ponderada(.x$NUMWORKC2, .x$SPFWT0),
        uso_lectura_trabajo = media_ponderada(.x$READWORKC2_T1, .x$SPFWT0),
        uso_escritura_trabajo = media_ponderada(.x$WRITWORKC2, .x$SPFWT0),
        uso_tic_trabajo = media_ponderada(.x$ICTWORKC2, .x$SPFWT0)
      )
    }) |>
    ungroup()
}

# --- Agregado por gran grupo (1 dígito) -------------------------------------
gran_grupo_nombres <- ciuo_master |> distinct(gran_grupo, gran_grupo_nombre) |>
  mutate(gran_grupo = as.character(gran_grupo))

piaac_gran_grupo <- agregar_piaac(piaac_ocupados, "gran_grupo") |>
  rename(gran_grupo = grupo) |>
  left_join(gran_grupo_nombres, by = "gran_grupo") |>
  arrange(desc(n_muestra))

# --- Agregado por subgrupo principal (2 dígitos) ----------------------------
subgrupo_principal_nombres <- ciuo_master |> distinct(subgrupo_principal, subgrupo_principal_nombre, gran_grupo, gran_grupo_nombre) |>
  mutate(gran_grupo = as.character(gran_grupo))

piaac_subgrupo_principal <- agregar_piaac(piaac_ocupados, "subgrupo_principal") |>
  rename(subgrupo_principal = grupo) |>
  left_join(subgrupo_principal_nombres, by = "subgrupo_principal") |>
  arrange(desc(n_muestra))

write_csv(piaac_gran_grupo, "data/processed/piaac_gran_grupo.csv")
write_csv(piaac_subgrupo_principal, "data/processed/piaac_subgrupo_principal.csv")

cat("PIAAC procesado.\n")
cat("Personas ocupadas con ISCO válido:", nrow(piaac_ocupados), "de", nrow(piaac), "\n")
cat("Gran grupos:", nrow(piaac_gran_grupo), "\n")
cat("Subgrupos principales:", nrow(piaac_subgrupo_principal), "\n")
cat("Subgrupos principales con muestra < 15:", sum(piaac_subgrupo_principal$n_muestra < 15), "\n")
