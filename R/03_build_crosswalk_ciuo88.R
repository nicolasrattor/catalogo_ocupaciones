# =============================================================================
# 03_build_crosswalk_ciuo88.R
# Construye una tabla de correspondencia CIUO-88 -> CIUO-08.CL, a partir de la
# tabla oficial de la OIT (data/reference/crosstable_isco08_isco88.csv, tomada
# del paquete R 'occupationcross' de Guidowe, que a su vez la construye desde
# el archivo oficial de la OIT: Correspondence_EN_ISCO_08_to_ISCO_88).
#
# METODOLOGÍA — REDISTRIBUCIÓN PROPORCIONAL PARA CÓDIGOS AMBIGUOS
# ---------------------------------------------------------------------------
# La correspondencia entre CIUO-88 y CIUO-08 NO es uno a uno: 356 de los 671
# pares de la tabla oficial están marcados como correspondencia PARCIAL, y
# además varios códigos CIUO-88 tienen MÚLTIPLES candidatos CIUO-08 igualmente
# válidos (ej. CIUO-88 "5220 Shop salespersons and demonstrators" tiene 6
# candidatos distintos en CIUO-08: supervisores, vendedores de tienda,
# demostradores, bomberos de gasolinera, comida rápida, y otros — ninguno
# marcado como más "correcto" que otro en la tabla de la OIT).
#
# La versión anterior de este script elegía siempre el candidato con el
# código numérico más bajo, lo que concentraba artificialmente el 100% de
# las personas de un código CIUO-88 ambiguo en un solo código CIUO-08 (ej.
# "Supervisores de locales comerciales" aparecía con ~496.000 personas en
# 2017, cuando en años con CIUO-08 nativo esa cifra ronda las 30-40.000 —
# un salto que no era una tendencia real, sino un artefacto de esta regla).
#
# Este script ahora conserva TODOS los candidatos por código CIUO-88 (no solo
# el primero). La asignación proporcional entre candidatos —calibrada contra
# la distribución observada en años con CIUO-08 nativo (2020/2022/2024)— se
# hace en 02_procesar_casen_multianual.R, no aquí (este script solo entrega
# el mapa de candidatos posibles).
# =============================================================================

library(dplyr)
library(readr)
library(stringr)

crosstable_oit <- read_csv("data/reference/crosstable_isco08_isco88.csv", show_col_types = FALSE)

candidatos <- crosstable_oit |>
  mutate(
    isco88_codigo = str_pad(as.character(isco88_codigo), 4, pad = "0"),
    isco08_codigo = str_pad(as.character(isco08_codigo), 4, pad = "0"),
    es_parcial = !is.na(isco08_parcial)
  ) |>
  distinct(isco88_codigo, isco08_codigo, es_parcial) |>
  group_by(isco88_codigo) |>
  mutate(
    n_candidatos_total = n_distinct(isco08_codigo),
    tiene_exacta = any(!es_parcial)
  ) |>
  # Si existen candidatos "exactos" (no parciales), se usan esos (pueden ser
  # varios, como en el ejemplo de 5220 de arriba); si no, se usan todos los
  # parciales disponibles.
  filter(!tiene_exacta | !es_parcial) |>
  group_by(isco88_codigo) |>
  mutate(n_candidatos = n_distinct(isco08_codigo)) |>
  ungroup() |>
  distinct(isco88_codigo, isco08_codigo, n_candidatos) |>
  mutate(es_ambiguo = n_candidatos > 1) |>
  arrange(isco88_codigo, isco08_codigo)

write_csv(candidatos, "data/reference/crosswalk_ciuo88_ciuo08_candidatos.csv")

# Compatibilidad: también se deja el archivo "clásico" de 1 candidato por
# código (el de menor numeración), por si algún script antiguo lo usa.
candidatos |>
  group_by(isco88_codigo) |>
  slice_min(isco08_codigo, n = 1) |>
  ungroup() |>
  transmute(isco88_codigo, ciuo08_codigo_aprox = isco08_codigo, es_ambiguo) |>
  write_csv("data/reference/crosswalk_ciuo88_ciuo08.csv")

n_isco88 <- n_distinct(candidatos$isco88_codigo)
n_ambiguos <- candidatos |> distinct(isco88_codigo, es_ambiguo) |> pull(es_ambiguo) |> sum()
cat("Crosswalk CIUO-88 -> CIUO-08.CL construido:", n_isco88, "códigos CIUO-88\n")
cat("Códigos ambiguos (más de un candidato CIUO-08):", n_ambiguos,
    sprintf("(%.0f%%)\n", 100 * n_ambiguos / n_isco88))
cat("Total pares código88->candidato08:", nrow(candidatos), "\n")

