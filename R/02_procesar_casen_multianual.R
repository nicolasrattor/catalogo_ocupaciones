# =============================================================================
# 02_procesar_casen_multianual.R
# Procesa cada año de CASEN disponible en data/processed/casen_<año>_seleccion.csv,
# armoniza las variables que cambian de nombre/estructura entre años, y
# construye:
#   1) data/processed/casen_<año>_ocupados.rds           (persona-ocupado)
#   2) data/processed/panel_ocupacion_anual.csv           (ocupación x año)
#   3) data/processed/panel_gran_grupo_anual.csv           (gran grupo x año)
#
# FORMALIDAD LABORAL — ESTÁNDAR OIT-INE (no simple cotización)
# ---------------------------------------------------------------------------
# Este script implementa el algoritmo oficial de formalidad/informalidad
# laboral validado por la mesa técnica INE - Observatorio Social del MDS
# (Nota Técnica N°7, Casen 2024), que sigue los lineamientos de la OIT.
# La regla NO es uniforme para todos: depende de la categoría ocupacional.
#   - Dependientes (asalariados, servicio doméstico, FF.AA.): formales si
#     cotizan simultáneamente en previsión Y salud, y NO trabajan a
#     honorarios (trabajar a honorarios los vuelve informales aunque coticen,
#     porque su acceso a seguridad social no deriva del vínculo laboral).
#   - Independientes (empleadores y cuenta propia): formales si su negocio
#     está registrado en el Servicio de Impuestos Internos (SII), o si
#     desempeñan un oficio calificado (miembros del poder ejecutivo,
#     profesionales, técnicos de nivel medio) cuando no hay dato de registro.
#   - Familiares no remunerados: siempre informales, por definición.
# El algoritmo cambia de variables según el año (2020, 2022/2024 usan un set
# de preguntas; 2017 usa el set anterior a la actualización metodológica de
# 2020). Ver función calcular_formalidad_oit() más abajo para el detalle
# variable por variable, fiel al algoritmo Stata publicado por el MDS.
#
# OTRAS NOTAS DE ARMONIZACIÓN ENTRE AÑOS
# ---------------------------------------------------------------------------
# - Categoría ocupacional: se usa `o15` de forma UNIFORME en los 4 años
#   (2017, 2020, 2022, 2024) — es la misma variable en los cuatro
#   cuestionarios, con 9 códigos que se agrupan en 6 categorías:
#   Empleador, Cuenta propia, Asalariado, Servicio doméstico,
#   Familiar no remunerado, FF.AA.
# - Horas semanales trabajadas (`o10`): NO existe en Casen en Pandemia 2020
#   (cuestionario reducido por la contingencia sanitaria). Queda como NA
#   ese año y se documenta la limitación en el dashboard.
# - Rama de actividad (CIIU): en 2020 se llama `rama4_rev4`/`rama1_rev4`;
#   en 2022/2024, `rama4`/`rama1`. Se unifican a `rama4`/`rama1`.
# - CIUO: 2020, 2022 y 2024 usan CIUO-08.CL en `oficio4_08`. 2017 usa
#   CIUO-88 y se reclasifica vía crosswalk (ver 03_build_crosswalk_ciuo88.R).
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

CODIGOS_NA <- c(-88, -99, -66)
reemplazar_na_casen <- function(x) { x[x %in% CODIGOS_NA] <- NA; x }

ciuo_master <- read_csv("data/reference/ciuo08_master.csv", show_col_types = FALSE)

media_ponderada <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

# --- Redistribución proporcional del crosswalk CIUO-88 -> CIUO-08.CL --------
# Cuando un código CIUO-88 tiene varios candidatos igualmente válidos en
# CIUO-08 (ver R/03_build_crosswalk_ciuo88.R para el detalle y un ejemplo
# concreto), en vez de asignar el 100% de los casos al candidato de menor
# numeración (lo que antes generaba saltos artificiales enormes, ej.
# "Supervisores de locales comerciales" con ~496.000 personas en 2017), se
# reparte proporcionalmente según la distribución observada de esos mismos
# candidatos en los años con CIUO-08.CL nativo (2020/2022/2024 combinados).
# Se usa suavizado aditivo (+1) para que ningún candidato quede con peso
# cero solo por mala suerte de muestreo.
construir_pesos_candidatos <- function(candidatos_df, distribucion_ref) {
  candidatos_df |>
    left_join(distribucion_ref, by = c("isco08_codigo" = "codigo_ciuo08")) |>
    mutate(peso_total = coalesce(peso_total, 0) + 1) |>
    group_by(isco88_codigo) |>
    mutate(peso_relativo = peso_total / sum(peso_total)) |>
    ungroup() |>
    select(isco88_codigo, isco08_codigo, peso_relativo, es_ambiguo)
}

# Asigna, para cada código CIUO-88 ambiguo, UN candidato CIUO-08 mediante un
# sorteo ponderado por peso_relativo (reproducible: mismo seed + mismo orden
# de entrada -> mismo resultado siempre). Los códigos no ambiguos (1 solo
# candidato) se asignan directamente, sin sorteo.
asignar_ciuo08_ambiguo <- function(isco88_codigos, pesos_candidatos, seed = 20240115) {
  set.seed(seed)
  n <- length(isco88_codigos)
  resultado <- character(n)
  candidatos_split <- split(pesos_candidatos, pesos_candidatos$isco88_codigo)
  for (i in seq_len(n)) {
    codigo <- isco88_codigos[i]
    if (is.na(codigo) || !codigo %in% names(candidatos_split)) {
      resultado[i] <- NA_character_
      next
    }
    cand <- candidatos_split[[codigo]]
    if (nrow(cand) == 1) {
      resultado[i] <- cand$isco08_codigo[1]
    } else {
      resultado[i] <- sample(cand$isco08_codigo, size = 1, prob = cand$peso_relativo)
    }
  }
  resultado
}

# --- Categoría ocupacional (6 niveles, variable o15 uniforme entre años) ---
construir_cat_ocup <- function(df) {
  df |> mutate(cat_ocup = case_when(
    o15 == 1 ~ "Empleador",
    o15 == 2 ~ "CuentaPropia",
    o15 %in% c(3, 4, 5) ~ "Asalariado",
    o15 %in% c(6, 7) ~ "ServicioDomestico",
    o15 == 9 ~ "FamiliarNoRemunerado",
    o15 == 8 ~ "FFAA",
    TRUE ~ NA_character_
  ))
}

# --- Formalidad laboral, estándar OIT-INE (Nota Técnica N°7, Casen 2024) ---
# Devuelve un vector 1 = formal, 0 = informal, NA = sin información suficiente.
calcular_formalidad_oit <- function(raw, cat_ocup, gran_grupo, anio) {
  n <- nrow(raw)
  formal <- rep(NA_real_, n)

  dependiente <- cat_ocup %in% c("Asalariado", "ServicioDomestico", "FFAA")
  independiente <- cat_ocup %in% c("Empleador", "CuentaPropia")
  familiar <- cat_ocup == "FamiliarNoRemunerado"
  oficio_calificado <- gran_grupo %in% c("1", "2", "3")

  get_col <- function(nombre) if (nombre %in% names(raw)) raw[[nombre]] else rep(NA_real_, n)

  if (anio == 2020) {
    o33a <- get_col("o33a"); o33b <- get_col("o33b"); o36 <- get_col("o36"); o16 <- get_col("o16")
    afp_si <- o33a == 1; afp_no <- o33a == 2
    salud_si <- o33b == 1; salud_no <- o33b == 2
    honorario <- o36 == 1
    sii_si <- o16 == 1; sii_no <- o16 == 2
  } else if (anio %in% c(2022, 2024)) {
    o31 <- get_col("o31"); o32 <- get_col("o32"); o14 <- get_col("o14")
    s13 <- get_col("s13"); o16 <- get_col("o16")
    sexo <- get_col("sexo"); edad <- get_col("edad")
    afp_cod <- case_when(
      o31 == 2 ~ 0,
      o32 == 6 ~ 0,
      o31 == 1 & o32 < 6 ~ 1,
      (o31 == 2 | o32 == 6) & o14 == 1 &
        ((sexo == 1 & edad < 55) | (sexo == 2 & edad < 50)) ~ 1,
      TRUE ~ NA_real_
    )
    salud_cod <- case_when(s13 %in% c(1, 2, 3, 5) ~ 1, s13 == 4 ~ 0, TRUE ~ NA_real_)
    afp_si <- afp_cod == 1; afp_no <- afp_cod == 0
    salud_si <- salud_cod == 1; salud_no <- salud_cod == 0
    honorario <- o14 == 1
    sii_si <- o16 == 1; sii_no <- o16 == 2
  } else if (anio %in% c(2015, 2017)) {
    o28 <- get_col("o28"); o29 <- get_col("o29"); s12 <- get_col("s12")
    o14 <- get_col("o14"); o23 <- get_col("o23"); oficio1_88 <- get_col("oficio1_88")
    afp_cod <- case_when(o28 == 2 ~ 0, o29 == 7 ~ 0, o28 == 1 & o29 >= 1 & o29 <= 6 ~ 1, TRUE ~ NA_real_)
    salud_cod <- case_when(s12 %in% c(1, 2, 3, 5, 6, 7) ~ 1, s12 == 8 ~ 0, TRUE ~ NA_real_)
    afp_si <- afp_cod == 1; afp_no <- afp_cod == 0
    salud_si <- salud_cod == 1; salud_no <- salud_cod == 0
    honorario <- o14 == 1
    tam_num <- case_when(
      o23 == "A" ~ 1, o23 == "B" ~ 2, o23 == "C" ~ 3,
      o23 == "D" ~ 4, o23 == "E" ~ 5, o23 == "F" ~ 6, TRUE ~ NA_real_
    )
    oficio1988_calif <- oficio1_88 %in% c(1, 2, 3)
    empresa_grande <- !is.na(tam_num) & tam_num >= 3
    empresa_chica <- !is.na(tam_num) & tam_num < 3
  } else {
    return(formal)
  }

  # --- Dependientes: formales si cotizan en ambos y no son a honorarios ---
  formal[dependiente & !is.na(afp_si) & !is.na(salud_si) & afp_si & salud_si & !coalesce(honorario, FALSE)] <- 1
  formal[dependiente & coalesce(honorario, FALSE)] <- 0
  formal[dependiente & (coalesce(afp_no, FALSE) | coalesce(salud_no, FALSE))] <- 0

  if (anio %in% c(2020, 2022, 2024)) {
    # --- Independientes: registro SII prevalece; oficio calificado es respaldo ---
    formal[independiente & coalesce(sii_si, FALSE)] <- 1
    formal[independiente & is.na(formal) & oficio_calificado] <- 1
    formal[independiente & coalesce(sii_no, FALSE)] <- 0
    formal[independiente & is.na(formal) & !oficio_calificado] <- 0
  } else if (anio %in% c(2015, 2017)) {
    # --- Independientes 2017: cuenta propia por oficio; empleador por tamaño ---
    formal[cat_ocup == "CuentaPropia" & oficio1988_calif] <- 1
    formal[cat_ocup == "CuentaPropia" & is.na(formal) & !is.na(oficio1988_calif) & !oficio1988_calif] <- 0
    formal[cat_ocup == "Empleador" & empresa_grande] <- 1
    formal[cat_ocup == "Empleador" & is.na(formal) & is.na(tam_num) & oficio1988_calif] <- 1
    formal[cat_ocup == "Empleador" & is.na(formal) & empresa_chica] <- 0
  }

  formal[familiar] <- 0
  formal
}

procesar_year <- function(anio, pesos_candidatos = NULL) {
  path_csv <- sprintf("data/processed/casen_%d_seleccion.csv", anio)
  if (!file.exists(path_csv)) {
    message("No existe ", path_csv, " -- se omite ", anio)
    return(invisible(NULL))
  }

  raw <- read_csv(path_csv, guess_max = 250000, show_col_types = FALSE)

  # Unificar nombre de rama de actividad (2020 usa sufijo _rev4)
  if (!"rama4" %in% names(raw) && "rama4_rev4" %in% names(raw)) {
    raw <- raw |> rename(rama4 = rama4_rev4)
  }
  if (!"rama1" %in% names(raw) && "rama1_rev4" %in% names(raw)) {
    raw <- raw |> rename(rama1 = rama1_rev4)
  }
  # Asegurar que existan todas las columnas usadas más abajo, aunque sea con NA
  for (col in c("o10", "cotiza", "ytrabajocor", "oficio4_08", "activ", "expr",
                "sexo", "edad", "esc", "region")) {
    if (!col %in% names(raw)) raw[[col]] <- NA
  }

  raw <- construir_cat_ocup(raw)

  vars_numericas <- c("oficio1_08", "oficio4_08", "activ",
                       "o10", "ytrabajocor", "esc", "edad", "sexo", "region")
  vars_numericas <- intersect(vars_numericas, names(raw))

  casen <- raw |>
    mutate(across(all_of(vars_numericas), reemplazar_na_casen)) |>
    mutate(
      codigo_ciuo08 = if_else(
        !is.na(oficio4_08),
        str_pad(as.character(as.integer(oficio4_08)), width = 4, pad = "0"),
        NA_character_
      ),
      ocupado = activ == 1,
      horas_semanales = o10,
      anio = anio
    )

  # --- Crosswalk CIUO-88 -> CIUO-08.CL (solo para años sin CIUO-08 nativo) ---
  # Si la cobertura de codigo_ciuo08 entre ocupados es muy baja pero existe
  # oficio4_88, este año usa CIUO-88 (p.ej. Casen 2017) y necesitamos
  # reclasificar vía la tabla de correspondencia de la OIT. Ver
  # R/03_build_crosswalk_ciuo88.R para el detalle y las limitaciones de esta
  # aproximación (43% de los códigos CIUO-88 tienen correspondencia
  # ambigua/parcial con CIUO-08).
  cobertura_ciuo08_nativo <- mean(!is.na(casen$codigo_ciuo08[casen$ocupado]))
  usa_crosswalk <- "oficio4_88" %in% names(raw) && cobertura_ciuo08_nativo < 0.5

  if (usa_crosswalk) {
    if (!is.null(pesos_candidatos)) {
      # --- Redistribución proporcional (ver funciones al inicio del script) ---
      casen <- casen |>
        mutate(oficio4_88 = reemplazar_na_casen(raw$oficio4_88)) |>
        mutate(isco88_tmp = if_else(
          !is.na(oficio4_88),
          str_pad(as.character(as.integer(oficio4_88)), width = 4, pad = "0"),
          NA_character_
        ))
      codigo_asignado <- asignar_ciuo08_ambiguo(casen$isco88_tmp, pesos_candidatos)
      es_ambiguo_vec <- pesos_candidatos |> distinct(isco88_codigo, es_ambiguo)
      ambiguo_lookup <- setNames(es_ambiguo_vec$es_ambiguo, es_ambiguo_vec$isco88_codigo)
      casen <- casen |>
        mutate(
          codigo_ciuo08 = codigo_asignado,
          ciuo_crosswalk_aplicado = TRUE,
          ciuo_crosswalk_ambiguo = coalesce(unname(ambiguo_lookup[isco88_tmp]), FALSE)
        ) |>
        select(-isco88_tmp)
      message(sprintf("  [%d] Usando crosswalk CIUO-88 -> CIUO-08.CL con redistribución proporcional (calibrada a años nativos).", anio))
    } else {
      # Sin distribución de referencia disponible (p.ej. si este script se
      # corre parcialmente): usa el crosswalk clásico de 1 candidato.
      if (!file.exists("data/reference/crosswalk_ciuo88_ciuo08.csv")) {
        stop("Falta data/reference/crosswalk_ciuo88_ciuo08.csv. Corre R/03_build_crosswalk_ciuo88.R primero.")
      }
      crosswalk <- read_csv("data/reference/crosswalk_ciuo88_ciuo08.csv", show_col_types = FALSE)
      casen <- casen |>
        mutate(oficio4_88 = reemplazar_na_casen(raw$oficio4_88)) |>
        mutate(isco88_tmp = if_else(
          !is.na(oficio4_88),
          str_pad(as.character(as.integer(oficio4_88)), width = 4, pad = "0"),
          NA_character_
        )) |>
        left_join(crosswalk, by = c("isco88_tmp" = "isco88_codigo")) |>
        mutate(
          codigo_ciuo08 = ciuo08_codigo_aprox,
          ciuo_crosswalk_aplicado = TRUE,
          ciuo_crosswalk_ambiguo = coalesce(es_ambiguo, FALSE)
        ) |>
        select(-isco88_tmp, -ciuo08_codigo_aprox, -es_ambiguo)
      message(sprintf("  [%d] Usando crosswalk CIUO-88 -> CIUO-08.CL (sin distribución de referencia; usando 1 candidato fijo).", anio))
    }
  } else {
    casen <- casen |> mutate(ciuo_crosswalk_aplicado = FALSE, ciuo_crosswalk_ambiguo = FALSE)
  }

  casen_ocupados <- casen |>
    filter(ocupado, !is.na(codigo_ciuo08)) |>
    left_join(ciuo_master, by = "codigo_ciuo08")

  # --- Formalidad laboral, estándar OIT-INE (no cotización simple) ---------
  casen_ocupados$formal <- calcular_formalidad_oit(
    casen_ocupados, casen_ocupados$cat_ocup, casen_ocupados$gran_grupo, anio
  )

  n_sin_match <- casen_ocupados |> filter(is.na(gran_grupo)) |> nrow()
  cat(sprintf("[%d] Ocupados: %d muestra | sin match CIUO-08.CL: %d\n",
              anio, nrow(casen_ocupados), n_sin_match))

  saveRDS(casen_ocupados, sprintf("data/processed/casen_%d_ocupados.rds", anio))

  agregado_ocupacion <- casen_ocupados |>
    group_by(codigo_ciuo08, ocupacion_nombre, subgrupo_principal, subgrupo_principal_nombre,
              subgrupo, subgrupo_nombre, gran_grupo, gran_grupo_nombre, anio) |>
    summarise(
      n_muestra = n(),
      personas_estimadas = sum(expr, na.rm = TRUE),
      ingreso_trabajo_prom = media_ponderada(ytrabajocor, expr),
      horas_semanales_prom = media_ponderada(horas_semanales, expr),
      tasa_formalidad = media_ponderada(formal, expr),
      pct_asalariados = media_ponderada(cat_ocup %in% c("Asalariado", "ServicioDomestico", "FFAA"), expr),
      pct_cuenta_propia = media_ponderada(cat_ocup == "CuentaPropia", expr),
      pct_empleadores = media_ponderada(cat_ocup == "Empleador", expr),
      pct_mujeres = media_ponderada(sexo == 2, expr),
      edad_prom = media_ponderada(edad, expr),
      escolaridad_prom = media_ponderada(esc, expr),
      es_crosswalk = any(ciuo_crosswalk_aplicado),
      pct_crosswalk_ambiguo = media_ponderada(ciuo_crosswalk_ambiguo, expr),
      .groups = "drop"
    ) |>
    mutate(muestra_chica = n_muestra < 30) |>
    arrange(desc(personas_estimadas))

  agregado_gran_grupo <- casen_ocupados |>
    group_by(gran_grupo, gran_grupo_nombre, anio) |>
    summarise(
      n_muestra = n(),
      personas_estimadas = sum(expr, na.rm = TRUE),
      ingreso_trabajo_prom = media_ponderada(ytrabajocor, expr),
      horas_semanales_prom = media_ponderada(horas_semanales, expr),
      tasa_formalidad = media_ponderada(formal, expr),
      pct_mujeres = media_ponderada(sexo == 2, expr),
      .groups = "drop"
    ) |>
    arrange(desc(personas_estimadas))

  list(ocupacion = agregado_ocupacion, gran_grupo = agregado_gran_grupo)
}

if (!exists("anios_disponibles")) {
  # Por defecto, si este script se corre solo (no desde build_site.R),
  # detecta automáticamente los años disponibles a partir de los CSV ya
  # convertidos en data/processed/.
  archivos <- list.files("data/processed", pattern = "^casen_[0-9]{4}_seleccion\\.csv$")
  anios_disponibles <- sort(as.integer(gsub("casen_([0-9]{4})_seleccion\\.csv", "\\1", archivos)))
  if (length(anios_disponibles) == 0) anios_disponibles <- c(2020, 2022, 2024)
}
message("Procesando años: ", paste(anios_disponibles, collapse = ", "))

# --- Detectar de antemano qué años son "nativos" (CIUO-08.CL ya en la
# encuesta) vs "crosswalk" (necesitan CIUO-88 -> CIUO-08.CL), revisando
# rápidamente si oficio4_08 tiene buena cobertura en cada archivo.
es_anio_nativo <- function(anio) {
  path_csv <- sprintf("data/processed/casen_%d_seleccion.csv", anio)
  if (!file.exists(path_csv)) return(NA)
  raw <- read_csv(path_csv, guess_max = 250000, show_col_types = FALSE,
                   col_select = any_of(c("oficio4_08", "activ")))
  if (!"oficio4_08" %in% names(raw)) return(FALSE)
  ocupado <- if ("activ" %in% names(raw)) raw$activ == 1 else TRUE
  mean(!is.na(raw$oficio4_08[ocupado])) >= 0.5
}
anios_nativos <- Filter(function(a) isTRUE(es_anio_nativo(a)), anios_disponibles)
anios_crosswalk <- setdiff(anios_disponibles, anios_nativos)
message("  Años con CIUO-08.CL nativo: ", paste(anios_nativos, collapse = ", "))
message("  Años que requieren crosswalk CIUO-88: ", paste(anios_crosswalk, collapse = ", "))

# --- Fase 1: procesar años nativos y construir la distribución de referencia
resultados_nativos <- map(anios_nativos, procesar_year) |> compact()

pesos_candidatos <- NULL
if (length(anios_crosswalk) > 0 && length(anios_nativos) > 0 &&
    file.exists("data/reference/crosswalk_ciuo88_ciuo08_candidatos.csv")) {
  distribucion_ref <- map_dfr(anios_nativos, function(a) {
    path <- sprintf("data/processed/casen_%d_ocupados.rds", a)
    if (!file.exists(path)) return(NULL)
    readRDS(path) |> select(codigo_ciuo08, expr)
  }) |>
    group_by(codigo_ciuo08) |>
    summarise(peso_total = sum(expr, na.rm = TRUE), .groups = "drop")

  candidatos_ambiguos <- read_csv("data/reference/crosswalk_ciuo88_ciuo08_candidatos.csv", show_col_types = FALSE)
  pesos_candidatos <- construir_pesos_candidatos(candidatos_ambiguos, distribucion_ref)
  message(sprintf("  Distribución de referencia construida desde %d año(s) nativo(s), %d códigos CIUO-08 observados.",
                   length(anios_nativos), nrow(distribucion_ref)))
}

# --- Fase 2: procesar años con crosswalk, usando la redistribución proporcional
resultados_crosswalk <- map(anios_crosswalk, procesar_year, pesos_candidatos = pesos_candidatos) |> compact()

resultados <- c(resultados_nativos, resultados_crosswalk)

panel_ocupacion  <- map_dfr(resultados, "ocupacion")
panel_gran_grupo <- map_dfr(resultados, "gran_grupo")

write_csv(panel_ocupacion, "data/processed/panel_ocupacion_anual.csv")
write_csv(panel_gran_grupo, "data/processed/panel_gran_grupo_anual.csv")

# Mantener también los agregados de un solo año para compatibilidad con el
# módulo 1 del dashboard (2024)
panel_ocupacion |> filter(anio == 2024) |> select(-anio) |>
  write_csv("data/processed/casen_2024_agregado_ocupacion.csv")
panel_gran_grupo |> filter(anio == 2024) |> select(-anio) |>
  write_csv("data/processed/casen_2024_agregado_gran_grupo.csv")

cat("\nPanel construido. Años:", paste(sort(unique(panel_ocupacion$anio)), collapse = ", "), "\n")
cat("Filas panel ocupación:", nrow(panel_ocupacion), "\n")
cat("Filas panel gran grupo:", nrow(panel_gran_grupo), "\n")

cat("\nAdvertencia: horas_semanales_prom no disponible para 2020 (cuestionario reducido, Casen en Pandemia).\n")
