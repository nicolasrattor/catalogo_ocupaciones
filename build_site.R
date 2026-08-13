# =============================================================================
# build_site.R
# -----------------------------------------------------------------------------
# SCRIPT MAESTRO — corre todo el catálogo de un tirón:
#   parquet (data/raw/) -> CSV -> tabla CIUO-08.CL -> panel armonizado
#   -> dashboard Quarto -> docs/index.html (listo para GitHub Pages)
#
# USO
#   1. Coloca tus archivos casen_<año>.parquet en data/raw/
#      (ej: data/raw/casen_2020.parquet, data/raw/casen_2022.parquet, ...)
#   2. Desde la raíz del proyecto, corre:  Rscript build_site.R
#   3. El sitio queda en docs/index.html. Sube el repo a GitHub y activa
#      GitHub Pages apuntando a la carpeta /docs (ver README.md).
#
# REQUISITOS
#   - R (>= 4.1)
#   - Quarto CLI instalado y disponible en el PATH (https://quarto.org)
#   - Paquete R 'arrow' (recomendado, para leer .parquet nativamente) o,
#     en su defecto, python3 con pandas + pyarrow instalados (se usa como
#     alternativa automática si 'arrow' no está disponible).
# =============================================================================

mensaje_paso <- function(txt) {
  cat("\n============================================================\n")
  cat(txt, "\n")
  cat("============================================================\n")
}

# --- 0. Preparación: paquetes y locale --------------------------------------
mensaje_paso("Paso 0/6 — Verificando paquetes y entorno")

paquetes_necesarios <- c("dplyr", "tidyr", "readr", "stringr", "purrr",
                          "ggplot2", "plotly", "DT", "scales", "jsonlite")
faltantes <- paquetes_necesarios[!paquetes_necesarios %in% rownames(installed.packages())]
if (length(faltantes) > 0) {
  message("Instalando paquetes faltantes: ", paste(faltantes, collapse = ", "))
  install.packages(faltantes)
}
invisible(lapply(paquetes_necesarios, library, character.only = TRUE))

tiene_arrow <- requireNamespace("arrow", quietly = TRUE)
if (tiene_arrow) {
  message("Paquete 'arrow' disponible: se leerán los .parquet directamente en R.")
} else {
  message("Paquete 'arrow' NO disponible: se intentará usar python3 (pandas + pyarrow) ",
          "para convertir los .parquet a CSV. Si prefieres evitar Python, instala 'arrow' con:\n",
          '  install.packages("arrow")')
}

try(Sys.setlocale("LC_ALL", "en_US.UTF-8"), silent = TRUE)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("data/reference", showWarnings = FALSE, recursive = TRUE)
dir.create("docs", showWarnings = FALSE, recursive = TRUE)

# --- 1. Detectar años disponibles a partir de los parquet -------------------
mensaje_paso("Paso 1/6 — Detectando archivos CASEN disponibles en data/raw/")

archivos_parquet <- list.files("data/raw", pattern = "^casen_[0-9]{4}\\.parquet$")
if (length(archivos_parquet) == 0) {
  stop(
    "No se encontraron archivos data/raw/casen_<año>.parquet.\n",
    "Copia tus archivos ahí con el nombre exacto casen_2020.parquet, casen_2022.parquet, etc.",
    call. = FALSE
  )
}
anios_disponibles <- sort(as.integer(gsub("casen_([0-9]{4})\\.parquet", "\\1", archivos_parquet)))
message("Años detectados: ", paste(anios_disponibles, collapse = ", "))

# --- 2. Convertir cada parquet a CSV filtrado --------------------------------
mensaje_paso("Paso 2/6 — Convirtiendo parquet -> CSV (selección de variables)")

VARS_DESEADAS <- c(
  "id_persona", "id_vivienda", "folio", "expr", "varstrat", "varunit",
  "region", "area", "sexo", "edad", "esc", "pueblos_indigenas",
  "oficio1_08", "oficio4_08", "oficio1_88", "oficio4_88",
  "rama1", "rama4", "rama1_rev4", "rama4_rev4",
  "activ", "asalariado", "contrato", "cotiza", "depen_fun",
  "o10", "o15", "o21", "o22", "o24",
  "o14", "o16", "o17", "o23", "o28", "o29",
  "o31", "o32", "o33a", "o33b", "o36", "s12", "s13",
  "cinef13_area", "cinef13_subarea", "e8",
  "ytotcor", "ypchtotcor", "ytrabajocor",
  "pobreza", "pobreza_multi"
)

convertir_con_arrow <- function(anio) {
  parquet_path <- sprintf("data/raw/casen_%d.parquet", anio)
  csv_path <- sprintf("data/processed/casen_%d_seleccion.csv", anio)
  tabla <- arrow::open_dataset(parquet_path, format = "parquet")
  disponibles <- intersect(VARS_DESEADAS, names(tabla))
  faltan <- setdiff(VARS_DESEADAS, names(tabla))
  df <- tabla |> dplyr::select(dplyr::all_of(disponibles)) |> dplyr::collect()
  readr::write_csv(df, csv_path)
  message(sprintf("  [%d] OK: %s filas, %d columnas -> %s", anio,
                   format(nrow(df), big.mark = "."), ncol(df), csv_path))
  if (length(faltan) > 0) {
    message("        (no presentes en esta base, se omiten: ", paste(faltan, collapse = ", "), ")")
  }
}

convertir_con_python <- function(anio) {
  parquet_path <- sprintf("data/raw/casen_%d.parquet", anio)
  csv_path <- sprintf("data/processed/casen_%d_seleccion.csv", anio)
  resultado <- system2("python3", c("R/00_convert_casen_parquet.py", parquet_path, csv_path))
  if (resultado != 0) {
    stop("Falló la conversión de ", parquet_path, ". Verifica que python3, pandas y pyarrow estén instalados.")
  }
}

for (anio in anios_disponibles) {
  if (tiene_arrow) convertir_con_arrow(anio) else convertir_con_python(anio)
}

# --- 3. Tabla maestra CIUO-08.CL --------------------------------------------
mensaje_paso("Paso 3/6 — Construyendo tabla maestra CIUO-08.CL")

if (!file.exists("data/reference/ciuo08cl_estructura_ine.txt")) {
  stop("Falta data/reference/ciuo08cl_estructura_ine.txt (fuente oficial INE). ",
       "Este archivo debe venir incluido en el proyecto.", call. = FALSE)
}
source("R/01_build_ciuo_master.R")

# --- 3b. Tabla de correspondencia CIUO-88 -> CIUO-08.CL (para años que aún
#     no usan CIUO-08 nativo, p.ej. Casen 2017) --------------------------------
if (file.exists("data/reference/crosstable_isco08_isco88.csv")) {
  source("R/03_build_crosswalk_ciuo88.R")
} else {
  message("(sin data/reference/crosstable_isco08_isco88.csv: se omite el crosswalk CIUO-88; ",
          "años anteriores a la adopción de CIUO-08.CL no podrán incorporarse)")
}

# --- 4. Procesar y armonizar todos los años, construir el panel -------------
mensaje_paso("Paso 4/6 — Procesando CASEN y armonizando variables entre años")

source("R/02_procesar_casen_multianual.R")

# --- 4b. PIAAC (opcional): habilidades por ocupación -------------------------
if (file.exists("data/raw/piaac_chile.parquet")) {
  mensaje_paso("Paso 4b/6 — Procesando PIAAC Chile (habilidades por ocupación)")
  if (tiene_arrow) {
    tabla_piaac <- arrow::open_dataset("data/raw/piaac_chile.parquet", format = "parquet")
    vars_piaac <- c(
      "ISCO08_C", "ISCO1C", "ISCO2C",
      paste0("PVLIT", 1:10), paste0("PVNUM", 1:10),
      "NUMWORKC2", "READWORKC2_T1", "WRITWORKC2", "ICTWORKC2",
      "SPFWT0", "AGE_R", "GENDER_R"
    )
    df_piaac <- tabla_piaac |> dplyr::select(dplyr::all_of(vars_piaac)) |> dplyr::collect()
    readr::write_csv(df_piaac, "data/processed/piaac_chile_seleccion.csv")
  } else {
    system2("python3", c("R/00c_convert_piaac_parquet.py",
                          "data/raw/piaac_chile.parquet",
                          "data/processed/piaac_chile_seleccion.csv"))
  }
  source("R/04_procesar_piaac.R")
} else {
  message("(sin data/raw/piaac_chile.parquet: se omite el módulo de habilidades PIAAC)")
}

# --- 4c. ENADEL (opcional): demanda laboral por ocupación --------------------
if (file.exists("data/raw/enadel_2025.parquet")) {
  mensaje_paso("Paso 4c/6 — Procesando ENADEL 2025 (demanda laboral por ocupación)")
  if (tiene_arrow) {
    tabla_enadel <- arrow::open_dataset("data/raw/enadel_2025.parquet", format = "parquet")
    base_vars_enadel <- c("idempresa_ficticio", "pond_final", "varunit", "varstrat", "fpc")
    slot_vars_enadel <- unlist(lapply(1:7, function(i) c(
      paste0("c1_Codigo_CIUO_", i), paste0("c1_glosa_CIUO_", i),
      paste0("c1_c_", i), paste0("c1_d_", i), paste0("c1_e_", i), paste0("c1_f_", i), paste0("c1_h_", i),
      paste0("c1_g_dif1_", i), paste0("c1_g_dif2_", i), paste0("c1_g_dif3_", i),
      paste0("c1_g_dif4_", i), paste0("c1_g_dif5_", i)
    )))
    slot_vars_enadel_c3 <- unlist(lapply(1:3, function(i) c(
      paste0("c3_Codigo_CIUO_", i), paste0("c3_glosa_CIUO_", i),
      paste0("c3_Puesto_Trabajo_", i), paste0("c3_c_", i), paste0("c3_d_", i)
    )))
    vars_enadel <- intersect(c(base_vars_enadel, slot_vars_enadel, slot_vars_enadel_c3), names(tabla_enadel))
    df_enadel <- tabla_enadel |> dplyr::select(dplyr::all_of(vars_enadel)) |> dplyr::collect()
    readr::write_csv(df_enadel, "data/processed/enadel_2025_seleccion.csv")
  } else {
    system2("python3", c("R/00d_convert_enadel_parquet.py",
                          "data/raw/enadel_2025.parquet",
                          "data/processed/enadel_2025_seleccion.csv"))
  }
  source("R/05_procesar_enadel.R")
} else {
  message("(sin data/raw/enadel_2025.parquet: se omite el módulo de demanda laboral ENADEL)")
}

# --- 5. Renderizar el dashboard ----------------------------------------------
mensaje_paso("Paso 5/6 — Renderizando el dashboard con Quarto")

quarto_bin <- Sys.which("quarto")
if (quarto_bin == "") {
  stop(
    "No se encontró el ejecutable 'quarto' en el PATH.\n",
    "Instálalo desde https://quarto.org/docs/get-started/ y vuelve a correr este script.",
    call. = FALSE
  )
}

directorio_original <- getwd()
setwd("dashboard")
codigo_salida <- system2("quarto", c("render", "catalogo_ocupaciones.qmd", "--to", "dashboard"))
setwd(directorio_original)

if (codigo_salida != 0) {
  stop("Quarto falló al renderizar el dashboard. Revisa los mensajes de error arriba.", call. = FALSE)
}

# --- 6. Copiar el resultado a docs/index.html (GitHub Pages) ----------------
mensaje_paso("Paso 6/6 — Publicando en docs/index.html")

archivo_render <- "dashboard/catalogo_ocupaciones.html"
if (!file.exists(archivo_render)) {
  stop("No se generó ", archivo_render, ". Revisa el paso anterior.", call. = FALSE)
}

file.copy(archivo_render, "docs/index.html", overwrite = TRUE)
# .nojekyll evita que GitHub Pages intente procesar el sitio con Jekyll
# (innecesario aquí y puede romper archivos que empiecen con "_").
file.create("docs/.nojekyll")

mensaje_paso("¡Listo! Sitio generado en docs/index.html")
cat("Próximos pasos para publicar en GitHub Pages:\n")
cat("  1. git add -A && git commit -m \"Actualiza catálogo de ocupaciones\"\n")
cat("  2. git push\n")
cat("  3. En GitHub: Settings > Pages > Source: 'Deploy from a branch',\n")
cat("     Branch: main, Folder: /docs\n")
cat("  (ver README.md para instrucciones completas)\n")
