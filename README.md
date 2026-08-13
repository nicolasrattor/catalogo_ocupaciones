# Catálogo de Ocupaciones de Chile

Catálogo que vincula información administrativa, de encuestas y estudios
cualitativos por ocupación (taxonomía **CIUO-08.CL**). Este es el **Módulo 1**:
mercado laboral por ocupación a partir de **CASEN 2020, 2022 y 2024**.

## Publicado en

https://nicolasrattor.github.io/

## Inicio rápido: correr todo con un solo comando

```bash
Rscript build_site.R
```

Este script:
1. Detecta automáticamente qué años de CASEN tienes en `data/raw/` (busca archivos con el patrón `casen_<año>.parquet`).
2. Convierte cada parquet a CSV (usa el paquete R `arrow` si está instalado; si no, usa `python3` + `pandas`/`pyarrow` como respaldo automático).
3. Construye la tabla maestra CIUO-08.CL.
4. Procesa y arma el panel armonizado entre años.
5. Renderiza el dashboard con Quarto.
6. Copia el resultado a **`docs/index.html`**, listo para GitHub Pages.

Requisitos: R (≥4.1) y [Quarto CLI](https://quarto.org/docs/get-started/) instalado y en el PATH. Si no tienes el paquete `arrow`, necesitas además `python3` con `pandas` y `pyarrow` (`pip install pandas pyarrow`).

## Publicar en GitHub Pages (nicolasrattor.github.io)

Como es un repositorio de usuario (`nicolasrattor.github.io`), GitHub Pages puede publicar tanto desde la raíz como desde la carpeta `/docs` — este proyecto ya deja todo listo en `/docs`.

```bash
# 1. Si es la primera vez, inicializa el repo y agrega el remoto
git init
git remote add origin https://github.com/nicolasrattor/nicolasrattor.github.io.git

# 2. Corre el pipeline (si no lo has hecho)
Rscript build_site.R

# 3. Sube todo
git add -A
git commit -m "Catálogo de ocupaciones de Chile"
git branch -M main
git push -u origin main
```

Luego, en GitHub: **Settings → Pages → Source**: elige **"Deploy from a branch"**, **Branch: `main`**, **Folder: `/docs`** → Save.

En unos minutos el sitio queda disponible en `https://nicolasrattor.github.io/`. Cada vez que corras `Rscript build_site.R` de nuevo y hagas `git push`, el sitio se actualiza automáticamente (GitHub Pages redepliega solo con cada push a la rama configurada).

**Nota sobre tamaño:** `docs/index.html` pesa varios MB (todo el dashboard, los datos y las fuentes van autocontenidos en un solo archivo). Esto es normal y GitHub lo maneja sin problema, pero si algún día se acerca al límite de 100MB de GitHub, se puede optimizar separando los datos a un archivo aparte.


## Estructura del proyecto

```
├── data/
│   ├── raw/            # Datos crudos (CASEN 2024 en parquet) — no incluido en este zip por peso
│   ├── reference/       # Tabla maestra CIUO-08.CL + fuente oficial del INE
│   └── processed/       # Salidas de los scripts R: base persona-ocupado y agregados
├── R/                   # Scripts de procesamiento, en orden de ejecución
├── dashboard/            # Dashboard Quarto (.qmd) y su versión renderizada (.html)
└── docs/                 # (reservado para documentación futura)
```

## Cómo reproducir el pipeline

Requisitos: R (>= 4.1), Quarto CLI, y los paquetes `dplyr`, `tidyr`, `readr`,
`stringr`, `purrr`, `ggplot2`, `plotly`, `DT`, `scales`, `jsonlite`.

```r
install.packages(c("dplyr","tidyr","readr","stringr","purrr",
                    "ggplot2","plotly","DT","scales","jsonlite"))
```

Si tienes acceso normal a CRAN, te recomendamos instalar también `arrow`
(`install.packages("arrow")`) para leer el `.parquet` de CASEN directamente
en R con `arrow::read_parquet()`, sin pasar por el script Python auxiliar
`R/00_convert_casen_parquet.py` (ese script existe solo porque el entorno
donde se construyó este proyecto no tenía acceso a CRAN).

Orden de ejecución:

```bash
# 1) (Opcional si no tienes `arrow`) Convierte cada parquet de CASEN a CSV filtrado
python3 R/00_convert_casen_parquet.py data/raw/casen_2020.parquet data/processed/casen_2020_seleccion.csv
python3 R/00_convert_casen_parquet.py data/raw/casen_2022.parquet data/processed/casen_2022_seleccion.csv
python3 R/00_convert_casen_parquet.py data/raw/casen_2024.parquet data/processed/casen_2024_seleccion.csv

# 2) Construye la tabla maestra CIUO-08.CL
Rscript R/01_build_ciuo_master.R

# 3) Procesa todos los años de CASEN disponibles, arma el panel ocupación x año
Rscript R/02_procesar_casen_multianual.R

# 4) Renderiza el dashboard
cd dashboard
quarto render catalogo_ocupaciones.qmd --to dashboard
```

Para agregar un nuevo año de CASEN: sube el parquet a `data/raw/`, conviértelo con el script del paso 1, agrega el año al vector `anios_disponibles` en `R/02_procesar_casen_multianual.R`, y vuelve a correr los pasos 3 y 4. Si el año usa CIUO-88 en vez de CIUO-08.CL (Casen 2017 y anteriores), se necesitará además una tabla de correspondencia CIUO-88 ↔ CIUO-08.CL antes de poder unirlo al panel.

## Fuentes de datos usadas

- **Taxonomía**: CIUO-08.CL (INE, 2018), parseada desde el documento oficial
  `data/reference/ciuo08cl_estructura_ine.txt` (extraído de
  https://www.ine.gob.cl/docs/default-source/buenas-practicas/clasificaciones/ciuo/clasificador/ciuo-08-cl.pdf).
  444 grupos primarios, 129 subgrupos, 44 subgrupos principales, 10 grandes grupos.
- **CASEN 2024**: Ministerio de Desarrollo Social y Familia. Variables usadas:
  `oficio4_08` (ocupación CIUO-08.CL), `activ` (condición de actividad),
  `ytrabajocor` (ingreso del trabajo corregido), `o10` (horas semanales),
  `cotiza` (formalidad/cotización previsional), `o21` (categoría ocupacional),
  `expr` (factor de expansión), entre otras. Todos los indicadores del
  dashboard están ponderados por `expr`.

## Resultados clave (CASEN 2024)

- 9.466.107 personas ocupadas estimadas a nivel país
- Ingreso del trabajo promedio: ~$953.000
- Tasa de formalidad (cotiza): 75,3%
- Participación femenina: 42,7%
- 437 de 444 ocupaciones CIUO-08.CL observadas en la muestra (127 con menos
  de 30 casos — usar con cautela)

## Panel multianual (CASEN 2015, 2017, 2020, 2022, 2024)

Los tres años ya usan CIUO-08.CL nativamente (no fue necesario convertir
desde CIUO-88). Se armonizaron las siguientes diferencias entre años:

- **Categoría ocupacional**: 2022/2024 usan `o21` (3 categorías); 2020 usa
  `o15` (9 categorías) — se armonizaron a 3 niveles comunes.
- **Rama de actividad**: 2020 usa `rama4_rev4`/`rama1_rev4`; 2022/2024 usan
  `rama4`/`rama1` — mismo estándar CIIU Rev. 4, distinto nombre de columna.
- **Horas semanales trabajadas**: no disponible en 2020 (Casen en Pandemia
  usó un cuestionario reducido, sin esta pregunta).

El dashboard incluye 3 pestañas construidas sobre este panel:
trayectoria de una ocupación específica (con buscador), recomposición
estructural del empleo entre grandes grupos, y ranking dinámico de
ocupaciones que más crecieron/cayeron entre 2020 y 2024.

## Hoja de ruta / próximos módulos

Este catálogo está pensado para crecer de forma incremental. Ideas para
próximas sesiones, en orden sugerido:

1. **Series históricas de encuestas de hogares**: sumar CASEN de años
   anteriores (2022, 2020, 2017…) y ENE (Encuesta Nacional de Empleo,
   trimestral, INE) para construir series de tiempo por ocupación desde
   idealmente 1990 (las clasificaciones anteriores a 2020 usan CIUO-88,
   por lo que se necesitará una tabla de correspondencia CIUO-88 ↔
   CIUO-08.CL — el INE la publica).
2. **Fuentes administrativas**: registros de cotizaciones (Previred/
   Superintendencia de Pensiones), remuneraciones (SII), contratos
   (Dirección del Trabajo) — todas requieren solicitud de acceso a datos,
   a diferencia de CASEN que es de descarga pública.
3. **Estudios cualitativos**: notas técnicas, informes sectoriales,
   estudios de brechas de habilidades por ocupación — se pueden vincular
   como texto/PDF asociado a cada código CIUO-08.CL en una tabla
   `data/reference/estudios_cualitativos.csv`.
4. **Base de datos persistente**: si el catálogo crece en volumen, migrar
   `data/processed/` a DuckDB para consultas más eficientes entre fuentes.
5. **Publicación del dashboard**: si se quiere compartir online (no solo
   como archivo HTML), evaluar Quarto Pub, GitHub Pages o Posit Connect.

## Notas de calidad de datos

- Los 19 códigos CIUO-08.CL con desagregación exclusivamente chilena
  (p. ej. 3611/3612 "Técnicos en educación parvularia/diferencial", 2134
  "Bioquímicos", 2147 "Ingenieros biomédicos") están correctamente
  incorporados gracias a usar la fuente oficial del INE en vez del
  índice internacional CIUO-08.
- Los valores -88, -99 y -66 de CASEN (no sabe/no aplica/no responde) se
  tratan como missing (`NA`) en todas las variables numéricas relevantes.
