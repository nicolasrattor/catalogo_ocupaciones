#!/usr/bin/env python3
"""
00_convert_casen_parquet.py (v2, multianual)
----------------------------------------------------------------------------
Convierte un parquet de CASEN (cualquier año) a CSV, seleccionando el
superset de variables usadas por el catálogo a través de los años. Si una
columna no existe en un año dado (p. ej. 'o10' no existe en Casen en
Pandemia 2020), simplemente se omite del CSV de salida -- la armonización
entre años se resuelve después, en R (02_procesar_casen_multianual.R).

Uso:
    python3 00_convert_casen_parquet.py <entrada.parquet> <salida.csv>
----------------------------------------------------------------------------
"""
import sys
import pandas as pd
import pyarrow.parquet as pq

# Superset de variables relevantes en cualquier año (2020, 2022, 2024...)
VARS_DESEADAS = [
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
    "pobreza", "pobreza_multi",
]

def main():
    if len(sys.argv) != 3:
        print(f"Uso: {sys.argv[0]} <entrada.parquet> <salida.csv>")
        sys.exit(1)

    src, dst = sys.argv[1], sys.argv[2]
    disponibles = pq.ParquetFile(src).schema.names
    cols = [c for c in VARS_DESEADAS if c in disponibles]
    faltantes = [c for c in VARS_DESEADAS if c not in disponibles]

    df = pd.read_parquet(src, columns=cols)
    df.to_csv(dst, index=False)
    print(f"OK: {len(df):,} filas, {len(df.columns)} columnas -> {dst}")
    if faltantes:
        print(f"(no presentes en esta base, se omiten: {', '.join(faltantes)})")

if __name__ == "__main__":
    main()
