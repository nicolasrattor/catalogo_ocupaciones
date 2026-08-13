#!/usr/bin/env python3
"""
00d_convert_enadel_parquet.py
----------------------------------------------------------------------------
Convierte el parquet de la Encuesta de Demanda Laboral (ENADEL) 2025 a un
CSV filtrado con las columnas necesarias para el módulo de demanda laboral
del catálogo: identificación, ponderador, y los hasta 7 bloques de
cargo/ocupación con vacante reportados por cada empresa (módulo C1 del
cuestionario ENADEL 2025).

Uso:
    python3 00d_convert_enadel_parquet.py <entrada.parquet> <salida.csv>
----------------------------------------------------------------------------
"""
import sys
import pandas as pd

BASE_VARS = ["idempresa_ficticio", "pond_final", "varunit", "varstrat", "fpc"]

def slot_vars(i):
    return [
        f"c1_Codigo_CIUO_{i}", f"c1_glosa_CIUO_{i}",
        f"c1_c_{i}", f"c1_d_{i}", f"c1_e_{i}", f"c1_f_{i}", f"c1_h_{i}",
        f"c1_g_dif1_{i}", f"c1_g_dif2_{i}", f"c1_g_dif3_{i}", f"c1_g_dif4_{i}", f"c1_g_dif5_{i}",
    ]

def slot_vars_c3(i):
    return [f"c3_Codigo_CIUO_{i}", f"c3_glosa_CIUO_{i}", f"c3_Puesto_Trabajo_{i}", f"c3_c_{i}", f"c3_d_{i}"]

VARS_DESEADAS = BASE_VARS.copy()
for i in range(1, 8):
    VARS_DESEADAS += slot_vars(i)
for i in range(1, 4):
    VARS_DESEADAS += slot_vars_c3(i)

def main():
    if len(sys.argv) != 3:
        print(f"Uso: {sys.argv[0]} <entrada.parquet> <salida.csv>")
        sys.exit(1)

    src, dst = sys.argv[1], sys.argv[2]
    import pyarrow.parquet as pq
    disponibles = pq.ParquetFile(src).schema.names
    cols = [c for c in VARS_DESEADAS if c in disponibles]
    faltantes = [c for c in VARS_DESEADAS if c not in disponibles]

    df = pd.read_parquet(src, columns=cols)
    df.to_csv(dst, index=False)
    print(f"OK: {len(df):,} filas, {len(df.columns)} columnas -> {dst}")
    if faltantes:
        print(f"(no presentes: {', '.join(faltantes)})")

if __name__ == "__main__":
    main()
