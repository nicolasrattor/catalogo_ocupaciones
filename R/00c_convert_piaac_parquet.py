#!/usr/bin/env python3
"""
00c_convert_piaac_parquet.py
----------------------------------------------------------------------------
Convierte el parquet de PIAAC Chile (Ciclo 2) a un CSV filtrado con las
columnas necesarias para el módulo de habilidades del catálogo:
  - ISCO08_C: código de ocupación (ISCO-08 / CIUO-08, 4 dígitos)
  - ISCO1C, ISCO2C: código de ocupación a 1 y 2 dígitos (ya provistos por
    el propio archivo PIAAC, evita tener que truncar strings a mano)
  - PVLIT1-10, PVNUM1-10: valores plausibles de literacidad y numeracidad
    (metodología estándar PIAAC/PISA: 10 valores plausibles por persona)
  - NUMWORKC2, READWORKC2_T1, WRITWORKC2, ICTWORKC2: escalas de uso de
    habilidades en el trabajo (numeracidad, lectura, escritura, TIC)
  - SPFWT0: ponderador muestral final
  - AGE_R, GENDER_R: variables demográficas de apoyo

Uso:
    python3 00c_convert_piaac_parquet.py <entrada.parquet> <salida.csv>
----------------------------------------------------------------------------
"""
import sys
import pandas as pd

VARS_DESEADAS = [
    "ISCO08_C", "ISCO1C", "ISCO2C",
    "PVLIT1", "PVLIT2", "PVLIT3", "PVLIT4", "PVLIT5",
    "PVLIT6", "PVLIT7", "PVLIT8", "PVLIT9", "PVLIT10",
    "PVNUM1", "PVNUM2", "PVNUM3", "PVNUM4", "PVNUM5",
    "PVNUM6", "PVNUM7", "PVNUM8", "PVNUM9", "PVNUM10",
    "NUMWORKC2", "READWORKC2_T1", "WRITWORKC2", "ICTWORKC2",
    "SPFWT0", "AGE_R", "GENDER_R",
]

def main():
    if len(sys.argv) != 3:
        print(f"Uso: {sys.argv[0]} <entrada.parquet> <salida.csv>")
        sys.exit(1)

    src, dst = sys.argv[1], sys.argv[2]
    df = pd.read_parquet(src, columns=VARS_DESEADAS)
    df.to_csv(dst, index=False)
    print(f"OK: {len(df):,} filas, {len(df.columns)} columnas -> {dst}")

if __name__ == "__main__":
    main()
