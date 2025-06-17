# Pipeline Oferta Laboral

Este directorio contiene el pipeline principal `pipeline_oferta_laboral.py` escrito con **Ruffus** y **CGATCore** para procesar las tablas del SIAP. Las tablas de Access deben convertirse a CSV antes de ejecutar el pipeline.

## Estructura general
- `pipeline_oferta_laboral.py`: define las tareas del pipeline.
- `scripts/accdb_to_csv_encodings_copy.sh`: convierte las bases de datos `.accdb` a archivos CSV en UTF‑8.
- `configuration/pipeline.yml`: archivo de configuración con rutas y opciones.

## Entradas esperadas
Las tablas originales del SIAP (por ejemplo `Qna_17_Plantilla_2024.csv`) deben estar en:

```
project_root/data/data_UP/raw/
```

## Tareas principales
Las funciones definidas en `pipeline_oferta_laboral.py` siguen esta secuencia:

1. **convert_to_csv** – convierte las tablas de Access a CSV.
2. **run_tables_check** / **run_1b_accdb_tables_check** – ejecuta el script de control `1b_accdb_tables_check.R`.
3. **countWords** – proceso de ejemplo sobre la salida anterior.
4. **loadWordCounts** – carga la información procesada en la base de datos.
5. **make_report** – genera el informe en `pipeline_report/`.
6. **conda_info** – guarda la información del entorno conda.
7. **full** – marca la finalización del pipeline.

## Diagrama del flujo
```
Access .accdb
    |
    v
accdb_to_csv_encodings_copy.sh
    |
    v
results/dummy.csv
    |
    v
run_tables_check
    |
    v
countWords
    |
    v
loadWordCounts
    |
    v
make_report -> pipeline_report/
    |
    v
conda_info.txt, pipeline_complete.touch
```

## Cómo ejecutar
Desde esta carpeta ejecuta:

```bash
python pipeline_oferta_laboral.py --help
python pipeline_oferta_laboral.py make full -v5
```

Los resultados intermedios se escriben en `results/` y el informe final en `pipeline_report/`.
