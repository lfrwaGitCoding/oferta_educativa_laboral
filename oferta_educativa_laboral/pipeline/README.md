# Pipeline Oferta Laboral

Este directorio contiene el pipeline principal `pipeline_oferta_laboral.py` escrito con **Ruffus** y **CGATCore** para procesar las tablas del SIAP.

## Estructura general
- `pipeline_oferta_laboral.py`: define las tareas del pipeline
- `scripts/accdb_to_csv_encodings_copy.sh`: convierte las bases de datos `.accdb` a archivos CSV en UTF‑8
- `configuration/pipeline.yml`: archivo de configuración con rutas y opciones

## Inputs esperados
Las tablas originales del SIAP (por ejemplo `Qna_17_Plantilla_2024.csv`) deben estar en p.ej.:

```
project_root/data/data_UP/raw/
```

## Outputs esperados
- Archivos intermedios rdata en data/data_UP/processed/
- Gráficas y tablas en results/ (subfolders por fecha y tabla input)
- Reporte PDF final en reporte/_report_outputs/.
- Logs (conda package lists, detalles del environment)

Los resultados intermedios se escriben en `results/` y el informe final en `pipeline_report/`


## Tareas principales
Las funciones definidas en `pipeline_oferta_laboral.py` siguen esta secuencia:

1. **convert_to_csv** – convierte las tablas de Access a CSV.
2. **run_tables_check** / **run_1b_accdb_tables_check** – ejecuta el script de control `1b_accdb_tables_check.R`.
3. **xxx** – xxx
4. **xxx** – xxx
5. **make_report** – genera el informe en `pipeline_report/`.
6. **conda_info** – guarda la información del entorno conda.
7. **full** – marca la finalización del pipeline.

## Diagrama del flujo
```
Raw MS Access data (.accdb)         → accdb_to_csv_encodings_copy.sh
                                     ↳ CSV tables (Qna_XX_*.csv)
          │
          ▼
1_dir_locations.R                    → dir_locations.rdata.gzip
          │
          ▼
1b_accdb_tables_check.R              → *_tables_check.rdata.gzip
          │
          ▼
2_clean_dups_col_types.R             → 2_clean_dups_col_types_<prefix>.rdata.gzip
          │
          ▼
(Optional: 2b_clean_subset.R / 2c_subset_PLZOCU.R)
          │
          ▼
3_explo.R                            → plots & summaries
          │
          ▼
4_bivar.R                            → additional plots/tables
          │
          ▼
Quarto (SIAP_desc_stats.qmd)         → PDF in report/_report_outputs/
          │
          ▼
conda_info + pipeline_complete.touch

```

## Cómo ejecutar
Desde esta carpeta ejecuta:

```bash
python pipeline_oferta_laboral.py --help
python pipeline_oferta_laboral.py make full -v5
```


