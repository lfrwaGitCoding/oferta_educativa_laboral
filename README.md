[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Python CI](https://github.com/antoniojbt/oferta_educativa_laboral/actions/workflows/python-tests.yaml/badge.svg?branch=main)](https://github.com/antoniojbt/oferta_educativa_laboral/actions/workflows/python-tests.yaml)
[![R](https://github.com/antoniojbt/oferta_educativa_laboral/actions/workflows/r-tests.yml/badge.svg)](https://github.com/antoniojbt/oferta_educativa_laboral/actions/workflows/r-tests.yml)
[![codecov](https://codecov.io/gh/AntonioJBT/oferta_educativa_laboral/branch/master/graph/badge.svg)](https://codecov.io/gh/antoniojbt/oferta_educativa_laboral/graphs/tree.svg?token=Z1CCVHIERZ)

# oferta_educativa_laboral

Este proyecto contiene scripts y un pipeline basado en **Ruffus/CGATCore** para analizar datos del **Sistema de Administración del Personal (SIAP)** del IMSS.
El pipeline limpia las tablas exportadas desde Microsoft Access, genera estadísticas descriptivas y produce informes en PDF con **Quarto**.

## Objetivos y productos

- Convierte las tablas de Access del SIAP a archivos CSV ordenados (mdb-tools)
- Elimina duplicados, adjudica tipos de columnas, realiza controles de calidad (scripts en R)
- Calcula estadísticas descriptivas y crea visualizaciones para cada variable
- Construye un informe en PDF (Quarto)
- Los archivos de salida (tablas, figuras e informe final) se escriben en el directorio `results/`, con subcarpetas nombradas por fecha y archivo de entrada.

## Instalación

### R (≥ 4.1) y los paquetes requeridos:

```r
install.packages(c("data.table", "tidyverse", "episcout", "skimr", "log4r"))
```

### Python 3 con `ruffus` y `cgatcore` para el pipeline:

```bash
pip install ruffus cgatcore
```

También se puede usar un entorno conda; el pipeline guardará los detalles del entorno al ejecutarse. Para una instalación ligera con sólo las herramientas de Python:

```bash
pip install -r requirements.txt
```

### Quarto si deseas generar el informe en PDF

Ver [Quarto](https://quarto.org/docs/get-started/)

## Conda

Para crear un entorno conda con todas las dependencias de Python y R:

```bash
conda env create -f environment.yml
conda activate oferta_educativa_laboral
```

## Estructura del proyecto

```
oferta_educativa_laboral/
├── scripts/      # Scripts en R para limpieza y análisis
├── pipeline/     # Pipeline en Python usando Ruffus
├── report/       # Informe en Quarto y funciones auxiliares en R
└── tests/        # Pruebas con pytest y testthat
```

## Recursos externos

Las fuentes tipográficas usadas en el informe PDF se almacenan en `report/resources/fonts`. Si esa carpeta está vacía, ejecuta:

```bash
cd oferta_educativa_laboral/report/resources/fonts
wget -qO- https://github.com/notofonts/noto-cjk/archive/refs/heads/main.zip | \
  bsdtar -xvf- --strip-components=1
```

Los scripts auxiliares adicionales que se referencian desde el pipeline deben colocarse en `scripts/` y configurarse en `pipeline.yml`.

## Preparación de datos

Las tablas originales del SIAP **no están incluidas** en este repositorio. Deben colocarse en:

```
project_root/data/data_UP/raw/
```

Ejemplos de archivos esperados en los scripts:  
`Qna_17_Plantilla_2024.csv`, `Qna_17_Bienestar_2024.csv`

### Estructura esperada del proyecto con datos originales:

```
project_root/
├── data/ # inputs
│   └── data_UP/raw/
│   └── data_UP/processed/
│   └── for_reuse_and_processed/datos_abiertos # outputs a inputs
│   └── for_reuse_and_processed/internal_IMSS
│   └── for_reuse_and_processed/internal_UEI
│   └── data_CES/
├── results/                    # outputs
└── oferta_educativa_laboral/   # código fuente
```

## Ejemplo de uso de un script individual en R

Puedes ejecutar los pasos de limpieza individualmente:

```bash
Rscript oferta_educativa_laboral/scripts/descriptive/2_clean_dups_col_types.R \
  Qna_17_Plantilla_2024.csv results/cleaned_Q17.rdata.gzip
```

Modifica los nombres de archivo según los nuevos datos.

## Ejecutar el pipeline

Desde la carpeta del pipeline:

```bash
cd oferta_educativa_laboral/pipeline
python pipeline_oferta_laboral.py --help         # lista de tareas
python pipeline_oferta_laboral.py make full -v5  # ejecuta todos los pasos
```

Puedes ejecutar tareas individuales reemplazando `full` con el nombre de la tarea deseada.

## Generar informes

La tarea `full` ejecuta y renderiza automáticamente `report/SIAP_desc_stats.qmd` con **Quarto**.  
También puedes renderizarlo manualmente desde el directorio `report/`:

```bash
cd ../report
quarto render SIAP_desc_stats.qmd
```

El PDF renderizado se guarda en la carpeta `_report_outputs/`.

### Ejemplo con parámetros:

```bash
quarto render SIAP_desc_stats.qmd \
  --to pdf --execute
```

## Rendimiento

Consulta el archivo `PERFORMANCE.md` para recomendaciones sobre cómo optimizar los scripts más intensivos si es necesario.
