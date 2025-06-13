[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Python CI](https://github.com/antoniojbt/oferta_educativa_laboral/actions/workflows/python-tests.yaml/badge.svg?branch=main)](https://github.com/antoniojbt/oferta_educativa_laboral/actions/workflows/python-tests.yaml)
[![R](https://github.com/antoniojbt/oferta_educativa_laboral/actions/workflows/R-CI.yml/badge.svg)](https://github.com/antoniojbt/oferta_educativa_laboral/actions/workflows/R-CI.yml)
[![codecov](https://codecov.io/gh/AntonioJBT/oferta_educativa_laboral/branch/master/graph/badge.svg)](https://codecov.io/gh/AntonioJBT/oferta_educativa_laboral)


# oferta_educativa_laboral

This project contains scripts and a Ruffus/CGATCore pipeline used to analyse workforce databases from the *Sistema de Administración del Personal* (SIAP) of IMSS. The pipeline cleans the raw tables exported from Microsoft Access, generates summary statistics and produces PDF reports with Quarto.

## Goals and outputs
* Convert the SIAP Access tables into tidy CSV files.
* Remove duplicates, fix column types and perform basic quality checks.
* Compute descriptive statistics and create visualisations.
* Build a PDF report describing each dataset.

Output files (tables, figures and the final report) are written to the `results/` directory with subfolders named by date and input file.

## Installation
1. Install [R](https://cran.r-project.org/) (\>=4.1) and the packages used by the scripts:
   ```R
   install.packages(c("data.table", "tidyverse", "episcout", "skimr", "log4r"))
   ```
2. Install Python 3 with `ruffus` and `cgatcore` for the pipeline:
    ```bash
    pip install ruffus cgatcore
    ```
    A conda environment also works; the pipeline will save environment details when run.
    For a lightweight setup containing only Python tools you can instead:
    ```bash
    pip install -r requirements.txt
    ```
3. Install [Quarto](https://quarto.org/) if you want to render the PDF report.

## Conda

Create a conda environment with all Python and R dependencies using the
provided `environment.yml`:

```bash
conda env create -f environment.yml
conda activate oferta_educativa_laboral
```

## Project structure

```
oferta_educativa_laboral/
├── scripts/      # R data cleaning and analysis scripts
├── pipeline/     # Ruffus pipeline in Python
├── report/       # Quarto report and helper R functions
└── tests/        # pytest and testthat tests
```

Each script directory has additional README notes describing expected inputs.

### External resources
Fonts used in the PDF report are stored under `report/resources/fonts`. If the
directory is empty run:

```bash
cd oferta_educativa_laboral/report/resources/fonts
wget -qO- https://github.com/notofonts/noto-cjk/archive/refs/heads/main.zip | \
  bsdtar -xvf- --strip-components=1
```

Additional helper scripts referenced by the pipeline should be placed in
`scripts/` and configured via `pipeline.yml` rather than using personal paths.



## Data preparation
The raw SIAP tables are not included in this repository. Place them under:
```
project_root/data/data_UP/access_SIAP_18092024/processed/
```
Example files referenced in the scripts are `Qna_17_Plantilla_2024.csv` and `Qna_07_Plantilla_2025.csv`.

Before running the pipeline create the directory description file by executing:
```R
Rscript pipeline/scripts/1_dir_locations.R
```
This writes `dir_locations.rdata.gzip` to the processed data directory which other R scripts load.

Expected top-level layout after placing data:
```
project_root/
├── data/
│   └── data_UP/access_SIAP_18092024/processed/
├── results/            # generated outputs
└── oferta_educativa_laboral/
```

### Example R script usage
After generating `dir_locations.rdata.gzip`, run the cleaning steps individually:

```bash
Rscript oferta_educativa_laboral/scripts/descriptive/2_clean_dups_col_types.R \
  Qna_17_Plantilla_2024.csv results/cleaned_Q17.rdata.gzip
```

Modify the input file names as needed for your dataset.

## Running the pipeline
From the `pipeline` folder run:
```bash
cd oferta_educativa_laboral/pipeline
python pipeline_oferta_laboral.py --help              # list tasks
python pipeline_oferta_laboral.py make full -v5       # execute all steps
```
Individual tasks can be run by replacing `full` with the task name.

## Generating reports
The `full` target renders `report/SIAP_desc_stats.qmd` with Quarto automatically.
You can also render manually from the `report` directory:
```bash
cd ../report
quarto render SIAP_desc_stats.qmd
```
The rendered PDF will appear in `_report_outputs/`.

Example invocation with parameters:

```bash
quarto render SIAP_desc_stats.qmd \
  --to pdf --execute
```

## Performance
See `PERFORMANCE.md` for notes on optimising the more intensive scripts.
