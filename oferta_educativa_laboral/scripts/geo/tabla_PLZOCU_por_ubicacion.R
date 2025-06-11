# ////////////
# Script information ----

# SIAP
# Abril 2025
# tabla con numero de meds especialistas por localidad, OOAD, etc. para cruzar con coordenadas
# usar dependencia, descrip_localidad, OOAD, IP, para identificar y cruzar
# Input is rdata output from script:
  # 2_dups_col_types.R
  # # SIAP con todas las columnas

# Output:
  # tabla de frecuencias de med esp (area resp) por ubicacion, coordenadas, etc.
  # Output: tabla de frecuencias ubicacion, plaza ocupada/vacante meds, etc.
# ////////////


# ////////////
# Import libraries ----
library(data.table)
library(episcout)
library(tidyverse)
# ////////////


# ////////////
# Set working directory to the project root  ----
setwd(here::here())
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts
getwd()
# ////////////


# ////////////
# Load rdata file ----

# ===
rdata_dir <- 'data/data_UP/access_SIAP_18092024/processed/old/'

# TO DO: Manually set:
# infile <- '2_clean_dups_col_types_Qna_17_Bienestar_2024.all_columns.rdata.gzip'
infile <- '2_clean_dups_col_types_Qna_17_Plantilla_2024.all_columns.rdata.gzip'

# Full path and file name:
infile_path <- paste0(rdata_dir, infile)
print(infile_path)

print(dir(path = normalizePath(rdata_dir), all.files = TRUE))

load(infile_path)
ls()
# ===

# ===
# Get rid of RStudio warnings for loaded objects:
project_root <- project_root
data_dir <- data_dir
results_dir <- results_dir
data_f <- data_f
code_dir <- code_dir

all_colnames <- all_colnames
char_cols <- char_cols
date_cols <- date_cols
fact_cols <- fact_cols
int_cols <- int_cols
id_cols <- id_cols
num_cols <- num_cols

print(project_root)
setwd(here::here())
getwd()
print(dir(path = normalizePath(project_root), all.files = TRUE))
# ===
# ////////////


# ////////////
# Source functions/scripts/etc
# TO DO:
# Source (until I update episcout)
source(file.path(paste0(code_dir, '/scripts/funcs_epi_source.R')))
# ////////////


# ////////////
# Output dir, based on today's date ----
infile_prefix <- strsplit(infile, "\\.")[[1]][1]
results_subdir <- sprintf('%s_%s',
                          format(Sys.Date(), '%d_%m_%Y'),
                          infile_prefix
                          )
results_subdir
results_subdir <- epi_create_dir(base_path = results_dir,
                                 subdir = results_subdir
                                 )
# ////////////



# ////////////
# Check column types ----

# ===
# Check loaded cols exist:
# stopifnot(FALSE)  # This will throw an error
stopifnot(length(all_colnames) == (ncol(data_f)))

if (!interactive()) {
    info(logger, "Expected columns match file")
    error(logger, "Expected columns do not match file")
}

colnames(data_f)

stopifnot(all(all_colnames %in% colnames(data_f)))
setdiff(as.character(all_colnames), as.character(colnames(data_f)))
# ===

# ===
# Check all column types accounted
dim(data_f)
epi_clean_count_classes(df = data_f)
# Looks good
# ===
# ////////////


# ////////////
# Output dir, based on today's date ----
script_n <- 'CUUMS_area_resp_SIAP'
infile_prefix <- strsplit(infile, "\\.")[[1]][1]
results_subdir <- sprintf('%s_%s',
                          format(Sys.Date(), '%d_%m_%Y'),
                          infile_prefix
)
results_subdir
results_subdir <- epi_create_dir(base_path = results_dir,
                                 subdir = results_subdir
)
# ////////////


# ////////////
# ===
#### Check other vars vs PLZOCUP ----
summary(data_f$SEXO)
summary(data_f$NOMBREAR)
summary(data_f$DELEGACION)
length(unique(data_f$DELEGACION))

summary(data_f$DESCRIP_LOCALIDAD)
length(unique(data_f$DESCRIP_LOCALIDAD))

summary(data_f$DEPENDENCIA)
length(unique(data_f$DEPENDENCIA))

# Keep only relevant columns for location:
colnames(data_f)
cols_keep <- c('IP',
               'DEPENDENCIA',
               'DESCRIP_LOCALIDAD',
               'DELEGACION',
               'NUMDEL',
               'CLASIF_UNIDAD',
               "ADSCRIPCION",
               'CVEADSC',
               "CVEUNI",
               'DESCRIPCION_SERVICIO',
               'CVELOC',
               'REGIMEN',
               # 'CLAVEAR',
               # 'NOMBREAR',
               'CVEZONABT',
               'ZONABT'
               # 'CVEAR'
               )

col_to_filter <- "DEPENDENCIA" # so that tidyverse will accept it

# 1) first occurrence of each key:
keep_rows <- !duplicated(data_f[[col_to_filter]])

# 2) subset both rows and the chosen columns
df_dependencias <- data_f[keep_rows, cols_keep, drop = FALSE]

epi_head_and_tail(df_dependencias)
colnames(df_dependencias)


dep_var <- "PLZOCU"
# ind_vars <- c("DEPENDENCIA", "DESCRIP_LOCALIDAD", "DELEGACION")
ind_vars <- c("DEPENDENCIA")
df_result <- epi_stats_table(
    df = data_f,
    dep_var = dep_var,
    ind_vars = ind_vars
    )
epi_head_and_tail(df_result, cols = ncol(df_result))
# ===

# ===
# Merge data frames, save, then pass to other script to merge with coordinates:
col_merge <- "DEPENDENCIA"
# Number of actual matches by presupuesto / IP:
length(intersect(df_dependencias[[col_merge]], df_result[[col_merge]]))
length(unique(data_f[[col_merge]]))


df_dependencias_PLZOCU <- full_join(df_dependencias, df_result, by = col_merge)
epi_head_and_tail(df_dependencias_PLZOCU)
epi_head_and_tail(df_dependencias_PLZOCU, last_cols = TRUE)

length(unique(df_dependencias_PLZOCU[[col_merge]]))
length(which(is.na(df_dependencias_PLZOCU[[col_merge]])))
# ===

# ===
# Save:
# file_n <- 'coordenadas_unidades_medicas_bienestar_SIAP_alberto'
file_n <- 'table_PLZOCU_DEPENDENCIA_cols_geo'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   paste0(results_subdir),
                   file_n,
                   suffix
                   )
outfile
epi_write(file_object = df_dependencias_PLZOCU,
          file_name = outfile
          )
# ===
# ////////////


# ////////////
# The end ----
# Outputs saved to disk, no need to save as rdata.
sessionInfo()

# Closing message loggers:
if (!interactive()) { # TRUE if not interactive, will then log output
    info(logger, "Script completed successfully")

    # Close screen output log (both screen and warnings/error messages):
    # Stop sinks
    sink(type = "message")
    close(sink_msg)  # Close the connection
    sink()
}

# q()
# ////////////
