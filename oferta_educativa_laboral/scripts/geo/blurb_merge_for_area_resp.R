# ////////////
# Script information ----

# SIAP
# Abril 2025
# Numero de medicos familiares por unidad, cruce con coordenadas
# Input:
  # coordenadas de Mateo DPTI (CUUMS Dic 2024)
  # SIAP con todas las columnas
  # tabla con numero de meds especialistas por localidad, OOAD, etc.
# Output:
  # tabla de frecuencias de med esp (area resp) por ubicacion, coordenadas, etc.
  # mapas de Mexico de calor
# ////////////


# ////////////
# vars needed:
# DESCRIP_LOCALIDAD
# area resp
# NUMDEL
# DELEGACION
# ADSCRIPCION
# CVEUNI
# DEPENDENCIA
# IP
# CVELOC
# CLAVEAR
# NOMBREAR
# CVEZONABT
# ZONABT
# CVEAR
# long
# lat
# todo CUUMS
# ////////////


# ////////////
# Import libraries ----
library(data.table)
library(episcout)
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
# ////////////


# ////////////
# Set working directory to the project root  ----
setwd("~/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/")
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts
getwd()
# ////////////


# ////////////
# Load rdata file ----

# ===
# rdata_dir <- 'data/data_UP/access_SIAP_18092024/processed/'
rdata_dir <- 'data/data_UP/access_SIAP_18092024/processed/old/'

# TO DO: Manually set:
# infile <- '2_clean_dups_col_types_Qna_17_Bienestar_2024.all_columns.rdata.gzip'
infile <- '2_clean_dups_col_types_Qna_17_Plantilla_2024.all_columns.rdata.gzip'
# ===

# ===
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

all_colnames <- all_colnames
char_cols <- char_cols
date_cols <- date_cols
fact_cols <- fact_cols
int_cols <- int_cols
id_cols <- id_cols
num_cols <- num_cols

print(project_root)
setwd(project_root)
getwd()
print(dir(path = normalizePath(project_root), all.files = TRUE))
# ===
# ////////////


# ////////////
# Source functions/scripts/etc ----
# TO DO:
# Source (until I update episcout)
source(file.path(paste0(code_dir, '/scripts/funcs_epi_source.R')))
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
# Load tsv files ----

# ===
# # TO DO: Manually set:
# # coords <- "/Users/antoniob/Documents/work/science/devel/github/med-comp-imss/geo_stats/data/CUUMS_Dic_2024_mod_for_R.tsv"
#
# # Full path and file name:
# # infile_path <- paste0(results_dir, coords)
# # infile_path <- paste0(data_dir, coords)
# # print(infile_path)
#
# # File has sep | and quotes for each entire row but not for headers:
# coords <- epi_read(coords, quote = "", sep = "|")
# epi_head_and_tail(coords)
# epi_head_and_tail(coords, last_cols = T)
# colnames(coords)
# # View(coords)
# ===

# # ===
# # TO DO: Manually set:
# esp_med_nums <- '/31_12_2024_2_clean_dups_col_types_Qna_17_Bienestar_2024/IP_NOMBREAR.txt'
# # esp_med_nums <- '/31_12_2024_2_clean_dups_col_types_Qna_17_Plantilla_2024/IP_NOMBREAR.txt'
# # Full path and file name:
# infile_path <- paste0(results_dir, esp_med_nums)
# print(infile_path)
# esp_med_nums <- epi_read(infile_path)
# epi_head_and_tail(esp_med_nums)
# # View(esp_med_nums)
# # ===
# ////////////


# ////////////
# Set up columns for merging ----

# ===
# TO DO: Manually set:
clave_presup <- 'presup' # if using original file from Alberto

colnames(coords)
coords[[clave_presup]] <- as.factor(coords[[clave_presup]])
summary(coords[[clave_presup]])
length(unique(coords[[clave_presup]]))

esp_med_nums$IP <- as.factor(esp_med_nums$IP)
summary(esp_med_nums$IP)
length(unique(esp_med_nums$IP))
# ===


# ===
length(intersect(unique(coords[[clave_presup]]), unique(esp_med_nums$IP)))
dim(esp_med_nums)
head(intersect(unique(coords[[clave_presup]]), unique(esp_med_nums$IP)))
# Most of the IP values match for those present in the SIAP dataset

length(setdiff(unique(coords[[clave_presup]]), unique(esp_med_nums$IP)))
head(setdiff(unique(coords[[clave_presup]]), unique(esp_med_nums$IP)))
# ===
# ////////////


# ////////////
# Merge files ----
coords_esp_med_nums_df <- coords %>% # so that matches are based on Alberto's file
    inner_join(esp_med_nums,
               by = join_by(!!sym(clave_presup) == "IP")
    )

dim(coords_esp_med_nums_df)
epi_head_and_tail(coords_esp_med_nums_df)
colnames(coords_esp_med_nums_df)
# View(coords_esp_med_nums_df)
# ////////////


# ////////////
# Save final merged file ----

# TO DO: Manually set:
infile_prefix <- '/31_12_2024_2_clean_dups_col_types_Qna_17_Bienestar_2024/'
# infile_prefix <- '/31_12_2024_2_clean_dups_col_types_Qna_17_Plantilla_2024/'

file_n <- 'coords_esp_med_nums'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s/%s.%s',
                   results_dir,
                   infile_prefix,
                   file_n,
                   suffix
)
outfile
epi_write(file_object = coords_esp_med_nums_df,
          file_name = outfile
)
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
