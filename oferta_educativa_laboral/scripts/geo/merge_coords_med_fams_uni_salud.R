# ////////////
# Script information ----

# SIAP
# Universidad de la Salud - Medicina rural
# Diciembre 2024
# Numero de medicos familiares por unidad, cruce con coordenadas
# Input son archivos con coordenadas de Alberto R y tabla con numero de meds especialistas, scripts:
# med_fams_uni_salud.R
# coords_unidades_medicas.R

# Output: tabla de frecuencias de med fam por unidad con IP (clave presupuestal) (unidad? y coordenadas)
# ////////////


# ////////////
# Import libraries ----
library(data.table)
library(episcout)
library(tidyverse)
# ////////////


# ////////////
# Load rdata file with directory locations ----

# ===
# Set working directory to the project root:
setwd(here::here())
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts
getwd()

# Directory locations ----

project_root <- here::here()
data_dir <- file.path(project_root, 'data')
results_dir <- file.path(project_root, 'results')
code_dir <- file.path(project_root, 'oferta_educativa_laboral')
# ===

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
# Load tsv files ----

# ===
# TO DO: Manually set:
# coords <- '/coords_unidades_medicas/coordenadas_unidades_medicas_bienestar_SIAP_alberto.txt'
# coords <- '/coords_unidades_medicas/coordenadas_unidades_medicas_plantilla_SIAP_alberto.txt'
coords <- '/external/coordenadas_ordinario_y_bienestar_alberto.csv'

# Full path and file name:
# infile_path <- paste0(results_dir, coords)
infile_path <- paste0(data_dir, coords)
print(infile_path)
coords <- epi_read(infile_path)
epi_head_and_tail(coords)
# View(coords)

# TO DO: Manually set:
esp_med_nums <- '/31_12_2024_2_clean_dups_col_types_Qna_17_Bienestar_2024/IP_NOMBREAR.txt'
# esp_med_nums <- '/31_12_2024_2_clean_dups_col_types_Qna_17_Plantilla_2024/IP_NOMBREAR.txt'
# Full path and file name:
infile_path <- paste0(results_dir, esp_med_nums)
print(infile_path)
esp_med_nums <- epi_read(infile_path)
epi_head_and_tail(esp_med_nums)
# View(esp_med_nums)
# ===
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
