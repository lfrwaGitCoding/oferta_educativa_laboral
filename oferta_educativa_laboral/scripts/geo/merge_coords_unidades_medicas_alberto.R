# ////////////
# Script information ----

# Coordinadas de unidades medicas de Alberto R. cruzadas con datos del SIAP
# Universidad de la Salud - Medicina rural
# Diciembre 2024
# Input is rdata output from script:
#

# Output: tabla de frecuencias de med fam por unidad y plaza ocupada/vacante
# ////////////


# ////////////
# Global options ----
# options(error = stop)
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
setwd(here::here())
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
script_n <- 'coords_unidades_medicas'
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


# # ////////////
# # Capture output / log ----
#
# # ===
# # Redirect standard output
# if (!interactive()) { # TRUE if not interactive, will then log output
#     script_n <- '2_clean_dups_col_types'
#     sink_stdout <- paste0(results_subdir, '/', script_n, '.sink_stdout.log')
#     sink(sink_stdout, split = TRUE)
#
#     # Redirect messages and warnings
#     sink_msg <- file(paste0(results_subdir, '/', script_n, '.sink_msg.log'), open = "wt")
#     sink(sink_msg, type = "message")
#
#     # Example outputs
#     cat("Test: This is standard output.\n")
#     message("Test: This is a message.")
#     warning("Test: This is a warning.")
#     }
# # ===
#
# # ===
# # Create a logger
# if (!interactive()) { # TRUE if not interactive, will then log output
#     logger <- create.logger()
#     log_n <- paste0(results_subdir, '/', script_n, '.log4r.log')
#     logfile(logger) <- log_n # Log file location
#     level(logger) <- "INFO"  # Set logging level (DEBUG, INFO, WARN, ERROR)
#
#     # Add log messages
#     # info(logger, "Script started")
#     # debug(logger, "This is a debug message")
#     # warn(logger, "This is a warning")
#     # error(logger, "This is an error")
#     }
# # ////////////



# ////////////
# Listado de unidades medicas ----

# ===
colnames(data_f)
dim(data_f)

summary(data_f$NOMBREAR)
summary(data_f$ADSCRIPCION)
summary(data_f$DELEGACION)
summary(data_f$CLASIF_UNIDAD)
# ===

# ===
# Unidades medicas usando clave presupuestal:
typeof(data_f$IP)
str(data_f$IP)
head(unique(data_f$IP))
summary(data_f$IP)

# Get dataframe where IP has unique values but keep all columns (selects first occurrence):
unique_IP_df <- data_f %>%
  distinct(IP, .keep_all = TRUE)

dim(unique_IP_df)
length(unique(unique_IP_df$IP))

# Keep only relevant columns for location:
colnames(unique_IP_df)
cols_keep <- c('IP',
               'NUMDEL',
               'DELEGACION',
               'CLASIF_UNIDAD',
               'CVEADSC',
               "ADSCRIPCION",
               "CVEUNI",
               'DEPENDENCIA',
               'DESCRIPCION_SERVICIO',
               'CVELOC',
               'DESCRIP_LOCALIDAD',
               'REGIMEN',
               'CLAVEAR',
               'NOMBREAR',
               'CVEZONABT',
               'ZONABT',
               'CVEAR'
               )

unique_IP_df <- unique_IP_df[, cols_keep, with = FALSE]
unique_IP_df
# View(unique_IP_df)
dim(unique_IP_df)
str(unique_IP_df)
sapply(unique_IP_df, summary)
# ===

# ===
# Save:
# file_n <- 'coordenadas_unidades_medicas_bienestar_SIAP'
file_n <- 'info_geo_admin_ordinario_Q17_2024'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   paste0(results_subdir),
                   file_n,
                   suffix
                   )
outfile
epi_write(file_object = unique_IP_df,
          file_name = outfile
          )
# ===
# ////////////


# ////////////
# Get external coords (unidades medicas, A Rascon, Mateo CUUMS)

# ===
# Alberto Rascon file with coordinates ----
# Has both plantilla and bienestar though
coords_match <- paste0(data_dir, '/external', '/coordenadas_ordinario_y_bienestar_alberto.csv')

# # Mateo CUUMS in separate script:
# coords_match <- "/Users/antoniob/Documents/work/science/devel/github/med-comp-imss/geo_stats/data/CUUMS_Dic_2024_mod_for_R.tsv"
coords_df <- epi_read(coords_match)
epi_head_and_tail(coords_df)
colnames(coords_df)
str(coords_df)

# Cols Alberto:
length(unique(coords_df$presup))
dim(coords_df)
unique(coords_df$tipologia)
unique(coords_df$presup6)
unique(coords_df$seleccion)
# ===

# ===
# Define column types:
fact_cols <- c('clavepersonal',
               'presup',
               'region',
               'tipologia',
               'nivel',
               'unidad',
               'regimen',
               'presup6',
               'niveldeatención',
               'tipodeservicio',
               'seleccion'
               )
for (i in fact_cols) {
  coords_df[[i]] <- as.factor(coords_df[[i]])
  }
str(coords_df)
sapply(coords_df, summary)
head(as.data.frame(coords_df[, c('latitud', 'longitud')]), n = 10)
# ===


# # ===
# # Subset to only plantilla:
# summary(coords_df$regimen)
#
# coords_df_plantilla <- coords_df %>%
#   filter(regimen == 'Ordinario')
# epi_head_and_tail(coords_df_plantilla)
# sapply(coords_df_plantilla, summary)
#
# # Save:
# file_n <- 'alberto_coordenadas_unidades_medicas_plantilla'
# suffix <- 'txt'
# outfile <- sprintf(fmt = '%s/%s.%s',
#                    paste0(results_dir, '/coords_unidades_medicas/'),
#                    file_n,
#                    suffix
#                    )
# outfile
# epi_write(file_object = coords_df_plantilla,
#           file_name = outfile
#           )
# # ===
#
#
# # ===
# # Subset to only bienestar:
# summary(coords_df$regimen)
#
# coords_df_bienestar <- coords_df %>%
#   filter(regimen == 'Bienestar')
# epi_head_and_tail(coords_df_bienestar)
# sapply(coords_df_bienestar, summary)
#
# # Save:
# file_n <- 'alberto_coordenadas_unidades_medicas_bienestar'
# suffix <- 'txt'
# outfile <- sprintf(fmt = '%s/%s.%s',
#                    paste0(results_dir, '/coords_unidades_medicas/'),
#                    file_n,
#                    suffix
#                    )
# outfile
# epi_write(file_object = coords_df_bienestar,
#           file_name = outfile
#           )
# # ===
# ////////////


# ////////////
# Full merge ----

# ===
# Match column names for merging:
colnames(unique_IP_df)[which(colnames(unique_IP_df) == 'IP')] <- 'IP_presup'
colnames(coords_df)[which(colnames(coords_df) == 'presup')] <- 'IP_presup'

dim(unique_IP_df)
dim(coords_df)
# ===

# ===
# Number of actual matches by presupuesto / IP:
length(intersect(unique_IP_df$IP_presup, coords_df$IP_presup))

# Number of mismatches by presupuesto / IP:
length(setdiff(unique_IP_df$IP_presup, coords_df$IP_presup))
# ===


# ===
all_IP_df <- coords_df %>%
  full_join(unique_IP_df, coords_df, by = 'IP_presup')
dim(all_IP_df)
epi_head_and_tail(all_IP_df)
# View(all_IP_df)


# Save:
# file_n <- 'coordenadas_unidades_medicas_bienestar_SIAP_alberto'
file_n <- 'coordenadas_unidades_medicas_plantilla_SIAP_alberto'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   paste0(results_dir, '/coords_unidades_medicas/'),
                   file_n,
                   suffix
                   )
outfile
epi_write(file_object = all_IP_df,
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
