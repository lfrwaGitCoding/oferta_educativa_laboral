# ////////////
# Script information ----

# Coordinadas de unidades medicas CUUMS cruzadas con datos del SIAP
# Abril 2025

# Input:
  # coordenadas de Mateo DPTI (CUUMS Dic 2024)
  # SIAP con todas las columnas

# Output:
  # tabla con SIAP y coordenadas para unidades medicas, contiene administrativas sin coordenadas
# ////////////


# ////////////
# Import libraries ----
library(data.table)
library(episcout)
library(tidyverse)
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
               # 'CLAVEAR',
               # 'NOMBREAR',
               'CVEZONABT',
               'ZONABT'
               # 'CVEAR'
               )

unique_IP_df <- unique_IP_df[, cols_keep, with = FALSE]
unique_IP_df
# View(unique_IP_df)
dim(unique_IP_df)
str(unique_IP_df)
sapply(unique_IP_df, summary)

col_merge <- "DEPENDENCIA"
length(unique(unique_IP_df[[col_merge]]))
length(which(is.na(unique_IP_df[[col_merge]])))
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
# coords_match <- paste0(data_dir, '/external', '/coordenadas_ordinario_y_bienestar_alberto.csv')

# CUUMS Mateo:
coords_match <- "/Users/antoniob/Documents/work/science/devel/github/med-comp-imss/geo_stats/data/CUUMS_Dic_2024_mod_for_R.tsv"
coords_df <- epi_read(coords_match)
epi_head_and_tail(coords_df)
colnames(coords_df)
str(coords_df)

unique(coords_df$Region_2021)
length(unique(coords_df$Denominacion_Unidad))
length(unique(coords_df$Clave_Personal))
length(unique(coords_df$Clave_Presupuestal))
dim(coords_df)
# ===

# ===
# Define column types:
fact_cols <- c("Region_2021",
               # "CLUES_Salud", # char ID
               # "Clave_Personal", # char ID
               # "Unidad_de_Informacion_PREI", # char ID
               # "Clave_Ubicacion_Admin", # char ID
               # "Clave_Presupuestal", # char ID
               "Clave_Delegacion_o_UMAE",
               "Nombre_Delegacion_o_UMAE",
               "Relacion_Delegacion-UMAE",
               "Unidad_Presupuestal",
               "Nivel_de_Atencion",
               # "Denominacion_Unidad", # char ID
               "Tipo_de_Servicio",
               "Descripcion_Tipo_Servicio",
               "Numero_de_Unidad",
               "Nombre_Unidad",
               "Ubicacion_o_Denominacion",
               "Tipo_de_Vialidad",
               "Tipo_de_Asentamiento",
               "Codigo_postal",
               "Clave_Municipio_o_Delegacion",
               "Municipio_o_Delegacion",
               "Clave_Localidad",
               "Localidad",
               "Clave_Entidad_Federativa",
               "Entidad_Federativa",
               "Clave_Jurisdiccion_Sanitaria",
               "Jurisdiccion_Sanitaria",
               "Grado_de_Marginacion"
               )
for (i in fact_cols) {
    coords_df[[i]] <- as.factor(coords_df[[i]])
    }
str(coords_df)
sapply(coords_df, summary)

head(as.data.frame(coords_df[, c('LATITUD', 'LONGITUD')]), n = 10)
# View(as.data.frame(coords_df[, c('LATITUD', 'LONGITUD')]))
summary(as.numeric(coords_df$LATITUD)) # one value is "##.############", rest are numeric

i <- "LATITUD"
coords_df[[i]] <- as.numeric(coords_df[[i]])

i <- "Inicio_de_Productividad"
coords_df[[i]] <- as.Date(coords_df[[i]]) # yyy-mm-dd
str(coords_df)
# ===
# ////////////


# ////////////
# Full merge ----
colnames(coords_df)
colnames(unique_IP_df)

# ===
# Different column names, all v all to check which ones to merge against:
df1 <- coords_df
df2 <- unique_IP_df

# Named vectors of unique values per column:
u1 <- lapply(df1, function(col) unique(na.omit(col)))
u2 <- lapply(df2, function(col) unique(na.omit(col)))
u1
u2

# Empty matrix:
overlap_matrix <- matrix(0, nrow = length(u1), ncol = length(u2),
                         dimnames = list(names(u1), names(u2)))

# Jaccard type overlap: |A ∩ B| / min(|A|, |B|)
for (i in names(u1)) {
    for (j in names(u2)) {
        intersection <- length(intersect(u1[[i]], u2[[j]]))
        denom <- min(length(u1[[i]]), length(u2[[j]]))
        if (denom > 0) {
            overlap_matrix[i, j] <- intersection / denom
        }
    }
}

# Data frame for easier viewing:
overlap_df <- reshape2::melt(overlap_matrix, varnames = c("df1_col", "df2_col"), value.name = "overlap")
overlap_df <- overlap_df %>% filter(overlap > 0) %>% arrange(desc(overlap))
print(overlap_df)

# "Clave_Presupuestal" == "IP", others are not exact matches
i <- "Clave_Presupuestal"
j <- "IP"
length(intersect(u1[[i]], u2[[j]]))
length(intersect(df1[[i]], df2[[j]]))

epi_head_and_tail(df1)
epi_head_and_tail(df2)

as.data.frame(head(df1[, c(1:10)], n = 10))
as.data.frame(head(df2[, c(1:10)], n = 10))

# View(df1)
# View(df2)
# ===

# ===
# TO DO: continue here
# re-filter based on IP and DEPENDENCIA
# don't get unique values for DEPENDENCIA if filtering with IP
# ===

# ===
# Match column names for merging:
col_merge <- "Clave_Presupuestal_IP"
col_df1 <- "IP"
col_df2 <- "Clave_Presupuestal"

colnames(unique_IP_df)[which(colnames(unique_IP_df) == col_df1)] <- col_merge
colnames(coords_df)[which(colnames(coords_df) == col_df2)] <- col_merge

dim(unique_IP_df)
dim(coords_df)
# ===

# ===
# Number of actual matches by presupuesto / IP:
length(intersect(unique_IP_df[[col_merge]], coords_df[[col_merge]]))

# Number of mismatches by presupuesto / IP:
length(setdiff(unique_IP_df[[col_merge]], coords_df[[col_merge]]))
# ===


# ===
all_IP_df <- coords_df %>%
    left_join(unique_IP_df, coords_df, by = col_merge)
dim(all_IP_df)
epi_head_and_tail(all_IP_df)
# View(all_IP_df)
length(unique(all_IP_df[[col_merge]]))
length(which(is.na(all_IP_df[[col_merge]])))


# Save:
# file_n <- 'coordenadas_unidades_medicas_bienestar_SIAP_alberto'
file_n <- 'coordenadas_unidades_medicas_ordinario_SIAP_CUUMS'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   paste0(results_subdir),
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
