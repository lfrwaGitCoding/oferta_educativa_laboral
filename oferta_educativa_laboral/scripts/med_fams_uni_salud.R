# ////////////
# Script information ----

# SIAP
# Universidad de la Salud - Medicina rural
# Diciembre 2024
# Numero de medicos familiares por unidad
# Input is rdata output from script:
# 2_dups_col_types.R

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
library(ggthemes)
library(cowplot)
library(tidyverse)
library(openxlsx)
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
rdata_dir <- 'data/data_UP/access_SIAP_18092024/processed/'

# TO DO: Manually set:
# infile <- '2_clean_dups_col_types_Qna_17_Bienestar_2024.rdata.gzip'
# infile <- '2_clean_dups_col_types_Qna_17_Plantilla_2024.rdata.gzip'

# infile <- "2b_clean_subset_2_clean_dups_col_types_Qna_17_Bienestar_2024_meds.rdata.gzip"
# infile <- '2b_clean_subset_2_clean_dups_col_types_Qna_17_Bienestar_2024_enfermeras.rdata.gzip'

infile <- "2b_clean_subset_2_clean_dups_col_types_Qna_17_Plantilla_2024_meds.rdata.gzip"
# infile <- '2b_clean_subset_2_clean_dups_col_types_Qna_17_Plantilla_2024_enfermeras.rdata.gzip'


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
setwd(project_root)
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
script_n <- 'med_fam_uni_salud'
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
# Listado de unidades medicas ----

# ===
colnames(data_f)

summary(data_f$NOMBREAR)
summary(data_f$ADSCRIPCION)
summary(data_f$DELEGACION)
summary(data_f$CLASIF_UNIDAD)

# Filter data based on these categories from CLASIF_UNIDAD:
unidades_meds <- c("01 Primer Nivel",
                   "02 Segundo Nivel",
                   "03 Tercer Nivel UMAE",
                   "04 Tercer Nivel"
                   )
data_f_un_meds <- data_f[data_f$CLASIF_UNIDAD %in% unidades_meds, ]
dim(data_f_un_meds)
dim(data_f)
epi_head_and_tail(data_f_un_meds)
# View(head(data_f_un_meds))
summary(data_f_un_meds$CLASIF_UNIDAD)

# Drop levels with zero count
levels(data_f_un_meds$CLASIF_UNIDAD)
data_f_un_meds <- droplevels(subset(data_f_un_meds, CLASIF_UNIDAD != 0))
levels(data_f_un_meds$CLASIF_UNIDAD)
summary(data_f_un_meds$CLASIF_UNIDAD)
dim(data_f_un_meds)

# Get values where ADSCRIPCION also has values in unidades_meds:
epi_head_and_tail(data_f_un_meds[, c("ADSCRIPCION", "CLASIF_UNIDAD")], cols = 2)
# View(head(data_f_un_meds[, c("ADSCRIPCION", "CLASIF_UNIDAD")], n = 1000))


summary(data_f_un_meds$CLASIF_UNIDAD)
summary(data_f_un_meds$ADSCRIPCION)
# ////////////


# ////////////
# Numero de medicos familiares por unidad ----

# ===
df <- data_f_un_meds

colnames(df)
# col_to_subset <- 'DESCRIP_CLASCATEG'
# value_to_subset <- '1.MÉDICOS'

summary(df$PLZOCU)
summary(df$ADSCRIPCION)
summary(df$CATEGORIA)
summary(df$DESCRIP_CLASCATEG)
summary(df$NOMBREAR)
# View(head(df, n = 100))
# ===


# ===
contingency_2x2_df <- epi_stats_contingency_2x2_df(df[, fact_cols],
                                                   x_var = "PLZOCU",
                                                   y_var = "ADSCRIPCION"
                                                   )
print(contingency_2x2_df)

contingency_2x2_df <- epi_stats_contingency_2x2_df(df[, fact_cols],
                                                   x_var = "ADSCRIPCION",
                                                   y_var = "NOMBREAR"
                                                   )
print(head(contingency_2x2_df))
print(contingency_2x2_df)
# ===

# ===
dep_var <- "NOMBREAR"
ind_vars <- c("ADSCRIPCION")

formula_str <- sprintf("~ %s + %s", dep_var, ind_vars)
formula_obj <- as.formula(formula_str)
formula_obj

f_tab <- ftable(xtabs(formula_obj, data = df))

# Convert to data frame
df_f_tab <- as.data.frame(f_tab)
df_f_tab

# Reshape to wide format
df_f_tab_wide <- tidyr::pivot_wider(
    df_f_tab,
    names_from = all_of(dep_var),
    values_from = c(Freq)
  )

epi_head_and_tail(df_f_tab_wide, cols = ncol(df_f_tab_wide))
# ===

# ===
# Save as table
# pwd already in results folder:
infile_prefix
file_n <- 'ADSCRIPCION_NOMBREAR'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_subdir,
                   file_n,
                   suffix
                   )
outfile
epi_write(file_object = df_f_tab_wide, file_name = outfile)

# Save as Excel file
suffix <- 'xlsx'
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_subdir,
                   file_n,
                   suffix
                   )
outfile
write.xlsx(df_f_tab_wide, file = outfile)
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
