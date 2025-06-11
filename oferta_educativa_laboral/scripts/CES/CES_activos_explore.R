# ////////////
# Script information ----

# SIAP
# Unidad de Personal
# Mayo 2025
#
#
# Input is rdata output from script:
#

# Output are
# ////////////


# ////////////
# Import libraries ----
library(data.table)
library(episcout)
library(ggthemes)
library(cowplot)
library(tidyverse)
library(skimr)
library(pander)
library(log4r)
# ////////////


# ////////////
# Set working directory to the project root  ----
# Should be there already if loaded as RStudio project
setwd(here::here())
# TO DO:
# Mac24:
# setwd(here::here())
# project_root <- "/Users/antoniob/Library/Mobile Documents/com~apple~CloudDocs/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral"
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts
getwd()
# ////////////


# ////////////
# Load files ----

# ===
# Load the .rdata.gzip file:



# ===


# ===
# CES data dir:
ces_data_dir <- file.path(data_dir, "data_CES/datos/")
dir.exists(ces_data_dir)
# ===

# ===
# Activos, CES data:
dir(ces_data_dir)

activos_2025 <- "Activos_Todos.060525UEI.csv"
activos_2025 <- paste0(ces_data_dir, activos_2025)
file.exists(activos_2025)
activos_2025 <- epi_read(activos_2025)
epi_head_and_tail(activos_2025)
# ===
# ////////////


# ////////////
# Source functions/scripts/etc
# TO DO:
# Source (until I update episcout)
# code_dir <- '/Users/antoniob/Library/Mobile Documents/com~apple~CloudDocs/Documents/work/science/devel/github/antoniojbt/oferta_educativa_laboral/oferta_educativa_laboral'
source(file.path(paste0(code_dir, '/scripts/funcs_epi_source.R')))
# ////////////


# ////////////
# Output dir, based on today's date ----
script_n <- 'CES_activos_explore'
infile_prefix <- strsplit(script_n, "\\.")[[1]][1]
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
# Capture output / log ----

# ===
# Redirect standard output
if (!interactive()) { # TRUE if not interactive, will then log output
    script_n <- '2_clean_dups_col_types'
    sink_stdout <- paste0(results_subdir, '/', script_n, '.sink_stdout.log')
    sink(sink_stdout, split = TRUE)

    # Redirect messages and warnings
    sink_msg <- file(paste0(results_subdir, '/', script_n, '.sink_msg.log'), open = "wt")
    sink(sink_msg, type = "message")

    # Example outputs
    cat("Test: This is standard output.\n")
    message("Test: This is a message.")
    warning("Test: This is a warning.")
}
# ===

# ===
# Create a logger
if (!interactive()) { # TRUE if not interactive, will then log output
    logger <- create.logger()
    log_n <- paste0(results_subdir, '/', script_n, '.log4r.log')
    logfile(logger) <- log_n # Log file location
    level(logger) <- "INFO"  # Set logging level (DEBUG, INFO, WARN, ERROR)

    # Add log messages
    # info(logger, "Script started")
    # debug(logger, "This is a debug message")
    # warn(logger, "This is a warning")
    # error(logger, "This is an error")
}
# ////////////



# ////////////
# Resids "egresables", ultimo a;o:
df <- activos_2025
epi_head_and_tail(df)
colnames(df)

# df$GRADO <- as.factor(df$GRADO)
summary(df$GRADO)

# Table by specialty and year:
tb_df_esp_grado <- df %>%
    group_by(ESPECIALIDAD, GRADO) %>%
    summarise(total = n()) %>%
    arrange(ESPECIALIDAD, desc(GRADO))
# View(tb_df_esp_grado)

# Save:
file_n <- "activos_2025_esp_grado"
suffix <- "txt"
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
epi_write(file_object = tb_df_esp_grado, file_name = outfile)


tb_df_max_grado <- df %>%
    group_by(ESPECIALIDAD, GRADO) %>%
    filter(GRADO == max(GRADO, na.rm = TRUE)) %>%
    summarise(total = n()) %>%
    arrange(desc(GRADO))
# View(tb_df_max_grado)

tb_df_max_grado_unique <- df %>%
    group_by(ESPECIALIDAD) %>%
    filter(GRADO == max(GRADO, na.rm = TRUE)) %>%
    distinct(ESPECIALIDAD, GRADO) %>%
    arrange(desc(GRADO))
# View(tb_df_max_grado_unique)

# Save:
file_n <- "activos_2025_esp_grado_unic"
suffix <- "txt"
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
epi_write(file_object = tb_df_max_grado_unique, file_name = outfile)


# ////////////



# ////////////
# The end  ----

print(sessionInfo())

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
