# ////////////
# Script information ----
# SIAP
# Unidad de Personal
# Noviembre 2024
# Subset dataframe for PLZOCU relevant variables only
# Input is rdata output from script:
# 2_clean_dups_col_types.R

# Output is rdata with subset dataframe for desc stats, plots, etc.
# ////////////


# ////////////
# Global options ----

# ===
# Stop on error:
# options(error = stop)
# ===
# ////////////


# ////////////
# Import libraries ----
library(data.table)
library(episcout)
library(tidyverse)
library(log4r)
# library(renv)
# ////////////

############
# Basic error handling
options(error = function() {q(status = 1)})

############
# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript 2c_subset_PLZOCU.R <infile> [results_dir]")
}
infile <- args[1]
results_dir_arg <- if (length(args) >= 2) args[2] else NA


# ////////////
# Set working directory to the project root  ----
setwd(here::here())
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts
getwd()
# ////////////


# ////////////
# Load rdata file ----

# ===
project_root <- here::here()
data_dir <- file.path(project_root, 'data')
results_dir <- file.path(project_root, 'results')
code_dir <- file.path(project_root, 'oferta_educativa_laboral')
rdata_dir <- file.path(data_dir, 'data_UP/access_SIAP_18092024/processed/')
if (!is.na(results_dir_arg)) results_dir <- results_dir_arg

# ===
# TO DO: set manually
# cols_to_keep <- 'xx'
# ===

# ===
# Full path and file name:
infile_path <- file.path(rdata_dir, infile)
print(infile_path)

print(dir(path = normalizePath(rdata_dir), all.files = TRUE))

load(infile_path)
ls()
# ===

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
source(file.path(paste0(code_dir, '/funcs_epi_source.R')))
# ////////////


# ////////////
# Output dir, based on today's date ----
script_n <- '2c_subset_PLZOCU'
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
# Capture output / log ----

# ===
# Redirect standard output
if (!interactive()) { # TRUE if not interactive, will then log output
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
# Columns with non-constant values for PLZOCU ----
# For exploration only, keep all columns for 0 vs 1 comparison

# ===
# Columns which have values when PLZOCUP == 0
summary(data_f$PLZOCU)
epi_head_and_tail(data_f[which(data_f$PLZOCU == 0), ], cols = ncol(data_f))
colnames(data_f)
cols_to_remove <- c(
    'MATRICULA',
    'RFC',
    'CURP',
    'NSS',
    'SEXO',
    'FECHAING',
    'FECHAPROBJUB',
    'FECHANOMINACION',
    'FECHAPRIMERCONFZA',
    'EDAD',
    'ANT_DIAS',
    'COMSIN',
    'JUBILA',
    'ULTIMACATEGBASE',
    'FALTASACUMULADAS',
    'MY',
    'CPTO180',
    'IMP_010',
    'IMP_024',
    'IMP_035',
    'IMP_037',
    'ESCOLARIDAD'
    # 'PLZAUT',
    # 'PLZSOB'
    )

cols_to_keep <- colnames(data_f)[!colnames(data_f) %in% cols_to_remove]
cols_to_keep
# ===

# ===
# Cols values when PLZOCU == 0
epi_head_and_tail(data_f[which(data_f$PLZOCU == 0), cols_to_remove],
                  cols = length(cols_to_remove)
                  )
summary(data_f[which(data_f$PLZOCU == 0), cols_to_remove])

epi_head_and_tail(data_f[which(data_f$PLZOCU == 0), cols_to_keep],
                  cols = length(cols_to_keep)
                  )
summary(data_f[which(data_f$PLZOCU == 0), cols_to_keep])
# ===

# ===
# Cols values when PLZOCU == 1
epi_head_and_tail(data_f[which(data_f$PLZOCU == 1), cols_to_remove],
                  cols = length(cols_to_remove)
                  )
summary(data_f[which(data_f$PLZOCU == 1), cols_to_remove])

epi_head_and_tail(data_f[which(data_f$PLZOCU == 1), cols_to_keep],
                  cols = length(cols_to_keep)
                  )
summary(data_f[which(data_f$PLZOCU == 1), cols_to_keep])
# ===


# ===
data_f_PLZOCU <- data_f[, cols_to_keep]
table(data_f$PLZOCU, data_f$DELEGACION)
table(data_f_PLZOCU$PLZOCU, data_f_PLZOCU$DELEGACION)
summary(data_f_PLZOCU)


summary(data_f[data_f$PLZOCU == 0, 'IMP_037'])
summary(data_f[data_f$PLZOCU == 1, 'IMP_037'])

summary(data_f[data_f$PLZOCU == 0, 'SEXO'])
summary(data_f[data_f$PLZOCU == 1, 'SEXO'])

table(data_f$PLZOCU, data_f$SEXO)
# ===
# ////////////


# ////////////
# The end ----
# Save objects, to eg .RData file:
print(data_dir)
dir(data_dir)

processed_data_dir <- sprintf('%s/data_UP/access_SIAP_18092024/processed/',
                              data_dir)
infile_prefix
suffix <- 'rdata.gzip'
outfile <- sprintf(fmt = '%s/%s_%s.%s',
                   processed_data_dir,
                   script_n,
                   infile_prefix,
                   suffix
                   )
outfile

# Check and remove objects that are not necessary to save:
object_sizes <- sapply(ls(), function(x) object.size(get(x)))
object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
object_sizes
ls()

objects_to_save <- (c('data_f',
                      'project_root',
                      'data_dir',
                      'devel_dir',
                      'code_dir',
                      'results_dir',
                      'id_cols',
                      'date_cols',
                      'char_cols',
                      'int_cols',
                      'fact_cols',
                      'num_cols',
                      'all_colnames'
                      )
                    )

# Save:
save(list = objects_to_save,
     file = outfile,
     compress = 'gzip'
     )

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

# Remove/clean up session:
all_objects <- ls()
all_objects
rm_list <- which(!all_objects %in% objects_to_save)
all_objects[rm_list]
rm(list = all_objects[rm_list])
ls() # Anything defined after objects_to_save will still be here

# q()
# ////////////
