############
# SIAP
# Dirección de Administración
# Unidad de Personal
# Octubre 2024
# Data base tables check
# Input is accdb mdbtools converted format (per table) and rdata with project directory locations from:
# 'dir_locations.R'

# Currently tables from SIAP are:
  # tabla Bienestar
  # tabla Plantilla, e.g. Qna_17_Bienestar_2024.csv

# No output as tables have the same structure
############


############
# Import libraries
library(data.table)
library(episcout)
library(tidyverse)
############

############
# Basic error handling so Ruffus sees failures
options(error = function() {q(status = 1)})

############
# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript 1b_accdb_tables_check.R <infile1> <infile2> [results_dir]")
}
infile <- args[1]
infile2 <- args[2]
results_dir_arg <- if (length(args) >= 3) args[3] else NA


############
# Load rdata file with directory locations

# Set working directory to the project root:
setwd(here::here())

# Load the .rdata.gzip file:
# TO DO: check if this is needed any longer
load("data/data_UP/processed/dir_locations.rdata.gzip")
ls()

# project_root <- here::here()
project_root <- project_root
print(project_root)
getwd()
print(dir(path = normalizePath(project_root), all.files = TRUE))

print(all_locs)
############


############
# Datasets:

if (!is.na(results_dir_arg)) results_dir <- results_dir_arg

# infile_path <- if (file.exists(infile)) infile else file.path(data_dir, infile)
# infile2_path <- if (file.exists(infile2)) infile2 else file.path(data_dir, infile2)

results_outdir <- epi_create_dir(results_dir)
typeof(results_outdir)
print(results_outdir)

# For saving/naming outputs:
infile_prefix <- 'setup'
############


############
# Read in:
infile <- episcout::epi_read(infile)
infile2 <- episcout::epi_read(infile2)

epi_head_and_tail(infile)
epi_head_and_tail(infile, last_cols = TRUE)
epi_head_and_tail(infile2)

colnames(infile)
colnames(infile2)
############


############
# Check overlap between datasets

###
# Column names shared:
colnames(infile)[colnames(infile) %in% colnames(infile2)]
# all, nice

# Not shared:
colnames(infile)[colnames(infile) != colnames(infile2)]

###
# Check manually:
column_names_dfs <- data.frame(df1 = colnames(infile),
                               df2 = colnames(infile2)
                               )
# View(column_names_dfs)
file_n <- 'df_shared_col_names'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s.%s', file_n, suffix)
outfile <- sprintf('%s/%s', results_outdir, outfile)
outfile
epi_write(column_names_dfs, outfile)
###


###
# Check how many rows match in both databases
head(infile$Nombre)
head(infile2$Nombre)
row_matches <- intersect(infile$Nombre, infile2$Nombre)
length(row_matches)
row_matches
# 356

head(infile$MATRICULA)
head(infile2$MATRICULA)
row_matches <- intersect(infile$MATRICULA, infile2$MATRICULA)
length(row_matches)
row_matches
# 0
# Consider adding origin and merging datasets
############


############
# The end:
# No need to save objects, file structure is the same
# Run next scripts per table

sessionInfo()
# q()
############
