############
# SIAP
# Unidad de Personal
# Noviembre 2024
# Descriptive stats
# Input is rdata output from script:
# 2_dups_col_types.R

# Output are many plots, tables, etc for descriptive stats
############

############
# Global options:

###
# options(error = stop) # batch
###
############


############
# Import libraries
library(data.table)
library(episcout)
library(ggthemes)
library(cowplot)
library(tidyverse)
library(skimr)
# library(renv)
############


############
# Load rdata file with directory locations

###
# Set working directory to the project root:
setwd("~/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/")
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts
getwd()
###

###
# Dataset:
rdata_dir <- 'data/data_UP/access_SIAP_18092024/processed/'
# infile <- '2_dups_col_types_Qna_17_Bienestar_2024.rdata.gzip'
infile <- '2_dups_col_types_Qna_17_Plantilla_2024.rdata.gzip'
infile_path <- paste0(rdata_dir, infile)
# Full path and file name:
infile_path

print(dir(path = normalizePath(rdata_dir), all.files = TRUE))

load(infile_path)
ls()

# Get rid of RStudio warnings for loaded objects:
data_f <- data_f
project_root <- project_root
data_dir <- data_dir
devel_dir <- devel_dir
code_dir <- code_dir
results_dir <- results_dir
id_cols <- id_cols
date_cols <- date_cols
char_cols <- char_cols
int_cols <- int_cols
fact_cols <- fact_cols
###

###
# Output dir, based on today's date:
infile_prefix <- strsplit(infile, "\\.")[[1]][1]
results_outdir <- sprintf('%s_%s', format(Sys.Date(), '%d_%m_%Y'), infile_prefix)
results_outdir
# results_outdir <-  create_results_dir(project_root = project_root,
                                      # name = results_outdir)
# typeof(results_outdir)
# print(results_outdir)
# print(dir(path = normalizePath(results_outdir), all.files = TRUE))
# getwd()
###
############


############
# If subsetting:

summary(data_f[, 'DESCRIP_CLASCATEG'])
# TO DO: set manually
to_subset <- '1.MÉDICOS'
# 2.ENFERMERAS

###
# Subset data:
df_subset <- which(data_f[, 'DESCRIP_CLASCATEG'] == to_subset)
df_subset  <- data_f[df_subset , ]
dim(df_subset )
dim(data_f)

epi_head_and_tail(df_subset )
###

###
# Clean up:
data_f <- df_subset
rm(list = c('df_subset'))
###
############


############
# # Random subset test
# # Set the seed for reproducibility
# set.seed(123)
#
# # Calculate sample size (10% of total rows)
# perc <- 0.01
# sample_size <- floor(perc * nrow(data_f))
#
# # Sample random rows
# sampled_df <- data_f[sample(nrow(data_f), sample_size), ]
# sampled_df
#
# data_f <- sampled_df # to run rest of code as is
###########

# TO DO: continue here



############
# The end:
# Save objects, to eg .RData file:
print(data_dir)
dir(data_dir)

processed_data_dir <- sprintf('%s/data_UP/access_SIAP_18092024/processed/',
                              data_dir)
script <- '2b_subset_meds'
infile_prefix
suffix <- 'rdata.gzip'
outfile <- sprintf(fmt = '%s/%s_%s.%s',
                   processed_data_dir,
                   script,
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
                      'results_dir'
                      )
                    )

# Save:
save(list = objects_to_save,
     file = outfile,
     compress = 'gzip'
     )

# Remove/clean up session:
all_objects <- ls()
all_objects
rm_list <- which(!all_objects %in% objects_to_save)
all_objects[rm_list]
rm(list = all_objects[rm_list])
ls() # Anything defined after objects_to_save will still be here

sessionInfo()
# q()
############
