############
# SIAP
# Unidad de Personal
# Noviembre 2024
# Column type conversion
# Input is a converted table from accdb format from SIAP and rdata with project locations from:
# 'dir_locations.R'

# TO DO:
# See sh script 'xxx'

# Output is rdata object with column types set-up
############

# TO DO: continue here

############
# Import libraries
library(data.table)
library(episcout)
library(ggthemes)
library(cowplot)
library(tidyverse)
############


############
# Load rdata file with directory locations

# Load renv properly and set working directory to the project root:
setwd("~/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/")

# Load the .rdata.gzip file:
load("data/data_UP/access_SIAP_18092024/processed/dir_locations.rdata.gzip")
ls()

print(project_root)
setwd(project_root)
getwd()
print(dir(path = normalizePath(project_root), all.files = TRUE))

print(all_locs)
############


############
# Dataset:
print(dir(path = normalizePath(data_dir), all.files = TRUE))

infile <- 'data_UP/access_SIAP_18092024/processed/Qna_17_Bienestar_2024.csv'

# Output dir, based on today's date:
results_outdir <- create_results_dir(project_root = project_root)
typeof(results_outdir)
print(results_outdir)

# For saving/naming outputs:
infile_prefix <- 'Qna_17_Bienestar_2024'
############


############
# Read in:
data_f <- episcout::epi_read(infile)
dim(data_f)
str(data_f)

epi_head_and_tail(data_f)
colnames(data_f)
############


############
# Find non-unique IDs
df_dups <- data_f
dups <- epi_clean_get_dups(df_dups, var = "MATRICULA")
# MATRICULA has '0' in ~4000 rows
dups <- epi_clean_get_dups(df_dups, var = "CURP")
dups <- epi_clean_get_dups(df_dups, var = "Nombre")
dups <- epi_clean_get_dups(df_dups, var = "NSS")
dups
# No duplicates by CURP as expected
# 31 by name
# 2 by NSS

# View(t(dups))
# No real duplicates (by CURP)
# None for infile2

# Clean up:
# data_f <- df_dups # not needed
rm(list = c('df_dups'))
############


############
###
# Many columns, manually looking at data and deciding what types are each

str(data_f)
typeof(data_f$MATRICULA) # typeof() gives base R's type
class(data_f$MATRICULA) # class() gives the converted type

# Extract column types:
df_col_types <- data.frame(variables = names(data_f),
                           class = sapply(data_f, class),
                           base_type = sapply(data_f, typeof)
                           )

df_col_types

# Save for reference / inspection:
file_n <- 'df_col_types2'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s.%s', file_n, suffix)
outfile <- sprintf('%s/%s', results_outdir, outfile)
outfile
epi_write(df_col_types, outfile)
###

###
# Convert based on manual inspection, re-read file for column types to convert to:
col_types_file <- outfile
col_types_file

col_types_file <- episcout::epi_read(col_types_file)
epi_head_and_tail(col_types_file)
colnames(col_types_file)

# Dummy object:
df_col_types <- data_f
str(df_col_types)

# Column type conversions, except for dates, will run separately:
# i <- 1
for (i in 1:nrow(col_types_file)) {
  col_name <- col_types_file$variables[i]
  orig_col_type <- col_types_file$base_type[i]
  target_type <- col_types_file$convert_to[i]

  # Skip if target_type is NA:
  if (is.na(target_type)) {
    next
  }

  # Convert based on target_type:
  if (target_type == "factor") {
    df_col_types[[col_name]] <- as.factor(df_col_types[[col_name]])
  } else if (target_type == "character") {
    df_col_types[[col_name]] <- as.character(df_col_types[[col_name]])
  } else if (target_type == "integer") {
    df_col_types[[col_name]] <- as.integer(df_col_types[[col_name]])
  } else if (target_type == "numeric") {
    df_col_types[[col_name]] <- as.numeric(df_col_types[[col_name]])
  }
}

# Check:
str(data_f)
str(df_col_types)

typeof(data_f$MATRICULA) # typeof() gives base R's type
typeof(df_col_types$MATRICULA)

class(data_f$MATRICULA) # class() gives the converted type
class(df_col_types$MATRICULA)

col1 <- t(as.data.frame(lapply(data_f, class)))
col1
col2 <- t(as.data.frame(lapply(df_col_types, class)))
col2

column_types_dfs <- data.frame(original_base = col1,
                               converted_class = col2
                               )
column_types_dfs
###


###
# Clean up:
data_f <- df_col_types
rm(list = c('df_col_types'))
###
############


############
# The end:
# Save objects, to eg .RData file:
print(data_dir)
dir(data_dir)

processed_data_dir <- sprintf('%s/data_UP/access_SIAP_18092024/processed/', data_dir)
script <- 'dir_locations'
suffix <- 'rdata.gzip'
outfile <- sprintf(fmt = '%s/%s.%s', processed_data_dir, script, suffix)
outfile

# Check and remove objects that are not necessary to save:
object_sizes <- sapply(ls(), function(x) object.size(get(x)))
object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
object_sizes

objects_to_save <- (c('all_locs',
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
