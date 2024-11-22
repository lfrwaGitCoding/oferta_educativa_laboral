############
# SIAP
# Unidad de Personal
# Noviembre 2024
# Column type conversion
# Input is a converted table from accdb format from SIAP and rdata with project locations from:
# 'dir_locations.R'

# TO DO:
# See sh script 'xxx'

# basic cleaning, duplicate removal, specify column types, re-order, basic summary (skimr):

# Output is rdata object with column types set-up
# See also 2_skim_summary.qmd for basic description
############


############
# Global options:

###
# options(error = stop) # batch
Sys.setenv(R_FAIL = TRUE) # interactive session
###

###
# renv can be a hassle
# disable temporarily if batch running, e.g.:
    # Disable renv prompts
# Sys.setenv(RENV_CONFIG_PROMPT_ENABLED = "FALSE") # except renv rewrites rprofile to source itself
    # Load the .rdata.gzip file
    # load("xxx.rdata.gzip")
    # Optionally re-enable prompts if desired
    # Sys.unsetenv("RENV_CONFIG_PROMPT_ENABLED")
###
############


############
# Import libraries
library(data.table)
library(episcout)
library(tidyverse)
############


############
# Load rdata file with directory locations

# Set working directory to the project root:
setwd("~/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/")
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts

# Load the .rdata.gzip file:
load("data/data_UP/access_SIAP_18092024/processed/dir_locations.rdata.gzip")
ls()

# Get rid of RStudio warnings for loaded objects:
project_root <- project_root
all_locs <- all_locs
data_dir <- data_dir
results_dir <- results_dir

print(project_root)
setwd(project_root)
getwd()
print(dir(path = normalizePath(project_root), all.files = TRUE))

print(all_locs)
############


############
# Dataset:
print(dir(path = normalizePath(data_dir), all.files = TRUE))

infiles_dir <- 'data_UP/access_SIAP_18092024/processed/'

# infile <- 'Qna_17_Bienestar_2024.csv'
infile <- 'Qna_17_Plantilla_2024.csv'
infile_path <- paste0(data_dir, infiles_dir, infile)
# Full path and file name:
infile_path

# No output dir needed
# # Output dir, based on today's date:
# infile_prefix <- strsplit(infile, "\\.")[[1]][1]
# results_outdir <- sprintf('%s_%s', format(Sys.Date(), '%d_%m_%Y'), infile_prefix)
# results_outdir
# # TO DO: add e.g. project_tools:: and move function
# results_outdir <-  create_results_dir(project_root = project_root,
#                                       name = results_outdir)
# typeof(results_outdir)
# print(results_outdir)
############


############
# Read in:
data_f <- episcout::epi_read(infile_path)
dim(data_f)
str(data_f)

epi_head_and_tail(data_f)
colnames(data_f)
############


############
# Find non-unique IDs
df_dups <- data_f
dups <- epi_clean_get_dups(df_dups, var = "MATRICULA")
dups$MATRICULA
# MATRICULA has '0' in thousands of rows
dups <- epi_clean_get_dups(df_dups, var = "CURP")
dups$CURP
# 0
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
# Re-order columns to make it easier to select

# ID columns are:
id_cols <- c('MATRICULA',
             'Nombre',
             'RFC',
             'CURP',
             # 'CORREO',
             'NSS'
             )
id_cols
epi_head_and_tail(data_f[, id_cols], cols = length(id_cols))

colnames(data_f)
df_ord <- data_f %>%
  select('MATRICULA',
         'Nombre',
         'RFC',
         'CURP',
         'CORREO',
         'NSS',
         'SEXO',
         matches("FECH"),
         everything()
  )

colnames(df_ord)
str(df_ord)
dim(df_ord)
epi_clean_count_classes(data_f)

# Clean up:
data_f <- df_ord
rm(list = c('df_ord'))
###
############



############
# Basic clean

###
# EDAD, min 15, max 95 (?)
summary(data_f$EDAD)
length(which(data_f$EDAD < 15))
which(data_f$EDAD < 15)
t(data_f[which(data_f$EDAD < 15), ])

length(which(data_f$EDAD > 65))
length(which(data_f$EDAD > 75))
length(which(data_f$EDAD > 95))

data_f$EDAD <- ifelse(data_f$EDAD < 15, NA, data_f$EDAD)
data_f$EDAD <- ifelse(data_f$EDAD > 95, NA, data_f$EDAD)
summary(data_f$EDAD)

# boxplot(data_f$EDAD)
# hist(data_f$EDAD)
# epi_plot_box(data_f, 'EDAD')
# epi_plot_hist(data_f, 'EDAD') + ylab('Conteo')
###

###
summary(data_f$FALTASACUMULADAS)
length(which(data_f$FALTASACUMULADAS < 0))
data_f$FALTASACUMULADAS <- ifelse(data_f$FALTASACUMULADAS < 0,
                                  NA,
                                  data_f$FALTASACUMULADAS
                                  )
summary(data_f$FALTASACUMULADAS)
summary(data_f$ANT_DIAS)
summary(data_f$FALTASACUMULADAS / data_f$ANT_DIAS)

data_f[, c('FALTASACUMULADAS', 'ANT_DIAS')]
head(data_f$FALTASACUMULADAS / data_f$ANT_DIAS)


# TO DO: check what this is
summary(data_f$ANTDD)
###


###
# TO DO:
# Get age from CURP:
###


###
# Column names:
# AntiguedadVacA√±os
# which(colnames(data_f) == 'AntiguedadVacAños')
# colnames(data_f)[88]
###

###
# Numbers must match for these:
# [1] "MATRICULA"             "Nombre"
#  [3] "RFC"                   "CURP"

data_f$MATRICULA
length(which(data_f$MATRICULA == '0'))

# Check for values with fewer than 9 digits:
col_check <- data_f$MATRICULA
count_char <- nchar(col_check)
summary(count_char)
summary(as.factor(count_char))
length(which(count_char < 8))
###
############


############
# Many columns duplicate information, check

###
identical(data_f$MATRICULA, data_f$TITULAR)
epi_head_and_tail(data_f[, c('MATRICULA', 'TITULAR')], cols = 2)
# some zero's
# Compare the columns and count matches
match_count <- sum(data_f$MATRICULA == data_f$TITULAR)
match_count
sum(data_f$TITULAR == '0')
dim(data_f)
# Looks like some zero's in both columns, but largely the same number/information
# Remove
###


###
identical(data_f$Nombre, data_f$NOMBRE_TITULAR)
epi_head_and_tail(data_f[, c('Nombre', 'NOMBRE_TITULAR')], cols = 2)
# some mismatches
# Compare the columns and count matches
match_count <- length(which(data_f$Nombre == data_f$NOMBRE_TITULAR))
match_count
dim(data_f)
# Looks like ~70% have the same information, 73577 / 90378
mismatches_index <- which(data_f$Nombre != data_f$NOMBRE_TITULAR)
epi_head_and_tail(data_f[mismatches_index, c('Nombre', 'NOMBRE_TITULAR')],
                  cols = 2)

# Remove for now, check
###
############



############
# Remove columns that aren't needed:

###
# Convert based on manual inspection, re-read file for column types to convert to
# 19 Nov 2024, added descriptions, columns to keep, etc. This was manual, directly to the same file:
print(results_dir)
col_types_file <- sprintf("%s/%s/%s",
                          results_dir,
                          "manual_col_types/",
                          "df_col_types2.txt"
                          )
print(col_types_file)


col_types_file <- episcout::epi_read(col_types_file)
epi_head_and_tail(col_types_file)
colnames(col_types_file)

# Dummy object:
df_col_types <- data_f
str(df_col_types)

# Columns to keep:
which_col <- 'keep_simple'
columns_to_keep <- col_types_file$variables[col_types_file[[which_col]] == "y"]
print(columns_to_keep)

# Subset:
df_col_types <- df_col_types[, columns_to_keep]
dim(df_col_types)
dim(data_f)
###

###
# Clean up:
data_f <- df_col_types
rm(list = c('df_col_types'))
###
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

# Only needed to run once
# # Save for reference / inspection:
# file_n <- 'df_col_types'
# suffix <- 'txt'
# outfile <- sprintf(fmt = '%s.%s', file_n, suffix)
# outfile <- sprintf('%s/%s', results_outdir, outfile)
# outfile
# epi_write(df_col_types, outfile)
###

###
# Convert based on manual inspection, re-read file for column types to convert to:

print(results_dir)
col_types_file <- sprintf("%s/%s/%s",
                          results_dir,
                          "manual_col_types/",
                          "df_col_types2.txt"
                          )
print(col_types_file)

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

summary(df_col_types)
# skimr::skim(df_col_types)
# Generally looks good, some non-sensical values
###

###
# Clean up:
data_f <- df_col_types
rm(list = c('df_col_types'))
###
############


############
###
# Convert dates

df_dates <- data_f

date_cols <- df_dates %>%
  select(contains("fech")) %>%
  colnames()
date_cols

epi_head_and_tail(df_dates[, date_cols])
# dd/mm/yyyy

summary(df_dates[, date_cols])
# chr
###

###
# Convert dates
col_test <- df_dates$FECHAING
summary(col_test)
summary(as.factor(col_test))
col_test
str(col_test)
as.character(col_test)

col_test <- as.Date(col_test, format = "%m/%d/%y")
str(col_test)
# formatted_col_test <- format(col_test, "%d/%m/%Y")

# col_test <- as.character(as.Date(col_test, format = "%m/%d/%y"))
# col_test <- format(as.Date(col_test), "%d/%m/%Y")
# str(col_test)

head(col_test)
head(df_dates$FECHAING)
summary(as.Date(col_test, "%d/%m/%Y"))
summary(as.Date(df_dates$FECHAING, "%m/%d/%y")) # format is different
# Looks good, now as dd/mm/yyyy without hms
# Left as R’s native yyyy-mm-dd format internally
###


###
# Loop and convert all:
epi_head_and_tail(df_dates[, date_cols])
summary(df_dates[, date_cols])
str(df_dates[, date_cols])

# i <- date_cols[1]
for (i in date_cols) {
    print(i)
    col_i <- df_dates[[i]]
    str(col_i)
    col_i <- as.Date(col_i, format = "%m/%d/%y")
    str(col_i)
    print(head(col_i))
    df_dates[[i]] <- col_i
    print(head(df_dates[, i]))
    print(summary(df_dates[[i]]))
        }

epi_head_and_tail(data_f[, date_cols])
epi_head_and_tail(df_dates[, date_cols])

head(data_f$FECHAING)
head(df_dates$FECHAING)


summary(df_dates[, date_cols])
str(data_f[, date_cols])
str(df_dates[, date_cols])
# Looks good, base R format: "2005-05-16"

skimr::skim(df_dates[, date_cols])
# Generally looks good
###

###
# TO DO:
# Converting to date seems to have cleaned these up, needs proper check
# # Clean up NAs and other values:
# # Replace invalid dates with NA:
# # Problematic strings:
# na_exclude <- c("04",
#                 "01",
#                 "00",
#                 "Definitiva",
#                 "<NA>"
#                 )
#
# which(col_test %in% na_exclude)
#
# col_test <- ifelse(col_test %in% na_exclude, NA, col_test)
# col_test <- as.Date(col_test)
# summary(col_test)
###


###
# Clean up:
data_f <- df_dates
str(data_f[, date_cols])
rm(list = c('df_dates'))
###
############


############
# Check classes

###
epi_clean_count_classes(df = data_f)
tibble::glimpse(data_f)
str(data_f)
epi_head_and_tail(data_f)
epi_head_and_tail(data_f, last_cols = T)

data_f %>%
  select_if(~ epi_clean_cond_chr_fct(.)) %>%
  colnames()

data_f %>%
  select_if(~ epi_clean_cond_numeric(.)) %>%
  colnames()

# Explicitly check the type of each column:
as.data.frame(sapply(data_f, typeof))
as.data.frame(sapply(data_f, class))
colnames(data_f)
###

###
# Get character columns:
char_cols <- data_f %>%
	select_if(is.character) %>%
	colnames()
char_cols
epi_head_and_tail(data_f[, char_cols], cols = length(char_cols))


# Get integer columns:
int_cols <- data_f %>%
	select_if(is.integer) %>%
	colnames()
int_cols
epi_head_and_tail(data_f[, int_cols], cols = length(int_cols))

# # Get numeric columns:
# num_cols <- data_f %>%
# 	select_if(is.numeric) %>%
# 	colnames()
# num_cols
# epi_head_and_tail(data_f[, num_cols], cols = length(num_cols))
# All are integer

# Get factor columns:
fact_cols <- data_f %>%
	select_if(is.factor) %>%
	colnames()
fact_cols
epi_head_and_tail(data_f[, fact_cols], cols = length(fact_cols))


# Check all column types accounted
dim(data_f)
epi_clean_count_classes(df = data_f)
# Looks good
###
############


############
# TO DO:
# save to disk as txt
# Get summaries from skimr
# skim_sum <- skimr::skim(data_f)
############


############
# The end:
# Save objects, to eg .RData file:
print(data_dir)
dir(data_dir)

processed_data_dir <- sprintf('%s/data_UP/access_SIAP_18092024/processed/',
                              data_dir)
script <- '2_dups_col_types'
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
                      'results_dir',
                      'id_cols',
                      'date_cols',
                      'char_cols',
                      'int_cols',
                      'fact_cols'
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
