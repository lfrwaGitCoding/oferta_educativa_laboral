############
# SIAP
# Dirección de Administración
# Unidad de Personal
# Octubre 2024
# Descriptive stats
# Input is accdb mdbtools converted format (per table)
# Currently:
  # tabla Bienestar
  # tabla Plantilla, e.g. Qna_17_Bienestar_2024.csv
# Output is
############

############
# TO DO:
# Column name descriptions
# Handle dates:
# dates with "01/01/50 00:00:00 " but also "<NA>"
# accdb to csv mdbtools conversion
# then csv to R reading
# mdbtools does introduce format changes, e.g. (note row order is not the same!):
# "03/01/04 00:00:00" csv mdbtools
# vs
# 1/1/2050 00:00:00 csv accdb export

# Re-convert with mdbtools but double check, use full ddmmyyyy export as ambiguity for 1950 vs 2050

############



############
code_loc <- "~/Documents/work/science/devel/github/antoniojbt/oferta_educativa_laboral/oferta_educativa_laboral"

project_loc <- "/Users/antoniob/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/data/"
setwd(project_loc)
getwd()
############


############
# Import libraries
library(data.table)
library(episcout)
library(ggthemes)
library(cowplot)
library(tidyverse)
############


############
# Datasets:
infile <- 'data_UP/access_SIAP_18092024/processed/Qna_17_Bienestar_2024.csv'
infile2 <- 'data_UP/access_SIAP_18092024/processed/Qna_17_Plantilla_2024.csv'

results_loc <- '~/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/results/'

results_dir <- 'october_2024/'
results_dir <- sprintf('%s/%s', results_loc, results_dir)
results_dir

infile <- episcout::epi_read(infile)
infile2 <- episcout::epi_read(infile2)

epi_head_and_tail(infile)
epi_head_and_tail(infile2)

colnames(infile)
colnames(infile2)

# For saving/naming outputs:
# infile_prefix <- 'infile'
infile_prefix <- 'plantilla_q17'
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
outfile <- sprintf('%s/%s', results_dir, outfile)
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
# Set up so that I can run one script for both datasets:
data_f <- infile
# data_f <- infile2
dim(data_f)
str(data_f)
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
rm(list = c('df_dups'))
############


############
# Check classes and convert

###
epi_clean_count_classes(df = data_f)
tibble::glimpse(data_f)
str(data_f)
epi_head_and_tail(data_f)
epi_head_and_tail(data_f, last_cols = T)

# Explicitly check the type of each column:
as.data.frame(sapply(data_f, typeof))
colnames(data_f)
###

###
# Get character columns:
char_cols <- data_f %>%
	select_if(~ epi_clean_cond_chr_fct(.)) %>%
	colnames()
char_cols
epi_head_and_tail(data_f[, char_cols], cols = length(char_cols))


# Get integer columns:
int_cols <- data_f %>%
	select_if(~ epi_clean_cond_numeric(.)) %>%
	colnames()
int_cols

# True numeric columns are:
colnames(data_f)
int_cols
epi_head_and_tail(data_f[, int_cols], cols = length(int_cols))
###


###
# Dates:
date_cols <- data_f %>%
  select(contains("fech")) %>%
  colnames()
date_cols

epi_head_and_tail(data_f[, date_cols])
# dd/mm/yyyy

# Problematic strings:
na_exclude <- c("04",
                "01",
                "00",
                "Definitiva",
                "<NA>"
                )

# Convert dates
# TO DO: continue here, errors
# Replace invalid dates with NA:
col_test <- data_f$FECHAING
summary(col_test)
summary(as.factor(col_test))
col_test
col_test <- as.POSIXct(col_test, format = "%d/%m/%y %H:%M:%S", tz = "Etc/GMT+6")
# "Etc/GMT+6" represents UTC-6 (Central Standard Time)
summary(col_test)
col_test


# col_test <- ifelse(col_test %in% na_exclude, NA, col_test)
# summary(col_test)

head(data_f$FECHAING)
head(col_test)


epi_head_and_tail(data_f[, c("Fecha de Ingreso", "Fecha de Baja")], cols = 2)

summary(data_f[, c("Fecha de Ingreso", "Fecha de Baja")])
str(data_f)

# This is the year extracted, convert to factor as only a few years and will use as counts:
summary(as.Date(data_f$'año Baja', format = "%Y"))
summary(as.factor(data_f$'año Baja'))

data_f$'año Baja' <- as.factor(data_f$'año Baja')
summary(data_f$'año Baja')

# Get date columns:
date_cols <- data_f %>%
	select_if(~ epi_clean_cond_date(.)) %>%
	colnames()
date_cols
###


###
summary(data_f)
colnames(data_f)

# Looks like these are factors:
fact_cols <- c("Núm OOAD",
               "OOAD",
               "CveAdscrip",
               "Adscripción",
               "Tipo Contratación",
               "Clasificación de Categoría",
               "Cve Categ",
               "Categoría",
               "Sexo",
               "Mca Baja",
               "Descripcion de baja",
               "Régimen",
               'Mca',
               "CveAR\n2018",
               "Área de Responsabilidad\n2018"
               )

# Check summary for each column as factor:
lapply(data_f[, fact_cols], function(x) summary(as.factor(x)))
# Yes, all look like factors

# Convert to factors:
for (i in fact_cols) {
  data_f[[i]] <- as.factor(data_f[[i]])
}

summary(data_f[, fact_cols])
###


###
# Check and convert manually any remaining:
tibble::glimpse(data_f)

# This is an ID:
data_f$Matrícula <- as.character(data_f$Matrícula)

tibble::glimpse(data_f)
summary(data_f)
epi_clean_count_classes(data_f)
# Looks good
###


###
data_f %>%
  select_if(~ epi_clean_cond_chr_fct(.)) %>%
  colnames()

data_f %>%
  select_if(~ epi_clean_cond_numeric(.)) %>%
  colnames()

# Re-order columns to make it easier to select:
colnames(data_f)
df_ord <- data_f %>%
  select('Matrícula',
         'Nombre',
         matches("Fecha"),
         matches("Antigüedad", ignore.case = FALSE),
         everything() # should all be factors now
  )

colnames(df_ord)
str(df_ord)
dim(df_ord)
epi_clean_count_classes(data_f)

# Clean up:
data_f <- df_ord
rm(list = c('df_ord'))
###


###
# Re-define column types for summarising, plotting, etc:
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


# Get factor columns:
fact_cols <- data_f %>%
	select_if(is.factor) %>%
	colnames()
fact_cols
epi_head_and_tail(data_f[, fact_cols], cols = length(fact_cols))
###

# Removed duplicates, specified column types, re-ordered up to here.
############


###########
# Missing data
count_missing <- sum(complete.cases(data_f))
count_missing
dim(data_f)
dim(infile2)
# some rows have missing data

# Complete cases
epi_head_and_tail(data_f)
table(complete.cases(data_f))
dim(data_f)

# Missing data
na_perc <- epi_stats_na_perc(df = data_f, margin = 2)
na_perc
# Only Adscripción has missing data, ~5%

ord <- order(na_perc$na_perc, decreasing = T)
ord
na_perc <- na_perc[c(ord), ]
na_perc

infile_prefix
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/na_perc.%s', infile_prefix, suffix)
outfile
dir()

epi_write(na_perc,
          outfile,
          row.names = TRUE,
          col.names = TRUE
          )
############


############
# Summary statistics
epi_clean_count_classes(data_f)
str(data_f)

###
# Date columns:
summary(data_f[, date_cols])

sum_dates_df <- data.frame('variable' = character(0),
                           'Min' = numeric(0),
                           'q25%' = numeric(0),
                           'Median' = numeric(0),
                           'q75%' = numeric(0),
                           'Max' = numeric(0),
                           'IQR' = numeric(0),
                           stringsAsFactors = FALSE
                           )
for (i in date_cols) {
    sum_dates <- epi_stats_dates(data_f[[i]])
    # print(sum_dates)
    stats_vector <- sum_dates$Value  # Extract the values

    # Data frame row to append:
    new_row <- data.frame(
        Variable = i,
        Min = stats_vector[1],
        `q25%` = stats_vector[2],
        Median = stats_vector[3],
        `q75%` = stats_vector[4],
        Max = stats_vector[5],
        IQR = stats_vector[6]
    )
    sum_dates_df <- rbind(sum_dates_df,
                          new_row)
}
sum_dates_df


infile_prefix
file_n <- 'desc_dates'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s', infile_prefix, file_n, suffix)
outfile

epi_write(sum_dates_df, outfile)
###

###
# Get frequency tables
dates_list <- list()
for (i in date_cols) {
    # Calculate differences between consecutive dates to find gaps and clusters:
    date_ord <- as.numeric(data_f[[i]])
    inds <- order(date_ord,
                  decreasing = FALSE,
                  na.last = TRUE
    )
    date_ord <- date_ord[inds]
    date_diffs <- diff(date_ord)
    # date_diffs
    # Convert numeric differences back to an interpretable form (e.g., days):
    # date_diffs_days <- as.Date(date_diffs, origin = "1970-01-01") - as.Date("1970-01-01") # although diff() was already working on days, so same result
    # date_diffs_days
    # Frequency table by month-year:
    date_frequencies <- table(format(data_f[[i]], "%Y-%m"))  # Counts by year and month
    # date_frequencies
    dates_list[[i]] <- list(
        Date_Differences = date_diffs,
        Frequencies = date_frequencies
    )
}
names(dates_list)
names(dates_list$'Fecha de Ingreso')
dates_list$'Fecha de Ingreso'

# Save:
for (i in names(dates_list)) {
    # print(i)
    infile_prefix
    file_n <- 'freq'
    file_n2 <- 'Date_Differences'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, file_n2, suffix)
    outfile

    # dates_list$xxx$Date_Differences
    df_out <- as.data.frame(dates_list[[i]][[1]])
    epi_write(df_out, outfile)

    infile_prefix
    file_n <- 'freq'
    file_n2 <- 'Frequencies'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s', infile_prefix, file_n, i, file_n2, suffix)
    outfile
    # dates_list$xxx$Frequencies
    df_out <- as.data.frame(dates_list[[i]][[2]])
    epi_write(df_out, outfile)
    }
###
############


############
###
# Numeric columns:
stats_num <- epi_stats_summary(df = data_f, class_type = 'int_num')
stats_num

# Numeric data summary doesn't need tidying but could be formatted:
stats_num <- epi_stats_format(stats_num, digits = 2)
# View(stats_num)

# Save as table
# pwd already in results folder:
infile_prefix
file_n <- 'sum_stats'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s', infile_prefix, file_n, suffix)
outfile

epi_write(file_object = stats_num, file_name = outfile)
###


###
# Factor columns:
str(data_f)
fact_cols

# Get desc stats:
stats_fct <- epi_stats_summary(df = data_f[, fact_cols], class_type = 'chr_fct')
stats_fct
unique(stats_fct$id)
# View(stats_fct)
dim(data_f)
colnames(data_f)

# Add total for percentage calculation and order column to tidy up results:
perc_n <- nrow(data_f[, fact_cols])
order_by <- 'percent'
stats_fct_tidy <- epi_stats_tidy(sum_df = stats_fct,
                                 order_by = order_by,
                                 perc_n = perc_n
                                 )
stats_fct_tidy
# Format them if needed:
stats_fct_tidy <- epi_stats_format(stats_fct_tidy, digits = 0)
stats_fct_tidy

# Save as table
infile_prefix
file_n <- 'stats_factors'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s', infile_prefix, file_n, suffix)
outfile

epi_write(file_object = stats_fct_tidy,
          file_name = outfile
          )
###
############



############
# Plots

###
num_vars <- list()
for (i in colnames(data_f)) {
  if (epi_clean_cond_numeric(data_f[[i]])) {
    num_vars <- c(num_vars, i)
  }
}
num_vars

# Numeric, boxplots:
epi_plot_box(df = data_f, var_y = "Antigüedad en Años")

# i <- "Antigüedad en Años"
num_list <- epi_plot_list(vars_to_plot = num_vars)
for (i in names(num_list)) {
  num_list[[i]] <- epi_plot_box(df = data_f, var_y = i)
  }

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(num_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
  # infile_prefix
  file_n <- 'plots_box_num'
  suffix <- 'pdf'
  outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
  # outfile
  start_i <- i
  end_i <- i + 3
  my_plot_grid <- epi_plots_to_grid(num_list[start_i:end_i])
  epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
###


###
# Histograms:
epi_plot_hist(df = data_f, var_x = "`Antigüedad en Años`")

num_list <- NULL
i <- NULL
num_list <- epi_plot_list(vars_to_plot = num_vars)
for (i in names(num_list)) {
  print(i)

  # TO DO for episcout: switch to dplyr::sym() for tidy evaluation and/or clean up column names initially. Can be useful to keep original column names though
  i <- paste0("`", i, "`") # because of spaces and special characters in column names
  num_list[[i]] <- epi_plot_hist(df = data_f, var_x = i)
  }
# num_list
# TO DO: switch to removing NULL objects from list:
# Remove of the first three as special characters in column names cause issues:
num_list <- num_list[-c(1:2)]
num_list

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(num_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
  # infile_prefix
  file_n <- 'plots_hist_num'
  suffix <- 'pdf'
  outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
  # outfile
  start_i <- i
  end_i <- i + 3
  my_plot_grid <- epi_plots_to_grid(num_list[start_i:end_i])
  epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
###


###
# Plots for factors
str(data_f)

plot_bar <- epi_plot_bar(df = data_f, 'Núm OOAD') #but backticks here error with 'object not found'
plot_bar

# Bar plots:
i <- "Núm OOAD"
epi_plot_bar(df = data_f, var_x = i, ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar_list <- epi_plot_list(vars_to_plot = fact_cols)
for (i in names(bar_list)) {
    # print(i)
    bar_list[[i]] <- epi_plot_bar(df = data_f, var_x = i) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
# bar_list

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(bar_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
    # infile_prefix
    file_n <- 'plots_bar'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
    # outfile
    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(bar_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile,
                      plot_grid = my_plot_grid,
                      base_width = 15,
                      base_height = 15
    )
    }
###
############


############
# Plot dates
date_cols
str(data_f[, date_cols])
summary(data_f[, date_cols])

####
# Histograms of date frequencies, to check for gaps and clusters:
# Column names are an issue, backticks needed here
ggplot(data = data_f, aes(x = `Fecha de Ingreso`)) +
    geom_histogram()

ggplot(data = data_f, aes(x = `Fecha de Baja`)) +
    geom_histogram()

table(data_f$"Fecha de Baja")

i <- '`Fecha de Baja`'
epi_plot_hist(df = data_f, var_x = i) +
    geom_density(col = 2)

hist_list <- epi_plot_list(vars_to_plot = date_cols)
for (i in names(hist_list)) {
    print(i)
    i <- paste0("`", i, "`") # because of spaces and special characters in column names
    hist_list[[i]] <- epi_plot_hist(df = data_f, var_x = i) +
        geom_density(col = 2)
}
hist_list
# same issue with backticks, remove the first two:
hist_list <- hist_list[-c(1:2)]

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(hist_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
    # infile_prefix
    file_n <- 'plots_hist_dates'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
    # outfile

    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(hist_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
####


####
# Box Plots: visualise range, median, quartiles, and outliers in date distributions:
i <- 'Fecha de Baja' # TO DO: need to sort out tidy evaluation in episcout
epi_plot_box(data_f, var_y = i)

box_list <- epi_plot_list(vars_to_plot = date_cols)
for (i in names(box_list)) {
    # print(i)
    box_list[[i]] <- epi_plot_box(df = data_f, var_y = i)
    }
# box_list

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(box_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
    # infile_prefix
    file_n <- 'plots_box_dates'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
    # outfile

    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(box_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
####


####
# Time Series Plot: Visualise distribution over time:

###
# Needs dates ordered, increasing:
i <- 'Fecha de Baja'
ord_i <- sort(as.Date(data_f[[i]]))
# ord_i <- data_f[ord_i, i]
# ord_i <- as.vector(ord_i)
str(ord_i)
head(ord_i)
tail(ord_i)
length(ord_i)

# Get a subset to plot:
rand_size <- 1000
rand_indices <- sort(sample(length(ord_i), size = rand_size, replace = FALSE))
# rand_indices <- rand_indices[order(rand_indices)]
head(rand_indices)
tail(rand_indices)

# Subset the data frame:
date <- ord_i[rand_indices]
value <- seq_along(date)
plot_data <- data.frame(i = date, value = value)
colnames(plot_data)[1] <- i
str(plot_data)
epi_head_and_tail(plot_data, cols = 2)

# Plot:
# But it's a random subset, so annotate:
label_annot <- sprintf("random subset of %s", rand_size)
ggplot(plot_data, aes(x = !!sym(i), y = value)) +
    geom_line() +
    geom_point() +
    annotate("text", x = mean(range(plot_data[[i]])), y = -Inf,
             label = label_annot, vjust = -1, size = 2, color = "gray20")
###

###
time_list <- epi_plot_list(vars_to_plot = date_cols)
for (i in names(time_list)) {
    # Get date, sort it, pass it as vector, add value var for plotting:
    date <- sort(as.Date(data_f[[i]]))
    value <- seq_along(date)
    plot_data <- data.frame(i = date, value = value)
    colnames(plot_data)[1] <- i
    # Plot:
    time_list[[i]] <- ggplot(plot_data, aes(x = !!sym(i), y = value)) +
        geom_line() +
        geom_point()
}
# time_list

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(time_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
    # infile_prefix
    file_n <- 'plots_time'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
    # outfile

    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(time_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
###
############


############
# The end:
# Save objects, to eg .RData file:
folder <- ''
script <- '1_setup_SIAP'
infile_prefix
suffix <- 'rdata.gzip'
outfile <- sprintf(fmt = '%s/%s_%s.%s', folder, script, infile_prefix, suffix)
outfile

# Check and remove objects that are not necessary to save:
object_sizes <- sapply(ls(), function(x) object.size(get(x)))
object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
object_sizes
objects_to_save <- (c('data_f', 'infile_prefix', 'outfile'))

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
ls() # Anything defined after all_objects and objects_to_save will still be here

sessionInfo()
# q()

# Next: run the script for xxx
############
