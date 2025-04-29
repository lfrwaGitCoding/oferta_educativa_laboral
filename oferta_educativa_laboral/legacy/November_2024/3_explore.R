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
library(quarto)
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
# infile <- '2_dups_col_types_Qna_17_Plantilla_2024.rdata.gzip'

# Subset files:
infile <- "2b_subset_meds_2_dups_col_types_Qna_17_Plantilla_2024.rdata.gzip"
# infile <- '2b_subset_enfermeras_2_dups_col_types_Qna_17_Plantilla_2024.rdata.gzip'

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
# id_cols <- id_cols
# date_cols <- date_cols
# char_cols <- char_cols
# int_cols <- int_cols
# fact_cols <- fact_cols
###

###
# Output dir, based on today's date:
infile_prefix <- strsplit(infile, "\\.")[[1]][1]
results_outdir <- sprintf('%s_%s', format(Sys.Date(), '%d_%m_%Y'), infile_prefix)
results_outdir
results_outdir <-  create_results_dir(project_root = project_root,
                                      name = results_outdir)
typeof(results_outdir)
print(results_outdir)
print(dir(path = normalizePath(results_outdir), all.files = TRUE))
getwd()
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



###########
# Get column types
# If data_f has been subset in i.e. 2b_xxx.R then columns won't match

###
id_cols <- c('MATRICULA',
             'Nombre',
             'RFC',
             'CURP',
             # 'CORREO',
             'NSS'
             )
epi_head_and_tail(data_f[, id_cols])
###

###
# Get character columns:
char_cols <- data_f %>%
	select_if(is.character) %>%
	colnames()
char_cols
epi_head_and_tail(data_f[, char_cols], cols = length(char_cols))
###

###
# Get integer columns:
int_cols <- data_f %>%
	select_if(is.integer) %>%
	colnames()
int_cols
epi_head_and_tail(data_f[, int_cols], cols = length(int_cols))
###

###
# # Get numeric columns:
# num_cols <- data_f %>%
# 	select_if(is.numeric) %>%
# 	colnames()
# num_cols
# epi_head_and_tail(data_f[, num_cols], cols = length(num_cols))
# All are integer
###

###
# Get factor columns:
fact_cols <- data_f %>%
	select_if(is.factor) %>%
	colnames()
fact_cols
epi_head_and_tail(data_f[, fact_cols], cols = length(fact_cols))
###

###
date_cols <- data_f %>%
	select_if(is.Date) %>%
	colnames()
date_cols
epi_head_and_tail(data_f[, date_cols], cols = length(date_cols))
###

###
# Check all column types accounted
dim(data_f)
epi_clean_count_classes(df = data_f)
# Looks good
###
###########


###########
# Missing data
count_missing <- sum(complete.cases(data_f))
count_missing
dim(data_f)
# all rows have missing data

# Complete cases
epi_head_and_tail(data_f)
table(complete.cases(data_f))
dim(data_f)

# Missing data
na_perc <- epi_stats_na_perc(df = data_f, margin = 2)
na_perc
#

ord <- order(na_perc$na_perc, decreasing = T)
ord
na_perc <- na_perc[c(ord), ]
na_perc

print(infile_prefix)
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/na_perc.%s',
                   results_outdir,
                   suffix
                   )
outfile

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
# Date columns
# Defined in previous script
date_cols

epi_head_and_tail(data_f[, date_cols])

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
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_outdir,
                   file_n,
                   suffix
                   )

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
names(dates_list$FECHAING)
dates_list$FECHAING

# Save:
for (i in names(dates_list)) {
    # print(i)
    infile_prefix
    file_n <- 'freq'
    file_n2 <- 'Date_Differences'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s',
                       results_outdir,
                       file_n,
                       i,
                       file_n2,
                       suffix
                       )
    outfile

    # dates_list$xxx$Date_Differences
    df_out <- as.data.frame(dates_list[[i]][[1]])
    epi_write(df_out, outfile)

    infile_prefix
    file_n <- 'freq'
    file_n2 <- 'Frequencies'
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s_%s_%s.%s',
                       results_outdir,
                       file_n,
                       i,
                       file_n2,
                       suffix
                       )
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
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_outdir,
                   file_n,
                   suffix
                   )
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
file_n <- 'stats_factors'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_outdir,
                   file_n,
                   suffix
                   )
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
epi_plot_box(df = data_f, var_y = "EDAD")

# i <- "EDAD"
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
  outfile <- sprintf(fmt = '%s/%s_%s.%s', results_outdir, file_n, i, suffix)
  # outfile
  start_i <- i
  end_i <- i + 3
  my_plot_grid <- epi_plots_to_grid(num_list[start_i:end_i])
  epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
###


###
# Histograms:
epi_plot_hist(df = data_f, var_x = "EDAD")

num_list <- NULL
i <- NULL
num_list <- epi_plot_list(vars_to_plot = num_vars)
for (i in names(num_list)) {
  print(i)

  # TO DO for episcout: switch to dplyr::sym() for tidy evaluation and/or clean up column names initially. Can be useful to keep original column names though
  # i <- paste0("`", i, "`") # because of spaces and special characters in column names
  num_list[[i]] <- epi_plot_hist(df = data_f, var_x = i)
  }
num_list

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(num_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
  # results_outdir
  file_n <- 'plots_hist_num'
  suffix <- 'pdf'
  outfile <- sprintf(fmt = '%s/%s_%s.%s', results_outdir, file_n, i, suffix)
  # outfile
  start_i <- i
  end_i <- i + 3
  my_plot_grid <- epi_plots_to_grid(num_list[start_i:end_i])
  epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
###

###
# IMSS colours:
custom_palette <- c(
  "#911034",      # Original Red
  "#b12f45",      # Slightly brighter red
  "#c19a53",      # Original Gold
  "#e1b86e",      # Lighter gold/beige
  "#2a5c4b",      # Original Green
  "#3b755f",      # Brighter green
  "#DACBA1",      # Original Beige
  "#bba483",      # Darker beige
  "#602218",      # Original Brown
  "#7b3a2a"       # Rich brown
)

# Modified to be colour-blind friendly:
accessible_palette <- c(
  "#911034",      # Original Red
  "#e69f00",      # Gold (more distinct from beige)
  "#56b4e9",      # Blue (replaces a green)
  "#009e73",      # Green (contrast with blue and yellow)
  "#f0e442",      # Yellow
  "#0072b2",      # Dark blue
  "#D55E00",      # Orange
  "#cc79a7",      # Purple (replaces a beige)
  "#602218",      # Original Brown
  "#7b3a2a"       # Rich brown
)

###

###
# Plots for factors
str(data_f[, fact_cols])

plot_bar <- epi_plot_bar(df = data_f,
                         var_x = 'SEXO',
                         custom_palette = custom_palette
                         )
plot_bar

bar_list <- epi_plot_list(vars_to_plot = fact_cols)
for (i in names(bar_list)) {
    # print(i)
    bar_list[[i]] <- epi_plot_bar(df = data_f,
                                  var_x = i,
                                  custom_palette = custom_palette
                                  ) +
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
    # results_outdir
    file_n <- 'plots_bar'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', results_outdir, file_n, i, suffix)
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
ggplot(data = data_f, aes(x = FECHAPROBJUB)) +
    geom_histogram()
table(data_f$FECHAPROBJUB)

i <- 'FECHAPROBJUB'
epi_plot_hist(df = data_f, var_x = i) +
    geom_density(col = 2)

hist_list <- epi_plot_list(vars_to_plot = date_cols)
for (i in names(hist_list)) {
    print(i)
    # i <- paste0("`", i, "`") # because of spaces and special characters in column names
    hist_list[[i]] <- epi_plot_hist(df = data_f, var_x = i) +
        geom_density(col = 2)
}
hist_list

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(hist_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
    # results_outdir
    file_n <- 'plots_hist_dates'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', results_outdir, file_n, i, suffix)
    # outfile

    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(hist_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
####


####
# Box Plots: visualise range, median, quartiles, and outliers in date distributions:
i <- 'FECHAPROBJUB' # TO DO: need to sort out tidy evaluation in episcout
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
    # results_outdir
    file_n <- 'plots_box_dates'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', results_outdir, file_n, i, suffix)
    # outfile

    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(box_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
####

####
# Time Series Plot: Visualise distribution over time:

# Needs dates ordered, increasing:
i <- 'FECHAPROBJUB'
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
    # results_outdir
    file_n <- 'plots_time'
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s_%s.%s', results_outdir, file_n, i, suffix)
    # outfile

    start_i <- i
    end_i <- i + 3
    my_plot_grid <- epi_plots_to_grid(time_list[start_i:end_i])
    epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
}
###
############


############
# Generate the skim summary
skim_summary <- skimr::skim(data_f)
skim_summary

# Save as table:
file_n <- 'skim_summary'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_outdir,
                   file_n,
                   suffix
                   )
outfile
epi_write(file_object = skim_summary,
          file_name = outfile
          )

# For a nicely formatted output run the skim summary in a qmd script.
# e.g. 2_skim_summary.qmd
# Running quarto from an R script is problematic as quarto won't take full paths
############


# Up to here: plots of all variable types, basic summaries

############
# The end:
# Outputs saved to disk, no need to save as rdata.
sessionInfo()
# q()
############
