############
# SIAP
# Unidad de Personal
# Noviembre 2024
# Descriptive stats - bivariable analysis
# Input is rdata output from script:
# 2_dups_col_types.R
# or
# 2b_clean_subset_manual.R

# Output are many plots, tables, etc for two or more variables
# Focus is on plazas vacantes vs ocupadas
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
# library(skimr)
# library(quarto)
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
results_outdir <- paste0(results_outdir, '_bivar')
results_outdir <-  create_results_dir(project_root = project_root,
                                      name = results_outdir
                                      )
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
# # All are integer
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



############
###
# TO DO:
# re re-order columns, got lost somewhere
# Re-order columns to make it easier to select

# ID columns:
id_cols
epi_head_and_tail(data_f[, id_cols], cols = length(id_cols))

colnames(data_f)
df_ord <- data_f %>%
  select('MATRICULA',
         'Nombre',
         'RFC',
         'CURP',
         # 'CORREO',
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
# Bivariate analysis
# Correlation matrix:
cormat_all <- epi_stats_corr(df = data_f[, int_cols], method = 'spearman')
names(cormat_all)
names(cormat_all$cormat)
cormat_all$cormat$r
cormat_all$cormat_melted_r
class(cormat_all)

# Save files:
r_values <- as.data.frame(cormat_all$cormat_melted_r)
p_values <- as.data.frame(cormat_all$cormat_melted_pval)

file_n <- 'spearman_r'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_outdir,
                   file_n,
                   suffix
                   )
outfile
epi_write(file_object = r_values,
          file_name = outfile
          )

file_n <- 'spearman_p_values'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_outdir,
                   file_n,
                   suffix
                   )
outfile
epi_write(p_values, outfile)
############


############
####
# Bivariate analysis
# Factors only

# Comparison
# 2x2 tables n, percent, signif tests
# nxn tables: max 3 variables, hard to visualise/summarise otherwise
# Loop for 'PLZOCU' plots and signif tests for factors

fact_cols
epi_head_and_tail(data_f[, fact_cols])

epi_plot_bar(df = data_f, var_x = 'PLZOCU')
epi_plot_hist(df = data_f, var_x = "EDAD")
####


####
# TO DO: create a function and move to episcout

summary(data_f)
f_tab <- ftable(xtabs(~ PLZOCU +
                          CLASIF_UNIDAD,
                      # +
                      #     DELEGACION,
                      # +
                      #     DESCRIP_CLASCATEG +
                      #     ESCOLARIDAD +
                      #     CLASIF_UNIDAD,
                      data = data_f
                      )
                )
# View(f_tab)

df_f_tab <- as.data.frame(f_tab)
df_f_tab
data.frame(t(df_f_tab))

df_f_tab_wide <- tidyr::pivot_wider(df_f_tab,
                                    names_from = PLZOCU,
                                    values_from = c(Freq)
                                    )
df_f_tab_wide
colnames(df_f_tab_wide)[colnames(df_f_tab_wide) == '0'] <- 'vacante'
colnames(df_f_tab_wide)[colnames(df_f_tab_wide) == '1'] <- 'ocupada'
df_f_tab_wide$total <- df_f_tab_wide$vacante + df_f_tab_wide$ocupada
df_f_tab_wide$perc_vacante <- round(((df_f_tab_wide$vacante / df_f_tab_wide$total) * 100), digits = 2)
df_f_tab_wide$perc_ocupada <- round(((df_f_tab_wide$ocupada / df_f_tab_wide$total) * 100), digits = 2)

epi_head_and_tail(df_f_tab_wide, cols = ncol(df_f_tab_wide))
df_f_tab_wide <- df_f_tab_wide[order(df_f_tab_wide$perc_vacante, decreasing = TRUE), ]

file_n <- 'df_f_tab_wide_PLZOCU_DELEGACION'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_outdir,
                   file_n,
                   suffix
                   )
outfile
epi_write(df_f_tab_wide, outfile)
####


####
# Check other vars vs PLZOCUP

colnames(data_f_PLZOCU)
summary(data_f_PLZOCU)

dep_var <- "PLZOCU"
# ind_vars <- c("CLASIF_UNIDAD")
ind_vars <- c("ADSCRIPCION")

df_result <- generate_summary_table(
  df = data_f,
  dep_var = dep_var,
  ind_vars = ind_vars
  )

epi_head_and_tail(df_result)

file_n <- paste0('df_f_tab_wide_', dep_var, '_', ind_vars)
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_outdir,
                   file_n,
                   suffix
                   )
outfile
epi_write(df_f_tab_wide, outfile)
####

####
# TO DO: loop through relevant vars, many are factors with many options though
colnames(data_f_PLZOCU)
summary(data_f_PLZOCU)

colnames(data_f_PLZOCU)[colnames(data_f_PLZOCU) == "DESCRIP_TIPO DE PLAZA"] = "DESCRIP_TIPO_DE_PLAZA"
colnames(data_f_PLZOCU)
summary(data_f_PLZOCU$TipoMcaOcup)

dep_var <- "PLZOCU"
# ind_vars <- c("TipoMcaOcup")
df <- data_f_PLZOCU

cols_to_loop <- colnames(df)[!colnames(df) %in% c("PLZOCU")]
cols_to_loop

for (i in cols_to_loop) {
    ind_vars <- i
    print(ind_vars)

    df_result <- epi_stats_table(
        df = df,
        dep_var = dep_var,
        ind_vars = ind_vars
        )
    # epi_head_and_tail(df_result)

    file_n <- paste0('table_', dep_var, '_', ind_vars)
    suffix <- 'txt'
    outfile <- sprintf(fmt = '%s/%s.%s',
                       results_outdir,
                       file_n,
                       suffix
                       )
    print(outfile)
    epi_write(df_result, outfile)
    }
####




# TO DO: continue here

####
# Contingency table significance tests
# Function:
# TO DO: move to episcout
twoxtwo_test <- function(df, target_var_name, other_var_name) {#, test = 'chisq.test') {
  tab <- table(df[[target_var_name]], df[[other_var_name]])
  # test <- chisq.test(tab)
  test <- fisher.test(tab)
  broom::tidy(test)  # Using broom to tidy the chi-squared test output
}

twoxtwo_test(col_facts, 'Estado', 'Sexo')
twoxtwo_test(col_facts, 'Estado', 'IRC')
col_facts$IRC  # will cause signif test to error as only one column
table(col_facts$Estado, col_facts$IRC)
summary(col_facts)

table(col_facts$Estado, col_facts$Otras)
table(col_facts$Estado, col_facts$BULUT_1)

df <- col_facts
unique_counts <- lapply(df, function(x) length(unique(x)))
print(unique_counts)

# Include only columns with unique values of 1 or less:
include <- which(unique_counts >= 2)

# Iterate over variables and test:
# TO DO: create a function and move to episcout
results <- lapply(names(col_facts[, include])[names(col_facts[, include]) != "Estado"], function(other_var_name) {
  twoxtwo_test(df = col_facts[, include], target_var_name = "Estado", other_var_name = other_var_name)
})

# Name list elements:
names(results) <- names(col_facts[, include])[names(col_facts[, include]) != "Estado"]
results

# Convert to data.frame:
results_df <- bind_rows(results, .id = "Variable")
results_df
epi_write(results_df, 'fishers.txt')
####

####
# Generate contingency tables:
# TO DO: move to episcout
cont_df <- function(df, x_var = 1, y_var = 2) {
  table_df <- as.data.frame(table(df[[x_var]], df[[y_var]]))
  colnames(table_df)[1] <- colnames(df)[x_var]
  colnames(table_df)[2] <- colnames(df)[y_var]
  return(table_df)
  }

contingency_df <- cont_df(df = col_facts, x_var = 1, y_var = 4)
contingency_df

# Loop:
# TO DO: create a function and move to episcout
results <- lapply(colnames(col_facts)[colnames(col_facts) != "Estado"], function(y_var) {
  cont_df(df = col_facts, x_var = 1, y_var = y_var)
})
colnames(results[[9]][2])
colnames(results[[7]])
results[[1]]
# Column names get lost:
for (i in 1:length(results)) {
  df <- results[[i]]
  colnames(df)[2] <- colnames(col_facts)[i + 1]
  results[[i]] <- df
  # Also name list elements:
  names(results)[i] <- colnames(col_facts)[i + 1]
}
results
names(results)

# TO DO: create a function and move to episcout
# Create a new workbook:
wb <- openxlsx::createWorkbook()
# Add each data frame from the list as a new sheet
lapply(names(results), function(x) {
  openxlsx::addWorksheet(wb, x)
  openxlsx::writeData(wb, sheet = x, results[[x]])
})
# Save the workbook
openxlsx::saveWorkbook(wb, "2x2_tables_vs_Estado.xlsx", overwrite = TRUE)


# Plot contingency tables:
# TO DO: move to episcout
cont_plot <- function(df, x_var = 'Var1', y_var = 'Var2') {
  ggplot(df, aes(x = !!sym(x_var), y = Freq, fill = !!sym(y_var))) +
    geom_bar(stat = "identity", position = "dodge")
  }

contingency_df <- cont_df(df = col_facts, x_var = 1, y_var = 3)
contingency_df
cont_plot(contingency_df, colnames(contingency_df)[1], colnames(contingency_df)[2])

for (i in 1:length(results)) {
  df <- results[[i]]
  fact_plot <- cont_plot(df = df, x_var = colnames(df)[1], y_var = colnames(df)[2])
  file_name <- sprintf('cont_bar_plot_Estado_%s.pdf', names(results)[i])
  epi_plot_cow_save(file_name = file_name,
                    # base_height = 12,
                    # base_width = 12,
                    plot_grid = fact_plot
  )
  }
####

####
# Pair plots of contingency tables, not sure they add much (?)
# library(GGally)
colnames(col_facts)
facts_plot <- GGally::ggpairs(col_facts[, c("Estado", "HAS")],
                                axisLabels = 'show',
                                showStrips = TRUE)
facts_plot
epi_plot_cow_save(file_name = 'ggpairs_edo_HAS.pdf',
                  base_height = 6,
                  base_width = 6,
                  plot_grid = facts_plot
                  )

facts_plot_1 <- GGally::ggpairs(col_facts[, c("Estado", "Sexo", "Inmunofenotipo_1")],
                                axisLabels = 'show',
                                showStrips = TRUE)
facts_plot_1
epi_plot_cow_save(file_name = 'ggpairs_edo_sexo_inmunofeno.pdf',
                  base_height = 6,
                  base_width = 6,
                  plot_grid = facts_plot_1
                  )
facts_plot_2 <- GGally::ggpairs(col_facts[, c("Estado", "Sexo", "Inmunofenotipo_1", "HAS")],
                                axisLabels = 'show',
                                showStrips = TRUE)
facts_plot_2
epi_plot_cow_save(file_name = 'ggpairs_edo_sexo_inmunofeno_HAS.pdf',
                  base_height = 6,
                  base_width = 6,
                  plot_grid = facts_plot_2
                  )


facts_plot_3 <- GGally::ggpairs(col_facts[, c("Estado", "Sexo", "Inmunofenotipo_1", "HAS",
                                              "DM2", "Obesidad", "EPOC", "IRC",
                                              "BULUT_1")],
                                axisLabels = 'show',
                                showStrips = TRUE)
facts_plot_3
epi_plot_cow_save(file_name = 'ggpairs_facts.pdf',
                  base_height = 40,
                  base_width = 40,
                  plot_grid = facts_plot_3
                  )

# # Create ggpairs plot, remove duplicated plots from diagonal:
# # Custom function for lower triangle: scatter plot
# show_lower <- function(data, mapping, ...) {
#   ggplot(data = data, mapping = mapping) #+ geom_point(...)
#   }
#
# # Custom function for upper triangle: blank
# blank_upper <- function(data, mapping, ...) {
#   ggplot() + theme_void()
#   }
#

# facts_plot <- GGally::ggpairs(col_facts[, c("Estado", "HAS")],
#                               lower = list(discrete = wrap("lower", show_lower)),
#                               upper = list(discrete = wrap("upper", blank_upper)),
#                               axisLabels = 'show',
#                               showStrips = TRUE
#                               )

####

####
# Boxplots / violin Plots
colnames(col_nums)

epi_plot_box(df = df_factor,
             var_y = 'Neu_count_1',
             var_x = 'Estado'
             )

epi_plot_box(df = df_factor,
             var_y = 'IFN_g',
             var_x = 'Estado'
             )

epi_plot_box(df = df_factor,
             var_y = 'Lin_count_1',
             var_x = 'Estado'
             )

epi_plot_box(df = df_factor,
             var_y = 'IgA_Lavado',
             var_x = 'Estado'
             )

# Plot all numeric variables vs 'Estado'
colnames(col_nums)
i <- "Lin_count_1"
# eval(i)
# sym(i)
epi_plot_box(df = df_factor, var_y = i)
epi_plot_box(df = df_factor, var_y = i, var_x = 'Estado')

box_list <- epi_plot_list(vars_to_plot = colnames(col_nums))
for (i in names(box_list)) {
  # print(i)
  box_list[[i]] <- epi_plot_box(df = df_factor,
                                var_y = i,
                                var_x = 'Estado'
                                )
  }
length(box_list)
names(box_list)

# Save plots
# Plot 4 per page or so:
per_file <- 4
jumps <- seq(1, length(box_list), per_file)
length(jumps)

# i <- 2
for (i in jumps) {
  file_name <- sprintf('plots_box_%s.pdf', i)
  start_i <- i
  end_i <- i + 3
  my_plot_grid <- epi_plots_to_grid(box_list[start_i:end_i])
  epi_plot_cow_save(file_name = file_name, plot_grid = my_plot_grid)
}

####
############


############
# TO DO: significance tests for num vars vs 'Estado'
# TO DO: create a function and move to episcout

# Wilcoxon Rank-Sum Test:
factor_var <- df_factor[['Estado']]
num_var <- df_factor$Neu_count_1

if (length(unique(factor_var)) == 2) {
  group1 <- num_var[factor_var == levels(factor_var)[1]]
  group2 <- num_var[factor_var == levels(factor_var)[2]]
  test <- wilcox.test(group1, group2,
                      exact = TRUE,
                      correct = TRUE,
                      conf.int = TRUE,
                      conf.level = 0.95
                      )
  print(test)
}
test$statistic

# TO DO: continue here
wilcoxon_res <- data.frame(Group1 = character(),
                           Group2 = character(),
                           Variable = character(),
                           W = numeric(),
                           P.Value = numeric(),
                           CI95_low = numeric(),
                           CI95_high = numeric(),
                           stringsAsFactors = FALSE
                           )

unique_groups <- unique(df_factor[['Estado']])
combinations <- combn(unique_groups, 2)
factor_var <- df_factor[['Estado']]
df <- df_factor
dim(combinations)

# c <- 'Neu_count_1' #'Peso'
# # Example manual test for verification
# test_example <- wilcox.test(df$Edad[df$Estado == "Defuncion"], df$Edad[df$Estado == "Mejoria"])
# print(test_example)

for (c in colnames(col_nums)) {
  for (i in 1:ncol(combinations)) {
    group1_data <- df[factor_var == combinations[1, i], c]
    group2_data <- df[factor_var == combinations[2, i], c]
    # print(head(group1_data))  # Check the data
    # print(head(group2_data))  # Check the data
    test <- wilcox.test(group1, group2,
                        exact = TRUE,
                        correct = TRUE,
                        conf.int = TRUE,
                        conf.level = 0.95
                        )
    wilcoxon_res <- rbind(wilcoxon_res,
                          data.frame(Group1 = combinations[1, i],
                                     Group2 = combinations[2, i],
                                     Variable = c,
                                     W = test$statistic,
                                     P.Value = test$p.value,
                                     CI95_low = test$conf.int[1],
                                     CI95_high = test$conf.int[2]
                                     )
                          )
  }
}
epi_head_and_tail(wilcoxon_res, cols = 7)


############


############
# Numerical for each factor? Too much


# TO DO:
# Local vs systemic comparison
# "IgA_Suero"
# "IgA_Lavado"
# "IgG_Suero"
# "IgG_Lavado"
# "IgM_Suero"
# "IgM_Lavado"

# To a table:
# (summary(na.omit(df_factor$IgA_Lavado)),
#       summary(na.omit(df_factor$IgA_Suero)),
#       summary(na.omit(df_factor$IgG_Lavado)),
#       summary(na.omit(df_factor$IgG_Suero)),
#       summary(na.omit(df_factor$IgM_Lavado)),
#       summary(na.omit(df_factor$IgM_Suero))
#       )


# Two/+ factors
# clustered bar, stacked bar, heatmap


# Pause after this as descriptive
############


############
#

############


############
#

############


############
# Next:
# Impute based on key variables
# Comparisons for survival
# Comparisons for local vs systemic response
# Inferential analysis
############


# Up to here: basic cleaning, data subset, manual though as needed for subsequent analysis


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
