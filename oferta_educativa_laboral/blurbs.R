# ////////////
# Correlations ----
# Plot significant correlations only, use Spearman or Kendall
# Too many variables to see properly
str(r_values)
str(p_values)
epi_head_and_tail(p_values, cols = 3)
epi_head_and_tail(r_values, cols = 3)
# Remove rows where Var1 and 2 match, ie the diagonal which is corr for the same var:
p_values <- p_values[!(p_values$Var1 == p_values$Var2), ]
r_values <- r_values[!(r_values$Var1 == r_values$Var2), ]
identical(p_values$Var1, r_values$Var1)
identical(p_values$Var2, r_values$Var2)
dim(p_values)
dim(r_values)

signif_cut <- 0.05
p_values_sig <- p_values[p_values$value < signif_cut, ]
dim(p_values_sig)
# 0.001 gives 160 pairs
# 0.01 gives 336 pairs
# 0.05 gives 622 pairs
length(unique(p_values_sig$Var1))
# Heatmap will look funny as empty spaces due to missing pairs of variables (ie from leaving only highly significant results)

p_values_sig_i <- which(p_values$value < signif_cut)
length(p_values_sig_i)
str(p_values_sig)
epi_head_and_tail(p_values_sig, cols = 3)
head(p_values_sig_i)
head(p_values_sig)

r_values_sig <- r_values[p_values_sig_i, ]
str(r_values_sig)

# Check:
identical(as.character(p_values_sig$Var1), as.character(r_values_sig$Var1))
head(as.character(p_values_sig$Var1))
head(as.character(r_values_sig$Var1))

# Plot significant values only ----
# Indices will not match for r and p files if using triangle functions
# Do manually, without triangle:

# Format names, etc:
colnames(col_nums)
unique(as.character(p_values_sig$Var1))
vars_list <- unique(as.character(p_values_sig$Var1))
renamed_sig_corr <- epi_stats_corr_rename(r_values_sig,
                                          p_values_sig,
                                          vars_list = vars_list,
                                          var_labels = vars_list,
                                          digits = 2
                                          )
renamed_sig_corr
class(renamed_sig_corr)
str(renamed_sig_corr)
epi_list_head(list = renamed_sig_corr)

# Get heatmaps:
epi_plot_heatmap(cormat_all$cormat_melted_r)
epi_plot_cow_save(file_name = 'plots_heatmap_spearman_r.pdf',
                  base_height = 12,
                  base_width = 12,
                  plot_grid = epi_plot_heatmap(cormat_all$cormat_melted_r)
                  )

epi_plot_heatmap(cormat_all$cormat_melted_pval)
epi_plot_cow_save(file_name = 'plots_heatmap_spearman_p.pdf',
                  base_height = 12,
                  base_width = 12,
                  plot_grid = epi_plot_heatmap(cormat_all$cormat_melted_pval)
                  )

epi_plot_heatmap(renamed_sig_corr$cormat_melted_triangle_r)
epi_plot_cow_save(file_name = 'plots_heatmap_spearman_sig_0.05_r.pdf',
                  base_height = 12,
                  base_width = 12,
                  plot_grid = epi_plot_heatmap(renamed_sig_corr$cormat_melted_triangle_r)
                  )

epi_plot_heatmap(renamed_sig_corr$cormat_melted_triangle_pval)
epi_plot_cow_save(file_name = 'plots_heatmap_spearman_sig_0.05_p.pdf',
                  base_height = 12,
                  base_width = 12,
                  plot_grid = epi_plot_heatmap(renamed_sig_corr$cormat_melted_triangle_pval)
                  )
# Significance only plots look ugly but still give information. Can extract groups of variables of interest from here.
# e.g. IgSuero, TARC, GROa, MIG
# Include standard heatmaps without labels
# ////////////

# ////////////
# Plot triangles for all but save with large area base height and width ----
cormat_all
melted_triangles <- epi_stats_corr_triangle(cormat = cormat_all$cormat)
class(melted_triangles)
melted_triangles$cormat_melted_triangle_r
vars_list <- colnames(col_nums)

renamed_triangles <- epi_stats_corr_rename(melted_triangles$cormat_melted_triangle_r,
                                           melted_triangles$cormat_melted_triangle_pval,
                                           vars_list = vars_list,
                                           var_labels = vars_list
                                           )

epi_plot_heatmap(cormat_all$cormat_melted_r)
epi_plot_heatmap(renamed_triangles$cormat_melted_triangle_r)
epi_plot_heatmap(renamed_triangles$cormat_melted_triangle_pval)

# Save:
epi_plot_cow_save(file_name = 'plots_heatmap_triangle_spearman_r.pdf',
                  base_height = 12,
                  base_width = 12,
                  plot_grid = epi_plot_heatmap(renamed_triangles$cormat_melted_triangle_r)
                  )
epi_plot_cow_save(file_name = 'plots_heatmap_triangle_spearman_p.pdf',
                  base_height = 12,
                  base_width = 12,
                  plot_grid = epi_plot_heatmap(renamed_triangles$cormat_melted_triangle_pval)
                  )
# ////////////


# ////////////
# # Random subset test ----
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
# ////////////


# ////////////
# Get column types ----
# If data_f has been subset in i.e. 2b_xxx.R then columns won't match

# Check loaded cols exist:
all(column_list %in% colnames(data_f))

# ===
id_cols <- c('MATRICULA',
             'RFC',
             'CURP',
             'NSS'
             )
epi_head_and_tail(data_f[, id_cols])
# ===

# ===
# Get character columns:
char_cols <- data_f %>%
	select_if(is.character) %>%
	colnames()
char_cols
epi_head_and_tail(data_f[, char_cols], cols = length(char_cols))
# ===

# ===
# Get integer columns:
int_cols <- data_f %>%
	select_if(is.integer) %>%
	colnames()
int_cols
epi_head_and_tail(data_f[, int_cols], cols = length(int_cols))
# ===

# ===
# # Get numeric columns:
# num_cols <- data_f %>%
# 	select_if(is.numeric) %>%
# 	colnames()
# num_cols
# epi_head_and_tail(data_f[, num_cols], cols = length(num_cols))
# All are integer
# ===

# ===
# Get factor columns:
fact_cols <- data_f %>%
	select_if(is.factor) %>%
	colnames()
fact_cols
epi_head_and_tail(data_f[, fact_cols], cols = length(fact_cols))
# ===

# ===
date_cols <- data_f %>%
	select_if(is.Date) %>%
	colnames()
date_cols
epi_head_and_tail(data_f[, date_cols], cols = length(date_cols))
# ===

# ===
# Check all column types accounted
dim(data_f)
epi_clean_count_classes(df = data_f)
# Looks good
# ===
# ////////////


# ////////////
# ===
# Contingency table significance tests ----
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

# Include only columns with unique values of 2 or more:
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
# ===
# ////////////


# ////////////
# ===
# Generate contingency tables ----
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
# ===
# ////////////
