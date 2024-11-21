############
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

# Plot significant values only
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
############

############
# Plot triangles for all but save with large area base height and width
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
############


############
# TO DO:
# Scatter/Pairwise/multi plots for relevant combinations
# Need to code a convenience function
scatter.smooth(df_factor$IgA_Lavado, df_factor$IgA_Suero)

ggplot(df_factor, aes(x = IgA_Lavado, y = IgA_Suero)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  # 'se' controls the confidence interval display
  theme_minimal() +
  labs(x = "IgA_Lavado", y = "IgA_Suero", title = "Scatter Plot with Best Fit Line")
############


