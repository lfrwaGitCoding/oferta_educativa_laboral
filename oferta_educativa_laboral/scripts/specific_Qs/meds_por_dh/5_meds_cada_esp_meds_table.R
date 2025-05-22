


# Absolute counts:
epi_head_and_tail(meds_OOAD_merged)
epi_head_and_tail(meds_OOAD_merged, last_cols = T)
colnames(meds_OOAD_merged)

# meds per 10k pop:
epi_head_and_tail(meds_OOAD_merged_per10k)
epi_head_and_tail(meds_OOAD_merged_per10k, last_cols = T)

# pop per med:
epi_head_and_tail(meds_OOAD_merged_DH_per_med)
epi_head_and_tail(meds_OOAD_merged_DH_per_med, last_cols = T)

# Merge and keep common cols, same data except for estimates:
common_cols <- intersect(names(meds_OOAD_merged_per10k), names(meds_OOAD_merged_DH_per_med))
common_cols

df_merged <- merge(
    meds_OOAD_merged_per10k,
    meds_OOAD_merged_DH_per_med,
    by   = common_cols,
    all  = TRUE
    )

dim(meds_OOAD_merged_per10k)
dim(meds_OOAD_merged_DH_per_med)

# Should be 104 + 101 + 102 or so:
dim(df_merged)
epi_head_and_tail(df_merged)
epi_head_and_tail(df_merged, last_cols = T)
colnames(df_merged)

colnames(df_merged)[which(colnames(df_merged) == "Total")] <- "Total_meds"

df_merged <- df_merged %>%
    relocate(Derechohabientes_DIR_03_2025, .after = Total_meds) %>%
    relocate(medicos_por_mil_derechohabientes_072025, .after = Derechohabientes_DIR_03_2025)
colnames(meds_OOAD_merged)
epi_head_and_tail(meds_OOAD_merged)


# re-order cols:
col_order <- c("DELEGACION",
               "Total_meds",
               "Derechohabientes_DIR_03_2025",
               "medicos_por_mil_derechohabientes_072025",
               "Total_por10k",
               "derechohabientes_por_med_total",
               "Total_DH_por_med"
               )

col_order <- c(col_order,
               setdiff(names(df_merged), col_order)
                       )
col_order

df_merged_reord <- df_merged %>% select(all_of(col_order))
epi_head_and_tail(df_merged_reord)
epi_head_and_tail(df_merged_reord, last_cols = T)

# Drop repeated cols:
df_merged_reord$Total_por10k <- NULL
df_merged_reord$Total_DH_por_med <- NULL


# Re-order rows:
df_merged_reord$DELEGACION
df_merged_reord <- df_merged_reord %>% arrange(DELEGACION)
df_merged_reord[c(1:31, 33:36, 32), c(1:3)]
# So Total is at the end:
df_merged_reord <- df_merged_reord[c(1:31, 33:36, 32), ]
df_merged_reord$DELEGACION

# Save:
dir.exists(results_subdir)
outfile <- 'tasas_meds_qna_07_2025_PLZ_totales.txt'
outfile <- file.path(results_subdir, outfile)
outfile
epi_write(df_merged_reord, outfile)

# As excel, various spreadsheets:
epi_head_and_tail(df_merged_reord, cols = 8)
epi_head_and_tail(df_merged_reord, last_cols = T)
colnames(df_merged_reord)

df_totals <- df_merged_reord[, c(1:105)]
epi_head_and_tail(df_totals, cols = 8)
epi_head_and_tail(df_totals, last_cols = T)

df_10k <- df_merged_reord[, c(1, 106:205)]
epi_head_and_tail(df_10k, cols = 8)
epi_head_and_tail(df_10k, last_cols = T)

df_dh_med <- df_merged_reord[, c(1, 206:ncol(df_merged_reord))]
epi_head_and_tail(df_dh_med, cols = 8)
epi_head_and_tail(df_dh_med, last_cols = T)


# Actual save:
wb <- createWorkbook()
addWorksheet(wb, "totales")
addWorksheet(wb, "10k")
addWorksheet(wb, "dh_med")


# write spreadsheets:
writeData(wb, sheet = "totales", x = df_totals)
writeData(wb, sheet = "10k", x = df_10k)
writeData(wb, sheet = "dh_med", x = df_dh_med)


# to disk:
outfile <- 'tasas_meds_qna_07_2025_PLZ_totales.xlsx'
outfile <- file.path(results_subdir, outfile)
outfile
saveWorkbook(wb,
             outfile,
             overwrite = TRUE
             )

