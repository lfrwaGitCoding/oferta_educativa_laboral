

# ===
# Calculate DH per med, ie how many pop per each specialty ----
epi_head_and_tail(meds_OOAD_merged)
epi_head_and_tail(meds_OOAD_merged, last_cols = T)
colnames(meds_OOAD_merged)
epi_head_and_tail(meds_OOAD_merged[, c("DELEGACION",
                                              "Total",
                                              "MEDICINA FAMILIAR",
                                              "MEDICINA DE URGENCIAS",
                                              "ANESTESIOLOGIA",
                                              "Derechohabientes_DIR_03_2025"
                                              )],
                  cols = 6
                  )
# DH per med spec:
# 63025789 / 93471
# 63025789 / 24632
# meds_OOAD_merged$derechohabientes_por_med_total <- round(meds_OOAD_merged$Derechohabientes_DIR_03_2025 / meds_OOAD_merged$Total, 2)
# summary(meds_OOAD_merged$derechohabientes_por_med_total)

# Inf values will be for eg:
round(meds_OOAD_merged$Derechohabientes_DIR_03_2025 / meds_OOAD_merged$TRANSPORTES, 2)
# where OOAD has no med in that specialty


# Run for all specialties:
meds_OOAD_merged_DH_per_med <- meds_OOAD_merged %>%
    # for every column except DELEGACION and population, compute rate per 10 000
    mutate(across(
        -c(DELEGACION, Derechohabientes_DIR_03_2025, medicos_por_mil_derechohabientes_072025),
        ~ round(Derechohabientes_DIR_03_2025 / .x, 2),
        .names = "{.col}_DH_por_med"
    )) %>%
    # replace any Inf (or -Inf) with 0:
    mutate(across(ends_with("_DH_por_med"), ~ replace(.x, is.infinite(.x), 0)))

epi_head_and_tail(meds_OOAD_merged_DH_per_med)
epi_head_and_tail(meds_OOAD_merged_DH_per_med, last_cols = T)

meds_OOAD_merged_DH_per_med[, c("DELEGACION", "Total",
                            "ANESTESIOLOGIA_DH_por_med",
                            "Total_DH_por_med",
                            "medicos_por_mil_derechohabientes_072025")
                            ]
colnames(meds_OOAD_merged_DH_per_med)
head(meds_OOAD_merged_DH_per_med[, c(1:4, 105:110, 202:205)])
tail(meds_OOAD_merged_DH_per_med[, c(1:4, 105:110, 202:205)])
# ===


# ===
# Plot DH por med ----
meds_OOAD_merged_DH_per_med_long <- meds_OOAD_merged_DH_per_med %>%
    select(DELEGACION, ends_with("_DH_por_med")) %>%
    pivot_longer(
        -DELEGACION,
        names_to  = "Área de Responsabilidad",
        values_to = "Derechohabientes por plaza de médico"
    )
meds_OOAD_merged_DH_per_med_long
colnames(meds_OOAD_merged_DH_per_med_long)
summary(as.factor(meds_OOAD_merged_DH_per_med_long$DELEGACION))
summary(meds_OOAD_merged_DH_per_med_long$`Derechohabientes por plaza de médico`)

# ===
# Totals:
totals_long <- meds_OOAD_merged_DH_per_med_long %>%
    filter(DELEGACION == "Total") %>%
    arrange(desc(`Derechohabientes por plaza de médico`))
# View(totals_long)

# Remove the string _por10k from each row value in `Área de Responsabilidad`:
totals_long$`Área de Responsabilidad` <- gsub("_DH_por_med", "", totals_long$`Área de Responsabilidad`)
totals_long


# TO DO: continue here
#
df <- totals_long[-1, ] # drop "Total" row
plot_1 <- ggplot(df, aes(x = `Área de Responsabilidad`,
    #                          reorder(
    # `Área de Responsabilidad`,
    # `Derechohabientes por plaza de médico`
    # ),
    y = `Derechohabientes por plaza de médico`,
    fill = `Derechohabientes por plaza de médico`)
    ) +
    geom_col() +
    coord_flip() +
    # scale_fill_viridis_c() + # continuous scale
    theme_minimal() +
    labs(
        x = NULL,
        y = NULL
        # title = "Tasa de especialistas por delegación"
    )
plot_1

# Save last plot:
file_n <- 'plot_bar_meds_esp_DH_10k'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile, plot = plot_1,
       height = 12, width = 12, units = "in",
       dpi = 300,  # Adjust DPI to maintain font size
       scale = 1  # Increase scale factor
)
# ===


# ===
# Per OOAD but only top 10:
meds_OOAD_merged_per10k_long

# Remove Total from DELEGACION:
df <- meds_OOAD_merged_per10k_long
df <- meds_OOAD_merged_per10k_long %>%
    filter(DELEGACION != "Total")
df

# Get top 10 per DELEGACION:
top_n <- 10
df <- df %>%
    group_by(DELEGACION) %>%
    slice_max(`Tasa por 10 mil derechohabientes`, n = top_n) %>%
    ungroup()
df

# Remove the string _por10k from each row value in `Área de Responsabilidad`:
df$`Área de Responsabilidad` <- gsub("_por10k", "", df$`Área de Responsabilidad`)
df

# For each DELEGACION, plot separately a bar plot of `Área de Responsabilidad` by `Tasa por 10 mil derechohabientes`:
# safe file‐name from DELEGACION:
safe_name <- function(x) {
    gsub("[^[:alnum:]_-]", "_", x)
}

# unique values:
dels <- unique(df$DELEGACION)

for(del in dels) {
    df_sub <- df %>%
        filter(DELEGACION == del)

    p <- ggplot(df_sub, aes(
        x = reorder(`Área de Responsabilidad`,
                    `Tasa por 10 mil derechohabientes`),
        y = `Tasa por 10 mil derechohabientes`
    )) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        theme_minimal() +
        labs(
            title = del,
            x     = NULL,
            y     = "Tasa por 10 000 derechohabientes"
        )

    # Save:
    file_n <- paste0("plot_tasa_10k_meds_esp_top_10_", safe_name(del))
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
    ggsave(outfile, plot = p,
           height = 12, width = 12, units = "in",
           dpi = 300,  # Adjust DPI to maintain font size
           scale = 1  # Increase scale factor
    )
}

# But keep object, will use in next script:
meds_OOAD_merged_per10k_long_top <- df
rm(df) # so meds_OOAD_merged_per10k_long still has Total row
# ===


# ===
# Maps
# Need to merge, Edo Mex, CDMX, Veracruz into one for each
# Continued in separate script
# ===

# ////////////







# ////////////
# The end ----
# Outputs saved to disk, no need to save as rdata.
sessionInfo()

# Closing message loggers:
if (!interactive()) { # TRUE if not interactive, will then log output
    info(logger, "Script completed successfully")

    # Close screen output log (both screen and warnings/error messages):
    # Stop sinks
    sink(type = "message")
    close(sink_msg)  # Close the connection
    sink()
}

# q()
# ////////////
