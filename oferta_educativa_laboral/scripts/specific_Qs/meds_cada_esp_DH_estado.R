# ////////////

# Now have shape files for OOADs though

# ===
# Same bar plots but by Estado not OOAD ---
# Need to merge, Edo Mex, CDMX, Veracruz into one for each
summary(as.factor(meds_OOAD_merged_per10k_long_top$DELEGACION))
meds_OOAD_merged_per10k_long_top

# Go back so that we can get counts per OOAD, merge to states, re-calc meds 10k:
colnames(meds_OOAD_merged)
summary(as.factor(meds_OOAD_merged$DELEGACION))
epi_head_and_tail(meds_OOAD_merged)
epi_head_and_tail(meds_OOAD_merged, last_cols = T)

# So sum Veracruz Norte and Veracruz Sur so that we have Veracruz only:
epi_head_and_tail(meds_OOAD_merged)
# View(meds_OOAD_merged)
sum_ids <- c("Veracruz Norte", "Veracruz Sur")
sub_df <- meds_OOAD_merged %>%
    filter(DELEGACION %in% sum_ids) %>%
    summarise(across(-DELEGACION, sum, na.rm = TRUE)) %>%
    mutate(DELEGACION = "Veracruz") %>%
    select(DELEGACION, everything())
sub_df

meds_OOAD_merged_states <- bind_rows(meds_OOAD_merged, sub_df)
epi_head_and_tail(meds_OOAD_merged_states)
# View(meds_OOAD_merged_states)

meds_OOAD_merged_states <- meds_OOAD_merged_states %>%
    filter(!DELEGACION %in% sum_ids)
epi_head_and_tail(meds_OOAD_merged_states)

# Same for Estado de México:
sum_ids <- c("Estado de México Oriente", "Estado de México Poniente")
sub_df <- meds_OOAD_merged_states %>%
    filter(DELEGACION %in% sum_ids) %>%
    summarise(across(-DELEGACION, sum, na.rm = TRUE)) %>%
    mutate(DELEGACION = "Estado de México") %>%
    select(DELEGACION, everything())
sub_df

meds_OOAD_merged_states <- bind_rows(meds_OOAD_merged_states, sub_df)
epi_head_and_tail(meds_OOAD_merged_states)
# View(meds_OOAD_merged_states)

meds_OOAD_merged_states <- meds_OOAD_merged_states %>%
    filter(!DELEGACION %in% sum_ids)
epi_head_and_tail(meds_OOAD_merged_states)

# Same for Ciudad de México:
sum_ids <- c("Ciudad de México Norte", "Ciudad de México Sur")
sub_df <- meds_OOAD_merged_states %>%
    filter(DELEGACION %in% sum_ids) %>%
    summarise(across(-DELEGACION, sum, na.rm = TRUE)) %>%
    mutate(DELEGACION = "Ciudad de México") %>%
    select(DELEGACION, everything())
sub_df

meds_OOAD_merged_states <- bind_rows(meds_OOAD_merged_states, sub_df)
epi_head_and_tail(meds_OOAD_merged_states)
# View(meds_OOAD_merged_states)

meds_OOAD_merged_states <- meds_OOAD_merged_states %>%
    filter(!DELEGACION %in% sum_ids)
epi_head_and_tail(meds_OOAD_merged_states)
# ===


# ===
# Rename to Estado:
colnames(meds_OOAD_merged_states)[colnames(meds_OOAD_merged_states) == "DELEGACION"] <- "Estado"
colnames(meds_OOAD_merged_states)
# ===

# ===
# Re calculate per 10k:
epi_head_and_tail(meds_OOAD_merged_states)

# Create new df with meds per eg 10,000 pop, this is per OOAD:
meds_OOAD_merged_states_per10k <- meds_OOAD_merged_states %>%
    # for every column except Estado & population, compute rate per 10 000
    mutate(across(
        -c(Estado, Derechohabientes_DIR_03_2025),
        ~ round(.x / Derechohabientes_DIR_03_2025 * 10000, 2),
        .names = "{.col}_por10k"
    ))

epi_head_and_tail(meds_OOAD_merged_states_per10k)
epi_head_and_tail(meds_OOAD_merged_states_per10k, last_cols = T)

meds_OOAD_merged_states_per10k[, c("Estado", "Total",
                            "ANESTESIOLOGIA_por10k",
                            "Total_por10k",
                            "medicos_por_mil_derechohabientes_072025")
]
colnames(meds_OOAD_merged_states_per10k)
# Drop cols not needed:
meds_OOAD_merged_states_per10k$medicos_por_mil_derechohabientes_072025_por10k <- NULL
# ===


# ===
# Plot per 10k ----
epi_head_and_tail(meds_OOAD_merged)
epi_head_and_tail(meds_OOAD_merged, last_cols = T)

epi_head_and_tail(meds_OOAD_merged_per10k, last_cols = T)
epi_head_and_tail(meds_OOAD_merged_per10k_long, cols = 3)
epi_head_and_tail(meds_OOAD_merged_per10k_long_top, cols = 3)


summary(as.factor(meds_OOAD_merged_per10k_long_top$DELEGACION))


# This was for states, not OOADs:
meds_OOAD_merged_states_per10k_long <- meds_OOAD_merged_states_per10k %>%
    select(Estado, ends_with("_por10k")) %>%
    pivot_longer(
        -Estado,
        names_to  = "Área de Responsabilidad",
        values_to = "Tasa por 10 mil derechohabientes"
    )
meds_OOAD_merged_states_per10k_long
summary(as.factor(meds_OOAD_merged_states_per10k_long$Estado))


# ===
# Totals:
df <- meds_OOAD_merged_per10k_long
unique(df$DELEGACION)

totals_long <- df %>%
    filter(Estado == "Total") %>%
    arrange(desc(`Tasa por 10 mil derechohabientes`))
# View(totals_long)

# Remove the string _por10k from each row value in `Área de Responsabilidad`:
totals_long$`Área de Responsabilidad` <- gsub("_por10k", "", totals_long$`Área de Responsabilidad`)
totals_long

df <- totals_long[-1, ] # drop "Total" row
plot_1 <- ggplot(df, aes(x = reorder(
    `Área de Responsabilidad`,
    `Tasa por 10 mil derechohabientes`
),
y = `Tasa por 10 mil derechohabientes`,
fill = `Tasa por 10 mil derechohabientes`)
) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis_c() +
    labs(
        x = NULL,
        y = NULL
        # title = "Tasa de especialistas por delegación"
    )
plot_1

# Save last plot:
file_n <- 'plot_bar_meds_esp_DH_10k_estado'
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
meds_OOAD_merged_states_per10k_long

# Remove Total from Estado:
meds_OOAD_merged_states_per10k_long <- meds_OOAD_merged_states_per10k_long %>%
    filter(Estado != "Total")
meds_OOAD_merged_states_per10k_long

# Get top 10 per Estado:
top_n <- 10
meds_OOAD_merged_states_per10k_long_top <- meds_OOAD_merged_states_per10k_long %>%
    group_by(Estado) %>%
    slice_max(`Tasa por 10 mil derechohabientes`, n = top_n) %>%
    ungroup()
meds_OOAD_merged_states_per10k_long_top

# Remove the string _por10k from each row value in `Área de Responsabilidad`:
meds_OOAD_merged_states_per10k_long_top$`Área de Responsabilidad` <- gsub("_por10k", "", meds_OOAD_merged_states_per10k_long_top$`Área de Responsabilidad`)
meds_OOAD_merged_states_per10k_long_top

# For each Estado, plot separately a bar plot of `Área de Responsabilidad` by `Tasa por 10 mil derechohabientes`:
# safe file‐name from Estado:
safe_name <- function(x) {
    gsub("[^[:alnum:]_-]", "_", x)
}

# unique values:
dels <- unique(meds_OOAD_merged_states_per10k_long_top$Estado)

for(del in dels) {
    df_sub <- meds_OOAD_merged_states_per10k_long_top %>%
        filter(Estado == del)

    p <- ggplot(df_sub, aes(
        x = reorder(`Área de Responsabilidad`,
                    `Tasa por 10 mil derechohabientes`),
        y = `Tasa por 10 mil derechohabientes`
    )) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(
            title = del,
            x     = NULL,
            y     = "Tasa por 10 000 derechohabientes"
        )

    # Save:
    file_n <- paste0("plot_tasa_10k_meds_esp_top_10_estado_", safe_name(del))
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
    ggsave(outfile, plot = p,
           height = 12, width = 12, units = "in",
           dpi = 300,  # Adjust DPI to maintain font size
           scale = 1  # Increase scale factor
    )
}
# ===
# ////////////



