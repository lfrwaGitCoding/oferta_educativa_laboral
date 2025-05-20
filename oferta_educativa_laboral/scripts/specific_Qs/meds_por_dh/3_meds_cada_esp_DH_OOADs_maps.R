library(sf)
library(gridExtra)
library(patchwork) # for wrap_elements()
library(stringi)

# ////////////
# Maps ----

# ===
# state boundaries, but now have OOAD boundaries (middle cut)
# mex_sf <- ne_states(country = "Mexico", returnclass = "sf") %>%
#     select(name_es) %>%
#     rename(Estado = name_es)

ls()

sh_dir <- "~/Documents/work/science/devel/github/med-comp-imss/geo_stats/shapefiles/por_OOAD/"
# read the shapefile
mex_sf <- st_read(paste0(sh_dir, "mexico_OOADs.shp"))
mex_sf

# check CRS
st_crs(mex_sf)

# Remove NAs:
str(mex_sf)
summary(as.factor(mex_sf$name_es))
mex_sf <- mex_sf %>%
    filter(!is.na(name_es))
summary(as.factor(mex_sf$name_es))
str(mex_sf)
# ===

# ===
# names should be OOADs:
unique(mex_sf$name_es)


# df <- meds_OOAD_merged_DH_per_med_long
df <- meds_OOAD_merged_per10k_long

unique(df$DELEGACION)
epi_head_and_tail(df, cols = 3)

# Manually pick `Área de Responsabilidad` and search across full file:
# TO DO: manual, also _DH_por_med
top_specs <- c("Total_por10k",
               "MEDICINA FAMILIAR_por10k",
               "MEDICINA DE URGENCIAS_por10k",
               "ANESTESIOLOGIA_por10k",
               "PEDIATRIA_por10k",
               "MEDICINA INTERNA_por10k",
               "GINECOLOGIA Y OBSTETRICIA_por10k",
               "CIRUGIA GENERAL_por10k",
               "IMAGENOLOGIA, DIAGNOSTICO Y TERAPEUTICA_por10k",
               "N.CENT.REG.DELEG.SUBDELEG.DIR.ADMTV.U.OP_por10k",
               "TRAUMATOLOGIA Y ORTOPEDIA_por10k"
               )

# Get top_specs:
length(which(df$`Área de Responsabilidad` %in% top_specs))

# Plot all:
df_to_map <- df

# Plot only top / most frequent specialties:
# df_to_map <- df %>%
#     filter(`Área de Responsabilidad` %in% top_specs)
# df_to_map


# ===


# ===
# Make col name match:
col_name_to_map <- "OOAD"

colnames(mex_sf)[which(colnames(mex_sf) == "name_es")] <- col_name_to_map
colnames(df_to_map)[which(colnames(df_to_map) == "DELEGACION")] <- col_name_to_map
colnames(mex_sf)
colnames(df_to_map)

summary(as.factor(mex_sf[[col_name_to_map]]))
as.factor(mex_sf[[col_name_to_map]])

unique(sort(mex_sf[[col_name_to_map]]))
unique(sort(df_to_map[[col_name_to_map]]))

which(!unique(mex_sf[[col_name_to_map]]) %in% unique(df_to_map[[col_name_to_map]]))

# name‐sets:
ne_names  <- sort(unique(mex_sf[[col_name_to_map]]))
data_names <- sort(unique(df_to_map[[col_name_to_map]]))

# in Natural Earth:
setdiff(ne_names, data_names)
# in SIAP/DIR:
setdiff(data_names, ne_names)

# meds_clean <- df_to_map %>%
#     mutate(
#         Estado = recode(
#             Estado,
#             "Ciudad de México" = "México",
#             "Coahuila"         = "Coahuila de Zaragoza",
#             "Veracruz"        = "Estado de Veracruz"
#         )
#     )

# setdiff(unique(mex_sf[[col_name_boundary]]), unique(meds_clean[[col_name_boundary]]))
# setdiff(unique(meds_clean[[col_name_boundary]]), unique(mex_sf[[col_name_boundary]]))
# ===



# ===
# Plot maps:
# TO DO: clean up df switching
df <- df_to_map
df

df_map <- df %>%
    rename(Tasa = `Tasa por 10 mil derechohabientes`)
    # rename(Tasa = `Derechohabientes por plaza de médico`)
epi_head_and_tail(df_map , cols = 3)

# Remove the string _por10k from each row value in `Área de Responsabilidad`:
# TO DO: manual
df_map$`Área de Responsabilidad` <- gsub("_por10k", "", df_map$`Área de Responsabilidad`)
# df_map$`Área de Responsabilidad` <- gsub("_DH_por_med", "", df_map$`Área de Responsabilidad`)
epi_head_and_tail(df_map , cols = 3)

# Exclude Total rows, these are per OOAD for all meds:
# df_map %>%
#     filter(grepl("Total", `Área de Responsabilidad`))
df_map <- df_map %>%
    filter(!grepl("Total", `Área de Responsabilidad`))
epi_head_and_tail(df_map , cols = 3)

# These are nationwide values for each specialty:
df_map <- df_map %>%
    filter(!grepl("Total", OOAD))
epi_head_and_tail(df_map , cols = 3)

# Global max for common legend:
max_tasa <- max(df_map$Tasa, na.rm = TRUE)

# safe‐file helper:
safe_fname <- function(x) stri_trans_general(x, "Latin-ASCII") %>%
    gsub("[^[:alnum:]_]", "_", .)

# loop over specialties:
specialties <- unique(df_map$`Área de Responsabilidad`)


for (spec in specialties) {
    sub <- df_map %>% filter(`Área de Responsabilidad` == spec)

    m   <- mex_sf %>%
        left_join(sub, by = col_name_to_map)

    # save the raw table:
    write.csv(
        st_drop_geometry(m),
        file.path(results_subdir, paste0(safe_fname(spec), ".csv")),
        row.names = FALSE
    )

    p <- ggplot(m) +
        geom_sf(aes(fill = Tasa), colour = "grey80", size = 0.2) +
        # No rounding:
        # geom_sf_text(aes(label = Tasa)) +
        geom_sf_text(aes(label = round(Tasa, 2)), size = 2.5) +
        scale_fill_gradient(
            # name    = "Tasa\n(×10 000)",
            low      = "red",
            high     = "green",
            # limits   = c(0, max_tasa),
            na.value = "white"
            ) +
        labs(title = spec,
             x     = NULL,
             y     = NULL
             ) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
              )

    print(p)

    # # Save:
    # file_n <- paste0("plot_map_tasa_10k_meds_esp_top_10_", safe_name(spec))
    # suffix <- 'pdf'
    # outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
    # ggsave(outfile, plot = p,
    #        height = 12, width = 12, units = "in",
    #        dpi = 300,  # Adjust DPI to maintain font size
    #        scale = 1  # Increase scale factor
    # )
}

# TO DO: continue here, remove numbers within polygons, invert colours for DH per med
# With table side by side:
for (spec in specialties) {
    sub <- df_map %>%
        filter(`Área de Responsabilidad` == spec)

    m <- mex_sf %>%
        left_join(sub, by = col_name_to_map)

    p_map <- ggplot(m) +
        geom_sf(aes(fill = Tasa), colour = "grey80", size = 0.2) +
        scale_fill_gradient(name    = "Tasa\n(×10 000)",
                            low = "red",
                            high = "green",
                            # limits   = c(0, max_tasa),
                            na.value = "white") +
        labs(title = spec) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))

    tbl_df <- st_drop_geometry(m) %>%
        select(!!sym(col_name_to_map), Tasa) %>%
        arrange(desc(Tasa))

    tbl_grob <- tableGrob(
        tbl_df,
        rows = NULL,
        theme = ttheme_minimal(
            core    = list(fg_params = list(cex = 0.6, fontface = "plain")),
            colhead = list(fg_params = list(cex = 0.7, fontface = "bold"))
        )
    )

    # wrap the grob so patchwork can combine it
    tbl_panel <- wrap_elements(full = tbl_grob)

    combined <- (p_map | tbl_panel) +
        plot_layout(widths = c(2, 1))

    print(combined)

    # # Save:
    file_n <- paste0("plot_map_tasa_10k_meds_esp_top_10_", safe_name(spec))
    suffix <- 'pdf'
    outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
    ggsave(outfile, plot = combined,
           height = 12, width = 12, units = "in",
           dpi = 300,  # Adjust DPI to maintain font size
           scale = 1  # Increase scale factor
    )
}


# ===
# ////////////
