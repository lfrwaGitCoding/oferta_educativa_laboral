# Import libraries ----
library(data.table)
library(episcout)
library(ggthemes)
library(cowplot)
library(tidyverse)
library(pander)
library(log4r)
library(janitor)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(sf)
library(rnaturalearth)
library(stringi)
library(openxlsx)


# ////////////
# Output dir, based on today's date ----
# script_n <- 'meds_cada_esp_DH_OOADs'
script_n <- 'meds_ads_cada_esp_DH_estado'
results_subdir <- "~/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/results/21_10_2025_2_clean_dups_col_types_Qna_15_Plantilla_2025"
results_subdir <- file.path(
  results_subdir,
  "21_10_2025_2_clean_dups_col_types_Qna_15_Plantilla_2025"
)
# ////////////

# Reading from Djaine directly:
datahub <- "~/Documents/work/comp_med_medicina_datos/projects/datahub/"
meds_plzocu_dh_ads_cons <- "agregados/internal_UEI/meds_por_DH_processed/matriz_medicos_202510211625.csv"
meds_plzocu_dh_ads_cons <- file.path(datahub, meds_plzocu_dh_ads_cons)

meds_plzocu_dh_ads_cons <- epi_read(meds_plzocu_dh_ads_cons)
epi_head_and_tail(meds_plzocu_dh_ads_cons)
colnames(meds_plzocu_dh_ads_cons)


# ===
# Create new df with meds per eg 10,000 pop, this is per OOAD:
meds_OOAD_merged_per10k <- meds_plzocu_dh_ads_cons %>%
  # for every column except ESTADO and population, compute rate per 10 000
  mutate(across(
    -c(
      CVEDELEGACION,
      ESTADO,
      DERECHOHABIENTES_PDA_consultorio_032025,
      ISM_meds_ads,
      TOTAL_MEDICOS_152025_PLZOCU
    ),
    ~ round(.x / DERECHOHABIENTES_PDA_consultorio_032025 * 10000, 2),
    .names = "{.col}_por10k"
  ))

epi_head_and_tail(meds_OOAD_merged_per10k)
epi_head_and_tail(meds_OOAD_merged_per10k, last_cols = T)

meds_OOAD_merged_per10k[, c(
  "ESTADO",
  "TOTAL_MEDICOS_152025_PLZOCU",
  "ANESTESIOLOGIA_por10k"
)]
colnames(meds_OOAD_merged_per10k)
# Drop cols not needed:
#meds_OOAD_merged_per10k$medicos_por_mil_derechohabientes_072025_por10k <- NULL
# ===

# ===
# Plot per 10k ----
meds_OOAD_merged_per10k_long <- meds_OOAD_merged_per10k %>%
  select(ESTADO, ends_with("_por10k")) %>%
  pivot_longer(
    -ESTADO,
    names_to = "Área de Responsabilidad",
    values_to = "Tasa por 10 mil derechohabientes"
  )
meds_OOAD_merged_per10k_long
summary(as.factor(meds_OOAD_merged_per10k_long$ESTADO))


# ===
# Totals:
totals_long <- meds_OOAD_merged_per10k_long %>%
  #filter(ESTADO == "Total") %>%
  arrange(desc(`Tasa por 10 mil derechohabientes`))
# View(totals_long)
totals_long

# Remove the string _por10k from each row value in `Área de Responsabilidad`:
totals_long$`Área de Responsabilidad` <- gsub(
  "_por10k",
  "",
  totals_long$`Área de Responsabilidad`
)
totals_long

#df <- totals_long[-1, ] # drop "Total" row

plot_1 <- ggplot(
  totals_long,
  aes(
    x = reorder(
      `Área de Responsabilidad`,
      `Tasa por 10 mil derechohabientes`
    ),
    y = `Tasa por 10 mil derechohabientes`,
    fill = `Tasa por 10 mil derechohabientes`
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c() +
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
ggsave(
  outfile,
  plot = plot_1,
  height = 12,
  width = 12,
  units = "in",
  dpi = 300, # Adjust DPI to maintain font size
  scale = 1 # Increase scale factor
)
# ===

# ===
# Per OOAD but only top 10:
meds_OOAD_merged_per10k_long

# Remove Total from ESTADO:
df <- meds_OOAD_merged_per10k_long
df <- meds_OOAD_merged_per10k_long %>%
  filter(ESTADO != "Total")
df

# Get top 10 per ESTADO:
top_n <- 10
df <- df %>%
  group_by(ESTADO) %>%
  slice_max(`Tasa por 10 mil derechohabientes`, n = top_n) %>%
  ungroup()
df

# Remove the string _por10k from each row value in `Área de Responsabilidad`:
df$`Área de Responsabilidad` <- gsub(
  "_por10k",
  "",
  df$`Área de Responsabilidad`
)
df

# For each ESTADO, plot separately a bar plot of `Área de Responsabilidad` by `Tasa por 10 mil derechohabientes`:
# safe file‐name from ESTADO:
safe_name <- function(x) {
  gsub("[^[:alnum:]_-]", "_", x)
}

# unique values:
dels <- unique(df$ESTADO)

for (del in dels) {
  df_sub <- df %>%
    filter(ESTADO == del)

  p <- ggplot(
    df_sub,
    aes(
      x = reorder(
        `Área de Responsabilidad`,
        `Tasa por 10 mil derechohabientes`
      ),
      y = `Tasa por 10 mil derechohabientes`
    )
  ) +
    geom_col(fill = "steelblue") +
    geom_text(
      aes(label = round(`Tasa por 10 mil derechohabientes`, 1)),
      hjust = -0.1, # negative to push label outside the bar
      size = 8
    ) +
    coord_flip() +
    labs(
      title = del,
      x = NULL,
      y = "Tasa por 10,000 derechohabientes"
    ) +
    expand_limits(
      y = max(df_sub$`Tasa por 10 mil derechohabientes`, na.rm = TRUE) *
        1.1
    ) # extra space for labels

  # Save:
  file_n <- paste0("plot_tasa_10k_meds_esp_top_10_", safe_name(del))
  suffix <- 'pdf'
  outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
  ggsave(
    outfile,
    plot = p,
    height = 12,
    width = 12,
    units = "in",
    dpi = 300, # Adjust DPI to maintain font size
    scale = 1 # Increase scale factor
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

# TO DO: separate and clean up
# ////////////
library(sf)
library(gridExtra)
library(patchwork) # for wrap_elements()
library(stringi)


# state boundaries, but now have OOAD boundaries (middle cut)
mex_sf <- ne_states(country = "Mexico", returnclass = "sf") %>%
  select(name_es) %>%
  rename(Estado = name_es)

# check CRS
st_crs(mex_sf)

# Remove NAs:
str(mex_sf)
summary(as.factor(mex_sf$Estado))
mex_sf <- mex_sf %>%
  filter(!is.na(Estado))
summary(as.factor(mex_sf$Estado))
str(mex_sf)

# Plot all:
meds_OOAD_merged_per10k_long_top
meds_OOAD_merged_per10k_long
df_to_map <- meds_OOAD_merged_per10k_long

# Plot only top / most frequent specialties:
# df_to_map <- df %>%
#     filter(`Área de Responsabilidad` %in% top_specs)
# df_to_map
# ===

# ===
# Make col name match:
col_name_to_map <- "ESTADO"

colnames(mex_sf)[which(colnames(mex_sf) == "Estado")] <- col_name_to_map
colnames(df_to_map)[which(colnames(df_to_map) == "ESTADO")] <- col_name_to_map
colnames(mex_sf)
colnames(df_to_map)

summary(as.factor(mex_sf[[col_name_to_map]]))
as.factor(mex_sf[[col_name_to_map]])

unique(sort(mex_sf[[col_name_to_map]]))
unique(sort(df_to_map[[col_name_to_map]]))

which(
  !unique(mex_sf[[col_name_to_map]]) %in% unique(df_to_map[[col_name_to_map]])
)

# name‐sets:
ne_names <- sort(unique(mex_sf[[col_name_to_map]]))
ne_names
data_names <- sort(unique(df_to_map[[col_name_to_map]]))
data_names

# in Natural Earth:
setdiff(ne_names, data_names)
# in SIAP/DIR:
setdiff(data_names, ne_names)

meds_clean <- df_to_map %>%
  mutate(
    ESTADO = recode(
      ESTADO,
      "Ciudad de México" = "México",
      "Coahuila" = "Coahuila de Zaragoza",
      "Veracruz" = "Estado de Veracruz"
    )
  )
meds_clean

setdiff(
  unique(mex_sf[[col_name_to_map]]),
  unique(meds_clean[[col_name_to_map]])
)
setdiff(
  unique(meds_clean[[col_name_to_map]]),
  unique(mex_sf[[col_name_to_map]])
)
# ===

# ===
# Plot maps:
# TO DO: clean up df switching
df <- meds_clean
df

epi_head_and_tail(df, cols = 3)

# Remove the string _por10k from each row value in `Área de Responsabilidad`:
df$`Área de Responsabilidad` <- gsub(
  "_por10k",
  "",
  df$`Área de Responsabilidad`
)
# df_map$`Área de Responsabilidad` <- gsub("_DH_por_med", "", df_map$`Área de Responsabilidad`)
epi_head_and_tail(df, cols = 3)


# Global max for common legend:
max_tasa <- max(df$`Tasa por 10 mil derechohabientes`, na.rm = TRUE)
max_tasa

# safe‐file helper:
safe_fname <- function(x) {
  stri_trans_general(x, "Latin-ASCII") %>%
    gsub("[^[:alnum:]_]", "_", .)
}

# loop over specialties:
specialties <- unique(df$`Área de Responsabilidad`)

for (spec in specialties) {
  sub <- df %>% filter(`Área de Responsabilidad` == spec)

  m <- mex_sf %>%
    left_join(sub, by = col_name_to_map)

  # save raw table:
  write.csv(
    st_drop_geometry(m),
    file.path(results_subdir, paste0(safe_fname(spec), ".csv")),
    row.names = FALSE
  )

  vals <- m$`Tasa por 10 mil derechohabientes`

  # TO DO: clean up, rate_col not defined
  m <- m %>%
    mutate(
      quintil = ifelse(
        is.na(.data[[rate_col]]),
        NA_integer_,
        dplyr::ntile(.data[[rate_col]], 5)
      )
    ) %>%
    mutate(quintil = factor(quintil, levels = 1:5))

  qs <- quantile(
    m[[rate_col]],
    probs = seq(0, 1, length.out = 6),
    na.rm = TRUE,
    type = 7
  )
  # qs[1:5] -> lower edges, qs[2:6] -> upper edges
  lab_all <- sprintf("Q%d: %.2f–%.2f", 1:5, qs[1:5], qs[2:6])
  names(lab_all) <- as.character(1:5)

  # Plot with discrete fill by quintile, keep labels in polygons
  p <- ggplot(m) +
    geom_sf(
      aes(fill = factor(quintil, levels = 1:5)),
      colour = "grey80",
      size = 0.2,
      na.rm = TRUE
    ) +
    geom_sf_text(
      aes(label = round(`Tasa por 10 mil derechohabientes`, 2)),
      size = 2.5,
      na.rm = TRUE
    ) +
    scale_fill_brewer(
      name = "Quintil de tasa\n(×10 000)",
      palette = "Blues",
      direction = 1,
      limits = as.character(1:5), # fixed mapping 1..5
      breaks = as.character(5:1), # legend order high→low
      labels = lab_all[as.character(5:1)],
      drop = FALSE,
      na.value = "white"
    ) +
    labs(title = spec, x = NULL, y = NULL) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

  #print(p)

  # Save:
  file_n <- paste0("plot_map_tasa_10k_meds_esp_", safe_name(spec))
  suffix <- 'pdf'
  outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
  ggsave(
    outfile,
    plot = p,
    height = 8,
    width = 14,
    units = "in",
    dpi = 300, # Adjust DPI to maintain font size
    scale = 1 # Increase scale factor
  )
}

# Save all sheets as one excel:
library(readr)
library(openxlsx)

# path
files <- list.files(results_subdir, pattern = "\\.csv$", full.names = TRUE)
files

# Read and stack, adding filename
df_all <- files |>
  lapply(\(f) {
    read_csv(f, show_col_types = FALSE) |> mutate(source = basename(f))
  }) |>
  bind_rows()
df_all
# drop last col, redundant now:
df_all <- df_all[c(1:3)]
df_all

# Wide
estado_col <- "ESTADO"
area_col <- "Área de Responsabilidad"
value_col <- "Tasa por 10 mil derechohabientes"
values_fill <- NA_real_ # or 0 if you prefer zeros
digits_out <- 4 # decimals for Excel formatting
sheet_name <- "Tasas_por_Estado_AR"

df_wide <- df_all %>%
  pivot_wider(
    names_from = all_of(area_col),
    values_from = all_of(value_col),
    values_fill = values_fill
  ) %>%
  arrange(.data[[estado_col]])
df_wide
TO
DO:outfile <- paste0(results_subdir, "/estado_tasa_10k_meds_esp.xlsx") # Write to Excel one sheet
outfile

write.xlsx(
  df_wide,
  outfile,
  sheetName = sheet_name,
  overwrite = TRUE
)


# TO DO: continue here
# With table side by side, not quintiles:
for (spec in specialties[1:3]) {
  sub <- df %>%
    filter(`Área de Responsabilidad` == spec)

  m <- mex_sf %>%
    left_join(sub, by = col_name_to_map)

  p_map <- ggplot(m) +
    geom_sf(
      aes(fill = `Tasa por 10 mil derechohabientes`),
      colour = "grey80",
      size = 0.2
    ) +
    scale_fill_gradient(
      name = "Tasa\n(×10 000)",
      low = "#bdc9e1", #red
      high = "#045a8d", #"green"
      # limits   = c(0, max_tasa),
      na.value = "white"
    ) +
    labs(title = spec) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  tbl_df <- st_drop_geometry(m) %>%
    select(!!sym(col_name_to_map), `Tasa por 10 mil derechohabientes`) %>%
    arrange(desc(`Tasa por 10 mil derechohabientes`))

  tbl_grob <- tableGrob(
    tbl_df,
    rows = NULL,
    theme = ttheme_minimal(
      core = list(fg_params = list(cex = 0.6, fontface = "plain")),
      colhead = list(fg_params = list(cex = 0.7, fontface = "bold"))
    )
  )

  # wrap the grob so patchwork can combine it
  tbl_panel <- wrap_elements(full = tbl_grob)

  combined <- (p_map | tbl_panel) +
    plot_layout(widths = c(2, 1))

  print(combined)

  # # Save:
  #file_n <- paste0("plot_map_tasa_10k_meds_esp_top_10_", safe_name(spec))
  #suffix <- 'pdf'
  #outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
  #ggsave(
  #  outfile,
  #  plot = combined,
  #  height = 12,
  #  width = 12,
  #  units = "in",
  #  dpi = 300, # Adjust DPI to maintain font size
  #  scale = 1 # Increase scale factor
  #)
}


# TO DO:
# For inset, doesn't quite work though:

library(dplyr)
library(ggplot2)
library(sf)
library(patchwork)

# --- config you may tweak ---
state_col <- "ESTADO" # column in mex_sf with state names
central_states <- c(
  "Ciudad de México",
  "México",
  "Hidalgo",
  "Morelos",
  "Puebla",
  "Tlaxcala",
  "Querétaro",
  "Guanajuato"
)
rate_col <- "Tasa por 10 mil derechohabientes"

# Inset placement (0..1 in panel coordinates). Tweak these to move it.
inset_pos <- list(left = 0.62, bottom = 0.04, right = 0.98, top = 0.8)

# Shared fill scale (identical in both plots)
scale_fill_quintiles <- function(q_labels) {
  scale_fill_brewer(
    name = "Quintil de tasa\n(×10 000)",
    palette = "Blues",
    direction = 1,
    limits = as.character(1:5), # ensure stable mapping 1..5 in both plots
    breaks = as.character(5:1), # legend order high → low
    labels = q_labels[as.character(5:1)],
    drop = FALSE,
    na.value = "white",
    guide = guide_legend(reverse = TRUE)
  )
}

# Build central bbox (rectangular area) and its sf geometry to draw on main map
central_bbox_and_rect <- function(mex_sf, state_col, central_states) {
  cs <- mex_sf %>% filter(.data[[state_col]] %in% central_states)
  if (nrow(cs) == 0) {
    return(NULL)
  }
  bb <- st_bbox(st_union(cs))
  rect <- st_as_sfc(bb) %>% st_as_sf()
  list(bbox = bb, rect = rect)
}

for (spec in specialties[1]) {
  sub <- df %>% filter(`Área de Responsabilidad` == spec)
  m <- mex_sf %>% left_join(sub, by = col_name_to_map)

  # Skip if all rates are NA
  if (all(is.na(m[[rate_col]]))) {
    warning(sprintf("All NA for '%s' in '%s' — skipping.", rate_col, spec))
    next
  }

  # Quintiles 1..5 (1 lowest, 5 highest). Keep as character for stable scale mapping.
  m <- m %>%
    mutate(
      quintil = ifelse(
        is.na(.data[[rate_col]]),
        NA_integer_,
        dplyr::ntile(.data[[rate_col]], 5)
      )
    ) %>%
    mutate(quintil = factor(quintil, levels = 1:5) |> as.character())

  # Labels per quintile (min–max)
  lab_tbl <- m %>%
    st_drop_geometry() %>%
    filter(!is.na(quintil)) %>%
    group_by(quintil) %>%
    summarise(
      minv = min(.data[[rate_col]], na.rm = TRUE),
      maxv = max(.data[[rate_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(label = sprintf("Q%s: %.2f–%.2f", quintil, minv, maxv))

  q_labels <- setNames(lab_tbl$label, lab_tbl$quintil)

  # Central rectangle (for inset view and locator box)
  cb <- central_bbox_and_rect(mex_sf, state_col, central_states)

  # ---------- MAIN MAP ----------
  p_main <- ggplot(m) +
    geom_sf(aes(fill = quintil), colour = "grey80", size = 0.2, na.rm = TRUE) +
    # Numeric labels
    geom_sf_text(
      aes(label = round(.data[[rate_col]], 2)),
      size = 2.5,
      na.rm = TRUE
    ) +
    # Draw locator rectangle on top (dashed)
    {
      if (!is.null(cb)) {
        geom_sf(
          data = cb$rect,
          fill = NA,
          colour = "black",
          linetype = "dashed",
          linewidth = 0.3
        )
      }
    } +
    scale_fill_quintiles(q_labels) +
    labs(title = spec, x = NULL, y = NULL) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

  # ---------- INSET (ZOOM TO RECTANGULAR BBOX) ----------
  if (!is.null(cb)) {
    p_inset <- ggplot(m) +
      geom_sf(
        aes(fill = quintil),
        colour = "grey80",
        size = 0.2,
        na.rm = TRUE
      ) +
      geom_sf_text(
        aes(label = round(.data[[rate_col]], 2)),
        size = 2.2,
        na.rm = TRUE
      ) +
      coord_sf(
        xlim = c(cb$bbox["xmin"], cb$bbox["xmax"]),
        ylim = c(cb$bbox["ymin"], cb$bbox["ymax"]),
        expand = FALSE
      ) +
      scale_fill_quintiles(q_labels) + # <— identical scale = identical colours
      theme_void() +
      theme(
        legend.position = "none",
        # Rectangular panel with a visible border so the inset itself is a rectangle
        panel.background = element_rect(
          fill = "white",
          colour = "black",
          linewidth = 0.5
        )
      )

    p <- p_main +
      inset_element(
        p_inset,
        left = inset_pos$left,
        bottom = inset_pos$bottom,
        right = inset_pos$right,
        top = inset_pos$top,
        align_to = "panel",
        on_top = TRUE
      )
  } else {
    warning("No central states matched; showing main map only.")
    p <- p_main
  }

  print(p)
}

# ===
# ////////////
