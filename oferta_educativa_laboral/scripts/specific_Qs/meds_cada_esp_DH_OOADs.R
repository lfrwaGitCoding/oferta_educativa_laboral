# ////////////
# Script information ----

# SIAP
# Unidad de Personal
# May 2025
# Meds esp por DH



# meds esp (cada una) por DH
# nacional
# por estado / OOAD


# filtrar por sede academica por especialidad

# residentes activos

# num derechohabientes

# Input is rdata output from script:
# 2_dups_col_types.R

# Output are tables por OOAD, meds esp, por DH
# ////////////


# ////////////
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
# ////////////


# ////////////
# Set working directory to the project root  ----
setwd("~/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/")
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts
getwd()
# ////////////


# ////////////
# Global options for plotting, generally need larger fot size:
# If rendering at 60% of linewidth ({ width=60% }) in qmd for PDF output,
# scale base_size accordingly. e.g. for fonts to appear ~12pt in the final PDF, set:

# font_size <- 12 / 0.6  # Final appearance size after scaling (≈20), where scaling is at eg 60% for latex PDF output
font_size <- 14
font_size_x <- font_size - 0 # axis ticks (x-axis)
font_size_y <- font_size - 0 # y-axis ticks

my_theme <- theme_minimal(base_size = font_size) +
    theme(
        plot.title    = element_text(size = font_size, face = "bold"),
        axis.title.x  = element_text(size = font_size),
        axis.title.y  = element_text(size = font_size),
        axis.text.x   = element_text(size = font_size_x),  # x-axis tick labels
        axis.text.y   = element_text(size = font_size_y)  # y-axis tick labels
        # panel.grid    = element_blank()
        # panel.background = element_rect(fill = "white", colour = NA),
        # plot.background  = element_rect(fill = "white", colour = NA),
        # axis.line     = element_blank(),
        # axis.ticks    = element_blank(),
        # legend.key    = element_blank()
    )
theme_set(my_theme)


# cowplot::ggsave2 has dpi = 300 as default
# epi_plot_cow_save has base_height = 11.69, base_width = 8.27, default units is "in"
# these are good options for high quality images
# ////////////


# ////////////
# Load rdata file ----

# ===
rdata_dir <- '/data/data_UP/access_SIAP_18092024/processed/'

# TO DO: Manually set:
# infile <- '2_clean_dups_col_types_Qna_17_Bienestar_2024.rdata.gzip'
# infile <- '2_clean_dups_col_types_Qna_17_Plantilla_2024.rdata.gzip'

# infile <- "2b_clean_subset_2_clean_dups_col_types_Qna_17_Bienestar_2024_meds.rdata.gzip"
# infile <- '2b_clean_subset_2_clean_dups_col_types_Qna_17_Bienestar_2024_enfermeras.rdata.gzip'

# infile <- "2b_clean_subset_2_clean_dups_col_types_Qna_17_Plantilla_2024_meds.rdata.gzip"
# infile <- '2b_clean_subset_2_clean_dups_col_types_Qna_17_Plantilla_2024_enfermeras.rdata.gzip'


infile <- "2b_clean_subset_2_clean_dups_col_types_Qna_07_Plantilla_2025_meds.rdata.gzip"


# Full path and file name:
infile_path <- paste0(rdata_dir, infile)
print(infile_path)

print(dir(path = normalizePath(rdata_dir), all.files = TRUE))

load(infile_path)
ls()
# ===

# ===
# num derechohabientes  ----
DIR_DH_rdata_dir <- '/results/specific_Qs/31_03_2025_medicos_por_mil_derechohabientes/'

# TO DO: Manually set:
infile <- 'medicos_por_mil_derechohabientes_utf8.csv'

# Full path and file name:
infile_path <- paste0(getwd(), DIR_DH_rdata_dir, infile)
print(infile_path)


DIR_num_DH <- epi_read(infile_path, encoding = "UTF-8")
epi_head_and_tail(DIR_num_DH)
epi_clean_count_classes(df = DIR_num_DH)
str(DIR_num_DH)
# ===


# ===
# Get rid of RStudio warnings for loaded objects:
project_root <- project_root
data_dir <- data_dir
results_dir <- results_dir
data_f <- data_f

# TO DO: needs updating:
code_dir <- code_dir

all_colnames <- all_colnames
char_cols <- char_cols
date_cols <- date_cols
fact_cols <- fact_cols
int_cols <- int_cols
id_cols <- id_cols
num_cols <- num_cols

print(project_root)
setwd(project_root)
getwd()
print(dir(path = normalizePath(project_root), all.files = TRUE))
# ===
# ////////////


# ////////////
# Source functions/scripts/etc ----
# TO DO:
# Source (until I update episcout)
source(file.path(paste0(code_dir, '/scripts/funcs_epi_source.R')))
# ////////////


# ////////////
# Output dir, based on today's date ----
script_n <- 'meds_cada_esp_DH_OOADs'
infile_prefix <- strsplit(infile, "\\.")[[1]][1]
results_subdir <- sprintf('%s_%s',
                          format(Sys.Date(), '%d_%m_%Y'),
                          infile_prefix
                          )
results_subdir
results_subdir <- epi_create_dir(base_path = results_dir,
                                 subdir = results_subdir
                                 )
# ////////////


# ////////////
# Capture output / log ----

# ===
# Redirect standard output
if (!interactive()) { # TRUE if not interactive, will then log output
    script_n <- '2_clean_dups_col_types'
    sink_stdout <- paste0(results_subdir, '/', script_n, '.sink_stdout.log')
    sink(sink_stdout, split = TRUE)

    # Redirect messages and warnings
    sink_msg <- file(paste0(results_subdir, '/', script_n, '.sink_msg.log'), open = "wt")
    sink(sink_msg, type = "message")

    # Example outputs
    cat("Test: This is standard output.\n")
    message("Test: This is a message.")
    warning("Test: This is a warning.")
}
# ===

# ===
# Create a logger
if (!interactive()) { # TRUE if not interactive, will then log output
    logger <- create.logger()
    log_n <- paste0(results_subdir, '/', script_n, '.log4r.log')
    logfile(logger) <- log_n # Log file location
    level(logger) <- "INFO"  # Set logging level (DEBUG, INFO, WARN, ERROR)

    # Add log messages
    # info(logger, "Script started")
    # debug(logger, "This is a debug message")
    # warn(logger, "This is a warning")
    # error(logger, "This is an error")
}
# ////////////


# ////////////
# Check column types ----

# ===
# Check loaded cols exist:
# stopifnot(FALSE)  # This will throw an error
stopifnot(length(all_colnames) == (ncol(data_f)))

if (!interactive()) {
    info(logger, "Expected columns match file")
    error(logger, "Expected columns do not match file")
}

colnames(data_f)

stopifnot(all(all_colnames %in% colnames(data_f)))
setdiff(as.character(all_colnames), as.character(colnames(data_f)))
# ===

# ===
# Check all column types accounted
dim(data_f)
epi_clean_count_classes(df = data_f)
# Looks good
# ===
# ////////////


# ////////////
# e.g. Meds Aguascalientes ----

# ===
# Get meds esp por OOAD, e.g.:
colnames(data_f)
summary(data_f$DESCRIP_CLASCATEG)
summary(data_f$DELEGACION)
summary(data_f$NOMBREAR)

summary(data_f[data_f$DELEGACION == "Aguascalientes", ])
summary(data_f[data_f$DELEGACION == "Aguascalientes", c("DESCRIP_CLASCATEG", "NOMBREAR")])

meds_OOAD <- data_f %>%
    count(DELEGACION, NOMBREAR) %>%
    pivot_wider(names_from = NOMBREAR, values_from = n, values_fill = 0) %>%
    arrange(DELEGACION) %>%
    adorn_totals(where = "row") %>%   # adds bottom “Total”
    adorn_totals(where = "col")       # adds right-hand “Total”

epi_head_and_tail(meds_OOAD)
epi_head_and_tail(meds_OOAD, last_cols = T)
# ===

# ===
# Totals, wide to long, etc. for plotting ----

# Extract and sort row‐totals (drop "Total" row):
row_totals <- meds_OOAD %>%
    filter(DELEGACION != "Total") %>%
    select(DELEGACION, Total) %>%
    arrange(Total)
levels_deleg <- row_totals$DELEGACION

# Extract and sort col‐totals (from "Total" row, then drop "Total" col):
col_totals <- meds_OOAD %>%
    filter(DELEGACION == "Total") %>%
    select(-DELEGACION) %>%
    pivot_longer(everything(), names_to = "NOMBREAR", values_to = "Total") %>%
    filter(NOMBREAR != "Total") %>%
    arrange(-Total)
levels_nombrear <- col_totals$NOMBREAR

# Pivot to long, drop Totals, set factor levels:
meds_plot <- meds_OOAD %>%
    pivot_longer(-DELEGACION, names_to = "NOMBREAR", values_to = "n") %>%
    filter(DELEGACION != "Total", NOMBREAR != "Total") %>%
    mutate(
        DELEGACION = factor(DELEGACION, levels = levels_deleg),
        NOMBREAR   = factor(NOMBREAR,   levels = levels_nombrear)
    )

# Plot:
plot_1 <- ggplot(meds_plot, aes(x = NOMBREAR, y = DELEGACION, fill = n)) +
    geom_tile(color = "grey90", size = 0.1) +
    scale_fill_gradient(
        low  = "white",
        high = "steelblue",
        trans = "sqrt"
    ) +
    # theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank()
    ) + # labs(fill = "Frecuencia\n(Escala de raíz cuadrada)")
    labs(
        x = NULL,
        y = NULL,
        fill = "Frecuencia", # \n(Escala de raíz cuadrada)",
        title = "Mapa de calor de Área de Responsabilidad y Delegación, quincena 07 2025"
        )
plot_1

# Save last plot:
file_n <- 'plot_heatmap_NOMBREAR_DELEGACION_1'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile, plot = plot_1,
       height = 25, width = 25, units = "in",
       dpi = 300,  # Adjust DPI to maintain font size
       scale = 1  # Increase scale factor
       )

plot_1 <- ggplot(meds_plot, aes(x = NOMBREAR, y = DELEGACION)) +
 # tiles white for zero
 geom_tile(aes(fill = n), color = "grey90", size = 0.1) +
 scale_fill_gradient(
  low = "white",
  high = "steelblue",
  trans = "sqrt"
    ) +
    # label the non-zeros
    geom_text(
        data = filter(meds_plot, n > 0),
        aes(label = n),
        size  = 2,
        color = "black"
    ) +
    theme(
        axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid   = element_blank()
    ) +
    labs(
        x    = NULL,
        y    = NULL,
        fill = "Frecuencia", # \n(Escala de raíz cuadrada)",
        title = "Mapa de calor de Área de Responsabilidad y Delegación, quincena 07 2025"
    )
plot_1

# Save last plot:
file_n <- 'plot_heatmap_NOMBREAR_DELEGACION_2'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile, plot = plot_1,
       height = 25, width = 25, units = "in",
       dpi = 300,  # Adjust DPI to maintain font size
       scale = 1  # Increase scale factor
       )

# ===
# ////////////


# ////////////
# Interactive plots ----
# “text” aesthetic o that counts are integers:
p_int <- ggplot(meds_plot, aes(
    x     = NOMBREAR,
    y     = DELEGACION,
    fill  = n,
    text  = sprintf("Delegación: %s<br>NOMBREAR: %s<br>Count: %d",
                    DELEGACION, NOMBREAR, n)
)) +
    geom_tile(color = "grey90", size = 0.1) +
    # label the non-zeros
    geom_text(
        data = filter(meds_plot, n > 0),
        aes(label = n),
        size  = 2,
        color = "black"
    ) +
    scale_fill_gradient(low = "white", high = "steelblue", trans = "sqrt") +
    theme_minimal(base_size = 11) +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid  = element_blank()
    ) +
    labs(x = NULL, y = NULL, fill = "Frecuencia")

# Make it interactive, with only “text” tooltip:
gg_int <- ggplotly(p_int, tooltip = "text")
gg_int

# Save to standalone html:
file_n <- 'plot_heatmap_NOMBREAR_DELEGACION_interactive'
suffix <- 'html'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
saveWidget(gg_int, outfile, selfcontained = TRUE)
# ////////////


# ////////////
# Estimate each med specialty by eg 10,000 users/pop ----

# ===
# Sort columns excluding DELEGACION based on row 'Total':
epi_head_and_tail(meds_OOAD)
epi_head_and_tail(meds_OOAD, last_cols = T)

ord_vect <- order(as.numeric(meds_OOAD[meds_OOAD$DELEGACION == "Total", -1]),
                  decreasing = TRUE
                 )
meds_OOAD2 <- meds_OOAD[, c(1, ord_vect + 1)] # plus 1 to include "DELEGACION" col
epi_head_and_tail(meds_OOAD2)
epi_head_and_tail(meds_OOAD2, last_cols = T)
# ===

# ===
# Get num derechohabientes por OOAD:

# View(DIR_num_DH)
summary(DIR_num_DH)
num_DH_OOAD <- DIR_num_DH[, c("Delegación", "Derechohabientes_DIR_03_2025")]
# Remove NAs:
num_DH_OOAD <- num_DH_OOAD[!is.na(num_DH_OOAD$Derechohabientes_DIR_03_2025), ]
num_DH_OOAD <- num_DH_OOAD[!is.na(num_DH_OOAD$Delegación), ]
# View(num_DH_OOAD)
# ===

# ===
# Merge DIR DH values with meds esp per OOAD"
meds_OOAD2$DELEGACION
num_DH_OOAD$Delegación

# 35 - DF Norte       36 - DF Norte       37 - DF Sur         38 - DF Sur
# are merged in DIR data as "Ciudad de México Norte"    "Ciudad de México Sur"
# Add rows with sums for 35 - DF Norte       36 - DF Norte as "Ciudad de México Norte":
sum_ids <- c("35 - DF Norte", "36 - DF Norte")
cdmx <- meds_OOAD2 %>%
    filter(DELEGACION %in% sum_ids) %>%
    summarise(across(-DELEGACION, sum, na.rm = TRUE)) %>%
    mutate(DELEGACION = "Ciudad de México Norte") %>%
    select(DELEGACION, everything())
meds_OOAD3 <- bind_rows(meds_OOAD2, cdmx)
meds_OOAD3 <- meds_OOAD2 %>%
    filter(!DELEGACION %in% sum_ids) %>%
    # bind_rows(cdmx) # to do, not needed but no error?
epi_head_and_tail(meds_OOAD3)

# Same for "Ciudad de México Sur":
sum_ids <- c("37 - DF Sur", "38 - DF Sur")
cdmx <- meds_OOAD3 %>%
    filter(DELEGACION %in% sum_ids) %>%
    summarise(across(-DELEGACION, sum, na.rm = TRUE)) %>%
    mutate(DELEGACION = "Ciudad de México Sur") %>%
    select(DELEGACION, everything())
meds_OOAD4 <- bind_rows(meds_OOAD3, cdmx)
meds_OOAD4 <- meds_OOAD3 %>%
    filter(!DELEGACION %in% sum_ids) %>%
    # bind_rows(cdmx) # to do, not needed but no error?
epi_head_and_tail(meds_OOAD4)

# Clean up, too many dfs:
ls()
rm(meds_OOAD2, meds_OOAD3, cdmx)

# México Oriente      México Poniente should be "Estado de México Oriente" "Estado de México Poniente"
meds_OOAD4$DELEGACION[meds_OOAD4$DELEGACION == "México Oriente"] <- "Estado de México Oriente"
meds_OOAD4$DELEGACION[meds_OOAD4$DELEGACION == "México Poniente"] <- "Estado de México Poniente"

meds_OOAD4$DELEGACION
length(which(meds_OOAD4$DELEGACION %in% num_DH_OOAD$Delegación))
# Mismatches:
meds_OOAD4[which(!meds_OOAD4$DELEGACION %in% num_DH_OOAD$Delegación), "DELEGACION"]
# "Total" and "Nivel Central" are also extra in SIAP data, here df is meds_OOAD4
# DIR data is df num_DH_OOAD

# Add pop per OOAD to meds_OOAD4 for each OOAD:
meds_OOAD_merged <- merge(meds_OOAD4, num_DH_OOAD, by.x = "DELEGACION", by.y = "Delegación", all.x = TRUE)
epi_head_and_tail(meds_OOAD_merged)
epi_head_and_tail(meds_OOAD_merged, last_cols = T)
summary(meds_OOAD_merged$Derechohabientes_DIR_03_2025)


# Sort rows by column "Total":
meds_OOAD_merged <- meds_OOAD_merged %>%
    arrange(desc(Total))
epi_head_and_tail(meds_OOAD_merged)
epi_head_and_tail(meds_OOAD_merged, last_cols = T)


# Get a total for Derechohabientes_DIR_03_2025 and insert in row 'Total':
get_value <- sum(meds_OOAD_merged$Derechohabientes_DIR_03_2025, na.rm = TRUE)
meds_OOAD_merged[meds_OOAD_merged$DELEGACION == "Total", "Derechohabientes_DIR_03_2025"] <- get_value
epi_head_and_tail(meds_OOAD_merged)
epi_head_and_tail(meds_OOAD_merged, last_cols = T)

# Drop "Nivel Central" row:
meds_OOAD_merged <- meds_OOAD_merged[!meds_OOAD_merged$DELEGACION %in% c("Nivel Central"), ]

# Drop cols not needed:
colnames(meds_OOAD_merged)
meds_OOAD_merged$NOMBREAR_DH <- NULL
epi_head_and_tail(meds_OOAD_merged)
epi_head_and_tail(meds_OOAD_merged, last_cols = T)
# ===


# ===
# Create new col with meds per eg 10,000 pop, this is total:
# e.g.:
meds_OOAD_merged[meds_OOAD_merged$DELEGACION == "Aguascalientes", "Derechohabientes_DIR_03_2025"]

# "Total" is plazas totales, not ocupadas!
meds_OOAD_merged$medicos_por_mil_derechohabientes_072025 <- round(meds_OOAD_merged$Total / (meds_OOAD_merged$Derechohabientes_DIR_03_2025 / 1000), 2)

DIR_num_DH
# View(meds_OOAD_merged[, c("DELEGACION", "medicos_por_mil_derechohabientes_072025")])
# View(DIR_num_DH[, c("Delegación", "medicos_por_mil_derechohabientes_072025")])

check_dfs <- merge(meds_OOAD_merged[, c("DELEGACION", "medicos_por_mil_derechohabientes_072025")],
                   DIR_num_DH[, c("Delegación", "medicos_por_mil_derechohabientes_072025")],
                   by.x = "DELEGACION",
                   by.y = "Delegación",
                   all.x = TRUE
                   )
# View(check_dfs)
# ===

# ===
# Create new df with meds per eg 10,000 pop, this is per OOAD:
meds_OOAD_merged_per10k <- meds_OOAD_merged %>%
    # for every column except DELEGACION & population, compute rate per 10 000
    mutate(across(
        -c(DELEGACION, Derechohabientes_DIR_03_2025),
        ~ round(.x / Derechohabientes_DIR_03_2025 * 10000, 2),
        .names = "{.col}_por10k"
    ))

epi_head_and_tail(meds_OOAD_merged_per10k)
epi_head_and_tail(meds_OOAD_merged_per10k, last_cols = T)

meds_OOAD_merged_per10k[, c("DELEGACION", "Total",
                            "ANESTESIOLOGIA_por10k",
                            "Total_por10k",
                            "medicos_por_mil_derechohabientes_072025")
                        ]
colnames(meds_OOAD_merged_per10k)
# Drop cols not needed:
meds_OOAD_merged_per10k$medicos_por_mil_derechohabientes_072025_por10k <- NULL
# ===


# ===
# Plot per 10k ----
meds_OOAD_merged_per10k_long <- meds_OOAD_merged_per10k %>%
    select(DELEGACION, ends_with("_por10k")) %>%
    pivot_longer(
        -DELEGACION,
        names_to  = "Área de Responsabilidad",
        values_to = "Tasa por 10 mil derechohabientes"
    )
meds_OOAD_merged_per10k_long
summary(as.factor(meds_OOAD_merged_per10k_long$DELEGACION))


# ===
# Totals:
totals_long <- meds_OOAD_merged_per10k_long %>%
    filter(DELEGACION == "Total") %>%
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
meds_OOAD_merged_per10k_long <- meds_OOAD_merged_per10k_long %>%
    filter(DELEGACION != "Total")
meds_OOAD_merged_per10k_long

# Get top 10 per DELEGACION:
top_n <- 10
meds_OOAD_merged_per10k_long_top <- meds_OOAD_merged_per10k_long %>%
    group_by(DELEGACION) %>%
    slice_max(`Tasa por 10 mil derechohabientes`, n = top_n) %>%
    ungroup()
meds_OOAD_merged_per10k_long_top

# Remove the string _por10k from each row value in `Área de Responsabilidad`:
meds_OOAD_merged_per10k_long_top$`Área de Responsabilidad` <- gsub("_por10k", "", meds_OOAD_merged_per10k_long_top$`Área de Responsabilidad`)
meds_OOAD_merged_per10k_long_top

# For each DELEGACION, plot separately a bar plot of `Área de Responsabilidad` by `Tasa por 10 mil derechohabientes`:
# safe file‐name from DELEGACION:
safe_name <- function(x) {
    gsub("[^[:alnum:]_-]", "_", x)
    }

# unique values:
dels <- unique(meds_OOAD_merged_per10k_long_top$DELEGACION)

for(del in dels) {
    df_sub <- meds_OOAD_merged_per10k_long_top %>%
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
