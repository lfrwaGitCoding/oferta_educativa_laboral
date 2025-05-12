# ////////////
# Script information ----

# SIAP
# EDA
# Marzo 2025
# Numero de plazas medicos por derechohabientes por Estado
# Input is:
# datos INEGI Djaine reporte interactivo

# Output: plot carga actual vs cubriendo vacantes por Estado
# ////////////


# ////////////
# Global options ----
# options(error = stop)
# ////////////


# ////////////
# Import libraries ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(episcout)
# ////////////


# ////////////
# Set working directory to the project root  ----
getwd()
project_dir <- "/Users/antoniob/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/"
results_dir <- "results/specific_Qs/meds_por_DH/"
infile <- "meds_OOAD_DH.txt"
# infile <- "medicos_por_mil_derechohabientes_utf8.csv"
infile_path <- file.path(paste0(project_dir, results_dir, infile))
infile_path
# ////////////


# ////////////
# Output dir, based on today's date ----
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
# Load data ----

# ===
data_f <- epi_read(infile_path)
str(data_f)
data_f$OOAD <- as.factor(data_f$OOAD)
str(data_f)

# Order and index the data
data_f <- data_f %>%
    arrange(Carga_Actual) %>%
    mutate(OOAD = factor(OOAD, levels = unique(OOAD)),
           OOAD_index = as.numeric(OOAD))

# Prepare long format with a 'layer' variable
df_long <- data_f %>%
    arrange(Carga_Actual) %>%
    mutate(
        OOAD = factor(OOAD, levels = unique(OOAD)),
        OOAD_index = as.numeric(OOAD)
    ) %>%
    select(OOAD, OOAD_index, Carga_con_Vacantes, Carga_Actual) %>%
    pivot_longer(
        cols = c(Carga_con_Vacantes, Carga_Actual),
        names_to = "tipo", values_to = "valor"
    )

# Back to wide format to access both values
df_plot <- data_f %>%
    arrange(Carga_Actual) %>%
    mutate(
        OOAD = factor(OOAD, levels = unique(OOAD)),
        OOAD_index = as.numeric(OOAD)
    )
# ===
# ////////////


# ////////////
# Plot ----
# ===
# Get font sizes to scale properly for qmd pdf output:
font_size <- 15 / 0.6  # Final appearance size after scaling (≈20), where scaling is at eg 60% for latex PDF output
font_size_x <- font_size + 2 # axis ticks (x-axis)
font_size_y <- font_size + 2 # y-axis ticks

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
# ===

# ===
line_plot <- ggplot() +
    # Ribbon = difference between actual and "con vacantes"
    geom_ribbon(data = df_plot, aes(x = OOAD_index, ymin = Carga_con_Vacantes, ymax = Carga_Actual, fill = "Plazas Vacantes")) +
    # Area = base level of con vacantes
    geom_area(data = df_plot, aes(x = OOAD_index, y = Carga_con_Vacantes, fill = "Carga con Vacantes")) +
    scale_x_continuous(
        breaks = df_plot$OOAD_index,
        labels = df_plot$OOAD
    ) +
    scale_fill_manual(
        name = NULL,
        values = c(
            "Carga con Vacantes" = "#6CAFB8",
            "Plazas Vacantes" = "#E19AC3"
        ),
        labels = c(
            "Carga con Vacantes" = "Derechohabientes por Médico",
            "Plazas Vacantes" = "Derechohabientes por Médico cubriendo Vacantes"
        )
    ) +
    labs(
        title = "Presión Actual vs Presión con Vacantes Cubiertas",
        x = NULL,
        y = NULL
        ) +
    epi_plot_theme_2(base_size = font_size,
                     font_size_x = font_size_x,
                     font_size_y = font_size_y) +
    theme(axis.text.x = element_text(angle = 75, hjust = 1),
          legend.position = "top"
    )
line_plot

# Save:
file_n <- 'plot_line_med_estado_DH'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile, plot = line_plot,
       # height = 12, width = 18, units = "in",
       # scale = 1,
       dpi = 300
       )
# ===
# ////////////



# ////////////
# The end ----
# Outputs saved to disk, no need to save as rdata.
sessionInfo()
# q()
# ////////////
