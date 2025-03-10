# ////////////
# Script information ----

# SIAP
# Universidad de la Salud - Medicina rural
# Febrero 2025
# Numero de medicos familiares por unidad
# Input is rdata output from script:
# 2_dups_col_types.R

# Output: tabla de frecuencias de med fam por unidad y plaza ocupada/vacante
# ////////////


# ////////////
# Global options ----
# options(error = stop)
# ////////////


# ////////////
# Import libraries ----
library(data.table)
library(episcout)
library(tidyverse)
# ////////////


# ////////////
# Set working directory to the project root  ----
setwd("~/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/")
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts
getwd()
dir()
# ////////////


# ////////////
# Load file ----

# ===
# rdata_dir <- '/results/19_02_2025_UP_meds_por_DH/'
rdata_dir <- '/results/19_02_2025_mac24/'
code_dir <- '/Users/antoniob/Documents/work/science/devel/github/antoniojbt/oferta_educativa_laboral/oferta_educativa_laboral'
results_dir <- '/Users/antoniob/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/results/'

# TO DO: Manually set:
infile <- 'medicos_por_mil_derechohabientes.csv'

# Full path and file name:
infile_path <- paste0(getwd(), rdata_dir, infile)
print(infile_path)


data_f <- epi_read(infile_path)
epi_head_and_tail(data_f)
epi_clean_count_classes(df = data_f)
str(data_f)
# ===
# ////////////


# ////////////
# Source functions/scripts/etc
# TO DO:
# Source (until I update episcout)
source(file.path(paste0(code_dir, '/scripts/funcs_epi_source.R')))
# ////////////


# ////////////
# Output dir, based on today's date ----
script_n <- 'plots_meds_DH_UP'
infile_prefix <- strsplit(infile, "\\.")[[1]][1]
results_subdir <- sprintf('%s_%s',
                          format(Sys.Date(), '%d_%m_%Y'),
                          infile_prefix
)
results_subdir
# results_subdir <- '/Users/antoniob/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/results/19_02_2025_mac24/'
# results_subdir <- epi_create_dir(base_path = results_dir,
#                                  subdir = results_subdir
# )
# ////////////



# ////////////
# Horizontal bar plot of Numero de medicos por estado ----
# Set a global theme with larger axis text
theme_set(theme_minimal(base_size = 11))  # Adjust base size for all text

# ===
epi_head_and_tail(data_f, cols = ncol(data_f))
str(data_f$medicos_por_mil_derechohabientes)
str(data_f$DELEGACION)

# Order ascending based on "medicos_por_mil_derechohabientes":
data_f$DELEGACION <- factor(data_f$DELEGACION,
                            levels = data_f$DELEGACION[order(data_f$medicos_por_mil_derechohabientes,
                                                             decreasing = TRUE)]
                            )
str(data_f$DELEGACION)

bar_plot <- epi_plot_bar(data_f,
                         var_x = "DELEGACION",
                         var_y = "medicos_por_mil_derechohabientes"
                                         ) +
    coord_flip() +
    labs(title = "Número de plazas ocupadas por médicos/as por 1000 derechohabientes") +
    theme(legend.position = "none",
                  axis.text.x = element_text(size = 11),  # Increase x-axis text size
                  axis.text.y = element_text(size = 11),  # Increase y-axis text size
                  axis.title.x = element_text(size = 12), # Increase x-axis title size
                  axis.title.y = element_text(size = 12)  # Increase y-axis title size
              ) +
  labs(x = NULL, y = NULL)  # Remove axis labels
  bar_plot

# Save:
file_n <- 'plot_bar_meds_DH_estado'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile, plot = bar_plot,
       height = 8.5, width = 11, units = "in",
       dpi = 300,  # Adjust DPI to maintain font size
       scale = 1  # Increase scale factor
)
# ===

# ===
# Bar plot of percent of PLAZAS_VACANTES by DELEGACION
# Add percent column:
data_f$PLAZAS_VACANTES_perc <- (data_f$PLAZAS_VACANTES / (data_f$PLAZAS_VACANTES + data_f$PLAZAS_OCUPADAS)) * 100
# (1200 / (1200 + 12977) ) * 100
data_f$PLAZAS_VACANTES_perc

# Order descending based on PLAZAS_VACANTES_perc:
data_f$DELEGACION <- factor(data_f$DELEGACION,
                            levels = data_f$DELEGACION[order(data_f$PLAZAS_VACANTES_perc,
                                                             decreasing = FALSE)]
                            )
str(data_f$DELEGACION)

bar_plot <- epi_plot_bar(data_f,
                         var_x = "DELEGACION",
                         var_y = "PLAZAS_VACANTES_perc"
) +
    coord_flip() +
    labs(title = "Porcentaje de plazas vacantes de médicos por estado") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11),  # Increase x-axis text size
          axis.text.y = element_text(size = 11),  # Increase y-axis text size
          axis.title.x = element_text(size = 12), # Increase x-axis title size
          axis.title.y = element_text(size = 12)  # Increase y-axis title size
    ) +
  labs(x = NULL, y = NULL)  # Remove axis labels
bar_plot

# Save:
file_n <- 'plot_bar_pza_vacs_estado'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile, plot = bar_plot,
       height = 8.5, width = 11, units = "in",
       dpi = 300,  # Adjust DPI to maintain font size
       scale = 1  # Increase scale factor
)


# ===








# ===
# Stacked bar plot:
stacked_bar <- ggplot(df_long_filt,
                      aes(x = DELEGACION, y = Cantidad, fill = Especialidad)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Número de médicos/as por especialidad y OOAD - especialidades más frecuentes") +
    theme(legend.position = "bottom")
stacked_bar

# Save:
# file_n <- 'plot_bar_med_esp_OOAD_top_ordinario'
file_n <- 'plot_bar_med_esp_OOAD_top'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile, plot = stacked_bar,
       height = 20, width = 20, units = "in"
)
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
