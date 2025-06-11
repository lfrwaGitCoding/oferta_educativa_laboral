# ////////////
# Script information ----

# SIAP
#
#  2025
# Numero de medicos familiares por unidad


# Output: tabla de frecuencias de med fam por unidad y plaza ocupada/vacante
# ////////////


# ////////////
# Global options ----
# options(error = stop)
# ////////////


# ////////////
library(data.table)
library(episcout)
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)
library(leaflet) # interactive maps
library(leaflet.extras)
# ////////////


# ////////////
# Set working directory to the project root  ----
setwd(here::here())
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts
getwd()
dir()
# ////////////


# ////////////
# Load file ----

# ===
# rdata_dir <- '/results/19_02_2025_UP_meds_por_DH/'
rdata_dir <- '/results/specific_Qs/31_03_2025_medicos_por_mil_derechohabientes/'
code_dir <- '/Users/antoniob/Documents/work/science/devel/github/antoniojbt/oferta_educativa_laboral/oferta_educativa_laboral/'
results_dir <- '/Users/antoniob/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/results/'

# TO DO: Manually set:
infile <- 'medicos_por_mil_derechohabientes_utf8.csv'

# Full path and file name:
infile_path <- paste0(getwd(), rdata_dir, infile)
print(infile_path)


data_f <- epi_read(infile_path, encoding = "UTF-8")
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
results_subdir <- epi_create_dir(base_path = results_dir,
                                 subdir = results_subdir
)
# ////////////



# ////////////
# Horizontal bar plot of Numero de medicos por estado ----
# Set a global theme with larger axis text
font_size <- 12 / 0.6  # Final appearance size after scaling (≈20), where scaling is at eg 60% for latex PDF output
font_size_x <- font_size - 0 # axis ticks (x-axis)
font_size_y <- font_size - 0 # y-axis ticks

my_theme <- theme_minimal(base_size = font_size) +
    theme(
        plot.title    = element_text(size = font_size - 3, face = "bold"),
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
# ////////////


# ////////////
# Load Mexico map
library(rnaturalearth)
library(rnaturalearthdata)

# Get Mexico country shape (scale = "medium" 'decent' detail):
mexico_map <- ne_countries(scale = "medium", country = "Mexico", returnclass = "sf")
str(mexico_map)
mexico_states <- ne_states(country = "Mexico", returnclass = "sf")
mexico_states
# If INEGI:
# mexico_map <- st_read("path_to_mexico_shapefile.shp") # or use `rnaturalearth`
# ////////////

# ////////////
# Convert to spatial object

df <- all_IP_df[, c("lat", "lon", "count")]

# Clean up NAs:
df <- df[!is.na(df$lat), ]

# Convert:
df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

# Transform CRS for better plotting (Web Mercator), but not necessary:
# df_sf <- st_transform(df_sf, crs = 3857)

# To match rnaturalearth:
df_sf <- st_transform(df_sf, crs = st_crs(mexico_states))
# ////////////


# ////////////
# Bubble Map (if counts are discrete and sparse):
# TO DO:
ggplot() +
    geom_sf(data = mexico_states, fill = "white", color = "grey70") +
    geom_sf(data = df_sf, aes(size = count), color = "red", alpha = 0.4) +
    scale_size_continuous(range = c(0.5, 4)) +  # Control bubble range
    coord_sf() +
    theme_minimal() +
    labs(title = "Mapa de calor del número de médicos por Unidad de Atención", size = "Frecuencia")
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
