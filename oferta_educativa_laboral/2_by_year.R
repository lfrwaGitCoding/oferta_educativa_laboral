############
# Movilización de residentes
# Dirección de Administración
# Unidad de Personal
# July 2024
# Descriptive stats
# Input is
# Output is
############


############
project_loc <- "/Users/antoniob/Documents/work/science/comp_med_medicina_datos/int_op/movilizacion_residentes/results/"
getwd()
setwd(project_loc)
############


############
# Import libraries
library(data.table)
library(episcout)
library(ggthemes)
library(cowplot)
library(tidyverse)
############


############
# Datasets:
load('../data/draft_y_match_residentes/processed/Base_de_Medicos_Especialistas_Jubilados_y_Marca_de_Baja/1_setup_jubilados.rdata.gzip', verbose = TRUE)

data_f <- data_f # for RStudio warnings

epi_head_and_tail(data_f)
colnames(data_f)

# For saving/naming outputs:
# infile_prefix <- 'jubilados'
# infile_prefix <- 'jubilados'
############


############
# Get table by of totals by year
totals_year <- summary(data_f$añoJub)
totals_year <- as.data.frame(totals_year)
colnames(totals_year)
totals_year$año <- rownames(totals_year)
totals_year$jubilados <- totals_year$totals_year
totals_year$totals_year <- NULL
totals_year <- totals_year[, c('año', 'jubilados')]
totals_year
# Reset row names:
rownames(totals_year) <- NULL
totals_year
############


############
load('../data/draft_y_match_residentes/processed/Base_de_Medicos_Especialistas_Jubilados_y_Marca_de_Baja/1_setup_bajas.rdata.gzip', verbose = TRUE)

epi_head_and_tail(data_f)
colnames(data_f)

# Again, totals by year for 'bajas':
bajas <- as.data.frame(summary(data_f$'año Baja'))

# Add these totals to data frame:
totals_year$bajas <- bajas$`summary(data_f$"año Baja")`
totals_year$bajas_jubilados <- totals_year$bajas + totals_year$jubilados
totals_year
############


############
# Add egresados from Excel sheet manually:
egresados <- c(3888,
               4240,
               4499,
               4830,
               5495,
               6522
               )
totals_year$egresados <- egresados
# totals_year$jubilados <- NULL
# totals_year$bajas <- NULL
totals_year

data_long <- totals_year %>%
  pivot_longer(cols = -`año`, names_to = "category", values_to = "value")
data_long

# Save to disk:
epi_write(data_long, "../data/draft_y_match_residentes/processed/Base_de_Medicos_Especialistas_Jubilados_y_Marca_de_Baja/jubilados_bajas_egresados_por_año.txt")

############


############
# Plot:
# Convert data to long format
df <- totals_year[, c('año', 'jubilados', 'bajas', 'egresados')]

data_long <- df %>%
  pivot_longer(cols = -`año`, names_to = "category", values_to = "value")
data_long

# Set the order:
data_long$category <- factor(data_long$category,
                             levels = c("bajas", "jubilados", "egresados"),
                             labels = c("Bajas", "Jubilados", "Egresados")
                             )

# TO DO: move to episcout
# IMSS colours:
custom_colours <- c(
  "Bajas" = "#911034",      # Red
  "Jubilados" = "#c19a53",  # Gold
  "Egresados" = "#2a5c4b"   # Green
  )


# Bar plot:
bar_p <- ggplot(data_long,
                aes(x = factor(`año`), y = value, fill = category)
                ) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Jubilados, Bajas y Egresados por Año",
       x = "Año",
       y = "Total",
       fill = "Categoría") +
    scale_fill_manual(values = custom_colours)

bar_p

# Save plot:
ggsave("jubilados_bajas_egresados_por_año.pdf",
       plot = bar_p,
       width = 10,
       height = 6,
       units = "in"
       )


# Line plot:
line_p <- ggplot(data_long, aes(x = año, y = value, color = category)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Jubilados, Bajas, and Egresados Over the Years",
       x = "Year",
       y = "Count",
       color = "Category") +
  theme_minimal()
############


############
# Convert data to long format for stacking jubilados and bajas
df <- totals_year[, c('año', 'jubilados', 'bajas', 'egresados')]

data_long <- df %>%
  pivot_longer(cols = c(jubilados, bajas), names_to = "category", values_to = "value")
data_long

# Plot stacked bar plot for jubilados and bajas, and separate bars for egresados

############


############
q()
############


