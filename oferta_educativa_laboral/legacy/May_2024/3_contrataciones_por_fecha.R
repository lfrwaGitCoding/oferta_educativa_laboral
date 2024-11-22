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
contratados <- epi_read("../data/draft_y_match_residentes/processed/Base_de_Medicos_Especialistas_Jubilados_y_Marca_de_Baja/totales_contratados_anio.txt")

data_f <- contratados

epi_head_and_tail(data_f, cols = ncol(data_f))
colnames(data_f)

# For saving/naming outputs:
infile_prefix <- 'contratados'
############


############
# Plot:
# Already in long format

# TO DO: move to episcout
# IMSS colours:
custom_colours <- c(
  "Residentes" = "#911034",      # Red
  "Externos" = "#c19a53",  # Gold
  "Total" = "#2a5c4b"   # Green
  )


# Bar plot:
bar_p <- ggplot(data_f,
                aes(x = factor(Fecha), y = Frecuencia, fill = Tipo)
                ) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Contrataciones por Tipo por Año",
       x = "Año",
       y = "Total",
       fill = "Tipo") +
    scale_fill_manual(values = custom_colours)

bar_p

# Save plot:
ggsave("contrataciones_por_año.pdf",
       plot = bar_p,
       width = 10,
       height = 6,
       units = "in"
       )
############


############
# Remove rows with Tipo == 'total' to stack:
df <- data_f[data_f$Tipo != 'Total', ]
epi_head_and_tail(df, cols = ncol(data_f))


# Bar plot:
bar_p2 <- ggplot(df,
                aes(x = factor(Fecha), y = Frecuencia, fill = Tipo)
                ) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Contrataciones por Tipo por Año",
       x = "Año",
       y = "Total",
       fill = "Tipo") +
    scale_fill_manual(values = custom_colours)

bar_p2

# Save plot:
ggsave("contrataciones_por_año2.pdf",
       plot = bar_p2,
       width = 10,
       height = 6,
       units = "in"
       )
############


############
# Plot all totals:
jub_bajas_egresados <- epi_read("../data/draft_y_match_residentes/processed/Base_de_Medicos_Especialistas_Jubilados_y_Marca_de_Baja/jubilados_bajas_egresados_por_año.txt")

epi_head_and_tail(jub_bajas_egresados, cols = ncol(jub_bajas_egresados))

# Join with contratados:
epi_head_and_tail(contratados, cols = ncol(contratados))

colnames(jub_bajas_egresados)
colnames(contratados)

# Match and order column names:
contratados <- contratados[, c('Fecha', 'Tipo', 'Frecuencia')]
colnames(contratados)[1] <- "año"
colnames(contratados)[2] <- "category"
colnames(contratados)[3] <- "value"

colnames(jub_bajas_egresados)
colnames(contratados)

# Merge:
df_merged <- rbind(jub_bajas_egresados, contratados)
epi_head_and_tail(df_merged, cols = ncol(df_merged))

# Remove rows with 'Total' in category:
df_merged <- df_merged[df_merged$category != 'Total', ]

# Remove rows with 'Total' in category:
df_merged <- df_merged[df_merged$category != 'bajas_jubilados', ]

epi_head_and_tail(df_merged, cols = ncol(df_merged))
############


############
# Bar plot:

# Set the order:
unique(df_merged$category)
df_merged$category <- factor(df_merged$category,
                             levels = c("bajas", "jubilados", "egresados",
                                        "Residentes", "Externos"
                                        ),
                             labels = c("Bajas", "Jubilados", "Egresados",
                                        "Residentes", "Externos"
                                        ),
                             )

custom_colours <- c(
  "Bajas" = "#911034",      # Red
  "Jubilados" = "#c19a53",  # Gold
  "Egresados" = "#2a5c4b",   # Green
  "Residentes" = '#DACBA1', # Beige
  "Externos" = '#602218' # Brown
  )


bar_p3 <- ggplot(df_merged,
                aes(x = factor(`año`), y = value, fill = category)
                ) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Entradas y Salidas por Año y Categoría",
       x = "Año",
       y = "Total",
       fill = "Categoría") +
    scale_fill_manual(values = custom_colours)

bar_p3

# Save plot:
ggsave("entradas_salidas_egresados_por_año.pdf",
       plot = bar_p3,
       width = 10,
       height = 6,
       units = "in"
       )
############






############
# Create a new column to handle custom stacking
df_stacks <- df_merged %>%
  mutate(custom_category = case_when(
    category %in% c("Bajas", "Jubilados") ~ "Bajas & Jubilados",
    category == "Egresados" ~ "Egresados",
    category %in% c("Residentes", "Externos") ~ "Residentes & Externos"
  ))

epi_head_and_tail(df_stacks, cols = ncol(df_stacks))

# Convert data to long format for stacking
df_stacks_long <- df_stacks %>%
  mutate(sub_category = category) %>%
  select(año, custom_category, sub_category, value)

epi_head_and_tail(df_stacks_long, cols = ncol(df_stacks_long))

# Set the factor levels for 'custom_category' and 'sub_category' to ensure the desired order
df_stacks_long$custom_category <- factor(df_stacks_long$custom_category,
                                         levels = c("Bajas & Jubilados",
                                                    "Egresados",
                                                    "Residentes & Externos")
                                         )
df_stacks_long$sub_category <- factor(df_stacks_long$sub_category,
                                      levels = c("Bajas",
                                                 "Jubilados",
                                                 "Egresados",
                                                 "Residentes",
                                                 "Externos")
                                      )

# Stacked bar plot
bar_p4 <- ggplot(df_stacks_long, aes(x = factor(año), y = value, fill = sub_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Contrataciones por Categoría por Año",
       x = "Año",
       y = "Total",
       fill = "Categoría") +
  scale_fill_manual(values = custom_colours)

# Display the plot
print(bar_p4)
############



############
# Aggregate the data
aggregated_data <- df_merged %>%
  mutate(custom_category = case_when(
    category %in% c("Bajas", "Jubilados") ~ "Bajas & Jubilados",
    category == "Egresados" ~ "Egresados",
    category %in% c("Residentes", "Externos") ~ "Residentes & Externos"
  )) %>%
  group_by(año, custom_category) %>%
  summarise(total_value = sum(value)) %>%
  ungroup()

aggregated_data

# Ensure 'custom_category' is a factor with the desired order
aggregated_data$custom_category <- factor(aggregated_data$custom_category,
                                          levels = c("Bajas & Jubilados",
                                                     "Egresados",
                                                     "Residentes & Externos")
                                          )

# Define custom colours:
custom_colours <- c(
  "Bajas & Jubilados" = "#911034",  # Gold
  "Egresados" = "#c19a53",          # Green
  "Residentes & Externos" = "#324F46"  # Dark Green
)

# Stacked bar plot
# Create the bar plot with three bars per year
bar_p5 <- ggplot(aggregated_data, aes(x = factor(año), y = total_value, fill = custom_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Contrataciones por Categoría por Año",
       x = "Año",
       y = "Total",
       fill = "Categoría") +
  scale_fill_manual(values = custom_colours)

# Display the plot
print(bar_p5)

# Save plot:
ggsave("entradas_salidas_egresados_por_año_stacked.pdf",
       plot = bar_p5,
       width = 10,
       height = 6,
       units = "in"
       )
############


############
q()
############
