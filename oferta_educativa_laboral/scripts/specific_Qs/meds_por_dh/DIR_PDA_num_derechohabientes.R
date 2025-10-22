# DIR PDA conteo por OOAD, DH en consultorio
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(episcout)

#infile <- "/Users/antoniob/Documents/work/comp_med_medicina_datos/projects/datahub/abiertos_IMSS/internal_IMSS_raw/DIR/pda-2025-03-31.csv"
datahub <- "~/Documents/work/comp_med_medicina_datos/projects/datahub"
infile <- "/abiertos_IMSS/internal_IMSS_raw/DIR/pda-2025-03-31.csv"
infile <- file.path(datahub, infile)

data_f2 <- epi_read(infile)
dim(data_f2)
epi_head_and_tail(data_f2)

dict_catalog <- file.path(
  datahub,
  "abiertos_IMSS/internal_IMSS_raw/DIR/catalogo_de_archivos_pda_4_5_0 (1).xlsx"
)

# data dictionary
dict_catalog <- read_excel(
  dict_catalog,
  sheet = "delegación-subdelegación",
  skip = 1
)
dim(dict_catalog)
# Last line is text, remove:
dict_catalog <- dict_catalog[c(1:nrow(dict_catalog) - 1), ]
dim(dict_catalog)
epi_head_and_tail(dict_catalog)

glimpse(data_f2)
glimpse(dict_catalog)
dict_catalog$ID_DELEG_RP <- as.integer(dict_catalog$ID_DELEG_RP)

# joined dataset
# clean dict with one row per ID
dict_map <- dict_catalog %>%
  distinct(ID_DELEG_RP, `descripcion delegación`) %>% # drop exact dup rows
  group_by(ID_DELEG_RP) %>%
  summarise(
    `descripcion delegación` = first(`descripcion delegación`),
    .groups = "drop"
  )
dict_map

# Sanity
conflicts <- dict_catalog %>%
  distinct(ID_DELEG_RP, `descripcion delegación`) %>%
  add_count(ID_DELEG_RP) %>%
  filter(n > 1)
conflicts
# Inspect/resolve before joining if (nrow(conflicts) > 0)

# IDs with no mapping
missing_in_dict <- anti_join(data_f2, dict_map, by = "ID_DELEG_RP")
missing_in_dict

# join (many-to-one)
data_f2_desc <- data_f2 %>%
  left_join(dict_map, by = "ID_DELEG_RP", relationship = "many-to-one")
data_f2_desc

# check
data_f2_desc |>
  select(ID_DELEG_RP, `descripcion delegación`) |>
  distinct() |>
  arrange(ID_DELEG_RP)

dim(data_f2)
dim(data_f2_desc)
nrow(data_f2) == nrow(data_f2_desc)

epi_head_and_tail(data_f2_desc, cols = 13)
head(data_f2_desc)
colnames(data_f2_desc)

# By `descripcion delegación`
# Total and filtered by consultorio:
summary(as.factor(data_f2_desc$ST_CONSULTORIO))
sum(data_f2_desc[data_f2_desc$ST_CONSULTORIO == 1, "TOT_CASOS"])
sum(data_f2_desc[data_f2_desc$ST_CONSULTORIO == 0, "TOT_CASOS"])
sum(data_f2_desc[, "TOT_CASOS"])

sum_OOAD <- data_f2_desc |>
  group_by(`descripcion delegación`, ID_DELEG_RP) |>
  summarise(
    total_derechohabientes = sum(TOT_CASOS, na.rm = TRUE),
    adscritos_consultorio = sum(
      ifelse(ST_CONSULTORIO == 1, TOT_CASOS, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    porcentaje_adscritos_consultorio = (adscritos_consultorio /
      total_derechohabientes) *
      100
  ) |>
  arrange(-desc(ID_DELEG_RP))
sum_OOAD


epi_write_df(sum_OOAD, results_subdir, "pda_OOAD_032025", "txt")


# Plot
# stacking (adscritos vs no adscritos)
plot_data <- sum_OOAD %>%
  mutate(
    no_adscritos = total_derechohabientes - adscritos_consultorio
  ) %>%
  select(`descripcion delegación`, adscritos_consultorio, no_adscritos) %>%
  pivot_longer(
    cols = c(adscritos_consultorio, no_adscritos),
    names_to = "tipo",
    values_to = "n"
  ) %>%
  group_by(`descripcion delegación`) %>%
  mutate(
    porcentaje = 100 * n / sum(n),
    tipo = recode(
      tipo,
      adscritos_consultorio = "Adscritos a consultorio",
      no_adscritos = "No adscritos"
    )
  ) %>%
  ungroup()

# Plot
order_levels <- sum_OOAD %>%
  mutate(pct_no_adscritos = 100 - porcentaje_adscritos_consultorio) %>%
  arrange(desc(pct_no_adscritos)) %>%
  pull(`descripcion delegación`)

plot_1 <- ggplot(
  plot_data,
  aes(
    y = factor(`descripcion delegación`, levels = order_levels),
    x = n,
    fill = tipo
  )
) +
  geom_col(position = "fill") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c(
      "No adscritos" = "#D55E00",
      "Adscritos a consultorio" = "#0072B2"
    ),
    name = NULL
  ) +
  labs(
    x = "",
    y = "",
    title = "Cobertura de adscripción a consultorio por OOAD \n 03 2025"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
plot_1

getwd()

# TO DO: clean up, not referenced here
results_subdir

ggsave(
  paste0(results_subdir, "/adscritos_por_delegacion.pdf"),
  plot_1,
  width = 10,
  height = 10
)
