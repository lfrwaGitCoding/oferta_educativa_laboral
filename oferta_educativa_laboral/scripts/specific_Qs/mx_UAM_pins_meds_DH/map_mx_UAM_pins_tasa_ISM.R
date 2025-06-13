
# ////////////
library(data.table)
library(episcout)
library(tidyverse)
library(DBI)
library(RMariaDB)

library(rnaturalearth)
library(ggplot2)
library(sf)
library(ggrepel)
library(leaflet) # interactive maps
library(leaflet.extras)
library(gridExtra)
library(patchwork) # for wrap_elements()
library(stringi)
library(RColorBrewer)
# ////////////


# ////////////
# TO DO: manual
project_dir <- here::here()
results_subdir <- file.path(project_dir,
                            "results/specific_Qs/meds_por_DH/20_05_2025_meds_DH_mx_unidades")
# ////////////


# ////////////
# ===
# # Open a connection to XAMPP MariaDB server, needs to be running, see notes for db_interactivo:
# con <- dbConnect(
#     MariaDB(),
#     user     = "root",
#     password = "",                  # or your root password
#     host     = "127.0.0.1",         # avoid 'localhost' so you use TCP
#     port     = 3306,                # default XAMPP port
#     dbname     = "personalaps",
#     unix.socket = "/Applications/XAMPP/xamppfiles/var/mysql/mysql.sock"
# )
#     # if you run into socket-errors, you can also specify:
#     # unix.socket = "/Applications/XAMPP/xamppfiles/var/mysql/mysql.sock"
#
# # tables:
# dbListTables(con)
# ===

# ===
# # Read one table into R:
# df <- dbReadTable(con, "unidades_imss")
# epi_head_and_tail(df)
# colnames(df)
# ===

# ===
# Run query
# "/Applications/XAMPP/xamppfiles/htdocs/MODELO/php/get_unidades.php"
# # Get UAM coords:
# df2 <- dbGetQuery(con, 'SELECT nombre_unidad AS nombre, latitud, longitud, nivel_atencion AS nivel
#                         FROM personalaps.unidades_imss ui
#                         WHERE clave_institucion = "IMS"
#                         AND estatus_operacion = "EN OPERACION"
#                         AND nivel_atencion != "NO APLICA";'
#                   )
# dim(df2)
# epi_head_and_tail(df2, cols = 4)
# colnames(df2)
# summary(as.factor(df2$latitud))
#
#
# # Save:
# file_n <- 'db_djaine_personalaps_unidades_imss'
# suffix <- 'txt'
# outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
# outfile
# epi_write(df2, outfile)

# Already saved:
df2 <- file.path(results_subdir,
                 "db_djaine_personalaps_unidades_imss.txt")
file.exists(df2)
df2 <- epi_read(df2)

# Remove NAs:
df2 <- df2 %>% drop_na(longitud, latitud)
summary(as.factor(df2$latitud))
summary(as.factor(df2$longitud))
# ===



# ===
# Add OOAD colouring by ISM:
# TO DO: manual
# Was created for sharing, only has 07 2025 data, missing 17 2024:
# df_ism <- file.path(results_subdir, "tasas_meds_qna_07_2025.txt")

# Messier but has estado, OOAD, INEGI, 17 2024, 2025, etc.:
df_ism <- file.path(project_dir,
                    "results/specific_Qs/meds_por_DH/31_03_2025_medicos_por_mil_derechohabientes/medicos_por_mil_derechohabientes_utf8.csv")
df_ism <- epi_read(df_ism)
colnames(df_ism)
epi_head_and_tail(df_ism, cols = 6)
epi_head_and_tail(df_ism, last_cols = T)
# df_ism <- df_ism[, 1:5]
# View(df_ism)

df_ism <- df_ism[, c("Estado", #"Delegación",
                     "PLAZAS_OCUPADAS_172024",
                     "Derechohabientes_DIR_03_2025",
                     "medicos_por_mil_derechohabientes_172024",
                     "medicos_por_mil_derechohabientes_072025",
                     "total_072025"
                     ),
                 ]
epi_head_and_tail(df_ism, cols = 5)

# meds per 1000:
df_ism$medicos_por_mil_derechohabientes_172024 <- round((df_ism$PLAZAS_OCUPADAS_172024 / df_ism$Derechohabientes_DIR_03_2025) * 1000, 2)
df_ism$medicos_por_mil_derechohabientes_172024
summary(df_ism$medicos_por_mil_derechohabientes_172024)
# View(df_ism)
# TO DO: for OOADs needs palzas ocupads, original calc

# plazas por DH 2025:
df_ism$plazas_por_mil_derechohabientes_072025 <- round((df_ism$total_072025 / df_ism$Derechohabientes_DIR_03_2025) * 1000, 2)
df_ism$plazas_por_mil_derechohabientes_072025
summary(df_ism$plazas_por_mil_derechohabientes_072025)

# Use Estados, not OOADs, to have 2024 vs 2025
epi_head_and_tail(df_ism)
colnames(df_ism)

# Drop NA rows:
df_ism <- df_ism[which(complete.cases(df_ism)), ]
# View(df_ism)
# ===
# ////////////


# ////////////
# Maps ----

# ===
# state boundaries, but now have OOAD boundaries (middle cut)
mex_sf <- ne_states(country = "Mexico", returnclass = "sf") %>%
    select(name_es) %>%
    rename(Estado = name_es)

# If by OOADs, use shapefile:
# sh_dir <- "~/Documents/work/science/devel/github/med-comp-imss/geo_stats/shapefiles/por_OOAD/"
# read the shapefile
# mex_sf <- st_read(paste0(sh_dir, "mexico_OOADs.shp"))

mex_sf

# check CRS
st_crs(mex_sf)

# Remove NAs:
str(mex_sf)
loc_col_name <- "Estado"

summary(as.factor(mex_sf[[loc_col_name]]))

mex_sf <- mex_sf %>%
    filter(!is.na(.data[[loc_col_name]]))

summary(as.factor(mex_sf[[loc_col_name]]))
str(mex_sf)

# names should match, check:
sf_names <- unique(mex_sf[[loc_col_name]])
df_names <- unique(df_ism[[loc_col_name]])

# Check for differences:
sf_names[which(!sf_names %in% df_names)]

# Match names:
mex_sf[[loc_col_name]] <- ifelse(
    mex_sf[[loc_col_name]] == "Coahuila de Zaragoza",
    "Coahuila",
    mex_sf[[loc_col_name]]
    )

mex_sf[[loc_col_name]] <- ifelse(
    mex_sf[[loc_col_name]] == "Estado de Veracruz",
    "Veracruz",
    mex_sf[[loc_col_name]]
)

mex_sf[[loc_col_name]] <- ifelse(
    mex_sf[[loc_col_name]] == "México",
    "Estado de México",
    mex_sf[[loc_col_name]]
)


sf_names <- unique(mex_sf[[loc_col_name]])
df_names <- unique(df_ism[[loc_col_name]])
sf_names[which(!sf_names %in% df_names)]
# ===


# ===
# Map pins UAM data:
df_sf <- st_as_sf(df2, coords = c("longitud","latitud"), crs = 4326)

p <- ggplot() +
    geom_sf(data = mex_sf, fill = "grey95", color = "grey70") +
    geom_sf(data = df_sf,
            aes(color = nombre),
            size = 2,
            show.legend = FALSE) +
    coord_sf(datum = NA) +           # drop graticule labels
    labs(title = "Unidades de Atención Médica") +
    theme_minimal()

p


# colouring by nivel:
p <- ggplot() +
    geom_sf(data = mex_sf, fill = "grey95", color = "grey80") +
    geom_sf(data = df_sf,
            aes(color = nivel),
            size  = 1) +
    scale_color_brewer(palette = "Paired", na.value = "grey50") + #Dark2 Set3, etc
    coord_sf(datum = NA) +
    labs(title    = "Unidades de Atención Médica",
         color    = NULL,
         # caption  = "Fuente: personalaps"
         ) +
    theme_minimal()
p

# to save:
# ===


# ===
# Map by meds per DH:

# return valor < 1.2  ? '#C62828' :        // rojo → insuficiencia clara
# valor < 1.5  ? '#FFD93D' :        // amarillo → cobertura limitada
# valor < 1.7  ? '#81C784' :        // verde claro → nivel adecuado
# valor < 1.9  ? '#4CAF50' :        // verde medio → buen nivel
# '#2E7D32';         // verde oscuro → nivel óptimo (histórico)

epi_head_and_tail(df_ism)

# Shapefile:
colnames(mex_sf)
colnames(mex_sf)[which(colnames(mex_sf) == "name_es")] <- loc_col_name
df_ism[[loc_col_name]]

# Join and bin, by year:
# meds_dh <- "medicos_por_mil_derechohabientes_172024"
# meds_dh <- "medicos_por_mil_derechohabientes_072025"

mex_sf_ism <- mex_sf %>%
    left_join(df_ism, by = loc_col_name) %>%
    mutate(
        nivel_cat = cut(
            # medicos_por_mil_derechohabientes_172024,
            # medicos_por_mil_derechohabientes_072025,
            plazas_por_mil_derechohabientes_072025,
            breaks = c(-Inf, 1.19, 1.5, 1.7, 1.9, Inf),
            labels = c(
                "<1.2",
                "1.2 – 1.49",
                "1.5 – 1.69",
                "1.7 – 1.89",
                "≥1.9"
            )
        )
    )

# Map:
p <- ggplot(mex_sf_ism) +
    geom_sf(aes(fill = nivel_cat), color = "white", size = 0.1) +
    scale_fill_manual(
        # name   = "Médicos por 1,000\nderechohabientes",
        name   = "Plazas por 1,000\nderechohabientes",
        values = c(
            "<1.2" = "#C62828",
            "1.2 – 1.49" = "#FFD93D",
            "1.5 – 1.69" = "#81C784",
            "1.7 – 1.89" = "#4CAF50",
            "≥1.9" = "#2E7D32"
        ),
        na.value = "grey90"
    ) +
    labs(
        # title = "Índice de Médicos por Derechohabiente (Diciembre 2024)"
        # title = "Índice de Médicos por Derechohabiente (Abril 2025)"
        title = "Índice de Plazas Ocupadas y Vacantes por Derechohabiente (Abril 2025)"
    ) +
    theme_minimal()
p

# Save:
# file_n <- 'plot_mapa_mx_meds_DH_172024'
# file_n <- 'plot_mapa_mx_meds_DH_072025'
file_n <- 'plot_mapa_mx_plz_totales_DH_072025'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile,
       plot = p,
       height = 20,
       width = 20,
       units = "in",
       scale = 1, dpi = 300
       )
# ===
# ////////////


# ////////////

joint_p <- ggplot() +
    # background choropleth
    geom_sf(
        data = mex_sf_ism,
        aes(fill = nivel_cat),
        color = "white",   # municipality borders
        size  = 0.1
    ) +
    # overlay medical‐unit points
    geom_sf(
        data = df_sf,
        aes(color = nivel),
        size = 1
    ) +
    scale_color_brewer(palette = "Dark2", name = "Nivel de atención") +
    # geom_sf(
    #     data  = df_sf,
    #     aes(),              # or aes(color = nivel) if you want them coloured by 'nivel'
    #     color = "black",    # pin colour
    #     size  = 1           # smaller circles
    # ) +
    # custom IMSS palette
    scale_fill_manual(
        # name   = "Médicos por 1,000\nderechohabientes",
        name   = "Plazas por 1,000\nderechohabientes",
        values = c(
            "<1.2" = "#C62828",
            "1.2 – 1.49" = "#FFD93D",
            "1.5 – 1.69" = "#81C784",
            "1.7 – 1.89" = "#4CAF50",
            "≥1.9" = "#2E7D32"
        ),
        na.value = "grey90"
    ) +
    coord_sf(datum = NA) +
    theme_minimal() +
    labs(
        # title = "Índice de Médicos por Derechohabiente (Diciembre 2024)"
        # title = "Índice de Médicos por Derechohabiente (Abril 2025)"
        title = "Índice de Plazas Ocupadas y Vacantes por Derechohabiente (Abril 2025)"
        # caption = "Unidades médicas sobre el índice IMSS"
    )
joint_p

# Save:
# file_n <- 'plot_mapa_mx_meds_DH_unidades_172024'
# file_n <- 'plot_mapa_mx_meds_DH_unidades_072025'
file_n <- 'plot_mapa_mx_plz_totales_DH_unidades_072025'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile,
       plot = joint_p,
       height = 20,
       width = 20,
       units = "in",
       scale = 1, dpi = 300
)
# ////////////


# ////////////
# To finish:
# dbDisconnect(con)
# ////////////
