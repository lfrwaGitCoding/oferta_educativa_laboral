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
# Static map

# Get data:
# df <- read.csv("your_data.csv")
# TO DO: replace
all_IP_df # is output from previous script, coords_unidades_medicas_CUUMS.R, using for tests for now, needs a count for heatmap

# dummy var for heatmap:
dim(all_IP_df)
# all_IP_df$count <- round(rnorm(nrow(all_IP_df), mean = 10, sd = 5))
all_IP_df$count <- sample(size = nrow(all_IP_df), x = 100, replace = TRUE)
summary(all_IP_df$count)

# Check column names: eg latitude, longitude, and count
colnames(all_IP_df)
names(all_IP_df)[names(all_IP_df) == "LATITUD"] <- "lat"
names(all_IP_df)[names(all_IP_df) == "LONGITUD"] <- "lon"
# names(all_IP_df)[names(all_IP_df) == "count_dummy"] <- "count"
colnames(all_IP_df)
# ////////////



# ////////////
# Load Mexico map
library(rnaturalearth)
library(rnaturalearthdata)

# Get Mexico country shape (scale = "medium" 'decent' detail):
mexico_map <- ne_countries(scale = "medium", country = "Mexico", returnclass = "sf")
str(mexico_map)
mexico_states <- ne_states(country = "Mexico", returnclass = "sf")

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
# TO DO:

# Plot map with eg hospital points:
# TO DO:
ggplot() +
    geom_sf(data = mexico_states) +
    geom_sf(data = df_sf, color = "red", size = 2) +
    theme_minimal() +
    labs(title = "Ubicación de Unidades de Atención Médica", x = "Longitude", y = "Latitude")

# Kernel Density Heatmap:
ggplot() +
    geom_sf(data = mexico_states, fill = "white", color = "grey40") +
    stat_density2d(
        data = df,
        aes(x = lon, y = lat, fill = after_stat(level), weight = count),
        geom = "polygon", alpha = 0.6
    ) +
    scale_fill_viridis_c(option = "inferno") +
    coord_sf() +
    labs(title = "Mapa de calor del número de médicos por Unidad de Atención (densidad por kernel)", fill = "Density")

# Bubble Map (if counts are discrete and sparse):
# TO DO:
ggplot() +
    geom_sf(data = mexico_states, fill = "white", color = "grey70") +
    geom_sf(data = df_sf, aes(size = count), color = "red", alpha = 0.4) +
    scale_size_continuous(range = c(0.5, 4)) +  # Control bubble range
    coord_sf() +
    theme_minimal() +
    labs(title = "Mapa de calor del número de médicos por Unidad de Atención", size = "Frecuencia")


# Interactive
# TO DO:
library(leaflet)
leaflet(df) %>%
    addTiles() %>%
    addCircleMarkers(~lon, ~lat, radius = ~sqrt(count), popup = ~as.character(count),
                     color = "red", fillOpacity = 0.7)
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
