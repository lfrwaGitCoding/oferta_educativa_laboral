library(sf)
library(dplyr)
library(ggplot2)

# Load Mexico map
library(rnaturalearth)
library(rnaturalearthdata)

# Get Mexico country shape (scale = "medium" 'decent' detail):
mexico_map <- ne_countries(scale = "medium", country = "Mexico", returnclass = "sf")
str(mexico_map)
mexico_states <- ne_states(country = "Mexico", returnclass = "sf")
mexico_states

# 1. Assume you already have `mexico_states` as sf object
cdmx <- mexico_states %>% filter(name == "Ciudad de México")  # adjust your column name if needed

# 2. Create a horizontal line at the middle latitude
mid_lat <- st_bbox(cdmx)["ymin"] + (st_bbox(cdmx)["ymax"] - st_bbox(cdmx)["ymin"]) / 2

# Line to split
split_line <- st_sfc(
    st_linestring(matrix(c(st_bbox(cdmx)["xmin"], mid_lat,
                           st_bbox(cdmx)["xmax"], mid_lat),
                         ncol = 2, byrow = TRUE)),
    crs = st_crs(cdmx)
)

# 3. Split the polygon
cdmx_split <- st_split(cdmx, split_line) %>% st_collection_extract("POLYGON")

# 4. Assign north/south manually
cdmx_split <- cdmx_split %>%
    mutate(region = ifelse(st_centroid(.) %>% st_coordinates() %>% .[,2] > mid_lat, "CDMX North", "CDMX South"))





df1 <- df_dependencias_PLZOCU
df2 <- all_IP_df

colnames(df1)
colnames(df2)
dim(df1)
dim(df2)

col_merge <- "CVEUNI"

length(unique(df1[[col_merge]]))
length(unique(df2[[col_merge]]))

length(which(is.na(df2[[col_merge]])))
View(df2)

# Match colnames:
colnames(df1)[which(colnames(df1) == "IP")] <- "Clave_Presupuestal_IP"
colnames(df1)

# Number of actual matches by presupuesto / IP:
length(intersect(df1[[col_merge]], df2[[col_merge]]))

df_merged <- left_join(df2, df1, by = col_merge)
epi_head_and_tail(df_merged)
epi_head_and_tail(df_merged, last_cols = TRUE)
View(df_merged[, c(33:68)])

length(unique(df_merged[[col_merge]]))
length(which(is.na(df_merged[[col_merge]])))

length(which(is.na(df_merged[["porc_vacante"]])))
# ===

