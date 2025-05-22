# ////////////
# Script information ----

# SIAP
# Unidad de Personal
# Mayo 2025

# modelo para estrategia 2-30-100, asignacion de recursos de OOADs con mas meds por DH a otros
# basado en linear programming (LP) and transportation problems

# '''
# the classic “transportation problem,” is a particular LP where
  # supplies at “origin” nodes
  # demands at “destination” nodes
  # unit costs for shipping from each origin→destination
# Needs a 'cost' matrix and vectors of supply/demand, it builds a constraint matrix, runs lp(..., all.int=TRUE)
# returns a solution matrix
# lp.transport() is a convenience for the network‐flow structure of a transportation/logistics problem.
# '''


# Input is
#

# Output is
# ////////////


# ////////////
# Import libraries ----
library(sf)
library(dplyr)
library(geosphere)
library(lpSolve)
# ////////////


# ////////////
# Load files ----

# TO DO: clean up, need files with med per dh, nombrear, delegacion, etc

# eg:
summary(as.factor(nuevos_adscritos$DELEGACION_2024_residencia))
colnames(nuevos_adscritos)

df2 <- nuevos_adscritos[which(nuevos_adscritos$NOMBREAR == "TRAUMATOLOGIA Y ORTOPEDIA"), ]
epi_head_and_tail(df2)


epi_head_and_tail(meds_2025)

# ===
# Get meds per DH by OOAD
# TO DO: manual
# Was created for sharing, only has 07 2025 data, missing 17 2024:
df_ism <- "/Users/antoniob/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/results/specific_Qs/meds_por_DH/15_05_2025_2b_clean_subset_2_clean_dups_col_types_Qna_07_Plantilla_2025_meds/tasas_meds_qna_07_2025.txt"

# Messier but has estado, OOAD, INEGI, 17 2024, 2025, etc.:
# df_ism <- "/Users/antoniob/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/results/specific_Qs/meds_por_DH/31_03_2025_medicos_por_mil_derechohabientes/medicos_por_mil_derechohabientes_utf8.csv"
df_ism <- epi_read(df_ism)
colnames(df_ism)
epi_head_and_tail(df_ism, cols = 6)
epi_head_and_tail(df_ism, last_cols = T)
df_ism <- df_ism[, 1:5]
epi_head_and_tail(df_ism, cols = 5)

summary(df_ism$medicos_por_mil_derechohabientes_072025)
# View(df_ism)
# ===


# ===


# ===
# ////////////


# ////////////
# Shape files ----

# ===
# state boundaries, but now have OOAD boundaries (middle cut)
# mex_sf <- ne_states(country = "Mexico", returnclass = "sf") %>%
#     select(name_es) %>%
#     rename(Estado = name_es)

ls()

sh_dir <- "~/Documents/work/science/devel/github/med-comp-imss/geo_stats/shapefiles/por_OOAD/"
# read the shapefile
mex_sf <- st_read(paste0(sh_dir, "mexico_OOADs.shp"))
mex_sf

# check CRS
st_crs(mex_sf)

# Remove NAs:
str(mex_sf)
summary(as.factor(mex_sf$name_es))
mex_sf <- mex_sf %>%
    filter(!is.na(name_es))
summary(as.factor(mex_sf$name_es))
str(mex_sf)

# names should be OOADs:
unique(mex_sf$name_es)
# ===
# ////////////



# ////////////
# Distances from polygon centroids ----

# st_centroid() on an sf object drops the original polygon geometry and replaces it with a single point (the centroid),
# Keeps all other attribute columns “as is


# Compute the centroid of each polygon:
centroids <- mex_sf %>%
    st_transform(4326) %>%                # ensure CRS is geographic (lon/lat WGS84)
    st_centroid()                         # returns the geometric “center” of each state polygon
                                          # or st_point_on_surface(), in multi‐piece polygons
                                          # the centroid can lie outside the true shape (e.g. horseshoe‐shaped state)
str(centroids)
str(mex_sf)

# Geo points within polygon with lat and lon:
centroids$geometry

# lon/lat into a matrix:
coords <- st_coordinates(centroids)
str(coords)

# Build distance matrix (km):
dist_mat <- distm(coords, coords) / 1000
str(dist_mat)

# dist_mat[i,j] is the circle distance between points i and j:
rownames(dist_mat) <- centroids$name_es
colnames(dist_mat) <- centroids$name_es

str(dist_mat)
epi_head_and_tail(as.data.frame(dist_mat))

# Distance between centroids:
dist_mat["Sonora", "Sonora"]
dist_mat["Sonora", "Baja California"]
# ===
# ////////////


# ////////////
# Estimate resources and distances ----


# ===
# TO DO:
# Loaded in other script, should be eg qna 07 2025
# but needs ratio, such as med per DH to use as population, and specialty (if estiamting by specialty)
# will estimate based on constraints (parameters), distance (centroids), and population

epi_head_and_tail(meds_2025)
colnames(meds_2025)


# Create dataframe and vars with needed inputs:
# DELEGACION | Population | meds_tasa | meds_totales

epi_head_and_tail(meds_2025) # rows are individual medics
epi_head_and_tail(df_ism) # rows are states

# Group residentes by OOAD:
summary(meds_2025$DESCRIP_CLASCATEG)
resids_OOAD <- meds_2025 %>%
    filter(DESCRIP_CLASCATEG == "9.RESIDENTES") %>%
    count(DELEGACION, name = "total_residentes")
epi_head_and_tail(resids_OOAD, cols = 2)
str(resids_OOAD)# View(resids_OOAD)
# ===


# ===
# OOADs need merging from SIAP to DIR data:
resids_OOAD$DELEGACION <- as.character(resids_OOAD$DELEGACION)
# México Oriente      México Poniente should be "Estado de México Oriente" "Estado de México Poniente"
resids_OOAD$DELEGACION[resids_OOAD$DELEGACION == "México Oriente"] <- "Estado de México Oriente"
resids_OOAD$DELEGACION[resids_OOAD$DELEGACION == "México Poniente"] <- "Estado de México Poniente"
resids_OOAD$DELEGACION

# 35 - DF Norte       36 - DF Norte       37 - DF Sur         38 - DF Sur
# are merged in DIR data as "Ciudad de México Norte"    "Ciudad de México Sur"
# Add rows with sums for 35 - DF Norte       36 - DF Norte as "Ciudad de México Norte":
df <- resids_OOAD
sum_ids <- c("35 - DF Norte", "36 - DF Norte")
cdmx <- df %>%
    filter(DELEGACION %in% sum_ids) %>%
    summarise(across(-DELEGACION, sum, na.rm = TRUE)) %>%
    mutate(DELEGACION = "Ciudad de México Norte")
df <- bind_rows(df, cdmx)
df <- df %>%
    filter(!DELEGACION %in% sum_ids)
epi_head_and_tail(df, cols = 2)

# Same for "Ciudad de México Sur":
sum_ids <- c("37 - DF Sur", "38 - DF Sur")
cdmx <- df %>%
    filter(DELEGACION %in% sum_ids) %>%
    summarise(across(-DELEGACION, sum, na.rm = TRUE)) %>%
    mutate(DELEGACION = "Ciudad de México Sur")
df <- bind_rows(df, cdmx)
df <- df %>%
    filter(!DELEGACION %in% sum_ids)
epi_head_and_tail(df, cols = 2)

resids_OOAD <- df
rm(df)
# ===

# ===
# Create LP dataframe from initial data
# get residentes totals per OOAD/DELEGACION, for full count of available medics

lp_df <- df_ism %>%
    left_join(resids_OOAD, by = "DELEGACION") %>%
    mutate(
        total_residentes = coalesce(total_residentes, 0)
    )
# View(lp_df)
# ===

# ===
# Add cols:
colnames(lp_df)

# df_ism$DELEGACION, # state / admin area
# df_ism$Derechohabientes_DIR_03_2025,  # = total population in the state
# df_ism$medicos_por_mil_derechohabientes_072025, # = medics per 1000 population in each state
# df_ism$Total_meds,

# all available medics, not jsut adscritos/consultants:
lp_df$resids_mas_adscritos <- lp_df$Total_meds + lp_df$total_residentes

# Remove 'total' row:
lp_df <- lp_df %>%
    filter(DELEGACION != "Total")
# View(lp_df)

# Change colnames to match code below:
colnames(lp_df)[which(colnames(lp_df) == "medicos_por_mil_derechohabientes_072025")] <- "meds_tasa"
colnames(lp_df)[which(colnames(lp_df) == "Derechohabientes_DIR_03_2025")] <- "Poblacion"
# ===
# ////////////


# ////////////
# Linear assignment parameters ----
summary(lp_df$meds_tasa)

# TO DO: manually set
target_ratio <- 1.47     # using median, desired medics per 10k population
pop_denominator <- 1e3      # denominator for population, eg 1000 people/units
# ////////////


# ////////////
# Compute surplus / deficit adscritos ----
# supply_all = adscritos 'sobrantes'
# demand_all = adscritos 'faltantes'

colnames(lp_df)

lp_df <- lp_df %>%
    mutate(
        # positive = surplus, zero if deficit
        supply_all = round(pmax(0, (meds_tasa - target_ratio) * Poblacion / pop_denominator), 2),
        # positive = deficit, zero if surplus
        demand_all = round(pmax(0, (target_ratio - meds_tasa) * Poblacion / pop_denominator), 2)
    )
# The difference in rates is “extra (or missing) doctors per 10 000 people
# To turn that into a head-count, multiply by Population / pop_denominator because there are eg
# 1000‐person units in study population
# supply and demand vars are the number of absolute doctors above or below the target that each state can spare or needs

# View(lp_df)
# ===


# ===
# Ensure at least one/x consultant / adscrito in every state where demand_all > 0

# TO DO: manually set
adscrito_target <- 10         # at least 1 consultant if demand > 0

lp_df <- lp_df %>%
    mutate(
        # how many consultants are actually available to send
        supply_adscritos = pmin(resids_mas_adscritos, floor(supply_all)),
        # if a state has any deficit, require ≥x consultant (adscrito_target)
        demand_adscritos = ifelse(demand_all > 0, adscrito_target, 0)
    )
# View(lp_df)

# Cols are:
# supply_adscritos – how many consultants you could spare
# demand_adscritos – 'adscrito_target' or 0, depending on whether the state needs consultants
# supply_all – how many any docs (consultants + residents) you could spare
# demand_all – how many any docs you need


# Rows with supply_all > 0 are supply nodes
# Rows with demand_all > 0 are demand nodes
# supply_adscritos/demand_adscritos** give the subset of consultant-only flows for your first LP pass;
# supply_all/demand_all** fill out the remainder in your second pass.
# ===


# ===
# Checks:
all(lp_df$DELEGACION %in% rownames(dist_mat))  # Should return TRUE

# Infeasibility can occur if demand_adscritos is greater than supply_adscritos in
# LP passes:
sum(lp_df$supply_adscritos) >= sum(lp_df$demand_adscritos)  # Should be TRUE
sum(lp_df$supply_all) >= sum(lp_df$demand_all)              # Should also be TRUE

# Check supply/demand vectors ----
colnames(lp_df)

col_check <- c("DELEGACION",
               "resids_mas_adscritos",
               "supply_all",
               "demand_all",
               "supply_adscritos",
               "demand_adscritos"
               )
# View(t(lp_df[which(lp_df$DELEGACION == "Ciudad de México Norte"), ]))
lp_df[, col_check]
# ===
# ////////////



# ////////////
# Run LP ----

# feed its four supply/demand vectors into lp.transport()
# Runs two LPs: first ensuring at least x consultants move into each under-served state,
# then filling all remaining gaps with the broader workforce.
# Biases the objective toward birth-state retention by subtracting a large bonus on the diagonal of the distance matrix
# result will be an eg nation-wide redeployment matrix as flow_total[i,j]
# stipulating the number of medics to shift from state i to state j


# ===
# use cost_mat to minimize travel distance (with any birth‐state bonus adjustment)
n_states <- nrow(lp_df)

# TO DO: manually set
birth_bonus <- -10       # distance of km‐equivalent “bonus” for move to birth state

# Build cost matrix with a birth‐state bonus on the diagonal:
bonus_mat <- matrix(0, n_states, n_states)
diag(bonus_mat) <- birth_bonus
cost_mat <- dist_mat[lp_df$DELEGACION, lp_df$DELEGACION] - bonus_mat
cost_mat
# ===

# ===
# Move consultants to cover at least one/x/target min adscritos consultant in each deficit state:
sol1 <- lp.transport(
    cost.mat  = cost_mat,
    direction = "min",
    row.signs = rep("<=", n_states),
    row.rhs   = lp_df$supply_adscritos,
    col.signs = rep(">=", n_states),
    col.rhs   = lp_df$demand_adscritos
    # integers   = TRUE
    )
flow1 <- matrix(sol1$solution, n_states, byrow = TRUE)
str(flow1)
if (sol1$status != 0) stop("First LP failed: check constraints or feasibility")
print(sol1$status)  # 0 = success, 2 = infeasible

# Compute the remaining supply and demand (ie consultants first, then all available medics, including residents):
rem_supply <- pmax(0, lp_df$supply_all - rowSums(flow1))
rem_supply
rem_demand <- pmax(0, lp_df$demand_all - colSums(flow1))
rem_demand
# ===

# ===
# Sanity checks:
stopifnot(length(lp_df$supply_adscritos) == n_states)
stopifnot(all(rem_supply >= 0))
stopifnot(all(rem_demand >= 0))
# ===


# ===
# Fill the rest with any doctors (consultants + residents), second LP pass:
sol2 <- lp.transport(
    cost.mat  = cost_mat,
    direction = "min",
    row.signs = rep("<=", n_states),
    row.rhs   = rem_supply,
    col.signs = rep(">=", n_states),
    col.rhs   = rem_demand
    # integers   = TRUE
    )
flow2 <- matrix(sol2$solution, n_states, byrow = TRUE)
str(flow2)

print(sol2$status)  # 0 = success, 2 = infeasible
if (sol2$status != 0) stop("Second LP failed: check remaining supply/demand")
# ===

# ===
# Combine for the final movement plan / solution:
flow_total <- flow1 + flow2

dimnames(flow_total) <- list(
    From = lp_df$DELEGACION,
    To   = lp_df$DELEGACION
    )
str(flow_total)
epi_head_and_tail(as.data.frame(flow_total))
flow_total

head(rownames(flow_total), 10)
tail(rownames(flow_total), 10)
# View(t(lp_df[which(lp_df$DELEGACION == "Ciudad de México Norte"), ]))
# View(flow_total)
# ===

# ===
# Checks:
# TO DO: correct this, in theory bonus (move to birth state should be positive but set it to negative for it to work)
# Sparse, parameters need adjustment
# By default, lp.transport() returns a flattened row-major solution, rownames and colnames need to match, if scrambled
# re-run/check

sum_moves_df <- data.frame(
    delegacion = rownames(flow_total),
    supply1 = rowSums(flow1),
    demand1 = colSums(flow1),
    supply2 = rowSums(flow2),
    demand2 = colSums(flow2)
    )
sum_moves_df
# supply1	Total consultants (adscritos) sent from each origin‐state in the first LP pass
# demand1	Total consultants received by each destination‐state in the first LP pass
# supply2	Total medics (consultants + residents) sent from each origin‐state in the second LP pass
# demand2	Total medics received by each destination‐state in the second LP pass

lapply(as.list(sum_moves_df[2:5]), sum) # supplies and demands must match

# A zero in supply1[i] means no consultant left state i in pass 1
# a zero in demand2[j] means no additional medics were needed in state j in pass 2

# ===
# ////////////


# ////////////
# Map LP 'flows' or
# join back to individual‐level data (using birth-state information) to select specific doctors for each move

# Summarise / plot, etc ----
flow_long <- as.data.frame(flow_total) %>%
    rownames_to_column("To") %>%
    pivot_longer(
        cols      = -To,
        names_to  = "From",
        values_to = "n"
    ) %>%
    filter(n > 0)
# View(flow_long)

# total outbound moves per state:
outbound <- flow_long %>%
    group_by(From) %>%
    summarise(out = sum(n)) %>%
    arrange(desc(out))
outbound

# assume `centroids` are state centroids sf with a column "DELEGACION"
flow_map <- flow_long %>%
    left_join(centroids %>% select(From = name_es, geometry),
              by = "From") %>%
    left_join(centroids %>% select(To   = name_es, geometry),
              by = "To", suffix = c(".from",".to")) %>%
    st_as_sf(crs = 4326)

# Plot:
plot1 <- ggplot() +
    geom_sf(data = mex_sf, fill="grey95", color="white") +
    geom_curve(
        data       = flow_map,
        aes(x      = st_coordinates(geometry.from)[,1],
            y      = st_coordinates(geometry.from)[,2],
            xend   = st_coordinates(geometry.to)[,1],
            yend   = st_coordinates(geometry.to)[,2],
            size   = n),
        color      = "darkred",
        curvature  = 0.2,
        alpha      = 0.8
    ) +
    scale_size_continuous(range = c(0.2, 2), name = "Doctors moved") +
    theme_minimal()

# ////////////


# ////////////


# ////////////


# ////////////
# q()
# ////////////

