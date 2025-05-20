


# ////////////
library(janitor)
library(vcd)      # for assocstats()
library(ggalluvial)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(arules)
library(arulesViz)
library(nnet)
library(sf)
library(viridis)
library(MASS)
library(ggrepel)
# ////////////


# ////////////
# ===
# Get estado de nacimiento plus any info from CES:
df <- nuevos_adscritos

summary(as.factor(activos_2024$EDO_NACIMIENTO))

df <- merge(df, activos_2024, by = "CURP", all.x = TRUE)
epi_head_and_tail(df)
colnames(df)
df <- as.data.frame(df)

# df <- merge(df, activos_2025, by = "CURP", all.x = TRUE)
# epi_head_and_tail(df)
# colnames(df)
# # View(df)
# class(df)
# df <- as.data.frame(df)
# summary(as.factor(df$TIPO))
# summary(as.factor(df$NACIONALIDAD))

# Manually, keep only relevant columns:
cols_keep <- c("CURP",
               "MATRICULA.x",
               "NSS",
               "SEXO",
               "DELEGACION_2024_residencia",
               "CLASIF_UNIDAD",
               "ADSCRIPCION",
               "DEPENDENCIA",
               "DESCRIPCION_SERVICIO",
               "DESCRIP_LOCALIDAD",
               "CATEGORIA.x",
               "DESCRIP_CLASCATEG",
               "EDAD",
               "NOMBREAR",
               "MCABAJA",
               "DescripcionTC",
               "DELEGACION_2025_adcsrito",
               "TIPO_ESPECIALIDAD",
               "ESPECIALIDAD",
               "CLUES",
               "SEDE_ACADEMICA",
               "NIVEL_DE_ATENCION",
               "DELEGACION/UMAE",
               "GRADO",
               "NACIONALIDAD",
               "AVAL_ACADEMICO",
               "CONTRATACION",
               "EDO_NACIMIENTO"
               )
df <- df[, cols_keep]
dim(df)
nuevos_adscritos <- df
rm(df)
# ===




# ===
# Collapse to eg DELEGACION_trab, resid and nac

# birth state:
summary(as.factor(nuevos_adscritos$EDO_NACIMIENTO))

# sede academico residencia:
summary(as.factor(nuevos_adscritos$DELEGACION_2024_residencia))

# work location:
summary(as.factor(nuevos_adscritos$DELEGACION_2025_adcsrito))


df <- nuevos_adscritos

summary(as.factor(df$EDO_NACIMIENTO))
# Convert "NINGUNO" to NA, may be foreign graduates:
df$EDO_NACIMIENTO <- ifelse(df$EDO_NACIMIENTO == "NINGUNO", NA, df$EDO_NACIMIENTO)
summary(as.factor(df$EDO_NACIMIENTO))

# Drop zero levels in factor cols, such as "Nivel Central" for DELEGACION vars:
str(df$DELEGACION_2024_residencia)
summary(as.factor(df$DELEGACION_2024_residencia))
summary(as.factor(df$DELEGACION_2025_adcsrito))

df <- droplevels(df)

summary(as.factor(df$DELEGACION_2024_residencia))
summary(as.factor(df$DELEGACION_2025_adcsrito))

nuevos_adscritos <- df
rm(df)
# ===
# ////////////



# ////////////
#

# 1. cross-tabs & χ²/V.
# 2. Visualise — mosaic plots, alluvial flows, geoflows.
# 3. Model — log‐linear, MCA, or even classifier/regression.
# 4. Deep dive — association rules for high‐lift combos.
# This should give both a statistical and visual understanding of how someone’s birth state and alma mater shape where they end up working.

# Heatmap for an at-a-glance matrix of counts.
# Alluvial for individual flow paths through three stages.
# MCA biplot to see clustering of categories in low-dimensional space.
# Flow map to ground the origin–destination in geography.

df <- nuevos_adscritos
nac <- "EDO_NACIMIENTO"
sede_resid <- "DELEGACION_2024_residencia"
ubic_lab <- "DELEGACION_2025_adcsrito"


# ===
# 1. Simple cross-tabs and χ²/Cramer’s V ----
# pairwise counts
dim(df)
summary(as.factor(df[[nac]]))
sum(summary(as.factor(df[[nac]])))

summary(as.factor(df[[sede_resid]]))
length(unique(df[[sede_resid]]))

summary(as.factor(df[[ubic_lab]]))
sum(summary(as.factor(df[[ubic_lab]])))




tab1 <- table(df[[nac]], df[[ubic_lab]])
tab2 <- table(df[[nac]], df[[sede_resid]])
tab3 <- table(df[[sede_resid]], df[[ubic_lab]])

tab1
tab2
tab3

# χ² and Cramer's V
assocstats(tab1)   # χ², Phi, Contingency coeff, Cramer's V
assocstats(tab2)
assocstats(tab3)
# Cramer’s V e.g. >0.2–0.3 indicates a non-trivial association
# Sparse tables and many zero's
# ===

# ===
# 2. Three-way log-linear model ----
# build 3-way table
tbl3 <- xtabs(~ df[[nac]] + df[[sede_resid]] + df[[ubic_lab]], data = df)
str(tbl3)
tbl3

# main‐effects‐only vs adding interactions
m0 <- loglm(~ df[[nac]] + df[[sede_resid]] + df[[ubic_lab]], data = tbl3)
m0
m1 <- loglm(~ (df[[nac]] + df[[sede_resid]] + df[[ubic_lab]])^2, data = tbl3)
m2 <- loglm(~ df[[nac]] * df[[sede_resid]] * df[[ubic_lab]], data = tbl3)

anova(m0, m1, m2)   # see which interactions significantly improve fit
# ===


# ===
# 3. Multiple Correspondence Analysis (MCA) biplot ----
# Treat as categorical dimensions
df[[nac]]
df[[sede_resid]]
df[[ubic_lab]]
res.mca <- MCA(df[, c(nac, sede_resid, ubic_lab)], graph = FALSE)
res.mca

# TO DO: Labels obscure plot
mca_plot <- fviz_mca_biplot(res.mca,
                repel = TRUE,
                ggtheme = theme_minimal(),
                title = "Gráfica de correspondencia múltiple (MCA): nacimiento x sede de residencia x adscripción laboral")
mca_plot
# associations may be present for .i.e. if points for eg Aguascalientes (nac) cluster near sede x
# ===


# ===
# 4. Alluvial/Sankey diagram ----
# Visualise flows through the three stages
# Birth → University → CurrentLocation

# TO DO: need to collapse sede as ~300, labels obscure plot
summary(as.factor(df$DELEGACION_2024_residencia))

alluv_plot <- df %>%
    count(EDO_NACIMIENTO, DELEGACION_2024_residencia, DELEGACION_2025_adcsrito) %>%
    ggplot(aes(axis1 = EDO_NACIMIENTO,
               axis2 = DELEGACION_2024_residencia,
               axis3 = DELEGACION_2025_adcsrito,
               y     = n)) +
    geom_alluvium(aes(fill = EDO_NACIMIENTO), width = 1/12) +
    geom_stratum(width = 1/12, fill = "grey80") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    scale_x_discrete(limits = c("Nacimiento","Residencia","Trabajo")) +
    # scale_x_discrete(limits = c("Birth", "Univ", "Work"), expand = c(.1, .1)) +
    labs(title = "Movimiento de médicos: nacimiento -> sede de residencia -> adscripción laboral",
         y = "Frecuencia"
         # fill  = "Lugar de nacimiento"
         ) +
    theme_minimal()
alluv_plot

alluv_plot4 <- df %>%
    count(EDO_NACIMIENTO,
          DELEGACION_2024_residencia,
          DELEGACION_2025_adcsrito) %>%
    ggplot(aes(axis1 = EDO_NACIMIENTO,
               axis2 = DELEGACION_2024_residencia,
               axis3 = DELEGACION_2025_adcsrito,
               y     = n)) +
    geom_alluvium(aes(fill = EDO_NACIMIENTO), width = 1/8, alpha = 0.8) +
    geom_stratum(width = 1/6, fill = "grey80", color = "white") +
    geom_text_repel(
        stat        = "stratum",
        aes(label   = after_stat(stratum)),
        # position    = position_stratum(vjust = 0.5),
        nudge_x     = 0.1,            # don’t move horizontally
        # direction   = "x",          # only repel up/down
        seed        = 42,
        size        = 3,
        max.overlaps= Inf
    ) +
    scale_x_discrete(
        limits = c("Nacimiento","Residencia","Trabajo"),
        expand = c(0.1, 0.1)
    ) +
    labs(
        title = "Movimiento de médicos: nacimiento → residencia → adscripción",
        y     = "Frecuencia",
        x     = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.position   = "none",            # drop the legend
        # panel.grid        = element_blank(),   # no grid
        # axis.ticks        = element_blank(),
        # axis.text.y       = element_blank(),   # hide the y labels if they overlap
        axis.text.x       = element_text(
            size   = 12,
            margin = margin(t = 20)
        ),
        plot.title        = element_text(
            hjust = 0.5,
            face  = "bold"
        )
    )
alluv_plot4

# Save:
file_n <- 'plot_alluvial_trayectoria_2024-2025'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile,
       plot = alluv_plot4,
       height = 20,
       width = 20,
       units = "in",
       scale = 1, dpi = 300
       )
# ===




# ===
# Alluvial filtered
colnames(df)
unique(df$NOMBREAR)

df2 <- df[which(df$NOMBREAR == "TRAUMATOLOGIA Y ORTOPEDIA"), ]
# df2 <- df[which(df$NOMBREAR == "OFTALMOLOGIA"), ]
epi_head_and_tail(df2)


alluv_plot4 <- df2 %>%
    count(EDO_NACIMIENTO,
          DELEGACION_2024_residencia,
          DELEGACION_2025_adcsrito) %>%
    ggplot(aes(axis1 = EDO_NACIMIENTO,
               axis2 = DELEGACION_2024_residencia,
               axis3 = DELEGACION_2025_adcsrito,
               y     = n)) +
    geom_alluvium(aes(fill = EDO_NACIMIENTO), width = 1/8, alpha = 0.8) +
    geom_stratum(width = 1/6, fill = "grey80", color = "white") +
    geom_text_repel(
        stat        = "stratum",
        aes(label   = after_stat(stratum)),
        # position    = position_stratum(vjust = 0.5),
        nudge_x     = 0.1,            # don’t move horizontally
        # direction   = "x",          # only repel up/down
        seed        = 42,
        size        = 3,
        max.overlaps= Inf
    ) +
    scale_x_discrete(
        limits = c("Nacimiento","Residencia","Trabajo"),
        expand = c(0.1, 0.1)
    ) +
    labs(
        title = "Movimiento de médicos: nacimiento → residencia → adscripción - Traumatología y Ortopedia",
        y     = "Frecuencia",
        x     = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.position   = "none",            # drop the legend
        # panel.grid        = element_blank(),   # no grid
        # axis.ticks        = element_blank(),
        # axis.text.y       = element_blank(),   # hide the y labels if they overlap
        axis.text.x       = element_text(
            size   = 12,
            margin = margin(t = 20)
        ),
        plot.title        = element_text(
            hjust = 0.5,
            face  = "bold"
        )
    )
alluv_plot4

# Save:
file_n <- 'plot_alluvial_trayectoria_2024-2025_TyO'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile,
       plot = alluv_plot4,
       height = 20,
       width = 20,
       units = "in",
       scale = 1, dpi = 300
)
# ===



# ===
# TO DO: continue here
# 5. Association‐rule mining ----
# If you treat each attribute as an item:
# install.packages("arules"); install.packages("arulesViz")

txn <- as(df %>%
              mutate(across(everything(), ~ paste(cur_column(), ., sep="=")))
          , "transactions")

rules <- apriori(txn,
                 parameter = list(supp = 0.01, conf = 0.5, maxlen = 3),
                 appearance = list(default = "lhs", rhs = grep("CurrentLocation=", itemLabels(txn), value=TRUE))
)
inspect(sort(rules, by="lift")[1:10])
plot(rules[1:20], method="graph", control=list(type="items"))

# You’ll discover, e.g., {BirthState=Yucatán,University=UNAM} => {CurrentLocation=Jalisco}  conf=0.65, lift=2.3
# ===


# ===
# 6. Geo-mapping origin–destination flows ----
# With our splitted state‐shapefile from before, you can map the net flow:

# count flows between birth–work by geo centroids
flows <- df %>%
    group_by(EDO_NACIMIENTO.x, DELEGACION.x) %>%
    tally(name = "n") %>%
    left_join(states_sf %>% st_centroid() %>%
                  select(DELEGACION.x, geometry) %>% rename(b_geom = geometry),
              by = c("EDO_NACIMIENTO.x" = "DELEGACION.x")) %>%
    left_join(states_sf %>% st_centroid() %>%
                  select(DELEGACION.x, geometry) %>% rename(w_geom = geometry),
              by = c("DELEGACION.x" = "DELEGACION.x"))

ggplot() +
    geom_sf(data = states_sf, fill = "grey95", color = "white") +
    geom_curve(
        data = flows,
        aes(x    = st_coordinates(b_geom)[,1],
            y    = st_coordinates(b_geom)[,2],
            xend = st_coordinates(w_geom)[,1],
            yend = st_coordinates(w_geom)[,2],
            size = log(n),
            alpha = n),
        curvature = 0.2,
        color = "darkblue"
    ) +
    scale_size_continuous(name = "log n") +
    scale_alpha(range = c(0.2,1), guide = FALSE) +
    theme_minimal() +
    labs(title = "Medic O-D flows: Birth→Work")
# ===


# ===
# 7. Regression modelling ----
# If you want to predict CurrentLocation (categorical, many levels) with BirthState & University:

mod <- multinom(CurrentLocation ~ BirthState + University, data = df)
summary(mod)

# assess goodness of fit, importance of predictors
car::Anova(mod)
# ===
# ////////////


# ////////////
# ===
# 1) Heatmap of BirthState × CurrentLocation
df %>%
    count(BirthState, CurrentLocation) %>%
    pivot_wider(names_from = CurrentLocation, values_from = n, values_fill = 0) %>%
    pivot_longer(-BirthState, names_to = "CurrentLocation", values_to = "n") %>%
    ggplot(aes(x = CurrentLocation, y = BirthState, fill = n)) +
    geom_tile() +
    scale_fill_viridis_c(option = "magma") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid = element_blank()
    ) +
    labs(
        x    = "Current Location",
        y    = "Birth State",
        fill = "Count",
        title = "Heatmap of Birth → Work"
    )
# ===


# ===
#
# get centroids by state
centroids <- states_sf %>%
    st_centroid() %>%
    select(state = STATE_NAME, geometry) %>%
    rename(centroid = geometry)

flows <- df %>%
    count(BirthState, CurrentLocation, name = "n") %>%
    left_join(centroids, by = c("BirthState" = "state")) %>%
    rename(b_centroid = centroid) %>%
    left_join(centroids, by = c("CurrentLocation" = "state")) %>%
    rename(w_centroid = centroid)

ggplot() +
    geom_sf(data = states_sf, fill = "grey95", color = "white") +
    geom_curve(
        data       = flows,
        aes(x    = st_coordinates(b_centroid)[,1],
            y    = st_coordinates(b_centroid)[,2],
            xend = st_coordinates(w_centroid)[,1],
            yend = st_coordinates(w_centroid)[,2],
            size = log(n),
            alpha = n),
        color     = "darkblue",
        curvature = 0.2
    ) +
    scale_size_continuous(name = "log(Count)") +
    scale_alpha(range = c(0.2,1), guide = "none") +
    theme_minimal() +
    labs(title = "Birth → Work Flows Across Mexico")
# ===

# ////////////
