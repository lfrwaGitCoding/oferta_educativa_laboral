# Alluvial - Sankey interactivo

library(dplyr)
library(networkD3)
library(htmlwidgets)

# raw counts:
df_links1 <- df %>%
    count(EDO_NACIMIENTO, DELEGACION_2024_residencia, name = "value")

df_links2 <- df %>%
    count(DELEGACION_2024_residencia, DELEGACION_2025_adcsrito, name = "value")

# unified nodes list:
nodes <- data.frame(
    name = c(
        paste0("Nacimiento: ", unique(df_links1$EDO_NACIMIENTO)),
        paste0("Residencia: ", unique(df_links1$DELEGACION_2024_residencia)),
        paste0("Trabajo: ", unique(df_links2$DELEGACION_2025_adcsrito))
    ),
    group = c(
        rep("Nacimiento", length(unique(df_links1$EDO_NACIMIENTO))),
        rep("Residencia", length(unique(df_links1$DELEGACION_2024_residencia))),
        rep("Trabajo", length(unique(df_links2$DELEGACION_2025_adcsrito)))
    ),
    stringsAsFactors = FALSE
) %>% distinct(name, .keep_all = TRUE)

# build links, convert names to node IDs:
make_links <- function(df, from_col, to_col, prefix_from, prefix_to) {
    df %>% transmute(
        source = match(paste0(prefix_from, ": ", .data[[from_col]]), nodes$name) - 1,
        target = match(paste0(prefix_to, ": ", .data[[to_col]]), nodes$name) - 1,
        value
    )
}




links <- bind_rows(
    make_links(df_links1, "EDO_NACIMIENTO",
               "DELEGACION_2024_residencia",
               "Nacimiento",
               "Residencia"),
    make_links(df_links2, "DELEGACION_2024_residencia",
               "DELEGACION_2025_adcsrito",
               "Residencia",
               "Trabajo")
    )

# plot:
colourScale <- 'd3.scaleOrdinal()
                 .domain(["Nacimiento", "Residencia", "Trabajo"])
                 .range(["#4C78A8", "#F58518", "#54A24B"]);'

sankey <- sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    NodeGroup  = "group",
    sinksRight = FALSE,    # force all flows left-to-right
    fontSize = 12,
    nodeWidth = 30,
    colourScale = colourScale,
    units = "médicos"
    )
sankey

# standalone HTML:
file_n <- 'plot_alluvial_trayectoria_2024-2025'
suffix <- 'html'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
saveWidget(sankey, file = outfile, selfcontained = TRUE)
