# ////////////
# Script information ----

# CES
# Trayectoria 2026 egresables
# Julio 2025
#
#
# Input is rdata output from script:
#

# Output are
# ////////////


# ////////////
# Import libraries ----
library(data.table)
library(episcout)
library(ggthemes)
library(cowplot)
library(tidyverse)
library(forcats)
library(readxl)
library(future)
library(future.apply)
library(gridExtra)  # tableGrob

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
# Set working directory to the project root  ----
# Should be there already if loaded as RStudio project
setwd(here::here())
# TO DO:
# Mac24:
# setwd(here::here())
# project_root <- "/Users/antoniob/Library/Mobile Documents/com~apple~CloudDocs/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral"
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts
getwd()
# ////////////


# ////////////
# Load files ----

# ===
# Load the .rdata.gzip file:
load("data/data_UP/processed/dir_locations.rdata.gzip")
ls()


# Get rid of RStudio warnings for loaded objects:
project_root <- project_root
data_dir <- data_dir
results_dir <- results_dir

code_dir <- code_dir
all_locs <- all_locs


# TO DO: needs updating:
# all_colnames <- all_colnames
# char_cols <- char_cols
# date_cols <- date_cols
# fact_cols <- fact_cols
# int_cols <- int_cols
# id_cols <- id_cols
# num_cols <- num_cols

print(project_root)
getwd()
print(dir(path = normalizePath(project_root), all.files = TRUE))
print(all_locs)
# ===


# ===
# UP and CES data dirs:
data_dir
# data_dir <- paste0(project_root, "/data/")
dir(data_dir)
up_data_dir <- file.path(data_dir, "data_UP/processed/")
dir.exists(up_data_dir)


ces_data_dir <- file.path(data_dir, "data_CES/raw/")
dir.exists(ces_data_dir)
# ===


# ===
# Dataset:
spec_loc <- "Julio_2025/"
infile <- "Egresos_Febrero_2026_14072025.xlsx"
sheet <- "SIMULACION EGRESO 2026"

infile_path <- file.path(sprintf("%s/%s/%s",
                                 ces_data_dir,
                                 spec_loc,
                                 infile
                                 ))
file.exists(infile_path)
data_f <- readxl::read_excel(path = infile_path, sheet = sheet)
epi_head_and_tail(data_f)
# ===
# ////////////


# ////////////
# Output dir, based on today's date ----
script_n <- 'CES_2026_egresables'
infile_prefix <- strsplit(script_n, "\\.")[[1]][1]
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
# Set up column types:
epi_clean_count_classes(data_f)
str(data_f)
colnames(data_f)

fact_cols <- c("CATEGORIA",
               "OOAD",
               "SEDE",
               "GRADO",
               "ESPECIALIDAD",
               "EDO_NACIMIENTO"
               )
# Convert to factor:
# data_f[fact_cols] <- lapply(data_f[fact_cols], as.factor)

# Convert to factor and reorder levels by count (descending)
data_f[fact_cols] <- lapply(
    data_f[fact_cols],
    function(x) fct_infreq(as.factor(x))
    )

# Id columns, keep as char:
id_cols <- c("CURP",
             "AP_PATERNO",
             "AP_MATERNO",
             "NOMBRE"
             )
# ////////////



# ////////////
# Basic bar plots for each of factor columns:
for (i in fact_cols) {
    p <- epi_plot_bar(df = data_f,
                     var_x = i,
                     custom_palette = custom_palette
                     ) +
        labs(y = "Frecuencia") +
        geom_text(
            aes(label = scales::percent(after_stat(count) / sum(after_stat(count)), accuracy = 0.1)),
            stat = "count",
            angle = 90,
            hjust = -0.1,
        ) +
        epi_plot_theme_imss() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(p)
    # Save each plot:
    file_n <- sprintf("plot_bar_%s", i)
    outfile <- sprintf(fmt = '%s/%s.pdf', results_subdir, file_n)
    ggsave(filename = outfile, plot = p, width = 12, height = 12)
}
# ////////////


# ////////////
# Generate Snakey / Alluvial plots
# for two-way flows of counts of:
# EDO_NACIMIENTO to "OOAD"

# ===
# Alluvial/Sankey diagram ----
# TO DO: need to collapse sede as ~300, labels obscure plot
df <- data_f
summary(df$EDO_NACIMIENTO)
summary(df$OOAD)

alluv_plot <- df %>%
    count(EDO_NACIMIENTO, OOAD) %>%
    ggplot(aes(axis1 = EDO_NACIMIENTO,
               axis2 = OOAD,
               y = n)) +
    geom_alluvium(aes(fill = EDO_NACIMIENTO), width = 1/12) +
    geom_stratum(width = 1/12, fill = "grey80") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    scale_x_discrete(limits = c("Nacimiento","Residencia")) +
    # scale_x_discrete(limits = c("Birth", "Univ", "Work"), expand = c(.1, .1)) +
    labs(title = "Movimiento de médicos egresables 2026: nacimiento -> sede de residencia",
         y = "Frecuencia"
         # fill  = "Lugar de nacimiento"
    ) +
    epi_plot_theme_imss()
alluv_plot

epi_plot_alluvial <- function(data,
                              var1, var2,
                              plot_title = "Alluvial Plot",
                              base_size = 12,
                              show_labels = FALSE,
                              fill_by = NULL
                              ) {
    library(ggplot2)
    library(ggalluvial)
    library(ggrepel)
    library(dplyr)
    library(forcats)  # to lump rare categories

    # Ensure variables are symbols for tidy evaluation:
    var1 <- rlang::ensym(var1)
    var2 <- rlang::ensym(var2)

    # fill by var1 if fill_by not specified:
    fill_var <- if (is.null(fill_by)) var1 else rlang::ensym(fill_by)

    # Counts:
    df_counts <- data %>%
        count(!!var1, !!var2)

    # Plot:
    p <- ggplot(df_counts,
                aes(axis1 = !!var1,
                    axis2 = !!var2,
                    y = n)) +
        geom_alluvium(aes(fill = !!fill_var), width = 1/8, alpha = 0.8) +
        geom_stratum(width = 1/6, fill = "grey80", color = "white") +
        scale_x_discrete(
            limits = c(as_label(var1), as_label(var2)),
            expand = c(0.1, 0.1)
        ) +
        labs(title = plot_title,
             y = "Frecuencia",
             x = NULL
             ) +
        epi_plot_theme_imss(base_size = base_size) +
        theme(legend.position = "none")

    # Add labels:
    if (show_labels) {
        p <- p + geom_text_repel(
            stat = "stratum",
            aes(
                label = after_stat(stratum),
                x = after_stat(x) + ifelse(after_stat(x) == 1, -0.05, 0.05) # nudge left/right
            ),
            size = 6,
            max.overlaps = Inf,
            segment.color = "grey50",
            segment.size = 0.3
        )
    }

    return(p)
}

alluv_plot4 <- epi_plot_alluvial(df, EDO_NACIMIENTO, OOAD,
                                 plot_title = "Movimiento de médicos egresables 2026 por OOAD",
                                 base_size = 30,
                                 show_labels = TRUE)
alluv_plot4

# Save:
file_n <- 'plot_alluvial_egresables_2026_EDO_NACIMIENTO_OOAD'
suffix <- 'pdf'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
ggsave(outfile,
       plot = alluv_plot4,
       height = 20,
       width = 20,
       units = "in"
       # scale = 1,
       # dpi = 300
       )
# ===



# ===
# EDO_NACIMIENTO to "SEDE"
colnames(data_f)
alluv_plot <- epi_plot_alluvial(df, EDO_NACIMIENTO, SEDE,
                                plot_title = "Movimiento de médicos egresables 2026 por sede",
                                base_size = 30,
                                show_labels = FALSE
                                )
alluv_plot
# ===



# ===
# by "ESPECIALIDAD" and OOAD
# Parallel execution:
plan(multisession, workers = 32)  # or `multicore` on Linux/macOS

# Function to generate and save one plot
parallel_alluvials <- function(i) {
    df_i <- data_f %>% filter(ESPECIALIDAD == i)

    alluv_plot <- epi_plot_alluvial(df_i,
                                    EDO_NACIMIENTO, OOAD,
                                    plot_title = sprintf("Movimiento de médicos egresables 2026 por OOAD:\n%s", i),
                                    base_size = 30,
                                    show_labels = TRUE
                                    )
    alluv_plot <- alluv_plot + theme(plot.title = element_text(size = 20, hjust = 0))
    # print(alluv_plot)

    # Save file
    file_n <- sprintf("plot_alluvial_egresables_2026_EDO_NACIMIENTO_OOAD_%s", i)
    suffix <- "pdf"
    outfile <- sprintf("%s/por_especialidad/%s.%s", results_subdir, file_n, suffix)
    ggsave(outfile, plot = alluv_plot, height = 20, width = 20, units = "in")

    return(outfile)
}

out_files <- future_lapply(unique(data_f$ESPECIALIDAD), parallel_alluvials)
# ===

# ===
# por_estado
# Parallel setup
plan(multisession, workers = 32)

parallel_alluvials_estado <- function(state_name) {
    df_state <- data_f %>%
        filter(EDO_NACIMIENTO == state_name)

    # Get counts:
    summary_tbl <- df_state %>%
        count(OOAD, name = "Frecuencia") %>%
        mutate(
            Porcentaje = scales::percent(Frecuencia / sum(Frecuencia), accuracy = 0.1)
        ) %>%
        arrange(desc(Frecuencia))

    # alluvial:
    alluv_plot <- epi_plot_alluvial(
        df_state,
        EDO_NACIMIENTO, OOAD,
        plot_title = sprintf("Movimiento de médicos egresables 2026:\nNacidos en %s", state_name),
        base_size = 30,
        show_labels = TRUE,
        fill_by = "OOAD"
    )
    alluv_plot <- alluv_plot + theme(plot.title = element_text(size = 20, hjust = 0))

    # table grob:
    tbl_grob <- gridExtra::tableGrob(summary_tbl, rows = NULL)

    # Plot plus table:
    combined_plot <- cowplot::plot_grid(
        alluv_plot,
        tbl_grob,
        nrow = 1, ncol = 2,
        rel_widths = c(2.5, 1)  # Wider plot, narrower table
        # rel_heights = c(2, 1)  # More space for plot, less for table
    )

    # print(combined_plot)

    # Save plot
    file_n <- sprintf("plot_alluvial_egresables_2026_NACIMIENTO_%s", state_name)
    outfile <- sprintf("%s/por_estado/%s.pdf", results_subdir, file_n)
    ggsave(outfile, plot = combined_plot, height = 20, width = 20, units = "in")

    return(outfile)
}
# parallel_alluvials_estado("JALISCO")  # Example call to the function)

# Run in parallel for all states
out_files <- future_lapply(
    unique(data_f$EDO_NACIMIENTO),
    parallel_alluvials_estado,
    future.seed = TRUE  # reproducible random numbers for geom_text_repel
    )

# ===
# ////////////


# ////////////
# Full counts:
# Long df of three-way counts (geom_alluvium data):
epi_head_and_tail(df)

flow_counts <- df %>%
    count(
        EDO_NACIMIENTO,
        OOAD,
        name = "n"
    ) %>%
    arrange(desc(x = n))
head(flow_counts, n = 10)
epi_head_and_tail(flow_counts)
# View(flow_counts)

# save:
file_n <- 'table_flow_counts'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
epi_write(flow_counts, outfile)

# Wide matrix edo nac a Residencia:
mat_birth_resid <- flow_counts %>%
    dplyr::select(EDO_NACIMIENTO, OOAD, n) %>%
    tidyr::pivot_wider(
        names_from  = OOAD,
        values_from = n,
        values_fill = 0
    ) %>%
    column_to_rownames("EDO_NACIMIENTO")

mat_birth_resid[1:5, 1:5]      # first 5×5 corner

# save:
file_n <- 'matrix_flow_counts_nac_OOAD'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
epi_write(mat_birth_resid, outfile, row.names = TRUE)


# # full 3-D contingency array:
# flow_array <- xtabs(
#     n ~ EDO_NACIMIENTO + OOAD,
#     data = flow_counts
# )
#
# flow_array["Aguascalientes", "Jalisco", "CDMX"] # how many went A→B→C
#
#
# ////////////


# ////////////
# Aluvial interactivo

library(dplyr)
library(networkD3)
library(htmlwidgets)

# raw counts:
df_links1 <- df %>%
    count(EDO_NACIMIENTO, OOAD, name = "value")

# unified nodes list:
nodes <- data.frame(
    name = c(
        paste0("Nacimiento: ", unique(df_links1$EDO_NACIMIENTO)),
        paste0("Residencia: ", unique(df_links1$OOAD))
    ),
    group = c(
        rep("Nacimiento", length(unique(df_links1$EDO_NACIMIENTO))),
        rep("Residencia", length(unique(df_links1$OOAD)))
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
               "OOAD",
               "Nacimiento",
               "Residencia")
)

# plot:
colourScale <- 'd3.scaleOrdinal()
                 .domain(["Nacimiento", "Residencia"])
                 .range(["#4C78A8", "#F58518"]);'

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
file_n <- 'interactivo_alluvial_egresables_2026'
suffix <- 'html'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
saveWidget(sankey, file = outfile, selfcontained = TRUE)
# ////////////


# ////////////
# The end  ----

print(sessionInfo())

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
