# ////////////
# Script information ----

# CES
# Agosto 2025
# tabla vacantes, especialidad/area resp, localidad
# Input is rdata output from script:
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
# spec_loc <- "Julio_2025/"
spec_loc <- "original_transfers/"

# infile <- "Egresos_Febrero_2026_14072025.xlsx"
infile <- "Copia de Base_Simulacion_Egresos_Febrero_OOAD_UMAE_2026_25082025.xlsx"

# sheet <- "SIMULACION EGRESO 2026"
sheet <- "EGRESOS FEBRERO 2026"

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
script_n <- 'CES_2026_egresables_tabla'
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

render_html <- function(tbl, pageLength = 10000) {
    widget <- DT::datatable(
        tbl,
        filter = "top", # per-column filters
        extensions = "Buttons",
        options = list(
            dom = "Bfrtip", # Buttons, filter, table
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            pageLength = pageLength,
            # sort:
            order = list(list(which(names(tbl) == "ESPECIALIDAD") - 1, "desc"))
        ),
        rownames = FALSE
    )
    }


# Re-order columns:
colnames(data_f)
cols_reord <- c("EDO_NACIMIENTO",
                "OOAD",
                "SEDE",
                "GRADO",
                "ESPECIALIDAD"
                # "CURP",
                # "AP_PATERNO",
                # "AP_MATERNO",
                # "NOMBRE"
                #"CATEGORIA"
                )
df_to_html <- data_f[, cols_reord]
epi_head_and_tail(df_to_html)
interactive_widget <- render_html(df_to_html)
interactive_widget

# Save:
infile_prefix
file_n <- "tabla_loc_egresables_2026_interactiva"
suffix <- 'html'
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_subdir,
                   file_n,
                   suffix
                   )
outfile
htmlwidgets::saveWidget(interactive_widget, outfile, selfcontained = TRUE)
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
