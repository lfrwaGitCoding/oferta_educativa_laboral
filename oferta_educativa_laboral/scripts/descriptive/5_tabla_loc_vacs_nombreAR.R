# ////////////
# Script information ----

# SIAP
# Unidad de Personal
# Julio 2025
# Descriptive stats
# tabla vacantes, especialidad/area resp, localidad
# Input is rdata output from script:
#

# Output
# ////////////


# ////////////
# Import libraries ----
library(data.table)
library(episcout)
library(ggthemes)
library(cowplot)
library(tidyverse)
library(htmlwidgets)
# ////////////


# ////////////
# Load rdata file ----

# ===
# Load locations
load("data/data_UP/processed/dir_locations.rdata.gzip")

data_dir <- data_dir
rdata_dir <- file.path(data_dir, "data_UP/processed")

# TO DO: Manually set:
# infile <- '2_clean_dups_col_types_Qna_17_Bienestar_2024.rdata.gzip'
# infile <- '2_clean_dups_col_types_Qna_17_Plantilla_2024.rdata.gzip'

# infile <- "2b_clean_subset_2_clean_dups_col_types_Qna_17_Bienestar_2024_meds.rdata.gzip"
# infile <- '2b_clean_subset_2_clean_dups_col_types_Qna_17_Bienestar_2024_enfermeras.rdata.gzip'

# infile <- "2b_clean_subset_2_clean_dups_col_types_Qna_17_Plantilla_2024_meds.rdata.gzip"
# infile <- '2b_clean_subset_2_clean_dups_col_types_Qna_17_Plantilla_2024_enfermeras.rdata.gzip'

# infile <- "2b_clean_subset_2_clean_dups_col_types_Qna_07_Plantilla_2025_resids.rdata.gzip"
infile <- "2b_clean_subset_2_clean_dups_col_types_Qna_07_Plantilla_2025_meds.rdata.gzip"

# Full path and file name:
infile_path <- file.path(rdata_dir, infile)
print(infile_path)
file.exists(infile_path)

print(dir(path = normalizePath(rdata_dir), all.files = TRUE))

load(infile_path)
ls()
# ===

# ===
# Get rid of RStudio warnings for loaded objects:
project_root <- project_root
results_dir <- results_dir
data_f <- data_f

# TO DO: needs updating:
code_dir <- code_dir

all_colnames <- all_colnames
char_cols <- char_cols
date_cols <- date_cols
fact_cols <- fact_cols
int_cols <- int_cols
id_cols <- id_cols
num_cols <- num_cols

print(project_root)
setwd(here::here())
getwd()
print(dir(path = normalizePath(project_root), all.files = TRUE))
# ===
# ////////////


# ////////////
# Output dir, based on today's date ----
script_n <- '5_tabla_loc_vacs_nombreAR'
infile
infile_prefix <- strsplit(infile, "\\.")[[1]][1]
infile_prefix
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
# Static table
colnames(data_f)
vars_loc_vac <- c("DELEGACION",
                  "CLASIF_UNIDAD",
                  "ADSCRIPCION",
                  "DEPENDENCIA",
                  "DESCRIPCION_SERVICIO",
                  "DESCRIP_LOCALIDAD",
                  "NOMBREAR",
                  "DESCRIP_TURNO",
                  "DESCRIP_HORARIO",
                  "DESCRIP_TIPO_DE_PLAZA",
                  "PLZOCU"
                  )
summary(data_f[, vars_loc_vac])

vars_loc_vac_min <- c("DELEGACION",
                  "DEPENDENCIA",
                  "DESCRIP_LOCALIDAD",
                  "NOMBREAR"
                  # "PLZOCU"
                  )
summary(data_f[, vars_loc_vac_min])
str(data_f[, vars_loc_vac_min])


# Generate lookup: distinct combinations of specified vars
vac_lookup <- function(df,
                       grouping_vars,
                       vac_var    = "PLZOCU",
                       vac_value  = "0") {

    # Summarise vacancy counts
    df %>%
        group_by(across(all_of(grouping_vars))) %>%
        summarise(
            vacantes = sum(.data[[vac_var]] == vac_value, na.rm = TRUE),
            .groups   = "drop"
        ) %>%
        arrange(desc(vacantes),
                across(all_of(grouping_vars))
        )
    }
lookup_tbl_vacs <- vac_lookup(data_f, vars_loc_vac_min, vac_var = "PLZOCU", vac_value = "0")
dim(lookup_tbl_vacs)
str(lookup_tbl_vacs)
sum(lookup_tbl_vacs$vacantes, na.rm = TRUE)

length(which(lookup_tbl_vacs$vacantes == 0))
length(which(lookup_tbl_vacs$vacantes == 1))
# View(lookup_tbl_vacs)


# Ocupadas:
lookup_tbl_ocu <- vac_lookup(data_f, vars_loc_vac_min, vac_var = "PLZOCU", vac_value = "1")
dim(lookup_tbl_ocu)
str(lookup_tbl_ocu)
sum(lookup_tbl_ocu$vacantes, na.rm = TRUE)

length(which(lookup_tbl_ocu$vacantes == 0))
length(which(lookup_tbl_ocu$vacantes == 1))
# View(lookup_tbl_ocu)

# Check:
sum(data_f$PLZOCU == "0", na.rm = TRUE) # vacantes
sum(data_f$PLZOCU == "1", na.rm = TRUE) # ocupadas
dim(data_f)

sum(lookup_tbl_vacs$vacantes, na.rm = TRUE) == sum(data_f$PLZOCU == "0", na.rm = TRUE)
sum(lookup_tbl_ocu$vacantes, na.rm = TRUE) == sum(data_f$PLZOCU == "1", na.rm = TRUE)

dim(data_f)[1] == (sum(lookup_tbl_vacs$vacantes, na.rm = TRUE) + sum(lookup_tbl_ocu$vacantes, na.rm = TRUE))

dim(data_f)
sum(data_f$PLZOCU == "0", na.rm = TRUE)
sum(data_f$PLZOCU == "1", na.rm = TRUE)
length(which(is.na(data_f$PLZOCU)))

lapply(data_f[, vars_loc_vac_min], function(x) sum(is.na(x)))


sum(lookup_tbl_vacs$vacantes, na.rm = TRUE)
sum(lookup_tbl_ocu$vacantes, na.rm = TRUE)


# Save:
infile_prefix
file_n <- "tabla_loc_vacs_nombreAR"
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s',
                   results_subdir,
                   file_n,
                   suffix
                   )

outfile
epi_write(lookup_tbl_vacs, outfile)
# ////////////



# ////////////
# Same but interactive
vac_lookup_interact <- function(df,
                                grouping_vars,
                                vac_var   = "PLZOCU",
                                vac_value = "1") {
    # Summarise vacancy counts per location
    tbl <- df %>%
        group_by(across(all_of(grouping_vars))) %>%
        summarise(
            vacantes = sum(.data[[vac_var]] == vac_value, na.rm = TRUE),
            .groups   = "drop"
        ) %>%
        arrange(desc(vacantes), across(all_of(grouping_vars)))

    # Create interactive DataTable with export buttons and search
    # add global search + column filters
    DT::datatable(
        tbl,
        filter = "top",               # column filters
        extensions = "Buttons",
        options = list(
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            pageLength = 25,
            order = list(list(which(names(tbl) == "vacantes") - 1, "desc"))
        ),
        rownames = FALSE
    )
}

interactive_widget <- vac_lookup_interact(data_f, vars_loc_vac_min)
interactive_widget

# Save:
infile_prefix
file_n <- "tabla_loc_vacs_nombreAR_interactiva"
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
