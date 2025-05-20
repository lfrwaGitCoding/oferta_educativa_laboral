# ////////////
# Script information ----

# SIAP
# Unidad de Personal
# Mayo 2025
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
library(skimr)
library(pander)
library(log4r)
# ////////////


# ////////////
# Set working directory to the project root  ----
# Should be there already if loaded as RStudio project
setwd("~/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/")
# renv should be picked up automatically, see 0_xx in project_tools if it interrupts
getwd()
# ////////////


# ////////////
# Load files ----

# ===
# Load the .rdata.gzip file:
load("data/data_UP/access_SIAP_18092024/processed/dir_locations.rdata.gzip")
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
setwd(project_root)
getwd()
print(dir(path = normalizePath(project_root), all.files = TRUE))
print(all_locs)
# ===


# ===
# UP and CES data dirs:
data_dir
dir(data_dir)
up_data_dir <- file.path(data_dir, "data_UP/access_SIAP_18092024/processed/")
dir.exists(up_data_dir)


ces_data_dir <- file.path(data_dir, "data_CES/datos/")
dir.exists(ces_data_dir)
# ===


# ===
# Global 2024, need resids and meds:
meds_2024 <- "2_clean_dups_col_types_Qna_17_Plantilla_2024.rdata.gzip"
meds_2024 <- file.path(up_data_dir, meds_2024)
file.exists(meds_2024)
load(meds_2024)
ls()

# Get rid of warning:
data_f <- data_f

# object names will get overwritten:
meds_2024 <- data_f
epi_head_and_tail(data_f)
epi_head_and_tail(meds_2024)

# TO DO: run clean, will do with pipeline, manual here:
# Didn't convert '0' to NA for this file
length(which(is.na(meds_2024$MATRICULA)))
to_NA <- c("0")
meds_2024$MATRICULA <- ifelse(as.character(meds_2024$MATRICULA) %in% to_NA, NA, meds_2024$MATRICULA)
# Should match other IDs:
length(which(is.na(meds_2024$MATRICULA)))
length(which(is.na(meds_2024$NSS)))

# Global 2025:
meds_2025 <- "2_clean_dups_col_types_Qna_07_Plantilla_2025.rdata.gzip"
meds_2025 <- file.path(up_data_dir, meds_2025)
file.exists(meds_2025)
load(meds_2025)
ls()

# object names will get overwritten:
meds_2025 <- data_f
epi_head_and_tail(data_f)
epi_head_and_tail(meds_2025)

length(which(is.na(meds_2025$MATRICULA)))

rm(data_f)
ls()
# ===


# ===
# Activos, CES data:
dir(ces_data_dir)

activos_2024 <- "Activos_Todos_31102024.csv"
activos_2024 <- paste0(ces_data_dir, activos_2024)
file.exists(activos_2024)
activos_2024 <- epi_read(activos_2024)
epi_head_and_tail(activos_2024)


activos_2025 <- "Activos_Todos.060525UEI.csv"
activos_2025 <- paste0(ces_data_dir, activos_2025)
file.exists(activos_2025)
activos_2025 <- epi_read(activos_2025)
epi_head_and_tail(activos_2025)
# ===
# ////////////


# ////////////
# Source functions/scripts/etc
# TO DO:
# Source (until I update episcout)
source(file.path(paste0(code_dir, '/scripts/funcs_epi_source.R')))
# ////////////


# ////////////
# Output dir, based on today's date ----
script_n <- 'CES_UP_trayectoria'
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
# Capture output / log ----

# ===
# Redirect standard output
if (!interactive()) { # TRUE if not interactive, will then log output
    script_n <- '2_clean_dups_col_types'
    sink_stdout <- paste0(results_subdir, '/', script_n, '.sink_stdout.log')
    sink(sink_stdout, split = TRUE)

    # Redirect messages and warnings
    sink_msg <- file(paste0(results_subdir, '/', script_n, '.sink_msg.log'), open = "wt")
    sink(sink_msg, type = "message")

    # Example outputs
    cat("Test: This is standard output.\n")
    message("Test: This is a message.")
    warning("Test: This is a warning.")
}
# ===

# ===
# Create a logger
if (!interactive()) { # TRUE if not interactive, will then log output
    logger <- create.logger()
    log_n <- paste0(results_subdir, '/', script_n, '.log4r.log')
    logfile(logger) <- log_n # Log file location
    level(logger) <- "INFO"  # Set logging level (DEBUG, INFO, WARN, ERROR)

    # Add log messages
    # info(logger, "Script started")
    # debug(logger, "This is a debug message")
    # warn(logger, "This is a warning")
    # error(logger, "This is an error")
}
# ////////////


# ////////////
# Merge data frames ----
# resid 2024, adscrito 2025, estado nac., estado resid., estado laboral, sede_academ, un med laboral, nombrear, ?
ls()

# ===
# UP merge:
colnames(meds_2024)
colnames(meds_2025)

# TO DO: continue here, check if subset needs to be by contract, and if this is the variable:
summary(meds_2024$DescripcionTC)
summary(meds_2024$DESCRIP_CLASCATEG)
# Total mataches for Residente, maybe add Becado ?



cols_keep <- c("MATRICULA",
               "CURP",
               "NSS",
               "SEXO",
               "DELEGACION",
               "CLASIF_UNIDAD",
               "ADSCRIPCION",
               "DEPENDENCIA",
               "DESCRIPCION_SERVICIO",
               "DESCRIP_LOCALIDAD",
               "CATEGORIA",
               "DESCRIP_CLASCATEG",
               "EDAD",
               "NOMBREAR",
               "MCABAJA",
               "DescripcionTC"
               )

epi_head_and_tail(meds_2024[, cols_keep], cols = 15)
epi_head_and_tail(meds_2025[, cols_keep], cols = 15)

meds_2024 <- (meds_2024[, cols_keep])
meds_2025 <- (meds_2025[, cols_keep])

meds_2024[anyDuplicated(meds_2024$CURP), ] # gives the index of the first duplicate it finds, NA here
dups <- duplicated(meds_2024$CURP) & !is.na(meds_2024$CURP)
epi_head_and_tail(meds_2024[dups, ])

# which CURPs appear more than once excluding NAs:
dups <- unique(meds_2024$CURP[duplicated(meds_2024$CURP) & !is.na(meds_2024$CURP)])
meds_2024[meds_2024$CURP %in% dups, ]
dups <- epi_clean_get_dups(meds_2024, var = "CURP")
dups

# No dups but DT complains

# Memory intensive with full dataframes, move to data.table
# convert, DT will try to match NAs as well though:
dt_meds_2024 <- as.data.table(meds_2024)
dt_meds_2025 <- as.data.table(meds_2025)

dt_meds_2024 <- dt_meds_2024[!is.na(CURP)]
dt_meds_2025 <- dt_meds_2025[!is.na(CURP)]

# set the key to CURP on both tables:
setkey(dt_meds_2024, CURP)
setkey(dt_meds_2025, CURP)

# inner-join:
up_merged <- dt_meds_2024[dt_meds_2025, nomatch = 0]   # inner join, no NAs, no cartesian
# View(epi_head_and_tail(up_merged, cols = 29))
# i. cols are 2025 (y df), as resids to meds 2024 to 2025 should have differences
epi_head_and_tail(up_merged)
epi_head_and_tail(up_merged, last_cols = T)


dim(up_merged)
nrow(meds_2024) - length(which(is.na(meds_2024$CURP)))
nrow(meds_2025) - length(which(is.na(meds_2025$CURP)))
(nrow(up_merged) / nrow(meds_2025)) * 100
# ~Sept 2024 to April 2025 10,000 increase, 88% present in both, sounds low
# ===

# ===
# Keep residentes medicos only:
summary(up_merged$DESCRIP_CLASCATEG)
summary(meds_2024$DESCRIP_CLASCATEG) # will contain plazas vacantes

meds_resids <- which(up_merged$DESCRIP_CLASCATEG == "1.MÉDICOS" | up_merged$DESCRIP_CLASCATEG == "9.RESIDENTES")
meds_resids <- up_merged[meds_resids, ]
dim(meds_resids)
# View(epi_head_and_tail(meds_resids, cols = 29))
epi_head_and_tail(meds_resids, cols = 29)

# Get categ mismatches to see which resids are now adscritos:
nuevos_adscritos <- meds_resids %>%
    filter(# keep rows where either
        (DESCRIP_CLASCATEG != i.DESCRIP_CLASCATEG) |   # unequal non‐NAs
            (is.na(DESCRIP_CLASCATEG) != is.na(i.DESCRIP_CLASCATEG))     # or one is NA, the other isn’t
    )
epi_head_and_tail(nuevos_adscritos, cols = 29)
summary(meds_resids$DESCRIP_CLASCATEG)
summary(meds_resids$i.DESCRIP_CLASCATEG)


# This is DELEGACION 2024 when residentes:
colnames(nuevos_adscritos)[which(colnames(nuevos_adscritos) == "DELEGACION")] <- "DELEGACION_2024_residencia"
summary(nuevos_adscritos$DELEGACION_2024_residencia)

# DELEGACION 2025 adscritos:
colnames(nuevos_adscritos)[which(colnames(nuevos_adscritos) == "i.DELEGACION")] <- "DELEGACION_2025_adcsrito"
summary(nuevos_adscritos$DELEGACION_2025_adcsrito)

#

# TO DO: continue here
# only ~3400, should be ~7500? Draft 2025 hired ~1000 externos?


# ===
# ////////////



# ////////////
# ===
# CES merge:
epi_head_and_tail(activos_2024, cols = 24)
epi_head_and_tail(activos_2025, cols = 15)

# Cols check
# in both:
common_cols <- intersect(names(activos_2024), names(activos_2025))
# only in activos_2024:
only_activos_2024 <- setdiff(names(activos_2024), names(activos_2025))
# in activos_2025:
only_activos_2025 <- setdiff(names(activos_2025), names(activos_2024))
# summary:
all_cols <- sort(union(names(activos_2024), names(activos_2025)))
col_summary <- data.frame(
    column = all_cols,
    in_activos_2024 = all_cols %in% names(activos_2024),
    in_activos_2025 = all_cols %in% names(activos_2025),
    row.names = NULL
    )
col_summary
# ===


# ===
# Compare UP vs CES, 2024:
resids_2024_up <- meds_2024[meds_2024$DESCRIP_CLASCATEG == "9.RESIDENTES", ]
ces_up_2024 <- merge(activos_2024, resids_2024_up, by = "CURP")
nrow(activos_2024)
nrow(resids_2024_up)
nrow(ces_up_2024)
# About 1000 difference, more in CES, might be Bienestar or CATEG == BECADOS in UP
summary(meds_2024$DESCRIP_CLASCATEG)

# Check mismatches:
df1 <- resids_2024_up
df2 <- activos_2024
df2$MATRICULA <- as.character(df2$MATRICULA)

only_in_df1 <- anti_join(df1, df2, by = "CURP")
only_in_df2 <- anti_join(df2, df1, by = "CURP")
only_in_df1 # 269 in UP 2024 not in CES 2024
only_in_df2 # 1,162 in CES 2024 not in UP 2024
# View(only_in_df1)
# View(only_in_df2)
# Most are BECADO but not all


# TO DO: continue here
# Check BECADOS IN UP, categ doesn't appear though
# Add Bienestar
#
# ===
# ////////////




# ////////////
#
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
