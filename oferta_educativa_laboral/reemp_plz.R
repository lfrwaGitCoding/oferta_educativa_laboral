
# ////////////
# Import libraries ----
library(data.table)
library(episcout)
library(tidyverse)
# ////////////

cwd <- file.path(getwd())
cwd
dir(cwd)

# dir_n <- file.path(paste0(cwd, '/data/data_UP/access_SIAP_18092024/processed/reemplazo_plaza_UEI'))
dir_n <- file.path(paste0(cwd, '/results/reemp_plaza_dpm/'))
dir(dir_n)

# file_n <- file.path(paste0(dir_n, '/Qna_17_Plantilla_2024_UEI_all.csv'))
file_n <- file.path(paste0(dir_n, '/uei_all.csv'))
file_n

data_f <- epi_read(file_n)
epi_head_and_tail(data_f)

data_f <- data_f[which(data_f$PLZVAC == 1), ]
data_f$IMP_SDO <- as.numeric(data_f$IMP_SDO)
data_f <- data_f[order(data_f$IMP_SDO, decreasing = TRUE), ]
epi_head_and_tail(data_f)
summary(data_f$IMP_SDO)
length(which(data_f$IMP_SDO > 8000))

data_f <- data_f[which(data_f$DELEGACION == "Nivel Central"), ]
epi_head_and_tail(data_f)

data_f <- data_f[which(data_f$TipoMcaOcup == "Definitiva"), ]
epi_head_and_tail(data_f)

View(t(data_f[c(1:20), ]))







