


library(episcout)

setwd("~/Documents/work/science/devel/github/antoniojbt/oferta_educativa_laboral/oferta_educativa_laboral/scripts/specific_Qs")
dir()

# Data in:
# /Users/antoniob/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/data/external/datos_DIR

infile <- "sum_deleg_pda-2025-03-31.tab"
data_f <- epi_read(infile)
head(data_f)
epi_head_and_tail(data_f, cols = 2)
# View(data_f)


deleg_id <- "deleg_num_ID.tab"
deleg_id <- epi_read(deleg_id)
head(deleg_id)
epi_head_and_tail(deleg_id, cols = 2)
unique(deleg_id$ID_DELEG_RP)
deleg_id[unique(deleg_id$ID_DELEG_RP), ]

deleg_id <- deleg_id[!duplicated(deleg_id$ID_DELEG_RP), c("ID_DELEG_RP","descripcion delegación")]
deleg_id
colnames(deleg_id)[2] <- "delegacion"

# Join:
data_f_merge <- merge(data_f, deleg_id, by = "ID_DELEG_RP", all.x = TRUE)
epi_head_and_tail(data_f_merge, cols = 3)

# Results in:
# /Users/antoniob/Documents/work/comp_med_medicina_datos/projects/int_op/oferta_educativa_laboral/results/02_05_2025_meds_DH_DIR
data_f_merge$estado <- data_f_merge$delegacion # manually edited for Ver, CDMX, Edo Mex.
epi_write(data_f_merge, "sum_deleg_pda-2025-03-31_merged.tab", sep = "\t")
