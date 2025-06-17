


library(dplyr)
library(tidyr)

# Long df of three-way counts (geom_alluvium data):
epi_head_and_tail(df)

flow_counts <- df %>%
    count(
        EDO_NACIMIENTO,
        DELEGACION_2024_residencia,
        DELEGACION_2025_adcsrito,
        name = "n"
    )
head(flow_counts, n = 10)
epi_head_and_tail(flow_counts, cols = 4)
# View(flow_counts)

# save:
file_n <- 'table_flow_counts'
suffix <- 'txt'
outfile <- sprintf(fmt = '%s/%s.%s', results_subdir, file_n, suffix)
outfile
epi_write(flow_counts, outfile)


# TO DO: continue here
# Wide matrix edo nac a Residencia:
mat_birth_resid <- flow_counts %>%
    dplyr::select(EDO_NACIMIENTO, DELEGACION_2024_residencia, n) %>%
    tidyr::pivot_wider(
        names_from  = DELEGACION_2024_residencia,
        values_from = n,
        fill = 0
    ) %>%
    column_to_rownames("EDO_NACIMIENTO")

# Wide matrix Residencia a Trabajo
mat_resid_trab <- flow_counts %>%
    dplyr::select(DELEGACION_2024_residencia, DELEGACION_2025_adcsrito, n) %>%
    pivot_wider(
        names_from  = DELEGACION_2025_adcsrito,
        values_from = n,
        values_fill = list(n = 0)
    ) %>%
    column_to_rownames("DELEGACION_2024_residencia")

mat_birth_resid[1:5, 1:5]      # first 5×5 corner
mat_resid_trab[1:5, 1:5]


# full 3-D contingency array:
flow_array <- xtabs(
    n ~ EDO_NACIMIENTO + DELEGACION_2024_residencia + DELEGACION_2025_adcsrito,
    data = flow_counts
    )

flow_array["Aguascalientes", "Jalisco", "CDMX"] # how many went A→B→C
