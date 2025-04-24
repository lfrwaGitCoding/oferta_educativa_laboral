# ===
#### Check other vars vs PLZOCUP ----
summary(data_f$SEXO)
summary(data_f$NOMBREAR)
summary(data_f$DELEGACION)
summary(data_f$DESCRIP_LOCALIDAD)
summary(data_f$DEPENDENCIA)

dep_var <- "PLZOCU"
ind_vars <- c("DELEGACION", "NOMBREAR")

df_result <- epi_stats_table(
    df = data_f,
    dep_var = dep_var,
    ind_vars = ind_vars
)

epi_head_and_tail(df_result, cols = ncol(df_result))
# ===

# ===
data_f[, c("DELEGACION", "DEPENDENCIA")]
df1 <- data_f[data_f$DELEGACION == "Aguascalientes", c("DELEGACION", "DEPENDENCIA", "IP")]
df1 <- unique(df1)
df1 <- data_f[, c("DELEGACION", "DEPENDENCIA", "IP")]
df1 <- unique(df1)

length(unique(data_f$DEPENDENCIA))
View(as.data.frame(unique(data_f$DEPENDENCIA)))
View(df1)

head(table(data_f[data_f$DELEGACION == "Aguascalientes", c("DELEGACION", "DEPENDENCIA")], data_f$DEPENDENCIA))

check_dups <- epi_clean_get_dups(df = df1, var = "DEPENDENCIA")
summary(check_dups$DELEGACION)

val_id <- "CENTRO DE CAPACITACION Y CALIDAD"
col_id <- "DEPENDENCIA"
comp <- epi_clean_compare_dup_rows(check_dups, val_id = val_id, col_id = col_id, 1, 2)
comp

summary(check_dups[comp$duplicate_indices, ])
as.data.frame(check_dups[comp$duplicate_indices, ])

View(t(check_dups[comp$duplicate_indices, ]))
View(t(check_dups[comp$duplicate_indices, comp$differing_cols]))
# ===

# ===
length(unique(data_f$IP))
df1 <- data_f[, c("DELEGACION", "DEPENDENCIA", "IP")]
# df1 <- data_f
check_dups <- epi_clean_get_dups(df = df1, var = "IP")
summary(check_dups$DELEGACION)
summary(check_dups$DEPENDENCIA)
summary(check_dups$IP)
length(unique(check_dups$IP))
length(unique(check_dups$DELEGACION))
length(unique(check_dups$DEPENDENCIA))


val_id <- "36A101A02153"
col_id <- "IP"
comp <- epi_clean_compare_dup_rows(check_dups, val_id = val_id, col_id = col_id, 1, 2)
comp

summary(check_dups[comp$duplicate_indices, ])
as.data.frame(check_dups[comp$duplicate_indices, ])

View(t(check_dups[comp$duplicate_indices, ]))
View(t(check_dups[comp$duplicate_indices, comp$differing_cols]))
# Looks like differing IPs are due to different plazas, so not a problem. Needs subsetting by "DELEGACION", "DEPENDENCIA", etc. though.
# ===

