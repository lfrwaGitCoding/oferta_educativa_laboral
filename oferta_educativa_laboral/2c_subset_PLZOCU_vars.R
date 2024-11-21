
####
# Columns which have values when PLZOCUP == 0
summary(data_f$PLZOCU)
epi_head_and_tail(data_f[which(data_f$PLZOCU == 0), ], cols = ncol(data_f))
colnames(data_f)
cols_to_remove <- c(
    'MATRICULA',
    'Nombre',
    'RFC',
    'CURP',
    'NSS',
    'SEXO',
    'FECHAING',
    # 'FECHAOCUP',
    'FECHAPROBJUB',
    'FECHANOMINACION',
    'FECHAPRIMERCONFZA',
    'EDAD',
    'ANT_DIAS',
    'ANTDD',
    'TITULAR',
    'NOMBRE_TITULAR',
    'COMSIN',
    'JUBILA',
    # 'DATOADIC_CATBASE',
    'ULTIMACATEGBASE',
    'FALTASACUMULADAS',
    'MY'
    # 'ESCOLARIDAD'
)

cols_to_keep <- colnames(data_f)[!colnames(data_f) %in% cols_to_remove]
cols_to_keep

epi_head_and_tail(data_f[which(data_f$PLZOCU == 0), cols_to_remove],
                  cols = length(cols_to_remove)
                  )
summary(data_f[which(data_f$PLZOCU == 0), cols_to_remove])

epi_head_and_tail(data_f[which(data_f$PLZOCU == 0), cols_to_keep],
                  cols = length(cols_to_keep)
                  )
summary(data_f[which(data_f$PLZOCU == 0), cols_to_keep])

summary(as.factor(data_f$NOMBREAR))
data_f$NOMBREAR <- as.factor(data_f$NOMBREAR)
summary(data_f$NOMBREAR)

epi_head_and_tail(data_f[, cols_to_remove])

data_f_PLZOCU <- data_f[, cols_to_keep]
table(data_f$PLZOCU, data_f$DELEGACION)
table(data_f_PLZOCU$PLZOCU, data_f_PLZOCU$DELEGACION)
####


####
summary(data_f_PLZOCU)
####
