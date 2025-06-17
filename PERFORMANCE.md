# Rendimiento

Varios scripts en R ubicados en `scripts/descriptive` realizan conversiones iterativas sobre data frames.  
Por ejemplo, el script `2_clean_dups_col_types.R` recorre cada columna de fechas y convierte los valores individualmente:

```r
for (i in date_cols) {
    df_dates[[i]] <- as.Date(df_dates[[i]], format = "%m/%d/%y")
}
```

Si es necesario optiimizar estas secciones pueden ser las primeras en cambiar.
Código vectorizado con p.ej. `data.table` es mucho más eficiente:

```r
df_dates[, (date_cols) := lapply(.SD, as.Date, format = "%m/%d/%y"),
         .SDcols = date_cols]
```

Varios scripts ya generan datos intermedios en rdata. 
