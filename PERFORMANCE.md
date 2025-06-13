# Performance Notes

Several R scripts in `scripts/descriptive` perform iterative conversions on data frames.
For example, `2_clean_dups_col_types.R` loops over each date column and converts
values individually:

```r
for (i in date_cols) {
    df_dates[[i]] <- as.Date(df_dates[[i]], format = "%m/%d/%y")
}
```

For large datasets this repeated subsetting can be slow. A vectorised approach
with `data.table` is faster:

```r
df_dates[, (date_cols) := lapply(.SD, as.Date, format = "%m/%d/%y"),
         .SDcols = date_cols]
```

Whenever possible avoid repeated disk reads. Cache intermediate objects with
`saveRDS()` and load them with `readRDS()` instead of recreating them.
