#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: some_script.R <input_csv> <output_csv>")
}

infile <- args[1]
outfile <- args[2]

if (!file.exists(infile)) {
  stop(paste("cannot open", infile))
}

df <- read.csv(infile)

df$new_var <- df[[1]] / df[[2]]

write.csv(df, outfile, row.names = FALSE)
