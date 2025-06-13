#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Must provide output CSV path", call. = FALSE)
}
out_path <- args[1]

data <- data.frame(x = 1:10, y = seq(1, 20, by = 2))
utils::write.csv(data, file = out_path, row.names = FALSE)
