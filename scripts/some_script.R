#!/usr/bin/env Rscript

#' Add ratio column from first two columns of a CSV file
#'
#' @param infile Path to input CSV.
#' @param outfile Path to write output CSV.
add_ratio <- function(infile, outfile) {
  df <- read.csv(infile)
  df$new_var <- df[[1]] / df[[2]]
  write.csv(df, outfile, row.names = FALSE)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2) {
    stop("Usage: some_script.R <input_csv> <output_csv>")
  }
  infile <- args[1]
  outfile <- args[2]
  if (!file.exists(infile)) {
    stop(paste("cannot open", infile))
  }
  add_ratio(infile, outfile)
}
