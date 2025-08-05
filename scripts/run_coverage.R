#!/usr/bin/env Rscript

# Identify test files and source files

test_files <- list.files(
  "tests/testthat",
  full.names = TRUE,
  recursive = TRUE,
  # Only include actual test files, exclude helper scripts like testthat.R
  pattern = '^test_.*\\.R$'
)

# Derive source file paths from test file names
rel_paths <- file.path(
  dirname(sub("^tests/testthat/?", "", test_files)),
  sub("^test_", "", basename(test_files))
)
src_candidates <- c(
  file.path("scripts", rel_paths),
  file.path("oferta_educativa_laboral/scripts", rel_paths)
)
src_files <- src_candidates[file.exists(src_candidates)]

# Run coverage and output in Cobertura format
cov <- covr::file_coverage(test_files, src_files)
covr::to_cobertura(cov)


 
