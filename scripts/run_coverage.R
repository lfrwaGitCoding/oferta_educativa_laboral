#!/usr/bin/env Rscript

# Identify test files and source files

test_files <- list.files('tests/testthat', full.names = TRUE,
                         pattern = '^test.*\.R$')
src_files <- list.files(c('scripts', 'oferta_educativa_laboral/scripts'),
                        recursive = TRUE, full.names = TRUE,
                        pattern = '\\.R$')

# Run coverage and output in Cobertura format
cov <- covr::file_coverage(test_files, src_files)
covr::to_cobertura(cov)

