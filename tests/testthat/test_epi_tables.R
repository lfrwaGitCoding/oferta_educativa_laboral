library(testthat)
source("oferta_educativa_laboral/report/_scripts/funcs.R")

test_that("epi_table_to_latex handles missing file", {
  out <- epi_table_to_latex("tests/fixtures/nonexistent.txt")
  expect_s3_class(out, "knit_asis")
})
