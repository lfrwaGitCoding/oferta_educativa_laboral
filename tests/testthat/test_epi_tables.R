library(testthat)

test_that("epi_table_to_latex handles missing file", {
  out <- epi_table_to_latex("tests/fixtures/sample_epi_table.txt")
  expect_s3_class(out, "knit_asis")
})

test_that("epi_table_to_latex reads a valid file", {
  out <- epi_table_to_latex("tests/fixtures/sample_epi_table.txt")
  expect_s3_class(out, "knit_asis")
})
