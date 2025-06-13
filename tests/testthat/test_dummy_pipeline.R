library(testthat)

scripts_dir <- file.path("..", "..", "scripts")
script_path <- file.path(scripts_dir, "some_script.R")


test_that("some_script.R runs without error and creates output", {
  out_csv <- tempfile(fileext = ".csv")
  exit_code <- system2("Rscript", c(script_path, out_csv))
  expect_equal(exit_code, 0)
  expect_true(file.exists(out_csv))
  out <- utils::read.csv(out_csv)
  expect_equal(names(out), c("x", "y"))
})

test_that("missing output path throws error", {
  expect_error(system2("Rscript", script_path))
})
