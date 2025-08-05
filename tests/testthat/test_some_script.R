library(testthat)
library(withr)

# Basic smoke test for scripts/some_script.R

#script <- file.path("..", "..", "scripts", "some_script.R")
script_loc <- testthat::test_path("..", "..", "scripts", "some_script.R")

test_that("some_script.R executes and creates output", {
  in_csv <- local_tempfile(fileext = ".csv")
  out_csv <- local_tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:2, b = 2:3), in_csv, row.names = FALSE)
  exit_code <- system2("Rscript", c(script_loc, in_csv, out_csv))
  expect_equal(exit_code, 0)
  expect_true(file.exists(out_csv))
})

test_that("output has expected columns", {
  in_csv <- local_tempfile(fileext = ".csv")
  out_csv <- local_tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:2, b = 2:3), in_csv, row.names = FALSE)
  system2("Rscript", c(script_loc, in_csv, out_csv))
  out <- read.csv(out_csv)
  expect_equal(names(out), c("a", "b", "new_var"))
  expect_type(out$new_var, "double")
})

test_that("new_var column contains ratio of first two columns", {
  in_csv <- local_tempfile(fileext = ".csv")
  out_csv <- local_tempfile(fileext = ".csv")
  df <- data.frame(a = c(2, 4), b = c(1, 2))
  write.csv(df, in_csv, row.names = FALSE)
  system2("Rscript", c(script_loc, in_csv, out_csv))
  out <- read.csv(out_csv)
  expect_equal(out$new_var, df$a / df$b)
})

#test_that("missing input triggers error", {
#  missing <- tempfile(fileext = ".csv")
#  out_csv <- tempfile(fileext = ".csv")
#  # Ensure input file does not exist
#  if (file.exists(missing)) file.remove(missing)
#  output <- system2(
#    "Rscript",
#    c(script_loc, missing, out_csv),
#    stderr = TRUE,
#    stdout = TRUE
#  )
  expect_true(exit_code != 0)
##  exit_code <- attr(output, "status")
#  # Accept the two specific error patterns your script produces:
#  expect_match(
#    paste(output, collapse = "\n"),
#    "cannot open|Usage: some_script\\.R <input_csv> <output_csv>"
#  )
#})

#test_that("missing arguments triggers usage error", {
#  output <- system2("Rscript", script_loc, stderr = TRUE, stdout = TRUE)
#  exit_code <- attr(output, "status")
#  expect_true(exit_code != 0)
#  expect_match(
#    paste(output, collapse = "\n"),
#    "Usage: some_script\\.R <input_csv> <output_csv>"
#  )
#})
