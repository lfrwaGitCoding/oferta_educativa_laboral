library(testthat)
library(withr)

script_loc <- testthat::test_path("..", "..", "scripts", "some_script.R")
source(script_loc)

test_that("add_ratio executes and creates output", {
  in_csv <- local_tempfile(fileext = ".csv")
  out_csv <- local_tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:2, b = 2:3), in_csv, row.names = FALSE)
  add_ratio(in_csv, out_csv)
  expect_true(file.exists(out_csv))
})

test_that("output has expected columns", {
  in_csv <- local_tempfile(fileext = ".csv")
  out_csv <- local_tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:2, b = 2:3), in_csv, row.names = FALSE)
  add_ratio(in_csv, out_csv)
  out <- read.csv(out_csv)
  expect_equal(names(out), c("a", "b", "new_var"))
  expect_type(out$new_var, "double")
})

test_that("new_var column contains ratio of first two columns", {
  in_csv <- local_tempfile(fileext = ".csv")
  out_csv <- local_tempfile(fileext = ".csv")
  df <- data.frame(a = c(2, 4), b = c(1, 2))
  write.csv(df, in_csv, row.names = FALSE)
  add_ratio(in_csv, out_csv)
  out <- read.csv(out_csv)
  expect_equal(out$new_var, df$a / df$b)
})
