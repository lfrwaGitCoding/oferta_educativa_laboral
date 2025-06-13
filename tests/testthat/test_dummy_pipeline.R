library(testthat)
library(withr)

test_that("some_script.R runs without error and creates output", {
  # 1) small dummy input:
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  in_csv  <- local_tempfile(fileext = ".csv")
  out_csv <- local_tempfile(fileext = ".csv")
  write.csv(df, in_csv, row.names = FALSE)

  # 2) Call Rscript, adjust path:
  exit_code <- system2("Rscript", args = c("scripts/some_script.R", in_csv, out_csv))
  expect_equal(exit_code, 0)

  # 3) Check output file:
  expect_true(file.exists(out_csv))
})

test_that("output schema matches expectations", {
  # Reuse dummy run:
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  in_csv  <- local_tempfile(fileext = ".csv")
  out_csv <- local_tempfile(fileext = ".csv")
  write.csv(df, in_csv, row.names = FALSE)
  system2("Rscript", args = c("scripts/some_script.R", in_csv, out_csv))

  # 4) Load output, check columns and types:
  out <- read.csv(out_csv)
  expect_true(all(c("a", "b", "new_var") %in% names(out)))          # eg add new_var
  expect_type(out$a, "integer")
  expect_type(out$new_var, "double")                                 # e.g. some computed ratio
})

test_that("missing input throws error", {
  bad_in <- tempfile("nope", fileext = ".csv")
  bad_out <- tempfile(fileext = ".csv")
  expect_error(
    system2("Rscript", args = c("scripts/some_script.R", bad_in, bad_out)),
    "cannot open.*nope.*"
  )
})