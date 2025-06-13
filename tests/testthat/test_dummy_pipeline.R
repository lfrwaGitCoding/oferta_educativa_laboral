library(testthat)
library(withr)

test_that("my_script.R runs without error and creates output", {
  # 1) Prepare a tiny dummy input
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  in_csv  <- local_tempfile(fileext = ".csv")
  out_csv <- local_tempfile(fileext = ".csv")
  write.csv(df, in_csv, row.names = FALSE)

  # 2) Call your script via Rscript; adjust path as needed
  exit_code <- system2("Rscript", args = c("scripts/my_script.R", in_csv, out_csv))
  expect_equal(exit_code, 0)

  # 3) Check that output file was created
  expect_true(file.exists(out_csv))
})

test_that("output schema matches expectations", {
  # Reuse the same dummy run (or rerun if needed)
  df <- data.frame(a = 1:3, b = c(4, 5, 6))
  in_csv  <- local_tempfile(fileext = ".csv")
  out_csv <- local_tempfile(fileext = ".csv")
  write.csv(df, in_csv, row.names = FALSE)
  system2("Rscript", args = c("scripts/my_script.R", in_csv, out_csv))

  # 4) Load output and check columns & types
  out <- read.csv(out_csv)
  expect_true(all(c("a", "b", "new_var") %in% names(out)))          # suppose you add new_var
  expect_type(out$a, "integer")
  expect_type(out$new_var, "double")                                 # e.g. some computed ratio
})

test_that("missing input throws error", {
  bad_in <- tempfile("nope", fileext = ".csv")
  bad_out <- tempfile(fileext = ".csv")
  expect_error(
    system2("Rscript", args = c("scripts/my_script.R", bad_in, bad_out)),
    "cannot open.*nope.*"
  )
})
