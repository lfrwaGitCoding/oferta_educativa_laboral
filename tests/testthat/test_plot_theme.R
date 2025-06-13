library(testthat)

script_path <- file.path("..", "..", "oferta_educativa_laboral", "scripts", "descriptive", "plots_theme_imss_palette.R")
source(script_path)

test_that("epi_plot_theme_imss returns a ggplot object", {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    skip("ggplot2 not available")
  }
    if (!requireNamespace("ggthemes", quietly = TRUE)) {
    skip("ggthemes not available")
  }
  
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point() +
    epi_plot_theme_imss()
  expect_s3_class(p, "ggplot")
})