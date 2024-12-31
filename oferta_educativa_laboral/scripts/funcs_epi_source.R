epi_create_dir <- function(base_path, subdir = NULL) {
   # Use the custom name if provided; otherwise, use the current date
  subdirectory_name <- if (!is.null(subdir)) subdir else format(Sys.Date(), "%d_%m_%Y")

  # Combine base path and subdirectory
  target_dir <- file.path(base_path, subdirectory_name)

  # Create the directory recursively if it doesn't exist
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
    message("Created directory: ", normalizePath(target_dir))
  } else {
    message("Directory already exists: ", normalizePath(target_dir))
  }

  # Print the contents of the directory
  contents <- dir(path = normalizePath(target_dir), all.files = TRUE)
  print(contents)

  # Return the full path of the target directory
  return(normalizePath(target_dir))
}


epi_write_df <- function(df, results_subdir, file_n, suffix) {
  # Construct the file path
  outfile <- sprintf(fmt = "%s/%s.%s",
                     results_subdir,
                     file_n,
                     suffix)

  # Write the data frame to the file
  epi_write(df, outfile)

  # Print the file path
  message("File saved to: ", outfile)

  # Return the full file path
  return(outfile)
}

epi_plot_bar <- function(df = NULL, var_x = NULL, var_y = "", fill = NULL,
                         bar_colour = "black", guides_fill = "none",
                         y_lab = "Count", x_lab = var_x,
                         custom_palette = NULL,  # Default to NULL
                         ...)
{
    # Load required packages
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package ggplot2 needed for this function to work. Please install it.",
             call. = FALSE)
    }

    # Check if `fill` is NULL and default to `var_x` for coloring
    if (is.null(fill)) fill <- var_x

    # Get the number of levels in the fill variable
    num_levels <- length(unique(df[[fill]]))

    # If a custom palette is provided, recycle colors if needed
    if (!is.null(custom_palette)) {
        custom_palette <- rep(custom_palette, length.out = num_levels)
    }

    # Handle the plot creation
    if (var_y == "") {
        # Count-based bar plot
        bar_plot_one <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(var_x),
                                                         fill = !!rlang::sym(fill))) +
            ggplot2::geom_bar(stat = "count", colour = bar_colour, ...) +
            ggplot2::guides(fill = guides_fill) +
            ggplot2::labs(y = y_lab, x = x_lab)

        # Apply custom color palette if provided
        if (!is.null(custom_palette)) {
            bar_plot_one <- bar_plot_one +
                ggplot2::scale_fill_manual(values = custom_palette)
        }

        return(bar_plot_one)
    } else {
        # Identity-based bar plot
        bar_plot <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(var_x),
                                                     y = !!rlang::sym(var_y),
                                                     fill = !!rlang::sym(fill))) +
            ggplot2::geom_bar(stat = "identity", position = "dodge", ...) +
            ggplot2::labs(y = y_lab, x = x_lab)

        # Apply custom color palette if provided
        if (!is.null(custom_palette)) {
            bar_plot <- bar_plot +
                ggplot2::scale_fill_manual(values = custom_palette)
        }

        return(bar_plot)
    }
}

epi_stats_table <- function(df, dep_var, ind_vars) {
  # Create the formula for xtabs dynamically
  formula_str <- sprintf("~ %s + %s", dep_var, ind_vars)
  formula_obj <- as.formula(formula_str)
  formula_obj

  # Create the ftable
  f_tab <- ftable(xtabs(formula_obj, data = df))

  # Convert to data frame
  df_f_tab <- as.data.frame(f_tab)

  # Reshape to wide format
  df_f_tab_wide <- tidyr::pivot_wider(
    df_f_tab,
    names_from = all_of(dep_var),
    values_from = c(Freq)
  )

  # Rename columns
  colnames(df_f_tab_wide)[colnames(df_f_tab_wide) == "0"] <- "vacante"
  colnames(df_f_tab_wide)[colnames(df_f_tab_wide) == "1"] <- "ocupada"

  # Add totals and percentages
  df_f_tab_wide$total <- df_f_tab_wide$vacante + df_f_tab_wide$ocupada
  df_f_tab_wide$perc_vacante <- round((df_f_tab_wide$vacante / df_f_tab_wide$total) * 100, 2)
  df_f_tab_wide$perc_ocupada <- round((df_f_tab_wide$ocupada / df_f_tab_wide$total) * 100, 2)

  # Sort by percentage of vacante
  df_f_tab_wide <- df_f_tab_wide[order(df_f_tab_wide$perc_vacante, decreasing = TRUE), ]

  # Return the final data frame for inspection
  return(df_f_tab_wide)
}


get_mode <- function(x) {
  x <- na.omit(x)  # Remove NAs
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}


#' @rdname combined_contingency_2x2_functions
#' @export
epi_stats_contingency_2x2_df <- function(df, x_var, y_var) {
  if (is.character(x_var)) x_var <- which(colnames(df) == x_var)
  if (is.character(y_var)) y_var <- which(colnames(df) == y_var)
  table_df <- as.data.frame(table(df[[x_var]], df[[y_var]]))
  colnames(table_df)[1] <- colnames(df)[x_var]
  colnames(table_df)[2] <- colnames(df)[y_var]
  return(table_df)
}

#' @rdname combined_contingency_2x2_functions
#' @export
epi_stats_contingency_2x2_tables <- function(df, x_var) {
  other_vars <- setdiff(colnames(df), x_var)
  results <- lapply(other_vars, function(y_var) {
    epi_stats_contingency_2x2_df(df, x_var = x_var, y_var = y_var)
  })
  names(results) <- other_vars
  return(results)
}

#' @rdname combined_contingency_2x2_functions
#' @export
rename_contingency_2x2_cols <- function(contingency_2x2_list, df, x_var) {
  for (i in seq_along(contingency_2x2_list)) {
    df_table <- contingency_2x2_list[[i]]
    dep_var <- names(contingency_2x2_list)[i]
    colnames(df_table)[2] <- dep_var
    contingency_2x2_list[[i]] <- df_table
  }
  return(contingency_2x2_list)
}

#' @rdname combined_contingency_2x2_functions
#' @export
epi_stats_contingency_2x2_test <- function(df, target_var, other_var, test_type = "fisher.test") {
  tab <- table(df[[target_var]], df[[other_var]])

  # Fallback to Chi-squared test for large tables or Monte Carlo for sparse ones
  if (min(dim(tab)) > 2 || any(tab > 100)) {
    test_type <- "chisq.test"
  }

  test <- tryCatch(
    {
      switch(
        test_type,
        "fisher.test" = fisher.test(tab, workspace = 2e7, simulate.p.value = TRUE, B = 1e6),
        "chisq.test" = chisq.test(tab),
        stop("Unsupported test type. Use 'fisher.test' or 'chisq.test'.")
      )
    },
    error = function(e) stop("Test failed: ", e$message)
  )

  result <- broom::tidy(test)
  result$variable <- other_var
  return(result)
}

#' @rdname combined_contingency_2x2_functions
#' @export
epi_stats_contingency_2x2_cols <- function(df, min_unique = 2) {
  unique_counts <- sapply(df, function(x) length(unique(x)))
  testable <- names(df)[unique_counts >= min_unique]
  return(testable)
}

#' @rdname combined_contingency_2x2_functions
#' @export
epi_stats_contingency_2x2_all <- function(df, target_var, test_type = "fisher.test") {
  testable_columns <- epi_stats_contingency_2x2_cols(df)
  testable_columns <- setdiff(testable_columns, target_var)
  results <- lapply(testable_columns, function(other_var) {
    epi_stats_contingency_2x2_test(df, target_var, other_var, test_type)
  })
  results_df <- dplyr::bind_rows(results)
  return(results_df)
}
