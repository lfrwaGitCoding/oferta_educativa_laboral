# Can delete, funcs now in episcout
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
  df_f_tab_wide$porc_vacante <- round((df_f_tab_wide$vacante / df_f_tab_wide$total) * 100, 2)
  df_f_tab_wide$porc_ocupada <- round((df_f_tab_wide$ocupada / df_f_tab_wide$total) * 100, 2)

  # Sort by percentage of vacante
  df_f_tab_wide <- df_f_tab_wide[order(df_f_tab_wide$porc_vacante, decreasing = TRUE), ]

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


#############
# # Descriptive stats for dates
# # Range, min, max,etc
# # Example data (already in testthat functions)
# # test_dates <- as.Date(c("2020-01-01", "2020-05-15", "2020-12-31", "2021-01-01", "2021-07-15"))
#
# # Set seed for reproducibility
# set.seed(42)
#
# # Define the start and end dates
# start_date <- as.Date("2020-01-01")
# end_date <- as.Date("2023-12-31")
#
# # Calculate the difference in days between start and end dates
# days_between <- as.integer(end_date - start_date)
# days_between
#
# # Generate 300 random numbers within the range of days
# random_days <- sample(0:days_between, 300, replace = TRUE)
#
# # Add these random days to the start date to get random dates
# random_dates <- start_date + random_days
# random_dates
#
# # Sort the dates
# sorted_random_dates <- sort(random_dates)
# sorted_random_dates
#
# # Convert to character for display or further processing
# sorted_random_dates_str <- as.character(sorted_random_dates)
# sorted_random_dates_str
#
# # Print the first 10 sorted random dates
# sorted_random_dates_str[1:10]
# str(sorted_random_dates_str)
# sorted_random_dates_str <- as.Date(sorted_random_dates_str)
# str(sorted_random_dates_str)
#
# test_dates <- sorted_random_dates_str
#############


#############
#' @title Calculate Descriptive Date Statistics
#'
#' @description epi_stats_dates() calculates and returns key descriptive statistics for a vector of dates, including minimum, maximum, median, interquartile range (IQR), and the quartiles. It is compatible with both `Date` and `IDate` objects.
#'
#' @param date_vector A vector of dates of class `Date` or `IDate`.
#' @return A `data.frame` containing the following statistics:
#'   \itemize{
#'     \item Min: The earliest date in the vector.
#'     \item Max: The latest date in the vector.
#'     \item Median: The median date.
#'     \item IQR: The interquartile range of the dates.
#'     \item 1st Quartile: The first quartile (25th percentile).
#'     \item Median (again): The median (50th percentile), duplicated for clarity.
#'     \item 3rd Quartile: The third quartile (75th percentile).
#'   }
#'
##' @examples
##' sample_dates <- as.Date(c("2020-01-01", "2020-05-15", "2020-12-31", "2021-01-01", "2021-07-15"))
##' date_stats <- epi_stats_dates(sample_dates)
##' print(date_stats)
#'
#'
#'
#' @note Note that origin date by default in R is "1970-01-01".
#'
#' @author Antonio J Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
# TO DO:
#' @seealso \code{\link{epi_stats_summary}},
#' \code{\link{epi_stats_format}},
#' \code{\link{epi_stats_numeric}}.
#'
#' TO DO:
##' @example vignettes/summary_funcs_examples.R
#'
#' @export
#'
#' @importFrom stats IQR filter


epi_stats_dates <- function(date_vector) {
  # Ensure the input is a Date or IDate class:
  # date_vector <- data_f$FECHA_INGRESO
  if (!inherits(date_vector, "Date") && !inherits(date_vector, "IDate")) {
    stop("Input must be a vector of type Date or IDate.")
  }

  # Convert IDate to Date if necessary
  if (inherits(date_vector, "IDate")) {
    date_vector <- as.Date(date_vector)
  }

  # Calculate various statistics
  min_date <- min(date_vector, na.rm = TRUE)
  max_date <- max(date_vector, na.rm = TRUE)
  # median_date <- median(date_vector, na.rm = TRUE)
  iqr_value <- IQR(date_vector, na.rm = TRUE)
  # Get quantiles from dates:
  # Convert dates to numeric, origin date by default in R is "1970-01-01":
  quantiles_numeric <- quantile(as.numeric(date_vector), na.rm = TRUE)
  # Convert numeric quantiles back to dates
  quartiles_dates <- as.Date(quantiles_numeric,
                             origin = "1970-01-01",
                             probs = c(0, 0.25, 0.5, 0.75, 100),
                             na.rm = TRUE
  )
  # quartiles_dates[3]

  # Create a dataframe to return results
  sum_stats <- data.frame(
    Statistic = c("N", "N Missing", "N Unique", "Min", "25%", "Median", "75%", "Max",
                  "IQR", "Most Common", "Range (Days)"),
    Value = c(as.character(length(date_vector)),                     # Total observations
              as.character(sum(is.na(date_vector))),                 # Missing values
              as.character(n_distinct(date_vector)),              # Unique dates
              as.character(min_date),
              as.character(quartiles_dates[2]),
              as.character(quartiles_dates[3]),
              as.character(quartiles_dates[4]),
              as.character(max_date),
              as.character(iqr_value),
              names(which.max(table(date_vector))), # Most Common Date (Mode)
              max_date - min_date # Date Range in Days
    )
  )

  return(sum_stats)
}


#' Summarize multiple date columns (Wide Format)
#'
#' @param df A dataframe containing multiple date columns
#' @return A wide-format tibble summarizing date statistics
epi_stats_dates_multi <- function(df) {
  # Select only date columns
  date_cols <- df %>% select(where(lubridate::is.Date))

  # Apply `epi_stats_dates()` to each column and store results in a wide format
  summary_table <- lapply(names(date_cols), function(col) {
    stats <- epi_stats_dates(date_cols[[col]]) %>%
      pivot_wider(names_from = Statistic, values_from = Value) %>%
      mutate(Column = col) %>%
      relocate(Column)  # Move Column name to first position
  }) %>%
    bind_rows()

  return(summary_table)
}
#############

#############
# # TO DO: Move to a function
# # Loop:
# sum_dates_df <- data.frame('variable' = character(0),
#                            'Min' = numeric(0),
#                            'q25%' = numeric(0),
#                            'Median' = numeric(0),
#                            'q75%' = numeric(0),
#                            'Max' = numeric(0),
#                            'IQR' = numeric(0),
#                            stringsAsFactors = FALSE
# )
# # sum_dates_df
#
# for (i in col_dates) {
#     sum_dates <- epi_stats_dates(data_f[[i]])
#     # print(sum_dates)
#     stats_vector <- sum_dates$Value  # Extract the values
#
#     # Data frame row to append:
#     new_row <- data.frame(
#         Variable = i,
#         Min = stats_vector[1],
#         `q25%` = stats_vector[2],
#         Median = stats_vector[3],
#         `q75%` = stats_vector[4],
#         Max = stats_vector[5],
#         IQR = stats_vector[6]
#     )
#     sum_dates_df <- rbind(sum_dates_df,
#                           new_row)
# }
# sum_dates_df
# file_n <- 'COISS_02052024/desc_dates.txt'
# epi_write(sum_dates_df, file_n)
#############


############
# # Get frequency tables
# dates_list <- list()
# for (i in col_dates) {
#     # Calculate differences between consecutive dates to find gaps and clusters:
#     date_ord <- as.numeric(data_f[[i]])
#     inds <- order(date_ord,
#                   decreasing = FALSE,
#                   na.last = TRUE
#     )
#     date_ord <- date_ord[inds]
#     date_diffs <- diff(date_ord)
#     # date_diffs
#     # Convert numeric differences back to an interpretable form (e.g., days):
#     # date_diffs_days <- as.Date(date_diffs, origin = "1970-01-01") - as.Date("1970-01-01") # although diff() was already working on days, so same result
#     # date_diffs_days
#     # Frequency table by month-year:
#     date_frequencies <- table(format(data_f[[i]], "%Y-%m"))  # Counts by year and month
#     # date_frequencies
#     dates_list[[i]] <- list(
#         Date_Differences = date_diffs,
#         Frequencies = date_frequencies
#     )
# }
# names(dates_list)
# names(dates_list$FECHA_INGRESO)
#
#
# # Save:
# for (i in names(dates_list)) {
#     # print(i)
#     out_n <- sprintf('freq_%s_%s.txt', i, "Date_Differences")
#     # dates_list$FECHA_ACTUALIZACION$Date_Differences
#     file_out <- as.data.frame(dates_list[[i]][[1]])
#     epi_write(file_out, out_n)
#
#     out_n <- sprintf('freq_%s_%s.txt', i, "Frequencies")
#     # dates_list$FECHA_ACTUALIZACION$Frequencies
#     file_out <- as.data.frame(dates_list[[i]][[2]])
#     epi_write(file_out, out_n)
# }

############

#############
#

#############

#' Summarize Factor Variables
#'
#' @param df A dataframe containing factor variables
#' @return A tibble summarizing factor variables in wide format
epi_stats_factors <- function(df) {
  df %>%
    select(where(is.factor)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>%
    summarise(
      n_missing = sum(is.na(Value)),  # Missing values count
      complete_rate = mean(!is.na(Value)),  # Proportion of non-missing values
      ordered = unique(is.ordered(Value)),  # Is the factor ordered?
      n_unique = n_distinct(Value, na.rm = TRUE),  # Unique levels count
      top_counts = paste(names(sort(table(Value), decreasing = TRUE)[1:3]),
                         sort(table(Value), decreasing = TRUE)[1:3],
                         sep = " (", collapse = "), ") # Most common factor levels
    ) %>%
    ungroup()
}

#' Summarize Character Variables
#'
#' @param df A dataframe containing character variables
#' @return A tibble summarizing character variable statistics
epis_stats_chars <- function(df) {
  df %>%
    select(where(is.character)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>%
    summarise(
      n_missing = sum(is.na(Value)),  # Count of missing values
      complete_rate = mean(!is.na(Value)),  # Proportion of non-missing values
      min_length = if_else(n_missing < n(), min(nchar(Value), na.rm = TRUE), NA_integer_),  # Shortest character length
      max_length = if_else(n_missing < n(), max(nchar(Value), na.rm = TRUE), NA_integer_),   # Longest character length
      empty = sum(Value == "", na.rm = TRUE),  # Count of empty strings
      n_unique = n_distinct(Value, na.rm = TRUE),  # Count of unique values
      whitespace = sum(str_trim(Value) == "", na.rm = TRUE)  # Count of whitespace-only strings
    ) %>%
    ungroup()
}




#' @title Send a list of plots to a grid object
#'
#' @description A light wrapper for cowplot::plot_grid().
#' Send a list of plots to a grid for multi-plot figures.
#' Makes assumptions and hard-codes preferences. All options are passed to
#' plot_grid().
#'
#' @param plot_list List of plots to be arranged into the grid.
#' @param align cowplot vertical and/or horizontal alignment. Default is 'hv'.
#' @param axis Align left, right, top or bottom and order. Default is 'lrtb'.
#' @param labels Default is 'AUTO'.
#' @param label_size Default is 12.
#' @param ncol Number of columns in the plot grid.
#' @param nrow Number of rows in the plot grid.
#' @param ... Pass any other parameter from plot_grid()
#'
#' @return a cowplot grid object
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_plot_list}},
#' \code{\link{epi_plot_cow_save}},
#' \code{\link[cowplot]{plot_grid}},
#' \code{\link[cowplot]{save_plot}}.
#'
#' @note See example in \code{\link{epi_plot_cow_save}} and ggplot2 wrappers epi_plot_*().
#'
#' @export
#'

epi_plots_to_grid <- function(plot_list = NULL,
                              align = 'hv',
                              axis = 'lrtb',
                              label_size = 12, # for the panel "A", "B", etc. labels only, does not affect axis or other plot text
                              ncol = NULL,
                              nrow = NULL,
                              ...) {
  if (!requireNamespace('cowplot', quietly = TRUE)) {
    stop("Package cowplot needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Disable labels if there's only one plot
  labels <- if (length(plot_list) > 1) "AUTO" else NULL

  my_plot_grid <- cowplot::plot_grid(plotlist = plot_list,
                                     align = align,
                                     axis = axis,
                                     labels = labels,
                                     label_size = label_size,
                                     ncol = ncol,
                                     nrow = nrow,
                                     ...
  )
  return(my_plot_grid)
}


#' @title Save a grid of plots
#'
#' @description epi_plot_cow_save() A light wrapper to save plots to disk with
#' cowplot::save_plot()
#'
#' @param file_name Name of the file to save
#' @param plot_grid plot to save, more often expecting a grid.
#' @param base_height Height in inches of the plot. Default is 11.69 (A4 size)
#' @param base_width Width in inches of the plot. Default is 8.27 (A4 size)
#' @param ... Pass any other parameter from cowplot::save_plot()
#'
#' @return None, file saved to disk.
#'
#' @author Antonio Berlanga-Taylor <\url{https://github.com/AntonioJBT/episcout}>
#'
#' @seealso \code{\link{epi_plot_list}},
#' \code{\link{epi_plots_to_grid}},
#' \code{\link[cowplot]{plot_grid}},
#' \code{\link[cowplot]{save_plot}}.
#'
#' @note height and width are for A4 size.
#' See also ggplot2 wrappers epi_plot_*().
#'
#' @examples
#'
#' \dontrun{
#' set.seed(12345)
#' n <- 20
#' df <- data.frame(var_id = rep(1:(n / 2), each = 2),
#'                  var_to_rep = rep(c("Pre", "Post"), n / 2),
#'                  x = rnorm(n),
#'                  y = rbinom(n, 1, 0.50),
#'                  z = rpois(n, 2)
#' )
#' df
#' df[, 'var_id'] <- as.character(df[, 'var_id'])
#' vars_to_plot <- df %>%
#'   select_if(epi_clean_cond_numeric) %>%
#'   names()
#' my_plot_list <- epi_plot_list(vars_to_plot)
#' my_plot_list
#' # Generate plots:
#' for (i in names(my_plot_list)) {
#'   print(i)
#'   my_plot_list[[i]] <- ggplot2::ggplot(df, aes_string(y = i)) + geom_boxplot()
#' }
#' # Pass to a grid and save to file:
#' # length(my_plot_list)
#' my_plot_grid <- epi_plots_to_grid(my_plot_list[1:length(my_plot_list)])
#' epi_plot_cow_save(file_name = 'plots_1.pdf', plot_grid = my_plot_grid)
#'
#' }
#'
#' @export
#'

epi_plot_cow_save <- function(file_name = NULL,
                              plot_grid = NULL,
                              base_height = 11.69, # A4
                              base_width = 8.27, # A4
                              ...) {
  if (!requireNamespace('cowplot', quietly = TRUE)) {
    stop("Package cowplot needed for this function to work. Please install it.",
         call. = FALSE)
  }

  print(class(plot_grid))  # Debugging line

  # Check if plot_grid is a single ggplot plot, avoids adding "A" to figure:
  if (inherits(plot_grid, "ggplot")) {
    message("Saving a single ggplot object.")
    cowplot::save_plot(filename = file_name,
                       plot = plot_grid,
                       base_height = base_height,
                       base_width = base_width,
                       ...)
  } else {
    message("Saving a multi-plot grid.")
    cowplot::save_plot(filename = file_name,
                       plot = cowplot::plot_grid(plot_grid),  # Force grid rendering
                       base_height = base_height,
                       base_width = base_width,
                       ...)
  }
}


epi_plot_theme_2 <- function(base_size = 13,
                             base_family = 'Times',
                             font_size_x = NULL,
                             font_size_y = NULL
) {
  # Use this instead or library or require inside functions:
  if (!requireNamespace('scales', quietly = TRUE)) {
    stop('Package scales needed for this function to work. Please install it.',
         call. = FALSE)
  }
  if (!requireNamespace('grid', quietly = TRUE)) {
    stop('Package grid needed for this function to work. Please install it.',
         call. = FALSE)
  }
  if (!requireNamespace('ggplot2', quietly = TRUE)) {
    stop('Package ggplot2 needed for this function to work. Please install it.',
         call. = FALSE)
  }
  if (!requireNamespace('ggthemes', quietly = TRUE)) {
    stop('Package ggthemes needed for this function to work. Please install it.',
         call. = FALSE)
  }
  # The following is modified from:
  # https://rpubs.com/Koundy/71792
  (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                        size = ggplot2::rel(1.2),
                                                        hjust = 0.5),
                     text = ggplot2::element_text(),
                     panel.background = ggplot2::element_rect(colour = NA),
                     plot.background = ggplot2::element_rect(colour = NA),
                     panel.border = ggplot2::element_rect(colour = NA),
                     axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
                     axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
                     axis.title.x = ggplot2::element_text(vjust = -0.2),
                     axis.text = ggplot2::element_text(),
                     axis.line = ggplot2::element_line(colour = "black"),
                     axis.ticks = ggplot2::element_line(),
                     axis.text.x = ggplot2::element_text(size = font_size_x),
                     axis.text.y = ggplot2::element_text(size = font_size_y),
                     # panel.grid.major = element_line(colour = "#f0f0f0"),
                     # this is like grey90 roughly
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     # legend.key = element_rect(fill = "white", colour = " light grey"),
                     legend.key = ggplot2::element_rect(colour = NA),
                     # legend.position = "bottom",
                     # legend.direction = "horizontal",
                     legend.key.size = ggplot2::unit(0.5, "cm"),
                     # legend.key.width = unit(0.2, "cm"),
                     # legend.margin = margin(0, 0, 0, 0),
                     # legend.title = element_text(face = "italic"),
                     # plot.margin = unit(c(10, 5, 5, 5),"mm"),
                     # plot.margin = unit(c(3, 5, 2, 2), "mm"), #t, r, b, l
                     strip.background = ggplot2::element_rect(colour = "#f0f0f0",
                                                              fill = "#f0f0f0"),
                     strip.text = ggplot2::element_text(face = "bold")
      )
  )
}

epi_stats_corr <- function(df = NULL,
                           method = 'spearman'
) {
  if (!requireNamespace('Hmisc', quietly = TRUE)) {
    stop('Package Hmisc needed for this function to work. Please install it.',
         call. = FALSE)
  }
  if (!requireNamespace('dplyr', quietly = TRUE)) {
    stop('Package dplyr needed for this function to work. Please install it.',
         call. = FALSE)
  }
  if (!requireNamespace('tidyr', quietly = TRUE)) {
    stop('Package tidyr needed for this function to work. Please install it.',
         call. = FALSE)
  }
  cormat <- Hmisc::rcorr(as.matrix(df), type = method)

  # Correlation values:
  cormat_melted_r <- as.data.frame(cormat$r) %>%
    tibble::rownames_to_column("Var1") %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")

  # P-values separately:
  cormat_melted_pval <- as.data.frame(cormat$P) %>%
    tibble::rownames_to_column("Var1") %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "pvalue")

  # Sanity: identical(rownames(cormat_melted_r),
  # rownames(cormat_melted_pval))
  cormat_all <- list(cormat = cormat,
                     cormat_melted_r = cormat_melted_r,
                     cormat_melted_pval = cormat_melted_pval
  )
  return(cormat_all)
}

epi_stats_numeric <- function(num_vec = NULL,
                              na.rm = TRUE,
                              coef = 1.5,
                              ...
) {
  if (!requireNamespace('e1071', quietly = TRUE)) {
    stop("Package e1071 needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Remove NAs first
  x <- num_vec[!is.na(num_vec)]

  # Default to NA for Shapiro
  normality <- NA_real_

  # Only try Shapiro if there are enough values AND they're not all identical
  if (length(x) > 3 && length(x) < 5000 && length(unique(x)) > 1) {
    normality <- tryCatch({
      shapiro.test(x)$p.value
    }, error = function(e) {
      NA_real_
    })
  }

  desc_stats <- data.frame(
    'min' = min(num_vec, na.rm = na.rm),
    'quantile_25' = quantile(num_vec, probs = 0.25, names = FALSE, na.rm = na.rm),
    'mean' = mean(num_vec, na.rm = na.rm),
    'median' = median(num_vec, na.rm = na.rm),
    'quantile_75' = quantile(num_vec, probs = 0.75, names = FALSE, na.rm = na.rm),
    'max' = max(num_vec, na.rm = na.rm),
    'SD' = sd(num_vec, na.rm = na.rm),
    'variance' = var(num_vec, na.rm = na.rm),
    'sem' = sd(num_vec, na.rm = na.rm) / sqrt(length(na.omit(num_vec))),
    'skewness' = e1071::skewness(num_vec, na.rm = na.rm, ...),
    'kurtosis' = e1071::kurtosis(num_vec, na.rm = na.rm, ...),
    'Shapiro_Wilk_p_value' = normality,
    'outlier_count' = epi_stats_count_outliers(num_vec, coef = coef),
    'NA_count' = sum(is.na(num_vec)),
    'NA_percentage' = (sum(is.na(num_vec)) / length(num_vec)) * 100
  )

  return(desc_stats)
}


epi_stats_summary <- function(df = NULL,
                              codes = NULL,
                              class_type = 'chr_fct', # 'int_num'
                              action = 'exclude' # 'codes_only'
) {
  if (!requireNamespace('dplyr', quietly = TRUE)) {
    stop("Package dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace('purrr', quietly = TRUE)) {
    stop("Package purrr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace('tibble', quietly = TRUE)) {
    stop("Package tibble needed for this function to work. Please install it.",
         call. = FALSE)
  }
  df <- tibble::as_tibble(df)
  # Determine which group of columns to use:
  if (class_type == 'chr_fct') {
    cond <- expression(epi_clean_cond_chr_fct(.))
  } else if (class_type == 'int_num') {
    cond <- expression(epi_clean_cond_numeric(.))
  } else {
    stop('class_type parameter not specified correctly?')
  }
  # Determine what to do with the codes provided (count only codes or
  # exclude codes from counting):
  if (action == 'codes_only') {
    map_func <- expression(purrr::keep(., .p = (. %in% codes)))
  } else if (action == 'exclude') {
    map_func <- expression(purrr::discard(., .p = (. %in% codes)))
  } else {
    stop('action parameter not specified correctly?')
  }
  # Determine if to count or sum depending on class cond and action asked for
  # codes are expected to be summarised as factors (so count()) as they are
  # assumed to represent database codes for NA explanations
  # chr and factor columns would be counted regardless of codes only or codes excluded
  # so summary() should only be needed for num/int columns where codes are excluded
  if (class_type == "int_num" & action == "exclude") {
    sum_func <- function(.x) epi_stats_numeric(.x)
  } else {
    # count is designed for data frames, not vectors, so pass as:
    sum_func <- function(.x) dplyr::count(data.frame(x = .x), x)
  }

  df <- df %>%
    dplyr::select_if(~eval(cond)) %>%
    purrr::map(~eval(map_func)) %>%
    purrr::map(sum_func) # Returns a list

  # Convert to dataframe with the same names for the var of interest:
  df <- as.data.frame(purrr::map_df(df,
                                    tibble::rownames_to_column,
                                    'var',
                                    .id = 'id')
  )
  # Returns a list if sum_func is summary()
  df <- tibble::as_tibble(as.data.frame(df))
  # Drop 'var' col as not needed:
  df$var <- NULL
  # Make the rownames a column and order columns:
  # df$id <- rownames(df)
  # df <- df %>%
  #   select(id,
  #          everything()
  #   )
  return(df)
}
