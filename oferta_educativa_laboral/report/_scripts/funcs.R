# Setup libraries for qmd, location not needed if running as a pipeline.


library(magrittr)  # pipes
library(knitr)
library(kableExtra)  # additional table formatting
library(dplyr)
library(data.table)
library(episcout)


# Load a table from a file (.txt format with tab-separated values).
# Apply formatting using kable() and epi_table_kable_format().
# Return a LaTeX-formatted table that Quarto can handle.
epi_table_to_latex <- function(file_path,
                               caption = "",
                               sep = "\t",
                               header = TRUE,
                               format = "latex",
                               digits = 2,
                               booktabs = TRUE,
                               longtable = FALSE, # epi_table_kable_format var
                               font_size = NULL, # epi_table_kable_format var
                               full_width = FALSE, # epi_table_kable_format var
                               position = 'center', # epi_table_kable_format var
                               align = "c", # Allows column alignment customization
                               repeat_header_text = "\\textit{(continuada)}"
) {

    # Check if file exists before reading
    if (!file.exists(file_path)) {
        warning(paste("File", file_path, "not found."))
        return(knitr::asis_output("\\textbf{Table not found}"))
    }

    # Read table from file:
    table_data <- tryCatch({
        read.table(file_path, header = header, sep = sep)
    }, error = function(e) {
        error(paste("Error reading table:", file_path))
        return(NULL)
    })

    # Handle case where table is empty
    if (is.null(table_data) || nrow(table_data) == 0) {
        error("Table is empty.")
        return(knitr::asis_output("\\textbf{Table is empty}"))
    }

    # Create formatted LaTeX table:
    table_to_latex <- knitr::kable(
        table_data,
        caption = caption,
        booktabs = booktabs,
        longtable = longtable,
        format = format,
        digits = digits,
        align = align # column alignment
    ) %>%
        epi_table_kable_format(longtable = longtable,
                               font_size = font_size,
                               full_width = full_width,
                               position = position,
                               repeat_header_text = repeat_header_text
        )

    # Output LaTeX for Quarto PDF conversion:
    knitr::asis_output(table_to_latex)
}


# Function to apply consistent styling to tables
epi_table_kable_format <- function(kable_input,
                                   longtable = FALSE,
                                   font_size = NULL,
                                   full_width = FALSE,
                                   position = 'center',
                                   repeat_header_text = repeat_header_text
) {
    if (longtable) {
        kable_input <- kable_input %>%
            kable_styling(
                latex_options = c("repeat_header", "striped"), #, "hold_position"),
                full_width = full_width,
                position = position,
                repeat_header_text = repeat_header_text,
                font_size = font_size
            ) %>%
            column_spec(1, bold = TRUE) %>%
            row_spec(0, bold = TRUE, background = "#D3D3D3")
    } else {
        kable_input <- kable_input %>%
            kable_styling(
                latex_options = c("striped", "scale_down"), #, "hold_position"),  # Ensures proper placement & auto-resize
                full_width = full_width,
                position = position,
                repeat_header_text = repeat_header_text,
                font_size = font_size
            ) %>%
            column_spec(1, bold = TRUE) %>%
            row_spec(0, bold = TRUE, background = "#D3D3D3")
    }

    return(kable_input)
}


# Reads a long table and outputs a summary table with the top n_rows rows for a PDF output
epi_table_to_latex_sum2 <- function(file_path, min_total = 10, n_rows = 25, caption = "") {
    table_df <- episcout::epi_read(file_path)

    table_df_sum <- table_df %>%
        filter(total >= min_total) %>%
        arrange(desc(porc_vacante)) %>%
        head(n_rows)

    epi_table_to_latex(table_df_sum, caption)
}



epi_table_to_latex_sum <- function(file_path,
                                   min_total = 10,
                                   n_rows = 25,
                                   caption = "",
                                   format = "latex",
                                   digits = 2,
                                   booktabs = TRUE,
                                   align = "c",
                                   font_size = NULL,
                                   full_width = FALSE,
                                   position = 'center',
                                   repeat_header_text = repeat_header_text
                                        ) {

    table_df <- epi_read(file_path)

    table_df_sum <- table_df %>%
        filter(total >= min_total) %>%
        arrange(desc(porc_vacante)) %>%
        head(n_rows)

    # Create formatted LaTeX table:
    table_to_latex <- knitr::kable(
        table_df_sum,
        caption = caption,
        booktabs = booktabs,
        format = format,
        digits = digits,
        align = align # column alignment
    ) %>%
        kable_styling(
            latex_options = c("striped", "scale_down"), #, "hold_position"),  # Ensures proper placement & auto-resize
            full_width = full_width,
            position = position,
            repeat_header_text = repeat_header_text,
            font_size = font_size
        ) %>%
        column_spec(1, bold = TRUE) %>%
        row_spec(0, bold = TRUE, background = "#D3D3D3")

    return(table_to_latex)
    }

