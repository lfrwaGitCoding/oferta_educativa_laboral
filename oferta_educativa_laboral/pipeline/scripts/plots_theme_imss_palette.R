# TO DO: continue here


library(scales)
library(grid)
library(ggplot2)
library(ggthemes)

# IMSS colours:
custom_palette <- c(
  "#911034",      # Original Red
  "#b12f45",      # Slightly brighter red
  "#c19a53",      # Original Gold
  "#e1b86e",      # Lighter gold/beige
  "#2a5c4b",      # Original Green
  "#3b755f",      # Brighter green
  "#DACBA1",      # Original Beige
  "#bba483",      # Darker beige
  "#602218",      # Original Brown
  "#7b3a2a"       # Rich brown
)

epi_plot_theme_imss <- function(base_size = 13,
                                base_family = 'Times'
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

# #' @rdname epi_plot_theme_imss
