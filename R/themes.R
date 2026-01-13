theme_violin <- function(
    p,
    colors = palette_discrete(),
    cex = 1,
    cex_main = 15 * cex,
    cex_sub = 13 * cex,
    cex_axis = 17 * cex,
    guide = FALSE,
    grid = FALSE,
    color_title = "black",
    hjust = 0.5,
    x_axis = FALSE,
    color_subtitle = "gray50",
    legend = FALSE,
    lwd = 1) {
    p <- p +
        theme_minimal() +
        theme(
            plot.title = element_text(
                hjust = hjust,
                size = cex_main,
                face = "bold",
                color = color_title
            ),
            plot.subtitle = element_text(
                hjust = hjust,
                size = cex_sub,
                color = "gray50"
            ),
            plot.caption = element_text(
                hjust = 1,
                size = cex * 13,
                color = color_subtitle
            ),
            axis.title.x = element_blank(),
            axis.text.y = element_text(colour = "black")
        ) +
        theme_custom(cex, cex_main, cex_sub, cex_axis, lwd)
    if (!x_axis) {
        p <- p + theme(axis.title.x = element_blank())
    }
    p <- p +
        theme(
            axis.text.x = element_text(
                hjust = hjust,
                vjust = 1,
                size = cex * 15,
                color = color_subtitle,
                angle = 45
            )
        )
    if (!isTRUE(grid)) {
        p <- p + theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )
    }
    if (!legend) {
        p <- p +
            guides(
                color = "none",
                fill = "none"
            )
    }
    return(p)
}

#' Custom ggplot2 theme
#'
#' A customizable ggplot2 theme with consistent typography and scalable elements
#' for creating publication-quality plots. All font sizes and line widths
#' can be adjusted proportionally using a single scaling factor (`cex`).
#'
#' @inheritParams plot_violin
#'
#' @return A complete ggplot2 theme object.
#'
#' \dontrun{
#' library(ggplot2)
#'
#' # Basic usage with default scaling
#' p <- ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point(aes(color = factor(cyl))) +
#'   labs(title = "Car Weight vs MPG",
#'        subtitle = "By Number of Cylinders",
#'        x = "Miles per Gallon",
#'        y = "Weight (1000 lbs)") +
#'   theme_custom()
#'
#' print(p)
#' }
#'
#' @export
theme_custom <- function(
    cex = 1,
    cex_main = 17 * cex,
    cex_sub = 15 * cex,
    cex_axis = 15 * cex,
    lwd = 1) {
    theme(
        axis.text = element_text(size = 13 * cex, color = "gray50"),
        axis.title = element_text(face = "bold.italic", size = cex_axis),
        strip.text = element_text(
            size = cex_main,
            face = "bold",
            hjust = 0.5,
            margin = margin(0.5, 0.5, 0.5, 0.5)
        ),
        plot.title = element_text(face = "bold", size = cex_main, hjust = 0.5),
        legend.title = element_text(face = "italic", size = cex_sub),
        legend.text = element_text(colour = "black", size = 10 * cex),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = lwd),
        axis.ticks = element_line(linewidth = .75 * lwd)
    )
}

theme_bar <- function(
    p,
    y = NULL,
    colors = c(
        brewer.pal(n = 9, name = "Set1")[-6],
        brewer.pal(n = 9, name = "Pastel2")
    ),
    cat = TRUE) {
    p <- p +
        theme_minimal() +
        theme_custom() +
        theme(legend.position = "none") +
        labs(x = "", y = "")
    if (cat) {
        p + scale_fill_manual(p, values = colors, na.value = "black")
    } else {
        p + scale_fill_gradientn(colors = colors, na.value = "black")
    }
}

#' Format numeric labels
#'
#' Formats numeric values into human-readable strings, with special handling for
#' zero and scientific notation. Designed for ggplot2 axis labels or table
#' formatting where scientific notation or trailing decimals might reduce
#' readability.
#'
#' @param x Numeric vector.
#'
#' @examples
#' # Basic usage
#' format_labels(c(0.0, 1, 1000, 1e-04, 2.5e+05))
#'
#' # With ggplot2
#' \dontrun{
#' ggplot(data.frame(x = 1:10, y = c(0.0, 1, 100, 1000, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9))) +
#'   geom_point() +
#'   scale_y_continuous(labels = format_labels)
#' }
#'
#' @return Character vector.
#'
#' @export
format_labels <- function(x) {
    labels <- scales::label_number_auto()(x)
    x <- as.character(x)
    x[x == "0.0"] <- "0"
    x[x == "1e+00"] <- "1"
    return(x)
}

#' Round numbers based on their magnitude
#'
#' Rounds numbers to appropriate significant digits depending on their size:
#' small numbers (absolute value < 1) are rounded to significant digits while
#' larger numbers are rounded to whole numbers.
#'
#' @param x Numeric vector.
#'
#' @examples
#' # Basic usage
#' round_multiple_digits(c(0.00123, 0.0123, 0.123, 1.23, 12.3, 123))
#'
#' # Works with negative numbers
#' round_multiple_digits(c(-0.000456, -0.0456, -0.456, -4.56, -45.6))
#'
#' # Handles zero and edge cases
#' round_multiple_digits(c(0, 0.999, 1, 1.0001, 1e-10, 1e10))
#'
#' @return Numeric vector.
#'
#' @export
round_multiple_digits <- function(x) {
    sapply(x, function(i) {
        if (i < 1) {
            exponent <- floor(log10(i))
            round(i, -exponent)
        } else {
            round(i, 0)
        }
    })
}

#' Create logarithmic axis transformations
#'
#' Generates ggplot2 axis scales with custom logarithmic transformations,
#' including support for handling zero values and custom break points.
#'
#' @param x Numeric vector or list of values to determine axis range.
#' @param axis Character specifying which axis to transform: "x" or "y".
#' @param log_power Integer specifying the base of the logarithm. Must be > 0. Common values:
#'   - `10`: Common logarithm (default)
#'   - `2`: Binary logarithm
#'   - `1`: Natural logarithm
#'   If `log_power <= 1`, uses natural log as fallback.
#' @param breaks Numeric vector of break points in the original data scale.
#'   If `NULL` (default), automatically generates appropriate breaks based on
#'   the data range and logarithmic transformation.
#'
#' @return A ggplot2 scale object (`ScaleContinuousPosition`).
#'
#' @section Mathematical details:
#' When `log_power > 1`:
#' - Forward: `log(x + add, base = log_power)`
#' - Inverse: `log_power^x - add`
#'
#' When `log_power <= 1` (uses natural log as fallback):
#' - Forward: `log(x + add)`
#' - Inverse: `exp(x) - add`
#'
#' Where `add = 0.1` if data contains zeros, otherwise `add = 0`.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Basic log10 scale
#' data <- data.frame(
#'   x = 10^(1:5),
#'   y = 1:5
#' )
#'
#' ggplot(data, aes(x, y)) +
#'   geom_point() +
#'   axis_log(data$x, axis = "x", log_power = 10)
#'
#' # Handle zeros with offset
#' data_with_zero <- data.frame(
#'   x = c(0, 1, 10, 100, 1000),
#'   y = 1:5
#' )
#'
#' ggplot(data_with_zero, aes(x, y)) +
#'   geom_point() +
#'   axis_log(data_with_zero$x, axis = "x", log_power = 10)
#'
#' # Natural log scale
#' ggplot(data, aes(x, y)) +
#'   geom_point() +
#'   axis_log(data$x, axis = "x", log_power = exp(1))
#'
#' # Custom breaks
#' custom_breaks <- c(1, 10, 100, 1000)
#' ggplot(data, aes(x, y)) +
#'   geom_point() +
#'   axis_log(data$x, axis = "x", breaks = custom_breaks)
#'
#' # Y-axis transformation
#' ggplot(data, aes(y, x)) +
#'   geom_point() +
#'   axis_log(data$x, axis = "y")
#'
#' # Multiple data series (list input)
#' multi_data <- list(
#'   series1 = c(1, 10, 100),
#'   series2 = c(5, 50, 500)
#' )
#' ggplot() +
#'   geom_point(aes(x = multi_data$series1, y = 1:3)) +
#'   geom_point(aes(x = multi_data$series2, y = 1:3), color = "red") +
#'   axis_log(multi_data, axis = "x")
#'
#' @note
#' \itemize{
#'   \item When data contains zeros, the offset (0.1) is added to all values
#'     before taking the logarithm. This may distort very small values.
#'   \item The function assumes positive values. Negative values will produce
#'     NaN with a warning.
#' }
#'
#' @export
axis_log <- function(x, axis = "x", log_power = 10, breaks = NULL) {
    min_x <- min(unlist(x), na.rm = TRUE)
    if (min_x == 0) {
        add <- 0.1
    } else {
        add <- 0
    }
    if (log_power > 1) {
        log_func <- function(x) log(x + add, base = log_power)
        comp_func <- function(x) `^`(log_power, x) - add
    } else {
        log_func <- function(x) log(x + add)
        comp_func <- function(x) exp(x) - add
    }
    if (is.null(breaks)) {
        breaks <- c(min_x, max(unlist(x), na.rm = TRUE)) %>%
            log_func() %>%
            ceiling()
        breaks <- seq(breaks[1], breaks[2]) %>%
            comp_func() %>%
            round_multiple_digits()
    }
    get(paste0("scale_", axis, "_continuous"))(
        trans = trans_new(
            "logxn",
            function(x) log_func(x),
            function(x) comp_func(x)
        ),
        breaks = breaks,
        labels = as.character(breaks)
    )
}

theme_histogram <- function(
    p,
    colors = palette_discrete(),
    cex = 1,
    cex_main = 15 * cex,
    cex_sub = 13 * cex,
    cex_axis = 17 * cex,
    guide = FALSE,
    grid = FALSE,
    color_title = "black",
    title_center = 0.5,
    x_axis = FALSE,
    color_subtitle = "gray50") {
    p <- p +
        ylab("Count") +
        theme_minimal() +
        guides(
            color = "none",
            fill = "none"
        ) +
        theme(
            plot.title = element_text(
                hjust = title_center,
                size = cex_main,
                face = "bold",
                color = color_title
            ),
            plot.subtitle = element_text(
                hjust = title_center,
                size = cex_sub,
                color = "gray50"
            ),
            plot.caption = element_text(
                hjust = 1,
                size = cex * 11,
                color = "gray"
            ),
            axis.title.x = element_blank(),
            axis.text.y = element_text(colour = "gray50")
        ) +
        theme_custom(cex, cex_main, cex_sub, cex_axis)
    if (!isTRUE(guide)) {
        p <- p + guides(x = "none")
    } else {
        p <- p +
            theme(
                axis.text.x = element_text(
                    size = cex * 13,
                    color = color_subtitle,
                    vjust = 1, hjust = 1
                )
            )
    }
    if (!isTRUE(grid)) {
        p <- p + theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )
    }
    return(p)
}
