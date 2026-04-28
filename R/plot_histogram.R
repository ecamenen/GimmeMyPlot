#' Plot histogram
#'
#' Visualize the distribution of single variable using histogram
#'
#' @inheritParams plot_violin
#' @param x Vector of numerical values visualized on the plot.
#' @param color Color for the plot.
#' @param color_stats Color for the median and quantile lines.
#' @param binwidth Double for the number of bins.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' # Default parameters
#' set.seed(123)
#' x <- rnorm(100)
#' plot_histogram(x)
#'
#' # Advanced parameters
#' plot_histogram(
#'     x,
#'     title = "Some numerical variable",
#'     width_title = 15,
#'     color = "blue",
#'     color_title = "orange",
#'     color_stats = "orange",
#'     cex = 1.2,
#'     digits = 1,
#'     binwidth = 0.5
#' )
plot_histogram <- function(
    x,
    title = NULL,
    width_title = 20,
    color = "red",
    color_title = color,
    color_stats = "black",
    cex = 1,
    cex_axis = 17 * cex,
    cex_main = 21 * cex,
    cex_sub = 15 * cex,
    digits = 0,
    subtitle = TRUE,
    probs = c(.25, .75),
    binwidth = 1.5) {
    df <- as.data.frame(x) %>%
        pivot_longer(everything())
    quant <- quantile(x, probs, na.rm = TRUE)
    if (is.null(title)) {
        title <- deparse(substitute(x))
    }
    if (subtitle) {
        subtitle <- paste0(
            print_dispersion(df$value, digits = digits),
            ", N=",
            length(na.omit(df$value))
        )
    }
    p <- gghistogram(
        df,
        x = "value",
        color = NA,
        fill = "name",
        palette = color,
        binwidth = binwidth,
        add = "median",
        rug = TRUE,
        add.params = list(linetype = "solid", size = 1)
    ) +
        geom_density(
            lwd = cex * 1.1,
            colour = color_stats,
            fill = color,
            alpha = 0.25
        ) +
        geom_vline(
            xintercept = quant,
            color = color_stats,
            lty = 2,
            lwd = cex * 1.25
        ) +
        labs(subtitle = subtitle, title = str_wrap(title, width_title)) +
        theme_minimal() +
        scale_x_continuous(
            expand = c(0.01, 0),
            labels = function(x) {
                paste0(x)
            }
        ) +
        guides(color = "none", fill = "none")
    theme_histogram(
        p,
        cex = cex,
        cex_main = cex_main,
        cex_sub = cex_sub,
        cex_axis = cex_axis,
        guide = TRUE,
        color_title = color_title
    )
}
