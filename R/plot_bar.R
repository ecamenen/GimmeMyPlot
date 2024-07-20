#' Barplot
#'
#' Display each numerical value separately using a barplot
#'
#' @inheritParams plot_violin
#' @inheritParams plot_bar_mcat
#' @param x Vector of numerical values visualized on the plot.
#' @param colour Color or vector of colors for the gradient of the bars.
#' @param threshold Double for the minimal percentage value before being
#'  hidden on the plot.
#'
#' @examples
#' library(magrittr)
#'
#' # Default parameters
#' x <- runif(10, 1, 10) %>%
#'     set_names(paste("Sample", LETTERS[seq(10)]))
#' plot_bar(x)
#'
#' # Advanced parameters
#' plot_bar(
#'     x = x,
#'     title = "Some numerical variable",
#'     width_title = 30,
#'     colour = c("yellow", "gray", "red"),
#'     color_title = "blue",
#'     cex = 1.2,
#'     digits = 1,
#'     n_max = 5,
#'     ratio = 15,
#'     hjust_title = 1
#' )
#'
#' @return A ggplot object.
#' @export
plot_bar <- function(
        x = NULL,
        title = NULL,
        width_text = 30,
        width_title = 20,
        colour = c("#cd5b45", "gray", "blue"),
        color_title = "black",
        colour_text = "white",
        cex = 1,
        cex_main = cex * 30,
        digits = 0,
        n_max = 100,
        ratio = 5,
        threshold = 2,
        hjust_title = 0.5,
        hjust_ratio = 0.25,
        sample_size = NULL,
        label_x = "percent",
        label_y = "none",
        sort = "asc",
        error = NULL,
        abs = FALSE,
        geom = "bar",
        nudge_x = 0
        ) {
    if (is.null(title)) {
        title <- deparse(substitute(x))
    }
    df0 <- as.data.frame(x) %>%
        set_colnames("val")
    if (n_max > nrow(df0)) {
        n_max <- nrow(df0)
    }
    df <- (
        df0 %>%
            mutate(name = rownames(.) %>% str_wrap(width_text)) %>%
            mutate(order0 = seq(nrow(.)))
    )
    if (!is.null(error)) {
        df <- df %>%
            mutate(error = error)
    }
    if (isTRUE(abs)) {
        func <- function(x) abs(x)
    } else {
        func <- function(x) x
    }
    if (sort == "asc") {
        df <- df %>%
            arrange(desc(func(val)))
    } else {
        df <- df %>%
            arrange(func(val))
    }
    df <- df %>% mutate(order = rev(seq(nrow(.)))) %>%
        head(n_max) %>%
        set_rownames(.$name)
    p <- ggplot(df, aes(order, val, fill = order, color = order)) +
        theme_minimal()
    if (length(colour) == nrow(df0)) {
        colour <- colour[pull(df, "order0")]
    }
    colors <- colorRampPalette(colour)(length(p$data$val))
    y_lab <- p$data$val / 2
    if (geom == "bar") {
        p <- p +
            geom_bar(stat = "identity")
    } else {
        p <- p +
            geom_point(size = cex * 3)
    }
    if (!is.null(error)) {
        p <- p +
            geom_errorbar(aes(ymin = val - error, ymax = val + error), width = 0.3)
    }
    p <- p +
        expand_limits(y = max(p$data$val) + max(p$data$val) / ratio) +
        coord_flip() +
        scale_x_continuous(breaks = df$order, labels = rownames(df)) +
        scale_y_continuous(
            breaks = pretty_breaks(n = 4),
            labels = label_number_auto()
        ) +
        labs(title = str_wrap(title, width_title))
    if (is.null(sample_size)) {
        sample_size <- nrow(df0)
    }

    x_lab <- (p$data$val / sample_size * 100) %>%
        round(digits) %>%
        paste0("%")
    # x_lab <- ifelse(x_lab < threshold, "", paste0(x_lab, "%"))
    x_lab0 <- round(p$data$val, digits)
    x_lab0[abs(p$data$val) < threshold] <- ""
    if (label_x == "percent") {
        lab_x <- x_lab
    } else {
        lab_x <- x_lab0
    }
    if (label_x != "none") {
        p <- p + geom_text(
            color = colour_text,
            aes(y = y_lab, label = lab_x),
            size = cex * 7
        )
    }
    if (label_y == "percent") {
        lab_y <- x_lab
    } else {
        lab_y <- x_lab0
    }
    if (label_y != "none") {
        p <- p + geom_text(
            aes(label =  lab_y),
            hjust = ifelse(p$data$val < 0, 1 + hjust_ratio, 0 - hjust_ratio),
            vjust = 0.5,
            size = cex * 7,
            color = colors,
            nudge_x = nudge_x
        )
    }
    p +
        theme(
            axis.text.y = element_text(
                size = cex * 20,
                face = "italic",
                color = colors
            ),
            axis.text.x = element_text(
                size = cex * 20,
                face = "italic",
                color = "darkgrey"
            ),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(
                size = cex_main,
                face = "bold",
                color = color_title,
                hjust = hjust_title
            ),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank()
        ) %>%
        suppressWarnings() +
        theme(legend.position = "none") +
        scale_fill_gradientn(colours = rev(colors), na.value = "black") +
        scale_color_gradientn(colours = rev(colors), na.value = "black") %>%
        suppressWarnings()
}
