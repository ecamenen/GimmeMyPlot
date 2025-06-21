#' Piechart
#'
#' Visualize the proportions of a categorical variable using a piechart
#'
#' @inheritParams plot_violin
#' @inheritParams count_cat
#' @inheritParams ggplot2::margin
#' @param x Vector of character values visualized on the plot.
#' @param width_text Integer for the maximum length of the text.
#' @param colour Color or vector of colors for the categories.
#' @param cex_label Integer for the maximum length of the title.
#' @param hsize Double for the size of the central hole in the pie chart
#' (in \[1, 2\]).
#' @param legend Boolean to toggle the display of the legend or a vector of
#' character to rename the legend.
#' @param label Boolean to toggle the display of the label.
#' @param sample_size Integer for the sample size of the dataset to calculate
#' percentages (if different from the length of the variable).
#' @param threshold Double for the minimal percentage value before being
#'  hidden on the plot.
#'
#' @examples
#' library(magrittr)
#' library(RColorBrewer)
#'
#' # Default parameters
#' x <- c(rep("A", 5), rep("B", 4))
#' plot_pie(x)
#'
#' # Advanced parameters
#' plot_pie(
#'     x,
#'     label = TRUE,
#'     legend = FALSE,
#'     sort = FALSE,
#'     percent = FALSE
#' )
#' plot_pie(
#'     x,
#'     sort = c("A", "B"),
#'     legend = paste("Level", seq(2)),
#'     sample_size = 11,
#'     threshold = 20
#' )
#'
#' k <- 10
#' n <- runif(k, 1, 10) %>% round()
#' x <- paste("Level", seq(k)) %>%
#'     mapply(function(x, y) rep(x, y), ., n) %>%
#'     unlist()
#' plot_pie(
#'     x,
#'     title = "Some categorical variable",
#'     width_text = 5,
#'     width_title = 20,
#'     colour = rev(brewer.pal(9, "Reds")),
#'     cex = 20,
#'     digits = 1,
#'     hsize = 1.5,
#'     collapse = TRUE,
#'     b = 3
#' )
#' @return A ggplot object.
#' @export
plot_pie <- function(
    x,
    title = NULL,
    width_text = 5,
    width_title = 20,
    colour = palette_discrete(),
    digits = .1,
    cex = 15,
    cex_main = cex * 1.5,
    cex_label = cex * 1.25,
    sort = TRUE,
    threshold = 5,
    hsize = 1.5,
    legend = TRUE,
    label = FALSE,
    percent = TRUE,
    sample_size = NULL,
    collapse = FALSE,
    t = -0.5,
    l = -1,
    r = -1,
    b = -1,
    angle = 0) {
    df <- count_cat(
        x,
        width = width_text,
        collapse = collapse,
        sort = sort
    )
    if (!is.null(sample_size)) {
        n0 <- c(sample_size - sum(df$n))
        if (!any(is.na(df$f))) {
            df <- rbind(df, data.frame(f = NA, n = n0))
        } else {
            df[which(is.na(df$f)), "n"] <- df$n + n0
        }
    }
    if (!is.null(legend) && !is.logical(legend)) {
        df$f <- factor(df$f, labels = legend)
    }
    df <- mutate(
        df,
        hsize = hsize,
        pos = rev(cumsum(rev(n))),
        pos = n / 2 + lead(pos, 1),
        pos = if_else(is.na(pos), n / 2, pos),
        label = str_wrap(str_glue("{f}"), width_text),
        text = scales::percent(n / sum(n), digits)
    )
    if (is.null(title)) {
        title <- deparse(substitute(x))
    }
    i <- df$n / sum(df$n) <= threshold / 100
    df$text[i] <- ""
    df$legend <- df$f
    df$legend0 <- paste0(df$legend, ": ", df$n)
    df$legend[df$legend == "NA"] <- NA
    if (!percent) {
        df$text <- df$n
    }
    p <- ggplot(df, aes(x = hsize, y = n, fill = f)) +
        geom_col(width = 1, color = NA) +
        geom_text(
            color = "white",
            size = cex / 2.5,
            aes(label = text),
            position = position_stack(vjust = 0.5)
        ) +
        coord_polar(theta = "y", clip = "off") +
        scale_fill_manual(
            values = colour,
            na.value = "gray",
            labels = df$legend0,
            breaks = df$legend,
            name = ""
        ) +
        scale_y_continuous(breaks = df$pos, labels = df$label) +
        ggtitle(str_wrap(title, width_title)) +
        theme(
            plot.title = element_text(
                hjust = 0.5,
                vjust = -4,
                size = cex_main,
                face = "bold"
            ),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.text = element_text(size = cex),
            legend.key = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.margin = unit(c(t, r, b, l), "cm")
        ) +
        xlim(0.5, hsize + 0.5)
    if (is.null(legend) || isFALSE(legend)) {
        p <- p + theme(
            legend.position = "none",
            axis.text = element_blank()
        )
    }
    if (label) {
        p + theme(
            axis.text = element_text(
                size = cex_label,
                colour = colour,
                vjust = .Machine$double.digits,
                angle = angle
            ) %>% suppressWarnings()
        )
    } else {
        p + theme(axis.text = element_blank())
    }
}
