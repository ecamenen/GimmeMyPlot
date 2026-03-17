#' Barplot for categorical variables
#'
#' Visualize the proportions of multiple categorical variables using a
#' barplot
#'
#' @inheritParams plot_pie
#' @inheritParams plot_violin
#' @inheritParams plot_bar
#' @param x Data.frame of character values visualized on the plot.
#' @param colour_gradient Color or vector of colors for the categories.
#' @param colour_text Color of the text.
#' @param ratio Double for the width scale.
#' @param n_collapse Integer for the maximum number of merged categories to show
#' @param n_max Integer for the maximum number of bars to show
#' (prioritizing those with the largest value).
#' @param hjust_title Double for the horizontal justification of the title.
#' @param hjust_text Double for the horizontal justification of the text.
#' @param vjust_text Double for the vertical justification of the text.
#'
#' @examples
#' library(magrittr)
#' library(RColorBrewer)
#'
#' # Default parameters
#' df <- sapply(seq(10), function(x) runif(10) %>% round()) %>% as.data.frame()
#' colnames(df) <- paste("Level", seq(10))
#' plot_bar_mcat(df)
#'
#' # Advanced parameters
#' plot_bar_mcat(
#'     df,
#'     sample_size = 15,
#'     title = "Some categorical variable",
#'     width_text = 30,
#'     width_title = 50,
#'     colour_gradient = brewer.pal(9, "Reds"),
#'     color_title = "red",
#'     cex = 8,
#'     digits = 1,
#'     collapse = TRUE,
#'     ratio = 2,
#'     n_collapse = 3,
#'     n_max = 4,
#'     hjust_title = 1
#' )
#'
#' @return A ggplot object.
#' @export
plot_bar_mcat <- function(
    x,
    sample_size = NULL,
    title = NULL,
    width_text = 20,
    width_title = width_text,
    colour_gradient = c("blue", "gray", "#cd5b45"),
    colour_text = "white",
    color_title = "black",
    cex = 10,
    digits = 0,
    threshold = 2,
    collapse = FALSE,
    ratio = 5,
    n_collapse = 5,
    n_max = Inf,
    hjust_title = -0.5,
    hjust_text = -0.1,
    vjust_text = 0.5,
    inverse = FALSE
    # label_x = "percent",
    # label_y = "none"
    ) {
    if (is.null(title)) {
        title <- deparse(substitute(x))
    }
    x0 <- as.data.frame(x)
    df <- count_category(
        x0,
        width = width_text,
        collapse = collapse,
        format = FALSE
    )
    if (is.null(sample_size)) {
        sample_size <- nrow(x0)
    }
    colour_gradient <- colorRampPalette(colour_gradient)(nrow(tail(df, n_max))) %>% tail(n_max)
    x_lab0 <- mapply(function(x, y) x / y, df$n * 100, sample_size)
    if (inverse) {
      df$x_lab <- df$n
      df$n <- x_lab0
      df$f <- fct_reorder(df$f, df$n)
      df <- arrange(df, n)
    } else {
      df$x_lab <- round(x_lab0, digits) %>% paste0("%")
    }
    df$y_lab <- df$n / 2
    df$n0 <- round(df$n, digits)
    if (inverse) {
      df$n0 <- paste0(df$n0, "%")
    }
    df <- df %>%
      data.frame(., order = as.numeric(rownames(.))) %>%
      tail(n_max)
    y_lab0 <- as.character(df$f)
    y_lab0[(str_count(y_lab0, "\\,") + 1) >= n_collapse] <- "..."
    i <- df$y_lab < threshold / 2
    df$x_lab[i] <- ""
    (ggplot(df, aes(f, n, fill = order, label = n0)) +
        geom_bar(stat = "identity") +
        expand_limits(y = max(df$n) + max(df$n) / ratio) +
        coord_flip() +
        scale_x_discrete(labels = y_lab0)
    ) %>%
        theme_bar(colors = colour_gradient, cat = FALSE) +
        geom_text(
            aes(color = I(colour_text), y = y_lab),
            size = cex
        ) +
        geom_text(
            aes(label = x_lab),
            data = df,
            color = colour_gradient,
            hjust = hjust_text,
            vjust = vjust_text,
            size = cex
        ) +
        ggtitle(str_wrap(title, width_title)) +
        theme(
            plot.title = element_text(
                hjust = hjust_title,
                vjust = 0,
                size = cex * 4,
                face = "bold",
                color = color_title
            ),
            axis.text.y = element_text(colour = colour_gradient, size = cex * 3),
            axis.text.x = element_text(size = cex * 2.2),
            plot.margin = unit(c(-0, 0, 0, 0.5), "cm"),
            panel.grid.major.y = element_blank()
        ) %>%
        suppressWarnings() +
        scale_y_continuous(
            breaks = breaks_pretty(2),
            labels = label_number_auto()
        )
}
