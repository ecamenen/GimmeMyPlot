#' Piechart
#'
#' Visualize the proportions of a categorical variable using a piechart.
#'
#' @inheritParams plot_violin
#' @inheritParams GimmeMyStats::count_category
#' @inheritParams ggplot2::margin
#' @inheritParams plot_bar_2cat
#' @inheritParams plot_venn
#' @inheritParams plot_bar
#' @inheritParams plot_alluvial
#' @param x Vector of character values of factor to be visualized.
#' @param cex_label Integer for the maximum length of the title.
#' @param hsize Double for the size of the central hole in the pie chart
#' (in \[1, 2\]).
#' @param legend Boolean to toggle the display of the legend or a vector of
#' character to rename the legend.
#' @param sample_size Integer for the sample size of the dataset to calculate
#' percentages (if different from the length of the variable).
#' @param threshold Double for the minimal value (percentage or raw value if percent is false)
#' before being hidden on the plot.
#'
#' @examples
#' library(magrittr)
#' library(RColorBrewer)
#'
#' # Example 1: Basic pie chart
#' x <- c(rep("B", 4), rep("A", 5))
#' x[4] <- NA
#' plot_pie(x)
#'
#' # Example 2: Pic chart with labels and raw values
#' plot_pie(
#'     x,
#'     label = TRUE,
#'     legend = FALSE,
#'     sort = FALSE,
#'     percent = FALSE
#' )
#' # Example 3: Custom legend and sample size
#' plot_pie(
#'     x,
#'     sort = c("B", "A"),
#'     legend = paste("Level", seq(3)),
#'     sample_size = 11,
#'     threshold = 30
#' )
#'
#' set.seed(123)
#' k <- 10
#' n <- runif(k, 1, 10) %>% round()
#' x <- paste("Level", seq(k)) %>%
#'     mapply(function(x, y) rep(x, y), ., n) %>%
#'     unlist()
#' # Example 4: Multiple categories with color gradient
#' plot_pie(
#'     x,
#'     title = "Some categorical variable",
#'     width_text = 100,
#'     width_title = 20,
#'     colour = rev(brewer.pal(9, "Reds")),
#'     cex = 20,
#'     digits = 1,
#'     hsize = 1.5,
#'     collapse = TRUE,
#'     l = 1
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
    threshold = 0,
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
    angle = 0,
    format = TRUE) {
   if (!is.character(legend)) {
       legend2 <- unique(x) %>% sort()
   } else {
       legend2 <- legend
   }
    if (isTRUE(sort)) {
        sort <- unique(x) %>% sort()
        x <- factor(x) %>%
            sort(na.last = TRUE)
    } else if (isFALSE(sort)) {
        sort <- unique(x) %>% na.omit()
        x <- factor(x)
    } else {
        x <- x %>%
            factor(levels = sort) %>%
            sort(na.last = TRUE)
    }
        tmp <- as.character(x) %>% sort()
        res <- as.list(legend2) %>%
            set_names(c(unique(tmp), tail(legend2, -length(unique(tmp)))))
        x <- fct_expand(x, names(res))
        res2 <- set_names(names(res), as.character(res))
        x <- do.call(fct_recode, c(list(.f = x), res2))
    df <- count_category(
        x,
        width = width_text,
        collapse = collapse,
        sort = FALSE,
        format = format
    )
    if (!is.null(sample_size)) {
        n0 <- c(sample_size - sum(df$n))
        if (!any(is.na(df$f))) {
            df <- rbind(df, data.frame(f = NA, n = n0))
        } else {
            df[which(is.na(df$f)), "n"] <- df[which(is.na(df$f)), "n"] + n0
        }
    }
    if (isFALSE(collapse) && !is.null(legend) && !is.logical(legend)) {
        legend2 <- str_wrap(legend2, width_text)
        missing <- setdiff(legend2, df$f)
        df$f <- as.character(df$f)
        df <- complete(df, f = c(f, missing), fill = list(n = 0))  %>%
            mutate(f = factor(f, levels = legend2))
        sort <- res[sort] %>%
            unlist() %>%
            str_wrap(width_text) %>%
            c(missing)
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
    if (isFALSE(collapse)) {
         df <- df %>%
            mutate(f = fct_relevel(f, sort)) %>%
            arrange(f)
    }
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
        df$text[df$text <= threshold] <- ""
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
            name = "",
            drop = FALSE
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
                colour = ifelse(is.na(df$f), "white", colour),
                vjust = .Machine$double.digits,
                angle = angle
            ) %>% suppressWarnings()
        )
    } else {
        p + theme(axis.text = element_blank())
    }
}
