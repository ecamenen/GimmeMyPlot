#' Alluvial diagram
#'
#' Display how the frequency of observations are distributed along multiple
#' categorical variables. The width of the links and the height of the bars
#' are proportional to the the number of cases. Alluvial diagrams are a good
#' alternative to multiple pie charts.
#'
#' @inheritParams plot_violin
#' @inheritParams plot_venn
#' @param x Data.frame of multiple categorical variables.
#' @param colour Color or vector of colors for the categories.
#'
#' @return ggplot object.
#' @export
#'
#' @examples
#' library(ggalluvial)
#' library(magrittr)
#' library(RColorBrewer)
#'
#' x <- lapply(seq(3), function(x) {
#'     runif(100, 1, 3) %>%
#'         round() %>%
#'         letters[.]
#' }) %>%
#'     as.data.frame() %>%
#'     set_colnames(paste("variable", LETTERS[seq(3)]))
#' x[x == "a"] <- NA
#' x[, 3][is.na(x[, 3])] <- "a"
#'
#' # Default parameters
#' plot_alluvial(x)
#'
#' # Advanced parameters
#' plot_alluvial(
#'     x,
#'     width_label = 5,
#'     colour = rev(brewer.pal(3, "Reds")),
#'     cex = 1.5
#' )
plot_alluvial <- function(
    x,
    width_label = 20,
    colour = palette_discrete(),
    cex = 1,
    cex_axis = 17 * cex) {
    colnames(x) <- colnames(x) %>%
        str_wrap(width_label) %>%
        to_title()
    x <- as.data.frame(x)
    df <- sapply(x, function(x) as.character(x)) %>% as.data.frame()
    n <- unlist(df) %>%
        unique() %>%
        na.omit() %>%
        seq()
    df[is.na(df)] <- "zz"
    df0 <- df %>%
        mutate(id = rownames(.)) %>%
        pivot_longer(-id)

    label_stratum <- df %>%
        select(sort(colnames(.))) %>%
        lapply(
            function(i) {
                unique(i) %>%
                    sort(decreasing = TRUE) %>%
                    str_replace_all("zz", "NA")
            }
        ) %>%
        unlist()
    p <- ggplot(
        df0,
        aes(
            x = name,
            stratum = value,
            alluvium = id,
            fill = value,
            label = value
        )
    ) +
        geom_flow() +
        geom_stratum(width = 0.5, color = "grey")

    strata_data <- df0 %>%
        count(name, value, name = "n") %>%
        group_by(name) %>%
        arrange(desc(value)) %>%
        mutate(
            ymax = cumsum(n),
            ymin = ymax - n,
            y = (ymax + ymin) / 2,
            label = ifelse(value == "zz", "NA", value)
        )

    p + geom_text(
        data = strata_data,
        aes(x = name, y = y, label = label, group = NULL),
        inherit.aes = FALSE,
        color = "white",
        size = cex * 5.5
    ) +
        scale_x_discrete(expand = c(.05, .05)) +
        scale_fill_manual(values = c(colour[n], "gray")) +
        labs(y = "n") +
        theme_minimal() +
        theme_custom(cex = cex * 1, cex_axis = cex_axis) +
        theme(
            legend.position = "none",
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title.x = element_blank()
        )
}
