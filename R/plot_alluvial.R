#' Alluvial diagram
#'
#' Plot an alluvial diagram to visualize how observations flow across multiple
#' categorical variables. Alluvial diagrams are an alternative to
#' multiple pie charts or stacked bar charts, showing the distribution
#' and transitions between categories across different dimensions. The width of
#' the links (flows) and the height of the strata (bars) are proportional to the
#' number of observations.
#'
#' @inheritParams plot_violin
#' @inheritParams plot_venn
#' @param x Data.frame containing multiple categorical variables (factors or
#'          character vectors). Each column represents a categorical variable,
#'          and each row represents an observation.
#' @param colour Color or vector of colors for the different categories.
#' @param sort Boolean indicating whether to sort the strata (bars) within each
#'             column alphabetically by category name. If \code{FALSE} (default),
#'             strata are displayed in the order they appear in the data
#'
#' @return A ggplot object.
#'
#' @examples
#' library(ggalluvial)
#' library(magrittr)
#' library(RColorBrewer)
#'
#' # Generate example data with three categorical variables
#' set.seed(123)
#' x <- lapply(seq(3), function(x) {
#'     runif(100, 1, 3) %>%
#'         round() %>%
#'         letters[.]
#' }) %>%
#'     as.data.frame() %>%
#'     set_colnames(paste("variable", LETTERS[seq(3)]))
#'
#' # Introduce some missing values for demonstration
#' x[x == "a"] <- NA
#' x[, 3][is.na(x[, 3])] <- "a"
#'
#' # Example 1: Basic alluvial diagram with default parameters
#' plot_alluvial(x)
#'
#' # Example 2: Customized diagram with red color palette
#' plot_alluvial(
#'     x,
#'     width_label = 5,
#'     colour = rev(brewer.pal(3, "Reds")),
#'     cex = 1.5
#' )
#'
#' @export
plot_alluvial <- function(
    x,
    width_label = 20,
    colour = palette_discrete(),
    cex = 1,
    cex_axis = 17 * cex,
    sort = FALSE) {
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


    if (isFALSE(sort)) {
        df0 <- mutate(df0, name = factor(name, levels = colnames(x)))
    }

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
        labs(y = "Count") +
        theme_minimal() +
        theme_custom(cex = cex * 1, cex_axis = cex_axis) +
        theme(
            legend.position = "none",
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title.x = element_blank()
        )
}
