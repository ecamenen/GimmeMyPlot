# library(tidyverse)
# library(magrittr)
# set.seed(123)
# Numerical variables
# df <- replicate(5, runif(10, 1, 100)) %>%
#     t() %>%
#     as.data.frame() %>%
#     set_rownames(paste("Sample", LETTERS[1:5])) %>%
#     set_colnames(paste("Variable", letters[1:10]))
# plot_heatmap(df)
# Categorial variables
# df <- replicate(5, sample(c("Yes", "No"), 10, replace = TRUE)) %>%
#     t() %>%
#     as.data.frame() %>%
#     set_rownames(paste("Sample", LETTERS[1:5])) %>%
#     set_colnames(paste("Variable", letters[1:10])) %>%
#     mutate(across(everything(), as.factor))
# plot_heatmap(df)
# Mixed categorical variables
# df <- cbind(
#     replicate(3, sample(c("Yes", "No"), 5, replace = TRUE)),
#     replicate(2, sample(c("Level A", "Level B", "Level C"), 5, replace = TRUE))
# ) %>%
#     as.data.frame() %>%
#     set_rownames(paste("Sample", LETTERS[1:5])) %>%
#     set_colnames(paste("Variable", letters[1:5])) %>%
#     mutate(across(everything(), as.factor))
# plot_heatmap(df)
#' @export
plot_heatmap <- function(
        x,
        cex = 1,
        width_text = 20,
        colour = NULL,
        normalize = FALSE,
        sort = TRUE
) {
    if (is.null(colour)) {
        if (!pull(x, 1) %>% is.numeric()) {
            colour <- palette_discrete()[c(1, 3, 2, 4:13)]
        } else {
            colour <- brewer.pal(11, "RdBu") %>% rev()
        }
    }
    if (!pull(x, 1) %>% is.numeric()) {
        x <- x %>% mutate(across(everything(), as.factor))
    } else {
        if (!isFALSE(normalize)) {
            if (normalize == TRUE || normalize == "zscore") {
                legend_label <- "Z-score"
                x <- x %>% mutate(across(where(is.numeric), scale))
                for (col in names(x)) {
                    attr(x[[col]], "scaled:center") <- NULL
                    attr(x[[col]], "scaled:scale") <- NULL
                }
            } else {
                legend_label <- "Min-max"
                x <- x %>% mutate(across(where(is.numeric), ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE))))
            }
        } else {
            legend_label <- "Values"
        }
    }

    x <- x %>% set_colnames(colnames(.) %>% str_wrap(width_text))
    if (isTRUE(sort)) {
        col_names <- sort(colnames(x))
    } else if (isFALSE(sort)) {
        col_names <- colnames(x)
    } else {
        custom_levels <- str_wrap(sort, width_text)
        col_names <- custom_levels[custom_levels %in% colnames(x)]
        col_names <- c(col_names, setdiff(colnames(x), col_names))
    }
    p <- x %>%
        as.data.frame() %>%
        mutate(across(where(is.factor), as.character)) %>%
        mutate(id = rownames(.)) %>%
        gather("key", "value", -id) %>%
        mutate(
            key = factor(key, levels = col_names),
            id = str_wrap(id, width_text),
            value = if (!pull(x, 1) %>% is.numeric() && !isTRUE(sort) && !isFALSE(sort)) {
                factor(value, levels = sort)
            } else if (!pull(x, 1) %>% is.numeric()) {
                factor(value, levels = unique(value))
            } else {
                as.numeric(value)
            }
        ) %>%
        ggplot(aes(id, key, fill = value)) +
        geom_tile() +
        theme_classic() +
        theme_custom(cex) +
        theme(
            axis.title = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.border = element_blank()
        )

    if (!pull(x, 1) %>% is.numeric()) {
        p +
            scale_fill_manual(
                values = colour,
                na.value = "white",
                name = "Values"
            )
    } else {
        p +
            scale_fill_gradientn(
                colours = colour,
                na.value = "white",
                name = legend_label,
                breaks = pretty_breaks(5)
            )
    }
}
