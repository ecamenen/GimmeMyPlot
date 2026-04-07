# df <- replicate(5, runif(10, 1, 100)) %>%
#     t() %>%
#     as.data.frame() %>%
#     set_rownames(paste("Sample", LETTERS[1:5])) %>%
#     set_colnames(paste("Variable", letters[1:10]))
# df <- replicate(5, sample(c("Yes", "No"), 10, replace = TRUE)) %>%
#     t() %>%
#     as.data.frame() %>%
#     set_rownames(paste("Sample", LETTERS[1:5])) %>%
#     set_colnames(paste("Variable", letters[1:10])) %>%
#     mutate(across(everything(), as.factor))
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
        colour <- brewer.pal(11, "RdBu") %>% rev()
    }
    if (!pull(x, 1) %>% is.numeric()) {
        x <- x %>% mutate(across(everything(), as.factor))
        colour <- palette_discrete()
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
    } else {
        col_names <- colnames(x)
    }
    p <- x %>%
        as.data.frame() %>%
        mutate(ID = rownames(.)) %>%
        gather("key", "value", -ID) %>%
        mutate(
            key = factor(key, levels = col_names),
            ID = str_wrap(ID, width_text)
        ) %>%
        ggplot(aes(ID, key, fill = value)) +
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
