#' Venn diagram
#'
#' Visualize a venn diagram with the element of in each set and their
#' intersection.
#'
#' @inheritParams plot_violin
#' @inheritParams plot_pie
#' @inheritParams plot_bar_mcat
#' @param x List (with names).
#' @param width_label Integer for the maximum length of the labels.
#' @param color_gradient Vector of colors for the gradient of number of
#' elements. If FALSE, hide the color.
#' @param n_max Integer for the maximum number of element to show.
#' After this threshold, only their statistics will be visible.
#' @param vjust_label Double for the vertical justification of the labels
#' (in \[0, 1\]).
#' @param label Boolean to toggle the display of the labels.
#' @param element Boolean to toggle the display of the elements.
#' If disabled, only their statistics will be visible.
#' @param percent Boolean to toggle the display of the percenatge of the
#' elements.
#'
#' @examples
#' library(magrittr)
#' library(RColorBrewer)
#'
#' x <- lapply(
#'     seq(3),
#'     function(x) {
#'         runif(10, 1, 10) %>%
#'             round() %>%
#'             letters[.] %>%
#'             unique() %>%
#'             paste("Element", .)
#'     }
#' ) %>%
#'     set_names(paste("Dataset", LETTERS[seq(3)]))
#'
#' # Default parameters
#' plot_venn(x)
#'
#' # Advanced parameters
#' plot_venn(
#'     x,
#'     width_text = 8,
#'     width_label = 5,
#'     colour = brewer.pal(3, "Reds"),
#'     color_gradient = c("white", "red"),
#'     cex = 1.2,
#'     cex_main = 1.2 * 6,
#'     cex_line = 1.5,
#'     n_max = 2,
#'     vjust_label = 0.75,
#'     ratio = 0.5,
#'     label = FALSE,
#'     element = FALSE,
#'     percent = FALSE,
#' )
#'
#' @return A ggplot object.
#' @export
plot_venn <- function(
    x,
    width_text = 30,
    width_label = 30,
    colour = palette_discrete(),
    color_gradient = c("white", "red"),
    cex = 1,
    cex_main = cex * 6,
    cex_line = cex * 2,
    n_max = 4,
    vjust_label = 0.5,
    ratio = 0.1,
    label = TRUE,
    element = TRUE,
    percent = TRUE) {
    data <- x %>%
        set_names(names(.) %>% str_wrap(width_label)) %>%
        Venn() %>%
        process_data()
    if (percent) {
        data$regionData <- data$regionData %>%
            mutate(
                percent = (count * 100 / sum(count)) %>%
                    round(digits = 0) %>%
                    paste0("%")
            ) %>%
            mutate(
                label = paste0("(", percent, ")") %>%
                    paste(count, ., sep = "\n")
            )
    } else {
        data$regionData <- mutate(data$regionData, label = count)
    }
    data$regionData$label[data$regionData$item %>% list.which(length(.) == 0)] <- ""
    if (element) {
        i <- data$regionData$item %>% list.which(length(.) <= n_max & length(.) > 0)
        data$regionData$label[unlist(i)] <- data$regionData$item %>%
            list.search(length(.) <= n_max & length(.) > 0) %>%
            list.mapv(str_pretty(., width_text) %>% paste(., collapse = "\n"))
    }
    p <- ggplot() +
        geom_sf(aes(fill = count), data = venn_region(data)) +
        geom_sf(
            aes(colour = id),
            data = venn_setedge(data),
            show.legend = FALSE,
            lwd = cex_line
        ) +
        geom_sf_label(
            aes(label = to_title(label)),
            data = venn_region(data),
            alpha = 0.5,
            label.size = NA,
            size = cex * 4
        ) +
        theme_void() +
        scale_color_manual(values = colour) +
        theme(
            legend.title = element_text(face = "bold.italic", size = cex * 13),
            legend.text = element_text(size = cex * 9)
        ) +
        scale_x_continuous(expand = expansion(mult = ratio))

    if (label) {
        p <- p + geom_sf_text(
            aes(label = name),
            colour = colour[seq(length(x))],
            vjust = vjust_label,
            data = venn_setlabel(data),
            size = cex_main
        )
    }
    p$labels$fill <- "N"
    if (!isFALSE(color_gradient)) {
        p <- p +
            scale_fill_gradientn(colors = color_gradient, na.value = "black")
    } else {
        p <- p +
            scale_fill_gradientn(colors = "white", na.value = "black") +
            theme(legend.position = "none")
    }
    return(p)
}
