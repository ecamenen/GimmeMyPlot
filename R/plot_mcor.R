#' Correlation matrix
#'
#' Display a heatmap of correlation coefficients (with p-values between several
#'  numerical variables). Each cell indicates the correlation between two
#'  variables. The darker the color, the stronger the correlation. Blue
#'  indicates a positive correlation, red a negative one. Non-significant
#'  adjusted p-values are crossed out. This is an wrapper for the function
#'  [corrplot::corrplot()].
#'
#' @inheritParams plot_violin
#' @inheritParams plot_bar
#' @inheritParams GimmeMyStats::mcor_test
#' @param x Data.frame of double variables (with column names).
#' @param colour Color or vector of colors for the gradient of the bars.
#' @param method Character for the test method ('pearson', 'kendall', or
#' 'spearman').
#' @param mat Matrix of double for correlation coefficients (with column and
#' row names).
#' @param p_mat Matrix of double for adjusted p-values (with column and row
#' names).
#'
#' @return NULL (launch a basic plot)
#' @export
#'
#' @examples
#' library(magrittr)
#' library(RColorBrewer)
#'
#' x0 <- runif(20)
#' x <- lapply(
#'     c(1, -1),
#'     function(i) sapply(seq(10), function(j) x0 * i + runif(10, max = 1))
#' ) %>%
#'     Reduce(cbind, .) %>%
#'     set_colnames(paste("Variable", seq(20)))
#' y <- lapply(
#'     c(1, -1),
#'     function(i) sapply(seq(10), function(j) x0 * i + runif(10, max = 1))
#' ) %>%
#'     Reduce(cbind, .) %>%
#'     set_colnames(paste("Variable", seq(20))) %>%
#'     .[, seq(5)]
#'
#' # Default parameters
#' plot_mcor(x)
#'
#' # Advanced parameters
#' plot_mcor(x, y)
#' plot_mcor(
#'     x,
#'     colour = c("black", brewer.pal(n = 6, name = "RdBu"), 1),
#'     method = "pearson",
#'     method_adjust = "none",
#'     cex = 0.8
#' )
plot_mcor <- function(
        x,
        y = NULL,
        colour = c(
            brewer.pal(n = 9, name = "Blues") %>% rev(),
            "white",
            brewer.pal(n = 9, name = "Reds")
        ),
        cex = 1,
        method = "spearman",
        method_adjust = "BH",
        mat = NULL,
    p_mat = NULL,
    digits = 2,
    cutoff = 0,
    # TODO: is_cor
    ...
) {
    if (is.null(mat) && is.null(p_mat)) {
        res <- mcor_test(
            x,
            y,
            TRUE,
            TRUE,
            method = method,
            method_adjust = method_adjust
        )
        mat <- res$estimate
        p_mat <- res$p.value
    }
    if (is.null(mat)) {
        mat <- mcor_test(x, y, TRUE, FALSE, method = method)
    }
    if (is.null(p_mat)) {
        p_mat <- mcor_test(
            x,
            y,
            FALSE,
            TRUE,
            method = method,
            method_adjust = method_adjust
        )
    }

    mat[abs(mat) < cutoff] <- 0
    p_mat[abs(mat) < cutoff] <- 1

    type <- ifelse(is.null(y), "upper", "full")

    ggcorrplot(
        mat,
        hc.order = FALSE,
        type = type,
        lab = TRUE,
        lab_size = 3 * cex,
        p.mat = p_mat,
        sig.level = 0.05,
        insig = "blank",
        show.diag = !is.null(y),
        digits = digits,
        ...
    ) +
        theme_custom() +
        theme(
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = "gray40", size = 10 * cex),
            axis.text.y = element_text(color = "gray40", size = 10 * cex),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank()
        ) +
        scale_fill_gradientn(
            name = "Correlation",
            colors = colour,
            limits = c(-1, 1)
        )
}
