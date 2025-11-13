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
    colour = brewer.pal(n = 8, name = "RdBu"),
    cex = 1,
    method = "spearman",
    method_adjust = "BH",
    mat = NULL,
    p_mat = NULL,
    digits = 2,
    cutoff = 0,
    # TODO: is_cor
    ...) {
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
        p_mat = res$p.value
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
    corrplot(
        mat,
        col = colour,
        type = ifelse(is.null(y), "upper", "full"),
        order = "original",
        tl.col = "gray40",
        tl.srt = 45,
        tl.cex = 1 * cex,
        p.mat = p_mat,
        sig.level = 0.05,
        # addgrid.col = NA,
        pch = 4,
        pch.cex = 2.5 * cex,
        # pch.col = "white",
        diag = !is.null(y),
        na.label = " ",
        cl.cex = cex * 0.95,
        cl.align.text = "l",
        insig = "blank",
        number.digits = digits,
        ...
    )
}
