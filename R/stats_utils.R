#' Print the median and standard deviation
#'
#' @inheritParams plot_violin
#' @inheritParams str_pretty
#' @param x Vector or integers.
#'
#' @return String
#' @export
#'
#' @examples
#' print_median(runif(10))
print_median <- function(x, digits = 1, width = 10) {
    x <- unlist(x)
    digits <- check_integer(digits, min = 0)
    width <- check_integer(width)
    tmp <- paste0(
        median(x, na.rm = TRUE) %>% round(digits),
        "\u00b1",
        IQR(x, na.rm = TRUE) %>% round(digits)
    )
    if (str_length(tmp) > width) {
        str_replace_all(tmp, "\u00b1", "\n\u00b1")
    } else {
        tmp
    }
}

#' Print the result of a mean comparison test
#'
#' @param x Mean comparison test object among `anova_test`, `kruskal_test` or
#' `wilcox_test`.
#' @param digits Integer for the number of decimal places of the test value.
#' @param digits_p Integer for the number of decimal places of the p-value.
#'
#' @return String
#' @export
#'
#' @examples
#' library(rstatix)
#' data("ToothGrowth")
#' df <- ToothGrowth
#' res <- anova_test(df, len ~ dose)
#' print_mean_test(res)
#' res <- kruskal_test(df, len ~ dose)
#' print_mean_test(res)
#' res <- wilcox_test(df, len ~ supp)
#' print_mean_test(res)
print_mean_test <- function(x, digits = 0, digits_p = 2) {
    if (!inherits(x, c("anova_test", "kruskal_test", "wilcox_test", "lmerModLmerTest", "htest"))) {
        stop("x must be a test object from anova_test, kruskal_test, wilcox_test, friedman.test or lmerTest::lmer.")
    }

    tmp <- class(x)
    if (length(tmp) == 1) {
        method <- tmp
    } else {
        method <- sub("_test", "", tmp[2])
        method <- ifelse(method == "data.frame", "anova", method)
    }

    if (method == "anova") {
        par <- paste0("(", x$DFn, ", ", x$DFd, ")")
        statistic <- round(x$F, digits)
        index <- "Anova, F"
    } else if (method %in% c("kruskal", "t")) {
        par <- paste0("(", x$df, ")")
        statistic <- round(x$statistic, digits)
        index <- switch(
            method,
            "t" = "T-test, F",
            "kruskal" = "Kruskal-Wallis, K",
            "htest" = paste0("Friedman, ", "\u03C7\u00B2")
        )
    } else if (method == "wilcox") {
        par <- ""
        statistic <- round(x$statistic, digits)
        index <- "Wilcoxon, W"
    } else if (method == "lmerModLmerTest") {
        x <- anova(x)
        par <- paste0("(", x$NumDF, ", ", round(x$DenDF), ")")
        statistic <- round(x$F, digits)
        index <- "Mixed model, T"
        x$p <- x[, "Pr(>F)"]
    }

    if (!"p.signif" %in% colnames(x)) {
        x <- add_significance0(x)
    }

    x[x == "ns"] <- ""
    x$p <- paste0("= ", round(x$p, digits_p)) %>%
        str_replace_all("^= 0$", "< 0.001")

    paste0(index, par, " = ", statistic, ",", " p ", x$p, x$p.signif)
}

#' @inherit rstatix::add_significance return title params
#' @description Add p-value significance symbols into a data frame.
#' This is an wrapper for the function [rstatix::add_significance()].
# @examples
# # Perform pairwise comparisons and adjust p-values
# library(magrittr)
# library(rstatix)
# data("ToothGrowth")
# ToothGrowth %>%
#     t_test(len ~ dose) %>%
#     adjust_pvalue() %>%
#     add_significance0("p.adj")
add_significance0 <- function(data, p.col = NULL, output.col = NULL) {
    add_significance(
        data,
        p.col = NULL,
        output.col = NULL,
        cutpoints = c(0, 1e-03, 1e-02, 5e-02, 1),
        symbols = c("***", "**", "*", "ns")
    )
}

#' Multiple correlation
#'
#' Tests correlation between several variables. If y is null, calculate the
#' correlation between each variable in x; otherwise, calculate the correlation
#' between each variable in x and each variable in y. This is an wrapper for
#' the function [stats::cor.test()].
#'
#' @inheritParams plot_violin
#' @param x Data.frame of numerical variables.
#' @param y Data.frame of numerical variables.
#' @param estimate Boolean to return the estimated measure of association.
#' @param p.value Boolean to return adjusted p-values.
#' @param method Character for the test method ('pearson', 'kendall', or
#' spearman').
#'
#' @return Data.frame symmetrical containing correlation test results
#' (coefficients and adjusted p-value) for each pair of variables.
#'
#' @examples
#' library(magrittr)
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
#' mcor_test(x)
#' mcor_test(
#'     x,
#'     y,
#'     p.value = TRUE,
#'     method = "pearson",
#'     method_adjust = "bonferroni"
#' )
mcor_test <- function(
    x,
    y = NULL,
    estimate = TRUE,
    p.value = FALSE,
    method = "spearman",
    method_adjust = "BH") {
    x <- as.data.frame(x)
    if (!is.null(y)) {
        y <- as.data.frame(y)
        if (nrow(x) != nrow(y)) {
            stop("The number of rows in x must be the same as in y.")
        }
    } else {
        y <- x
    }
    p.value <- check_boolean(p.value)
    check_choices(method, c("pearson", "kendall", "spearman"))
    check_choices(method_adjust, p.adjust.methods)
    res <- lapply(
        seq(ncol(x)),
        function(i) {
            lapply(
                seq(ncol(y)),
                function(j) {
                    if (is.numeric(x[, i]) & is.numeric(y[, j])) {
                        tryCatch(
                            {
                                cor.test(
                                    x[, i],
                                    y[, j],
                                    method = method,
                                    use = "complete.obs"
                                )
                            },
                            error = function(e) NA
                        )
                    } else {
                        NA
                    }
                }
            )
        }
    )
    # TODO: if !p.value
    # estimate <- TRUE
    if (estimate) {
        rho <- lapply(res, function(i) lapply(i, function(j) j$estimate)) %>%
            unlist() %>%
            matrix(nrow = NCOL(y), ncol = NCOL(x))
        colnames(rho) <- colnames(x)
        rownames(rho) <- colnames(y)
    }
    if (p.value) {
       p <- lapply(res, function(i) lapply(i, function(j) j$p.value)) %>%
           unlist() %>%
           matrix(nrow = NCOL(y), ncol = NCOL(x))
    }

    if (p.value && method_adjust != "none") {
        p <- as.vector(p) %>%
            p.adjust(method_adjust) %>%
            matrix(nrow = NCOL(y), ncol = NCOL(x))
    }
    if (p.value) {
        colnames(p) <- colnames(x)
        rownames(p) <- colnames(y)
    }

    if (estimate && p.value) {
        return(list(estimate = rho, p.value = p))
    } else if (estimate) {
        return(rho)
    } else {
        return(p)
    }
}

#' @examples
#' x <- c(A = 100, B = 78, C = 25)
#' print_chi2_test(chisq_test(x))
# TODO
#' xtab <- as.table(rbind(c(490, 10), c(400, 100)))
#' dimnames(xtab) <- list(
#'     group = c("grp1", "grp2"),
#'     smoker = c("yes", "no")
#' )
#' print_chi2_test(fisher_test(x))
print_chi2_test <- function(x, digits = 3) {
    if ("chisq_test" %in% class(x)) {
        x$statistic <- paste0("X²(", x$df, ") = ", round(x$statistic, 1), ", ")
        x$method <- paste0(x$method, ", ")
    } else {
        x$method <- "Fisher's Exact test"
        x$statistic <- ""
    }
    if (x$p.signif == "ns") {
        x$p.signif <- ""
    }
    if (x$p < 0.001) {
        x$p <- "< 0.001"
    } else {
        x$p <- paste("=", round(x$p, digits))
    }
    x$p.signif[x$p.signif == "****"] <- "***"
    paste0(x$statistic, "P ", x$p, x$p.signif, ", N = ", x$n)
}

#' Performs post hoc analysis for chi-squared or Fisher's exact test
#'
#' Identifies pairwise differences between categories following a chi-squared
#' or Fisher's exact test.
#'
#' @inheritParams print_mean_test
#' @inheritParams mcor_test
#' @param x Data frame, vector, or table. If numeric, treated as a contingency
#' table and the names are considered as categories; otherwise, the levels of
#' the factor or the characters are used.
#' @param method Character specifying the type of test: `chisq` for chi-squared
#' or `fisher` for Fisher's exact test.
#' @param count Logical indicating if `x` is a contingency table.
#' @param ... Additional arguments passed to `chisq.test` or `fisher.test`.
#' @details If x is numeric, it is treated as a contingency table and the names
#' are considered as categories; otherwise, the levels of the factor or the
#' characters are used.
#' @return Data frame with pairwise test results.
#'
#' @examples
#' x <- c(rep("A", 100), rep("B", 78), rep("C", 25))
#' post_hoc_chi2(x)
#'
#' x <- data.frame(G1 = c(Yes = 100, No = 78), G2 =  c(Yes = 75, No = 23))
#' post_hoc_chi2(x, count = TRUE, method = "chisq")
#'
#' data("housetasks")
#' housetasks[, c("Wife", "Husband")] %>%
#'     t() %>%
#'     post_hoc_chi2(count = TRUE, workspace = 1e6)
#'
#' x <- cbind(
#'     mapply(function(x, y) rep(x, y), letters[seq(3)], c(7, 5, 8)) %>% unlist(),
#'     mapply(function(x, y) rep(x, y), LETTERS[seq(3)], c(6, 6, 8)) %>% unlist()
#' )
#' post_hoc_chi2(x)
#'
#' @export
post_hoc_chi2 <- function(
        x,
        method = "fisher",
        method_adjust = "BH",
        digits = 3,
        count = FALSE,
        ...
) {
    df0 <- as.data.frame(x)

    if (ncol(df0) > 1) {
        if (count) {
            x <- colnames(df0)
        } else {
            x <- pull(df0, 2)
        }
    }

    comb <- combn(unique(x) %>% length() %>% seq(), 2)

    res <- lapply(
        seq(ncol(comb)),
        function(i) {
            if (ncol(df0) > 1) {
                if (!count) {
                    x0 <- table(df0)
                } else {
                    x0 <- df0
                }
                df <- x0[, comb[, i]]
                dimn <- colnames(df)
            } else {
                method <- "chisq"
                warning(
                    "With a single categorical data, Fisher's test cannot be performed. Using chi-squared test instead."
                )
                if (!count) {
                    x0 <-  as.character(x) %>% table()
                } else {
                    x0 <- x
                }
                df <- x0[comb[, i]]
                dimn <- names(df)
            }
            get(paste0(method, "_test"))(df, ...) %>%
                mutate(group1 = dimn[1], group2 = dimn[2])
        }
    ) %>%
        Reduce(rbind, .) %>%
        mutate(FDR = p.adjust(p, method_adjust)) %>%
        add_significance(p.col = "FDR", output.col = "fdr.signif") %>%
        mutate(
            p = ifelse(p < 0.001, "< 0.001", round(p, digits)),
            FDR = ifelse(FDR < 0.001, "< 0.001", round(FDR, digits))
        ) %>%
        select(-matches("method"))

    res[res == "****"] <- "***"

    if (method == "chisq") {
        relocate(res, df, .before = p)
    } else {
        res
    }
}

get_outliers <- function(
        x,
        probalities = c(0.25, 0.75),
        index = "iqr",
        weight = 1.5,
        replace = TRUE
) {
    stopifnot(index %in% c("iqr", "percentiles", "hampel", "mad", "sd"))
    med <- median(x, na.rm = TRUE)
    if (index %in% c("hampel", "mad", "sd")) {
        if (index %in% c("hampel", "mad")) {
            # mediane absolute deviation: 3 * MAD
            mad3 <- weight * mad(x, na.rm = TRUE, constant = 1)
        } else {
            mad3 <- weight * sd(x, na.rm = TRUE)
        }
        up <- med + mad3
        low <- med - mad3
    } else {
        # percentiles: probs = c(0.025, 0.975)
        quant <- quantile(x, probs = probalities, na.rm = TRUE)
        if (index == "iqr") {
            # interquartile range: 1.5 * IQR
            iqr <- (quant[2] - quant[1]) * weight
            quant[2] <- med + iqr
            quant[1] <- med - iqr
        }
        up <- quant[2]
        low <- quant[1]
    }
    if (!replace) {
        i <- which(x < low | x > up)
        x <- x[i]
        names(x) <- i
    } else {
        x[which(x < low | x > up)] <- NA
    }
    return(x)
}
