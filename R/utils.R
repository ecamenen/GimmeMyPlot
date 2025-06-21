#' Color palette
#'
#' This is an wrapper for the function [RColorBrewer::brewer.pal()].
#' @return Color vector
#' @export
palette_discrete <- function() {
    c(
        brewer.pal(9, "Set1")[-c(6:7, 9)],
        rev(brewer.pal(7, "Set2")[c(6:5)]),
        brewer.pal(8, "Pastel1")[-c(3, 6:7, 9)]
    )
}

#' Color palette
#'
#' This is an wrapper for the function [RColorBrewer::brewer.pal()].
#' @param x Integer for the length of the palette
#' @return Color vector
#' @export
palette_continuous <- function(x, gray = TRUE) {
    res <- c(brewer.pal(9, "YlOrBr")[c(3, 5, 7)],
             brewer.pal(9, "RdBu")[1:4],
             brewer.pal(11, "PiYG")[4:1],
             brewer.pal(11, "PRGn")[1:4],
             brewer.pal(11, "RdYlBu")[7:11],
             brewer.pal(11, "BrBG")[10:8],
             brewer.pal(11, "PiYG")[7:11]
    )
    if (gray) {
        c(res, rev(brewer.pal(7, "Greys")[-1])) %>%
            colorRampPalette()
    } else {
        colorRampPalette(res)
    }
}

palette_discrete2 <- function(x) {
    list.map(
        c("RdBu", "PiYG", "PuOr"),
        f(i) ~ brewer.pal(9, i)[-5]
    ) %>%
        unlist() %>%
        c(
            brewer.pal(9, "YlOrBr")[4:2],
            brewer.pal(9, "BrBG")[7:9],
            rev(brewer.pal(7, "Greys")[-seq(2)])
        ) %>% unname()
}

margin_spacer <- function(x, ratio = 5) {
    tmp <- nchar(levels(factor(x)))[1]
    if (tmp > 8) {
        return((tmp - 8) * ratio)
    }
    else
        return(0)
}

#' Frequency of categorical variable
#'
#' Formats a data.frame containing categorical variables and calculates the
#' frequency of each category.
#'
#' @inheritParams str_pretty
#' @param x Vector or data.frame of categorical variables.
#' @param collapse Boolean to merge categories with identical proportions.
#' @param sort Boolean to order the categories by value. If set to false,
#' the categories are ranked by their names. Alternatively, it could be
#' a character vector of the same length as the number of categories,
#' allowing for renaming.
#' @param format Boolean to format the names of the categories if the input is
#' a vector.
#' @return Data.frame with two columns (f and n).
#'
# @examples
# library(magrittr)
# # Vector of categorical variable
# k <- 10
# n <- runif(k, 1, 10) %>% round()
# x <- paste("Level", seq(k)) %>%
#     mapply(function(x, y) rep(x, y), ., n) %>%
#     unlist()
# count_cat(x)
#
# # Data.frame of categorical variable
# df <- sapply(seq(10), function(x) runif(10) %>% round()) %>% as.data.frame()
# colnames(df) <- paste("Level", seq(10))
# count_cat(df)
count_cat <- function(
    x,
    width = 20,
    collapse = FALSE,
    sort = TRUE,
    format = TRUE) {
    x <- check_data_frame(x)
    col_name <- colnames(x)
    width <- check_integer(width)
    collapse <- check_boolean(collapse)
    if (ncol(x) > 1) {
        x <- sapply(
            colnames(x),
            function(i) rep(i, pull(x, i) %>% unlist() %>% sum(na.rm = TRUE))
        )
    }
    x0 <- unlist(x) %>%
        stri_trans_general("latin-ascii") %>%
        str_replace_all("\n", " ") %>%
        to_title() %>%
        str_wrap(width) %>%
        factor()
    if (isTRUE(sort)) {
        x0 <- fct_infreq(x0) %>%
            fct_rev()
    } else if (!isFALSE(sort)) {
        x0 <- ordered(x0, levels = str_wrap(sort, width))
    }
    df <- fct_relabel(x0, ~ str_remove_all(.x, "\\s*\\([^\\)]+\\)")) %>%
        fct_relabel(~ str_remove_all(.x, "\\$\\$[^\\)]+"))
    if (format) {
        df <- df %>%
            fct_relabel(~ str_replace_all(.x, "^0$", "No")) %>%
            fct_relabel(
                ~ str_replace_all(
                    .x,
                    "^1$",
                    ifelse(col_name[1] == "x", "Yes", col_name[1])
                )
            )
    }

    df <- fct_count(df)
    if (collapse) {
        df <- group_by(df, n) %>%
            summarise(
                f = paste(f, collapse = ", ") %>%
                    str_wrap(width)
            ) %>%
            mutate(f = factor(f))
        df$f <- reorder(df$f, df$n)
    }
    return(df)
}

#' Capitalize  only the first letter
#' @inherit str_trunc0 return params
#' @examples
#' to_title("hi there, I'm a sentence to format.")
#' @export
to_title <- function(x) {
    lapply(
        x,
        function(i) {
            if (!is.na(i) && !is.null(i)) {
                paste0(toupper(substr(i, 1, 1)), substr(i, 2, nchar(i)))
            } else {
                i
            }
        }
    ) %>% unlist()
}


#' @inherit str_pretty
#' @param sep Character to separate the terms.
# @examples
# str_trunc1("Hi there, I'm a sentence to format.")
str_trunc1 <- function(x, width = 20, sep = " ") {
    x <- check_character(x)
    sep <- check_character(sep)
    x0 <- strsplit(x, sep)[[1]]
    n <- str_length(x0[[1]])
    width <- check_integer(width, min = n)
    lapply(seq(length(x0)), function(i) str_trunc0(x, i, sep)) %>%
        detect(function(x) str_length(x) <= width, .dir = "backward")
}

#' Truncate a string to maximum number of words.
#'
#' @inherit str_pretty return params
#' @inheritParams str_trunc1
#' @param n Maximum number of words.
#'
# @examples
# str_trunc0("Hi there, I'm a sentence to format.")
str_trunc0 <- function(x, n = 5, sep = " ") {
    x <- check_character(x)
    sep <- check_character(sep)
    res <- strsplit(x, sep)[[1]]
    n <- check_integer(n, max = length(res))
    res[seq(n)] %>%
        paste(collapse = sep)
}

#' Truncate a string to maximum width
#'
#' Truncate a string to maximum width while ensuring that whole words are
#' retained
#' @param x String
#' @param width Maximum width of string.
#' @return String
#' @export
#'
#' @examples
#' str_pretty("Hi there, I'm a sentence to format.")
#' @export
str_pretty <- function(x, width = 20) {
    sapply(
        x,
        function(i) {
            i <- str_trim(i) %>% to_title()
            res <- str_trunc1(i, width)
            if (is.null(res)) {
                res <- str_trunc1(i, width, "-")
            }
            if (is.null(res)) {
                return(res)
            }
            if (str_width(res) < str_width(i)) {
                paste0(res, "...")
            } else {
                res
            }
        }
    ) %>% unname()
}

check_length <- function(par, val, l = 1) {
    res <- unlist(val)
    if (length(res) > l) {
        res <- res[seq(l)]
        warning(paste0(par, " must be of length ", l, "."))
    }
    return(res)
}

check_integer <- function(x, l = 1, min = 1, max = Inf) {
    par <- deparse(substitute(x))
    x <- check_length(par, x, l)
    if (!is.numeric(x)) {
        if (!is.character(x)) {
            stop(paste(par, "is not an integer."))
        }
        x <- as.numeric(x) %>% suppressWarnings()
        if (any(is.na(x)) || is.null(x)) {
            stop(paste(par, "is not an integer."))
        }
    }
    if (any(x < min)) {
        warning(paste0(par, " must be greater than ", min, "."))
    }
    if (any(x > max)) {
        warning(paste0(par, " must be lower than ", max, "."))
    }
    for (i in seq_along(x)) {
        if (x[i] < min) {
            x[i] <- min
        }
        if (x[i] > max) {
            x[i] <- max
        }
    }
    x
}

check_colors <- function(x, l = Inf) {
    par <- deparse(substitute(x))
    x <- check_length(par, x, l)
    f <- function() {
        stop(paste(par, "must be in colors() or in an hexadecimal format."))
    }
    if (!is.null(x) && (is.vector(x) || is.character(x))) {
        for (i in x) {
            if (is.na(i) || is.logical(x) || (
                !(i %in% colors()) &&
                    (as.numeric(i) %>% suppressWarnings() %>% is.na()) &&
                    !grepl("^#{1}[a-zA-Z0-9]{6,8}$", i))
            ) {
                f()
            }
        }
    } else {
        f()
    }
    return(x)
}

check_data_frame <- function(x) {
    if (is.null(x)) {
        stop(paste(deparse(substitute(x)), "is NULL."))
    }
    as.data.frame(x)
}

check_boolean <- function(x, l = 1) {
    par <- deparse(substitute(x))
    x <- check_length(par, x, l)
    if (any(!is(x, "logical") || any(is.na(x)))) {
        stop(paste(par, "is not a boolean."))
    }
    return(x)
}

check_character <- function(x, l = 1) {
    x <- check_length(deparse(substitute(x)), x, l) %>% paste()
    if (length(x) < 1) {
        stop(paste(deparse(substitute(x), "is NULL.")))
    }
    return(x)
}

# y <- 0.05
# check_type(y, "numeric")
# check_type(y, "logical")
# "y must be a bool."
check_type <- function(x, y) {
    if (length(y) == 1) {
        y0 <- paste("a", y)
    } else {
        y0 <- paste("among", paste(y, collapse = ", "))
    }
    if (!any(class(x) %in% y)) {
        stop(paste0(deparse(substitute(x)), " must be ", y0, "."))
    }
}

check_choices <- function(x, y) {
    if (is.null(x) || !x %in% y) {
        stop(
            paste0(
                deparse(substitute(x)),
                " must be among ",
                paste(y, collapse = ", "),
                "."
            )
        )
    }
}
