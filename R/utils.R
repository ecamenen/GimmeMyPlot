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
             brewer.pal(9, "RdBu")[seq(4)],
             brewer.pal(11, "PiYG")[4:1],
             brewer.pal(11, "PRGn")[seq(4)],
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
