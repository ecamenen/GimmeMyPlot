#' Custom discrete color palette
#'
#' Generates a visually distinct color palette for categorical/discrete data by
#' combining multiple RColorBrewer palettes.
#'
#' @return A character vector of 11 hexadecimal color codes representing
#'   distinct colors suitable for categorical data visualization.
#'
#' @examples
#' # Get the complete palette
#' colors <- palette_discrete()
#' print(colors)
#'
#' # Visualize the palette
#' plot(1:11, rep(1, 11), pch = 15, cex = 5, col = colors,
#'      xlab = "Color index", ylab = "", yaxt = "n",
#'      main = "Discrete Color Palette")
#' text(1:11, rep(1, 11), 1:11, col = "white", font = 2)
#'
#' # Use in ggplot2
#' \dontrun{
#' library(ggplot2)
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
#'                  color = Species)) +
#'   geom_point(size = 3) +
#'   scale_color_manual(values = palette_discrete()[1:3])
#' }
#'
#' @seealso
#' \code{\link[RColorBrewer]{brewer.pal}} for the source palettes.
#'
#' @export
palette_discrete <- function() {
    c(
        brewer.pal(9, "Set1")[-c(6:7, 9)],
        rev(brewer.pal(7, "Set2")[c(6:5)]),
        brewer.pal(8, "Pastel1")[-c(3, 6:7, 9)]
    )
}

#' Custom continuous color palette
#'
#' Generates a continuous color palette combining multiple RColorBrewer
#' palettes.
#'
#' @param gray Boolean indicating whether to include gray tones at the end of
#'   the palette.
#'
#' @return A colorRampPalette object.
#'
#' @examples
#' # Create a palette with gray tones
#' pal_with_gray <- palette_continuous(gray = TRUE)
#' plot(1:100, col = pal_with_gray(100), pch = 16, cex = 2)
#'
#' # Create a palette without gray tones
#' pal_no_gray <- palette_continuous(gray = FALSE)
#' image(volcano, col = pal_no_gray(50))
#'
#' # Direct usage in ggplot2
#' library(ggplot2)
#' ggplot(data.frame(x = rnorm(1000), y = rnorm(1000))) +
#'   geom_point(aes(x = x, y = y, color = x + y)) +
#'   scale_color_gradientn(colors = palette_continuous()(100))
#'
#' @seealso
#' \code{\link[RColorBrewer]{brewer.pal}} for the source palettes,
#' \code{\link[grDevices]{colorRampPalette}} for gradient generation
#'
#' @export
palette_continuous <- function(gray = TRUE) {
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

#' Capitalize only the first letter
#'
#' Converts the first character of each string to uppercase and leaves all
#' other characters unchanged.
#'
#' @param x A character vector or a single string.
#'
#' @return A character vector of the same length as `x`, where each element
#'   has its first letter capitalized.
#'
#' @examples
#' # Basic usage
#' to_title("hello world")
#' # Returns: "Hello world"
#'
#' # Vectorized operation
#' to_title(c("apple", "banana", "cherry"))
#' # Returns: c("Apple", "Banana", "Cherry")
#'
#' # Edge cases
#' to_title(c("", "a", "A", "123abc", NA, NULL))
#' # Returns: c("", "A", "A", "123abc", NA, NULL)
#'
#' @note
#' This function differs from `tools::toTitleCase()` which capitalizes the
#' first letter of \emph{each} word. Use `to_title()` when you want only the
#' first character of the entire string capitalized.
#'
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

#' Truncate to a maximum width
#'
#' Truncates a string to a maximum width while attempting to preserve complete
#' words. The function intelligently breaks the string at word boundaries
#' (specified by `sep`) to create a truncated version that doesn't exceed
#' the desired width.
#' @param x A character string to format.
#' @param width Integer specifying the maximum width (in characters) for the truncated string.
#'   Must be at least as long as the first word in the string.
#' @param sep Character used to separate words in the string.
#' @return A character vector of the same length as `x`, with each element
#'   formatted.
#'
#' @examples
#' # Basic truncation
#' str_trunc1("The quick brown fox jumps over the lazy dog", width = 20)
#' # Returns: "The quick brown fox"
#'
#' # With different separator
#' str_trunc1("apple,banana,cherry,date,elderberry", width = 15, sep = ",")
#' # Returns: "apple,banana,cherry"
#'
#' # Edge cases
#' str_trunc1("Short", width = 10)  # Returns: "Short" (no truncation needed)
#'
#' # NA handling
#' str_trunc1(NA, width = 10)  # Returns: "NA"
#'
#' @seealso
#' \code{\link{str_trunc0}} for truncation to a number of words,
#' \code{\link{str_pretty}} for pretty truncation.
#'
#' @export
str_trunc1 <- function(x, width = 20, sep = " ") {
    x <- check_character(x)
    sep <- check_character(sep)
    x0 <- strsplit(x, sep)[[1]]
    n <- str_length(x0[[1]])
    width <- check_integer(width, min = n)
    lapply(seq(length(x0)), function(i) str_trunc0(x, i, sep)) %>%
        detect(function(x) str_length(x) <= width, .dir = "backward")
}

#' Truncate to a number of words
#'
#' Extracts the first `n` words from a string, separated by a specified
#' delimiter.
#'
#' @inheritParams str_trunc1
#' @param n Integer specifying the maximum number of words to keep.
#' @param sep Character to separate words in the string.
#'
#' @return A character string containing the first `n` words of the input,
#'   joined by `sep`. If the input has fewer than `n` words,
#'   returns the original string.
#'
#' @examples
#' # Basic usage
#' str_trunc0("The quick brown fox jumps over the lazy dog", n = 4)
#' # Returns: "The quick brown fox"
#'
#' # Different separators
#' str_trunc0("apple,banana,cherry,date,elderberry", n = 3, sep = ",")
#' # Returns: "apple,banana,cherry"
#'
#' @note
#' To count words in a string before truncating, you can use:
#' \code{length(strsplit(trimws(x), sep)[[1]])}
#'
#' @seealso
#' \code{\link{str_trunc1}} for width-based truncation,
#' \code{\link{str_pretty}} for pretty truncation.
#'
#' @export
str_trunc0 <- function(x, n = 5, sep = " ") {
    x <- check_character(x)
    sep <- check_character(sep)
    res <- str_squish(x) %>%
        strsplit(sep) %>%
        .[[1]]
    n <- check_integer(n, max = length(res))
    res[seq(n)] %>%
        paste(collapse = sep)
}

#' Pretty display with intelligent truncation
#'
#' Creates aesthetically formatted strings by trimming, capitalizing, and
#' intelligently truncating text to fit within a specified width while
#' preserving readability. Adds ellipsis (...) when truncation occurs.
#'
#' @inheritParams str_trunc1
#' @inherit str_trunc1 return
#' @param x Character vector of strings to format.
#'
#' @details
#' This function applies a series of formatting steps to each string:
#' 1. **Trim**: Remove extra whitespace.
#' 2. **Capitalize**: Convert first letter to uppercase.
#' 3. **Truncate**: Intelligently truncate to fit `width` using word boundaries
#'    - First attempts truncation using spaces as word separators
#'    - If that fails, tries using hyphens as separators.
#' 4. **Ellipsis**: Adds "..." if the string was truncated.
#'
#' @examples
#' # Basic usage
#' str_pretty("  hello world, this is a test  ", width = 20)
#' # Returns: "Hello world, this..."
#'
#' # Multiple strings
#' fruits <- c("  apple pie recipe  ", "banana split dessert", "cherry tart")
#' str_pretty(fruits, width = 15)
#' # Returns: c("Apple pie...", "Banana split...", "Cherry tart")
#'
#' # Exact fit (no ellipsis)
#' str_pretty("Perfect fit", width = 11)  # Returns: "Perfect fit"
#'
#' # Edge cases
#' str_pretty(NA, width = 10)             # Returns: "NA"
#' str_pretty("   ", width = 10)          # Returns: ""
#'
#' @seealso
#' \code{\link{to_title}} for first-letter capitalization,
#' \code{\link{str_trunc1}} for width-based truncation,
#' \code{\link{str_trunc0}} for truncation to a number of words.
#'
#' @export
str_pretty <- function(x, width = 20) {
    sapply(
        x,
        function(i) {
            i <- str_squish(i) %>% to_title()
            if (is.na(i) | i == "") {
                return(i)
            }
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
