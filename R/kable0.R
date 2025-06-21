#' Simplified kable table formatting
#'
#' Creates a minimalist kable table with standardized formatting, including left-aligned
#' row names and customizable column alignment. Provides quick styling for consistent
#' table outputs in R Markdown documents.
#'
#' @inheritParams kableExtra::kbl
#'
#' @details
#' This wrapper around \code{kableExtra} provides:
#' \itemize{
#'   \item First column in bold with custom color (typically for row names)
#'   \item Minimalist styling without vertical lines
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' kable0(head(mtcars))
#'
#' # Custom alignment and color
#' kable0(head(iris), align = "r", color = "#2b8cbe")
#'
#' # In R Markdown chunk
#' ```{r}
#' kable0(head(cars), align = "l")
#' ```
#' }
#'
#' @return A kableExtra table object with specified formatting.
#' @seealso
#' \code{\link[kableExtra]{kable}}, \code{\link[kableExtra]{kable_styling}}
#' @export
kable0 <- function(x, align = "c", color = "#a9a9a9") {
    x %>%
        kbl(escape = FALSE, align = c("l", rep(align, ncol(x) -1))) %>%
        kable_minimal(full_width = FALSE) %>%
        column_spec(1, bold = TRUE, color = color)
}
