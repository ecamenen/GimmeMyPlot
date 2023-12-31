% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_histogram.R
\name{plot_histogram}
\alias{plot_histogram}
\title{Plot histogram}
\usage{
plot_histogram(
  x,
  title = NULL,
  width_title = 20,
  color = "red",
  color_title = color,
  color_stats = "black",
  cex = 1,
  cex_axis = 17 * cex,
  cex_main = 21 * cex,
  cex_sub = 15 * cex,
  digits = 0,
  subtitle = TRUE,
  probs = c(0.25, 0.75),
  binwidth = 1.5
)
}
\arguments{
\item{x}{Vector of numerical values visualized on the plot}

\item{title}{Character for the title.}

\item{width_title}{Integer for the maximum length of the title.}

\item{color}{Color for the plot}

\item{color_title}{Color for the title.}

\item{color_stats}{Color for the median and quantile lines}

\item{cex}{Double for the magnification factor for the text relative to the
default.}

\item{cex_axis}{Double for the magnification factor for the axis labels
relative to the default.}

\item{cex_main}{Double for the magnification factor for the subtitles
relative to the default.}

\item{cex_sub}{Double for the magnification factor for the main title
relative to the default.}

\item{digits}{Integer for the number of decimals.}

\item{subtitle}{Boolean to display the subtitle.}

\item{probs}{Double vector for the probabilities (in [0, 1]).}

\item{binwidth}{Double for the number of bins}
}
\value{
A ggplot object.
}
\description{
Visualize the distribution of single variable using histogram
}
\examples{
# Default parameters
x <- rnorm(100)
plot_histogram(x)

# Advanced parameters
plot_histogram(
    x,
    title = "Some numerical variable",
    width_title = 15,
    color = "blue",
    color_title = "orange",
    color_stats = "orange",
    cex = 1.2,
    digits = 1,
    binwidth = 0.5
)
}
