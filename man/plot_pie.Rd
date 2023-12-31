% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_pie.R
\name{plot_pie}
\alias{plot_pie}
\title{Piechart}
\usage{
plot_pie(
  x,
  title = NULL,
  width_text = 5,
  width_title = 20,
  colour = get_colors(),
  digits = 0.1,
  cex = 15,
  cex_main = cex * 1.5,
  hsize = 1.2,
  legend = TRUE,
  sample_size = NULL,
  collapse = FALSE,
  threshold = 5,
  t = -0.5,
  l = -1,
  r = -1,
  b = -1
)
}
\arguments{
\item{x}{Vector of character values visualized on the plot.}

\item{title}{Character for the title.}

\item{width_text}{Integer for the maximum length of the subtitle(s).}

\item{width_title}{Integer for the maximum length of the title.}

\item{colour}{Color or vector of colors for the categories.}

\item{digits}{Integer for the number of decimals.}

\item{cex}{Double for the magnification factor for the text relative to the
default.}

\item{cex_main}{Double for the magnification factor for the subtitles
relative to the default.}

\item{hsize}{Double for the size of the central hole in the pie chart
(in [1, 2]).}

\item{legend}{Boolean to toggle the display of the legend.}

\item{sample_size}{Integer for the sample size of the dataset to calculate
percentages (if different from the length of the variable).}

\item{collapse}{Boolean to merge categories with identical proportions.}

\item{threshold}{Double for the minimal percentage value before being
hidden on the plot.}

\item{t, r, b, l}{Dimensions of each margin. (To remember order, think trouble).}
}
\value{
A ggplot object.
}
\description{
Visualize the proportions of a categorical variable using a piechart
}
\examples{
library(magrittr)
library(RColorBrewer)

# Default parameters
x <- c(rep("A", 5), rep("B", 4))
plot_pie(x)

# Advanced parameters
k <- 10
n <- runif(k, 1, 10) \%>\% round()
x <- paste("Level", seq(k)) \%>\%
    mapply(function(x, y) rep(x, y), ., n) \%>\%
    unlist()
plot_pie(
    x,
    title = "Some categorical variable",
    width_text = 5,
    width_title = 20,
    colour = brewer.pal(9, "Reds"),
    cex = 20,
    digits = 1,
    hsize = 1.5,
    collapse = TRUE,
    b = 3
)

}
