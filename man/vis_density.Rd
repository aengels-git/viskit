% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vis_univariate.R
\name{vis_density}
\alias{vis_density}
\title{Density ggplot.}
\usage{
vis_density(
  data,
  x,
  x_label = function(x) {
     x
 },
  x_breaks = pretty_breaks(n = 5),
  y_label = function(x) {
     x
 },
  y_breaks = pretty_breaks(n = 5),
  fontsize = 14,
  col = "#4E79A7",
  density_factor = 1
)
}
\arguments{
\item{data}{A data frame or tibble.}

\item{x}{Unquoted x aesthetic variable.}

\item{x_label}{label function for x}

\item{x_breaks}{break function for x}

\item{y_label}{label function for y}

\item{y_breaks}{break function for x}

\item{fontsize}{general fontsize of labels}

\item{col}{color of density}

\item{density_factor}{adjust the granularity of the density plot, between 1 and 10}
}
\value{
A ggplot object.
}
\description{
Create a Density plot
}
