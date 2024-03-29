% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ManhattanPlot.R
\name{ManhattanPlot}
\alias{ManhattanPlot}
\title{Depicting Manhattan plots for GWAS without ggplot2}
\usage{
ManhattanPlot(chr, pos, p, ...)
}
\arguments{
\item{chr}{Chromosome number. It should have the same length as \code{pos} and \code{p}.}

\item{pos}{Positions on the chromosomes.}

\item{p}{A vector of p-values ranging from 0 to 1. If it is outside 0 or 1, p is not log10-transformed.}

\item{...}{Other arguments passed to plot().}
}
\description{
A simple plot function to draw Manhattan plot for GWAS results.
}
\author{
Yasuhiro Sato (\email{sato.yasuhiro.36c@kyoto-u.jp})
}
