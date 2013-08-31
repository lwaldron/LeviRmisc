\name{multiHist}
\alias{multiHist}
\title{## Plot one or more histograms on the same axes using lines}
\description{If obj is a vector, this function acts like graphics::hist, but
only sketches a line along the top of the histogram, rather than
plotting bars.  If obj is a matrix or dataframe, it creates a line
for each column.}
\usage{multiHist(obj, lty = 1:ncol(obj), lwd = rep(1, ncol(obj)), col = rep(1, 
    ncol(obj)), ...)}
\arguments{
  \item{obj}{A numeric/integer vector, dataframe, or matrix.  If obj is a
dataframe or matrix, a histogram line will be created for each
column.}
  \item{lty}{lty for each histogram line.}
  \item{lwd}{lwd for each histogram line.}
  \item{col}{col for each histogram line.}
  \item{\dots}{Extra arguments passedon to graphics::hist}
}

\value{Invisibly returns the output of hist() obj (vector) or the first
column of obj (matrix or dataframe).}

\author{Levi Waldron and Markus Riester}




\examples{
x <- matrix(rnorm(2000), ncol=2)
x[, 2] <- x[, 2] - 2
multiHist(x)
multiHist(x[, 1])
multiHist(round(x[, 1] * 1000), xlab="x*1000", main="Integer input")
}
