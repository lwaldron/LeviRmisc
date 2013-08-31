\name{fold}
\alias{fold}
\title{fold - also known variously as reduce, accumulate, compress, or inject}
\description{http://en.wikipedia.org/wiki/Fold_\%28higher-order_function\%29
shamelessly copied from this post by Hadley Wickam:
https://stat.ethz.ch/pipermail/r-help/2007-April/130462.html}
\usage{fold(x, fun)}
\arguments{
  \item{x}{a list of vectors}
  \item{fun}{a function to apply recursively to x}
}

\value{result of x fold fun.}

\author{Levi Waldron and Markus Riester}




\examples{
 a <- list(c(1,3,5), c(1,3), c(1, 2, 5, 6))
fold(a, intersect) 
}
