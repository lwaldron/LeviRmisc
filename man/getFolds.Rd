\name{getFolds}
\alias{getFolds}
\title{split N samples into nfolds folds.}
\description{Convenient function for cross-validation
Function by Levi Waldron.}
\usage{getFolds(N, nfolds)}
\arguments{
  \item{N}{number of samples}
  \item{nfolds}{number of folds}
}

\value{integer vector indicating to which fold each sample is assigned.}

\author{Levi Waldron and Markus Riester}




\examples{
getFolds(5, 3)
}
