\name{getGEO2}
\alias{getGEO2}
\title{## Wrapper for GEOquery::getGEO }
\description{This function combines the list elements returned by getGEO 
(when possible), and prunes the phenoData.}
\usage{getGEO2(...)}
\arguments{
  \item{\dots}{Arguments passed on to GEOquery::getGEO}
}

\value{ExpressionSet fetched from GEO.}

\author{Levi Waldron and Markus Riester}



\seealso{GEOquery::getGEO}
\examples{
getGEO2("GSE27561")
}
