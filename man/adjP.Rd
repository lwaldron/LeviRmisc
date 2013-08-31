\name{adjP}
\alias{adjP}
\title{user-friendly wrapper to the multtest package to adjust for multiple hypothesis testing}
\description{Uses the multtest package with a friendlier interface.  x is your
vector of raw p values, and the output is the corrected vector with
the order maintained.
Function by Levi Waldron.}
\usage{adjP(x, proc = "BH", ...)}
\arguments{
  \item{x}{raw p-values}
  \item{proc}{procedure for correcting for multiple testing, "BH" by default for Benjamini-Hochberg.  See ?mt.rawp2adjp from multtest for options}
  \item{\dots}{additional arguments passed to mt.rawp2adjp}
}

\value{a matrix containing columns for raw and adjusted p-values}

\author{Levi Waldron and Markus Riester}




\examples{
rawp <- c(0.01, 0.05, 0.1, 0.2, 0.3)
adjP(rawp)
}
