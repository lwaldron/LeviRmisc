\name{writeGMT}
\alias{writeGMT}
\title{Create a gmt (gene matrix transposed) file}
\description{Createss a gmt (gene matrix transposed) file such as those
provided by mSigDB or geneSigDB, from an R list object.
Function by Levi Waldron.}
\usage{writeGMT(object, fname)}
\arguments{
  \item{object}{R list object that will be converted to GMT file.  Each element
should contain a vector of gene names, and the names of the
elements will used for the gene set names}
  \item{fname}{Output file name for .gmt file}
}

\value{Called for the effect of writing a .gmt file}

\author{Levi Waldron and Markus Riester}





