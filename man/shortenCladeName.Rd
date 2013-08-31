\name{shortenCladeName}
\alias{shortenCladeName}
\title{Shorten Maaslin-compliant or QIIME-compliant taxonomical clade names}
\description{Keeps only the last informative bit (containing a __, plus taxid
integer if present), for nicer plotting.
Function by Levi Waldron.}
\usage{shortenCladeName(x, separator = "|")}
\arguments{
  \item{x}{A vector of QIIME taxonomic names, pipe-separated.}
  \item{separator}{The character used to separate levels of the taxonomy: ";" for QIIME, "|" for Maaslin.}
}

\value{a vector the same length as x, providing only the leaf-level
names of the taxonomy.}

\author{Levi Waldron and Markus Riester}





