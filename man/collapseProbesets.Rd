\name{collapseProbesets}
\alias{collapseProbesets}
\title{## Wrapper for jetset or possibly other}
\description{methods for collapsing probesets to genes.  Create a tab separated
file for the phenoData slot. Used for Level 1 Affymetrix data.
Note that the jetset package must be installed but currently
available only from http://www.cbs.dtu.dk/biotools/jetset/.
Function by Markus Riester.}
\usage{collapseProbesets(eset, method = c("jetset"))}
\arguments{
  \item{eset}{ExpressionSet with probe sets ids as features. Requires as valid
annotation slot for the mapping to gene symbols (rma/frma normalized
ExpressionSets have that slot).}
  \item{method}{The collapsing method.  "jetset" is a wrapper for the Jetset package.}
}

\value{ExpressionSet with probe sets mapped to gene symbols.}

\author{Levi Waldron and Markus Riester}





