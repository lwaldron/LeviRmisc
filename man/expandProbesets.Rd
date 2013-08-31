\name{expandProbesets}
\alias{expandProbesets}
\title{## expand featureNames of an ExpressionSet that are 2+ delimited names.}
\description{If a single probeset is collapsed to multiple ids, for example
ZNRF2///MIR550-1, then this will create an ExpressionSet with single
ids. ZNRF2 and MIR550-1 will be two identical rows in the exprs().
Useful for accessing genes. If ids are not unique, the first instance is
chosen. Priority is given however to probesets with single ids (i.e. if
there is a "ZNRF2" and a "ZNRF2///MIR550-1" probe, the uncollapsed
ExpressionSet will have the first probe ("ZNRF2") and the second
("MIR550-1"), not the second probe twice).  Function by Markus Riester.}
\usage{expandProbesets(eset, sep = ";")}
\arguments{
  \item{eset}{ExpressionSet to uncollapse.}
  \item{sep}{The string that separates multiple probes}
}



\author{Levi Waldron and Markus Riester}





