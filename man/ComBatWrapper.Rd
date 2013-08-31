\name{ComBatWrapper}
\alias{ComBatWrapper}
\title{Wrapper for the ComBat.R function}
\description{Applies the ComBat batch correction algorithm without having to
manually create the input files.
Function by Levi Waldron.}
\usage{ComBatWrapper(expr.uncorrected, batchvar, combat.source = "http://www.bu.edu/jlab/wp-assets/ComBat/Download_files/ComBat.R", 
    cleanup = TRUE, ...)}
\arguments{
  \item{expr.uncorrected}{A matrix of uncorrected expression values, with samples in columns and features in rows}
  \item{batchvar}{A character string of length equal to the number of columns in expr.uncorrected, defining the batches}
  \item{combat.source}{A location where the ComBat.R script can be found}
  \item{cleanup}{If TRUE, clean up intermediate files when done}
  \item{\dots}{... are extra arguments passed to the ComBat function}
}

\value{a matrix of batch-corrected values, of the same dimensions as the
input expr.uncorrected.  Note that this script also makes a plot,
unless prior.plots=FALSE is passed in the ...}

\author{Levi Waldron and Markus Riester}





