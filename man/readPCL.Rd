\name{readPCL}
\alias{readPCL}
\title{Read numeric plus metadata from a .pcl file}
\description{This function assumes that the first row is a header.  It can
attempt to automatically determine which rows are metadata and
which are phenotype data, or the number of phenotype rows can be
specified explicitly.
Function by Levi Waldron.}
\usage{readPCL(filename, number.pheno.rows = NA, ...)}
\arguments{
  \item{filename}{fully specified path to a .pcl file}
  \item{number.pheno.rows}{If number.pheno.rows is NA, this function will figure out the
number of rows of phenotypes, under the assumption that 5
consecutive rows of numeric values signifies the start of the
numeric data at the startn of those consecutive rows.  If
number.pheno.rows is specified, it tells the program how many rows
at the start of the file (not including the header) are phenotype
data.  }
  \item{\dots}{... are arguments passed on to read.table()}
}

\value{a list object containing two elements: phenodat, a dataframe
containing the potentially non-numeric phenotype data, and
numericdat, a matrix containing the numeric expression data.}

\author{Levi Waldron and Markus Riester}





