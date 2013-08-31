\name{makeCLS}
\alias{makeCLS}
\title{function to create a valid cls file}
\description{This function makes a valid CLS file for phenotype data, as
specified by the BROAD Institute for use with its GSEA tool.
Function by Levi Waldron.}
\usage{makeCLS(vec, fname, varname = NULL, continuousCategories = NULL)}
\arguments{
  \item{vec}{a vector of any class}
  \item{fname}{Fully specified filename to which the CLS file will be written.}
  \item{varname}{variable name, needed only for numeric variables.}
  \item{continuousCategories}{number of categories to split numeric data into.  Ignored for
non-numeric data, or if equal to NULL.}
}

\value{returns NULL; this function is called in order to write a valid .cls file.}

\author{Levi Waldron and Markus Riester}





