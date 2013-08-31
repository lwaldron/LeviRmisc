\name{makeTXT}
\alias{makeTXT}
\title{function to create a valid cls file}
\description{This function makes a valid TXT file for expression data, as
specified by the BROAD Institute for use with its GSEA tool.
Function by Levi Waldron.}
\usage{makeTXT(mat, fname)}
\arguments{
  \item{mat}{a numeric matrix }
  \item{fname}{Fully specified filename to which the TXT file will be written.}
}

\value{returns NULL; this function is called in order to write a valid .cls file.}

\author{Levi Waldron and Markus Riester}





