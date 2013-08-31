\name{writePCL}
\alias{writePCL}
\title{Write a PCL file}
\description{Convenience function for writing a matrix or dataframe to a
Maaslin-compatible PCL file.
Function by Levi Waldron.}
\usage{writePCL(obj, filename, transpose = TRUE, topleft.txt = "sample", 
    ...)}
\arguments{
  \item{obj}{Matrix or dataframe to write to file.}
  \item{filename}{Output filename.}
  \item{transpose}{Transpose before writing?}
  \item{topleft.txt}{Need a word to put at the top of the first column.}
  \item{\dots}{Extra arguments passed to write.table.}
}

\value{No output, function is called to write to file.}

\author{Levi Waldron and Markus Riester}





