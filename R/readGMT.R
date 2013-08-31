readGMT <- function #Read a gmt (gene matrix transposed) file
### Reads a gmt (gene matrix transposed) file as provided by mSigDB or
### geneSigDB, to an R list object.
### Function by Levi Waldron.
(fname,
### fully specified path to a .gmt file, ie from mSigDB or geneSigDB
 name.column=1
### name.column = which column to use for names (typically 1 or 2).
 ){
  if (! (name.column == 1 | name.column == 2))
    stop("name.column should be 1 or 2")
  x <- readLines(fname)
  x <- strsplit(x,split="\t")
  output <- lapply(x,function(y) y[-1:-2])
  names(output) <- make.names(sapply(x,function(y) y[name.column]),unique=TRUE)
  return(output)
### a list object where each element is a string of genes, and the
### elements are named by the first column of the .gmt file.
}
