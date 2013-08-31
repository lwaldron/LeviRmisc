readPCL <- function  # Read numeric plus metadata from a .pcl file
### This function assumes that the first row is a header.  It can
### attempt to automatically determine which rows are metadata and
### which are phenotype data, or the number of phenotype rows can be
### specified explicitly.
### Function by Levi Waldron.
(filename,
### fully specified path to a .pcl file
number.pheno.rows=NA,
### If number.pheno.rows is NA, this function will figure out the
### number of rows of phenotypes, under the assumption that 5
### consecutive rows of numeric values signifies the start of the
### numeric data at the startn of those consecutive rows.  If
### number.pheno.rows is specified, it tells the program how many rows
### at the start of the file (not including the header) are phenotype
### data.  
...
### ... are arguments passed on to read.table()
 ){
    whichPheno <- function(filename, n, consecutive.numeric = 0, 
        ...) {
        if (consecutive.numeric >= 5) {
            return(n - consecutive.numeric + 1)
        }
        else {
            one.row <- read.table(filename, skip = n, nrows = 1, 
                ...)
            one.row <- t(one.row)[, 1]
            if (is.numeric(one.row)) {
                return(whichPheno(filename, n + 1, consecutive.numeric + 
                  1, ...))
            }
            else {
                return(whichPheno(filename, n + 1, consecutive.numeric, 
                  ...))
            }
        }
    }
    if (is.na(number.pheno.rows)) {
        number.pheno.rows <- whichPheno(filename, n = 1, consecutive.numeric = 0, 
            ...)
    }
    phenodat <- read.delim(filename, nrows = number.pheno.rows, header = TRUE, ...)
    numericdat <- read.delim(filename, skip = number.pheno.rows + 1, header = FALSE, row.names=1, ...)
    colnames(numericdat) <- colnames(phenodat)[-1]
    numericdat <- as.matrix(numericdat)
    tmp.file <- tempfile()
    write.table(t(phenodat), file = tmp.file, col.names=FALSE, sep=",")
    phenodat <- read.csv(tmp.file, row.names = 1, ...)
    file.remove(tmp.file)
    output <- list(phenodat = phenodat, numericdat = numericdat)
    return(output)
### a list object containing two elements: phenodat, a dataframe
### containing the potentially non-numeric phenotype data, and
### numericdat, a matrix containing the numeric expression data.
}
