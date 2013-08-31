writePCL <- function # Write a PCL file
### Convenience function for writing a matrix or dataframe to a
### Maaslin-compatible PCL file.
### Function by Levi Waldron.
(obj,
### Matrix or dataframe to write to file.
 filename,
### Output filename.
 transpose=TRUE,
### Transpose before writing?
 topleft.txt="sample",
### Need a word to put at the top of the first column.
 ...
### Extra arguments passed to write.table.
 ){
    obj <- as.matrix(obj)
    if(transpose)
        obj <- t(obj)
    tmp <- cbind(rownames(obj), obj)
    tmp <- rbind(colnames(tmp), tmp)
    tmp[1, 1] <- topleft.txt
    write.table(tmp, file=filename, sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, ...)
### No output, function is called to write to file.
}
