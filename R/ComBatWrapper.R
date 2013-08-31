ComBatWrapper <- function #Wrapper for the ComBat.R function
### Applies the ComBat batch correction algorithm without having to
### manually create the input files.
### Function by Levi Waldron.
(expr.uncorrected,
### A matrix of uncorrected expression values, with samples in columns and features in rows
 batchvar,
### A character string of length equal to the number of columns in expr.uncorrected, defining the batches
 combat.source="http://www.bu.edu/jlab/wp-assets/ComBat/Download_files/ComBat.R",
### A location where the ComBat.R script can be found
 cleanup=TRUE,
### If TRUE, clean up intermediate files when done
 ...
 ### ... are extra arguments passed to the ComBat function
 ){
    source(combat.source)
    sam.info <- data.frame("Array name"=colnames(expr.uncorrected),
                           "Sample name"=colnames(expr.uncorrected),
                           Batch=batchvar,
                           check.names=FALSE)
    write.table(expr.uncorrected, "combat_expression_xls.txt", 
                quote=FALSE, sep="\t", row.names=TRUE)
    write.table(sam.info, file="combat_sample_info_file.txt",
                quote=FALSE,sep="\t",row.names=FALSE)
    ComBat(expression_xls="combat_expression_xls.txt",
           sample_info_file="combat_sample_info_file.txt",
           skip=1, ...)
    expr.corrected <- 
        as.matrix(
                  read.delim(paste("Adjusted_combat_expression_xls.txt_.xls",sep=""), 
                             row.names=1))
    if(cleanup)
        unlink(c("combat_expression_xls.txt", 
                 "combat_sample_info_file.txt", 
                 "Adjusted_combat_expression_xls.txt_.xls"))
    return(expr.corrected)
### a matrix of batch-corrected values, of the same dimensions as the
### input expr.uncorrected.  Note that this script also makes a plot,
### unless prior.plots=FALSE is passed in the ...
}
