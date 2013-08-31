makeTXT <- function  #function to create a valid cls file
### This function makes a valid TXT file for expression data, as
### specified by the BROAD Institute for use with its GSEA tool.
### Function by Levi Waldron.
(mat,
### a numeric matrix 
 fname
### Fully specified filename to which the TXT file will be written.
 ){
  line1 <- c("NAME",colnames(mat))
  write.table(t(line1),file=fname,sep="\t",col.names=FALSE,row.names=FALSE,quote=FALSE)
  write.table(mat,file=fname,sep="\t",col.names=FALSE,row.names=TRUE,quote=FALSE,append=TRUE)
  return(NULL)
### returns NULL; this function is called in order to write a valid .cls file.
}
