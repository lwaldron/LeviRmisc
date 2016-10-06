makeCLS <- function #function to create a valid cls file
### This function makes a valid CLS file for phenotype data, as
### specified by the BROAD Institute for use with its GSEA tool.
### Function by Levi Waldron.
(vec,
### a vector of any class
 fname,
### Fully specified filename to which the CLS file will be written.
 varname=NULL,
### variable name, needed only for numeric variables.
 continuousCategories=NULL
### number of categories to split numeric data into.  Ignored for
### non-numeric data, or if equal to NULL.
 ){
  if (class(vec) == "numeric" & !is.null(continuousCategories))
    {
      if (continuousCategories < 2) stop("continuousCategories should be an integer greater than or equal to 2.")
      continuousCategories <- round(continuousCategories)
      print(paste("discretizing into",continuousCategories,"categories."))
      quantiles <- quantile(vec,probs=seq(0,1,length.out=continuousCategories+1),na.rm=TRUE)
      vec <- cut(vec,quantiles,include.lowest=TRUE,labels=1:continuousCategories)
    }
  if (class(vec) == "numeric")
    {
      line1 <- "#numeric"
      line2 <- paste("#",varname,sep="")
    }else{
      if(!is(vec, "factor")) stop("vec should be numeric or factor")
      line1 <- c(length(vec), length(levels(vec)), 1)
      line2 <- c("#", levels(vec))
    }
  line3 <- as.integer(vec) - 1
  write.table(t(line1),file=fname,sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE)
  write.table(t(line2),file=fname,sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
  write.table(t(line3),file=fname,sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
  return(NULL)
### returns NULL; this function is called in order to write a valid .cls file.
}
