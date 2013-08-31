getGEO2 <- structure(function ### Wrapper for GEOquery::getGEO 
### This function combines the list elements returned by getGEO 
### (when possible), and prunes the phenoData.
(...
### Arguments passed on to GEOquery::getGEO
 ){
    ## combine ExpressionSets if there are multiple
    if( is(try(eset.geoquery <- try(getGEO(...))), "try-error"))
        stop("Could not fetch GEO series")
    if( is(try(eset <- Reduce(Biobase::combine, eset.geoquery)), "try-error")){
        warning("Could not combine GEO series, returning GEOquery object as-is")
        return(eset.geoquery)
    }
    ids <- grep("characteristics_ch", varLabels(eset))
    if(length(ids) == 0){
        warning("No characteristic_ch columns, returning metadata as-is")
        return( eset )
    }
    ## exract clinical variables
    uncurated <- sapply(ids, function(i) eset[[i]])
    ##
    getCol <- function(x,string){
        output <- x[match(string,substr(x,1,nchar(string)))]
        if(length(output)==0) output <- NA
        return(output)
    }
    ##
    getVal <-function(string) {
        gsub(string, "", apply(uncurated,1,getCol,string=string), fixed=TRUE)
    }
    ##
    fields <- apply(uncurated,2,function(x) strsplit(x,": ")[[1]][1])
    ##
    for (i in 1:length(fields)) eset[[make.names(fields)[i]]] <- getVal(paste(fields[i], ": ", sep=""))
    eset
### ExpressionSet fetched from GEO.
##seealso<< GEOquery::getGEO
},ex=function(){
    getGEO2("GSE27561")
})
