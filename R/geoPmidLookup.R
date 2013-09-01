geoPmidLookup <- structure(function ### Look up metadata for any combination of GEO series and PMID identifiers
### If GEO series IDs are provided, the function will also add data
### from pubmed if the PMID can be found from GEO.
### Function by Levi Waldron.
(id,
### A character string of one or more GSE identifiers and/or
### pubmed IDs.  Pubmed IDs may or may not be preceded by "PMID".
### Optionally, GEO series identifiers can have a platform specified
### by appending it to the series identifier, separated by a hyphen,
### eg: GSE1722-GPL96.
 con=NULL
### Connection to a GEOmetadb SQLite database.  Will be created if not provided.
 ){
    ## The ncbiPubmed function is modified from the library(genomes),
    ## because I didn't like it automatically truncating authors lists
    ## larger than 3.
    ncbiPubmed <- function (term, abstract = FALSE){
        if (length(term) > 1) {
            term <- paste(term, collapse = ",")
        }
        if (grepl("^[0-9, ]*$", term)) {
            x <- efetch(term, "pubmed", retmode = "xml")
        }
        else {
            x <- efetch(esearch(term, "pubmed"), retmode = "xml")
        }
        doc <- xmlParse(x)
        z <- getNodeSet(doc, "//PubmedArticle")
        n <- length(z)
        if (n == 0) {
            stop("No results found")
        }
        pubs <- vector("list", n)
        for (i in 1:n) {
            z2 <- xmlDoc(z[[i]])
            pmid <- as.numeric(xvalue(z2, "//PMID"))
            a1 <- xpathSApply(z2, "//Author/LastName", xmlValue)
            a2 <- xpathSApply(z2, "//Author/Initials", xmlValue)
            a3 <- paste(a1, a2)
            authors <- paste(a3, collapse = ", ")
            year <- as.numeric(xvalue(z2, "//PubDate/Year"))
            title <- xvalue(z2, "//ArticleTitle")
            title <- gsub("\\.$", "", title)
            journal <- xvalue(z2, "//ISOAbbreviation")
            volume <- xvalue(z2, "//Volume")
            pages <- xvalue(z2, "//Pagination")
            pubdate <- xvalue(z2, "//PubDate")
            artdate <- xvalue(z2, "//ArticleDate")
            abstracttext <- xvalue(z2, "//AbstractText")
            pubs[[i]] <- data.frame(pmid, authors, year, title, journal, 
                                    volume, pages, pubdate, artdate, abstracttext, stringsAsFactors = FALSE)
            free(z2)
        }
        x <- do.call("rbind", pubs)
        if (!abstract) 
            x <- x[, -10]
        x
    }    
    ##id = GSE or PMID identifiers
    lookup <- geoLookup(id, con=con)
    if(any(grepl("character|logical", class(lookup))))
        lookup <- data.frame(matrix(lookup, nrow=1, dimnames=list("id", names(lookup))), stringsAsFactors=FALSE)
    if(is(lookup, "matrix"))
        lookup <- data.frame(lookup, stringsAsFactors=FALSE)
    is.pmid <- grepl("^(PMID)?[0-9]+$", id)
    if(any(is.pmid))
        lookup[is.pmid, "pubMedIds"] <- sub("pmid", "", id[is.pmid], ignore.case=TRUE)
    if(any(!is.na(lookup[, "pubMedIds"]))){
        pmid.dat <- ncbiPubmed(na.omit(lookup[, "pubMedIds"]))
        lookup <- cbind(lookup, pmid.dat[match(lookup[, "pubMedIds"], pmid.dat$pmid), ])
        lookup <- lookup[, !grepl("pmid", colnames(lookup))]
    }
    if(is(lookup, "data.frame") && nrow(lookup) == 1){
        output <- t(lookup)[, 1]
        names(output) <- colnames(lookup)
    }else{
        output <- lookup
    }
    return(output)
### A character vector or dataframe of platform, series, and publication
### information retrieved through GEOmetadb and Pubmed.  If multiple IDs are
### provided, the dataframe will have one row per ID.
}, ex=function(){
    ## Takes too long to run for the first time in an example, because
    ## the sqlite file containing all GEO metadata has to be downloaded:
    if( interactive() ){
        ids <- c("GSE100", "GSE1000", "PMID100", "PMID1000")
        geoLookup(ids)
    }
})

