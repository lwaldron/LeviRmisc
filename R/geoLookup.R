geoLookup <- structure(function ### Look up basic experiment and platform information from GEO series or dataset identifiers
### This function uses recursion to handle multiple identifiers, so
### very large number of identifiers might consume appreciable memory.
### Function by Levi Waldron.
(id,
### A character string of one or more GSE or GDS identifiers.
### Optionally, a platform can be specified by appending it to the
### series identifier, separated by a hyphen, eg: GSE1722-GPL96.
 con=NULL
### Connection to a GEOmetadb SQLite database.  Will be created if not provided.
 ){
    ## Recurse for more than one ID.  This could be done more
    ## efficiently through more complicated db lookup, but this is
    ## already instantaneous for the dozens of IDs.
    if(length(id) > 1)
        return( t(sapply(id, geoLookup)) )
    ##blank vector to store info:
    output.names <- c("pubMedIds", "platform_accession", "platform_summary", "platform_title",
                      "platform_technology", "platform_distribution", "platform_manufacturer",
                      "series_title", "series_summary", "series_overall_design", "series_contributor")
    lookup <- as.list(rep(NA, length(output.names)))
    names(lookup) <- output.names
    if (!identical(grepl("^GSE|GDS[0-9]+$", id), TRUE))  ##if not a series ID, return the NA vector:
        return(do.call(c, lookup))
    ## con = GEOmetadb connection.  If NULL, it will be set up.
    ## Note that GEOmetadb does not export dbGetQuery, so the package must be loaded explicitly.
    ##Open connection to GEOmetadb if not provided:
    if(is.null(con)){
        if(file.exists("GEOmetadb.sqlite")){
            sqlfile <- "GEOmetadb.sqlite"
        }else{
            sqlfile = getSQLiteFile(destdir=".")
        }
        con = dbConnect(SQLite(), sqlfile)
    }
    ##Deal with the case of GSEnnnn-GPLnnn
    if(grepl("-", id)){
        ##Split if the id has form GSEnnnn-GPLnnn
        id.split <- strsplit(id, split="-")[[1]]
        gse.id <- id.split[1]
        if(grepl("GPL", id.split[2]))
            gpl.id <- id.split[2]
    }else{
        ##If the id is just GSEnnnn
        gse.id <- id
        gpl.id <- dbGetQuery(con, paste("select gpl from gse_gpl where gse='", id, "'", sep="") )[, 1]
        if(length(gpl.id) > 1)
            gpl.id <- paste(gpl.id, collapse="|")
    }
    ## GSE lookup
    gse.query <- dbGetQuery(con, paste("select * from gse where gse='", gse.id, "'", sep="") )
    if(nrow(gse.query) == 0)
        return(do.call(c, lookup))
    ## Platform lookup.  Sometimes there is more than one platform.
    gpl.ids.split <- strsplit(gpl.id, split="\\|")[[1]]
    gpl.query <- dbGetQuery(con, paste("select * from gpl where gpl='", paste(gpl.ids.split, collapse="' OR gpl='"), "'", sep=""))
    gpl.query <- gpl.query[match(gpl.ids.split, gpl.query$gpl), ]
    if(nrow(gpl.query) > 1){
        ## This covers cases where the GSE is associated with
        ## multiple GPL, but a GPL has not been specified using the
        ## GSEnnnn-GPLnnn convention.
        gpl.query <- sapply(gpl.query, paste, collapse="|")
        gpl.query <- matrix(data=gpl.query, nrow=1, dimnames=list(rownames=NULL, colnames=names(gpl.query)))
        gpl.query <- data.frame(gpl.query, stringsAsFactors=FALSE)
    }
    ## Finally populate lookup:
    lookup["platform_summary"] <- gpl.query["bioc_package"]
    lookup["platform_accession"] <- gpl.id
    lookup["pubMedIds"] <- gse.query["pubmed_id"]
    lookup["platform_title"] <- gpl.query["title"]
    lookup["platform_technology"] <- gpl.query["technology"]
    lookup["platform_distribution"] <- gpl.query["distribution"]
    lookup["platform_manufacturer"] <- gpl.query["manufacturer"]
    lookup["series_title"] <- gse.query["title"]
    lookup["series_summary"] <- gsub("\t", " ", gse.query["summary"])
    lookup["series_overall_design"] <- gse.query["overall_design"]
    lookup["series_contributor"] <- gsub("\t", " ", gse.query["contributor"])
    return(do.call(c, lookup))
### If one ID is provided, a character vector of platform and series
### information retrieved through GEOmetadb.  If multiple IDs are
### provided, a matrix with one row per ID.
}, ex=function(){
    ## Takes too long to run for the first time in an example, because
    ## the sqlite file containing all GEO metadata has to be downloaded.
    if( interactive() ){
        geoLookup("GSE100")
        geoLookup("GSE1722-GPL96")
        geoLookup("GSE1722-GPL1384")
        geoLookup(c("GSE1722-GPL96", "GSE1722-GPL1384", "GSE100"))
    }
})
