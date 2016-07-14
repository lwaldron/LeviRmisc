efetch <-function(id, db="pubmed", rettype="", retmode="text", seq_stop=700, ...)
{  
   email <- Sys.getenv("email")  
   if(email == ""){print("WARNING: please set your email using Sys.setenv(email='name@email.com')" ) }
   # ID can be a comma-separated list of ids or the default results from esearch
   if(class(id)[1]=="EntrezHistory"){
      # is db always the same as ESearch db?
      opts<-c(db=id$db, query_key = id$query_key, WebEnv = id$WebEnv)
      
   }else{
      id <- gsub(" ", "", id) # remove spaces
      if(is.vector(id)) id<-paste(id, collapse=",")
      opts<-c( id=id, db=db)
   }
   opts <- c(email=email, tool="efetch.R", opts, rettype=rettype, retmode=retmode, seq_stop=seq_stop, ...)
   opts <- paste( paste(names(opts), opts, sep="="), collapse="&")  
   if( any(duplicated(names(opts)))){ stop("Duplicated keys are not allowed in url strings")}
   fetch <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
   fetch <- paste(fetch, opts, sep = "?")
   # print(fetch)

   # if retmode=xml-  will complain about  incomplete final line - use getURL 
   gp <- readLines(fetch)
   gp
}
# enaSRA requires these 3 functions to loop through samples (returning NA if missing) and MATCH:

# 1) values matching tag
#     <TAXON_ID>520450</TAXON_ID>

xvalue <- function(doc, tag) {
        n <- xpathSApply(doc, tag, xmlValue)
        if ( length(n)>0 ) 
           ## if multiple values, return first (for pubmed) or paste?
           n[1]  
           #paste(n, collapse=",")
        else NA
    }

# 2) attributes within tags, for example, find the center within the SAMPLE tag
# <SAMPLE accession="SRS000899" center_name="Broad Institute, Cambridge, MA, USA" alias="24604.0"> 


xattr <-function(doc, tag, att) {
      y <- xpathApply(doc, tag, xmlAttrs)
     # no matches to tag (give warning)
      if(length(y)==0){ 
         print(paste("Warning: no matches to", tag))
         NA 
     } else{
      if( att %in% names(y[[1]]) )
         y[[1]][[att]]
      else NA
     }
}


# 3) values within REPEATED tags, for example, find the ID associated with DB=ENA-STUDY in the XREF_LINK tag 

#    <XREF_LINK>
#        <DB>ENA-STUDY</DB>
#        <ID>SRP000850</ID>
#    </XREF_LINK>
#    <XREF_LINK>
#        <DB>ENA-EXPERIMENT</DB>
#        <ID>SRX001146</ID>
#    </XREF_LINK>

xtags <- function(doc, tag, subtag1, subtag2, value1 ) {
         n <- xpathSApply(doc, paste(tag, subtag1, sep="/"), xmlValue)==value1
         if ( any(n) ) 
             xpathSApply(doc, paste(tag, subtag2, sep="/"), xmlValue)[n] 
         else NA
}

esearch <-function(term, db="pubmed", usehistory="y", parse=TRUE, verbose=TRUE, showURL=FALSE, ...)
{
    # or use options("verbose")
   if(is.vector(term)) term <- paste(term, collapse=",")   # collapse=" OR " ?
   term  <- gsub(" ", "+", term)  # replace spaces
   email <- Sys.getenv("email")  # will be empty string if not set
   if(email == ""){print("WARNING: please set your email using Sys.setenv(email='name@email.com')" ) }
   opts <- c(email=email, tool="esearch.R", db=db, term=term, usehistory=usehistory, ...)
   if( any(duplicated(names(opts)))){ stop("Duplicated keys are not allowed in url strings")}
   opts <- paste( paste(names(opts), opts, sep="="), collapse="&")

   search <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
   search <- paste(search, opts, sep = "?")
   
   if(showURL) print(search)

   gp <-xmlParse( readLines(search) )
   # parse the output
   if(parse){
      # xvalue in genomes package 
      if( !is.na( xvalue(gp, "//ERROR") )){
         xvalue(gp, "//ERROR")
      }else{
         count <- as.numeric(xvalue(gp, "//Count"))
         if(count == 0){  
            stop( "No results found.")
            # xvalue(gp, "//OutputMessage")  #OR
         }
         # return history object for esummary or efetch
         if(usehistory == "y"){
           if(verbose){ 
              if(count == 1){ print("1 result found") 
              }else{         print(paste( count, "results found")) }
           }
             query <- xvalue(gp, "//QueryKey")
             web   <- xvalue(gp, "//WebEnv")
             y <- data.frame( db= db, results=count,  query_key = query, WebEnv = web, stringsAsFactors=FALSE)
             class(y) <- c("EntrezHistory", "data.frame")
             y
          # or id list
         }else{
             retmax <- xvalue(gp, "//RetMax")
             if(verbose){
               if(retmax == count){
                 if(count >1 ) print(paste( retmax, db, "ids returned"))
               }else{          print(paste( retmax, " ", db, " ids returned (", count, " total ids)", sep="")) }
             }
             # paste(xpathSApply(gp, "//Id", xmlValue), collapse=",")  # comma-separated?
             as.numeric( xpathSApply(gp, "//Id", xmlValue) )
         } 
      }
   # or return XML
   }else{
      gp
   } 
}
