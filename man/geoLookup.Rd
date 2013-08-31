\name{geoLookup}
\alias{geoLookup}
\title{## Look up basic experiment and platform information from GEO series or dataset identifiers}
\description{This function uses recursion to handle multiple identifiers, so
very large number of identifiers might consume appreciable memory.
Function by Levi Waldron.}
\usage{geoLookup(id, con = NULL)}
\arguments{
  \item{id}{A character string of one or more GSE or GDS identifiers.
Optionally, a platform can be specified by appending it to the
series identifier, separated by a hyphen, eg: GSE1722-GPL96.}
  \item{con}{Connection to a GEOmetadb SQLite database.  Will be created if not provided.}
}

\value{If one ID is provided, a character vector of platform and series
information retrieved through GEOmetadb.  If multiple IDs are
provided, a matrix with one row per ID.}

\author{Levi Waldron and Markus Riester}




\examples{
## Takes too long to run for the first time in an example, because
## the sqlite file containing all GEO metadata has to be downloaded.
if( interactive() ){
    geoLookup("GSE100")
    geoLookup("GSE1722-GPL96")
    geoLookup("GSE1722-GPL1384")
    geoLookup(c("GSE1722-GPL96", "GSE1722-GPL1384", "GSE100"))
}
}
