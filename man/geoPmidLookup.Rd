\name{geoPmidLookup}
\alias{geoPmidLookup}
\title{## Look up metadata for any combination of GEO series and PMID identifiers}
\description{If GEO series IDs are provided, the function will also add data
from pubmed if the PMID can be found from GEO.
Function by Levi Waldron.}
\usage{geoPmidLookup(id, con = NULL)}
\arguments{
  \item{id}{A character string of one or more GSE identifiers and/or
pubmed IDs.  Pubmed IDs may or may not be preceded by "PMID".
Optionally, GEO series identifiers can have a platform specified
by appending it to the series identifier, separated by a hyphen,
eg: GSE1722-GPL96.}
  \item{con}{Connection to a GEOmetadb SQLite database.  Will be created if not provided.}
}

\value{A character vector or dataframe of platform, series, and publication
information retrieved through GEOmetadb and Pubmed.  If multiple IDs are
provided, the dataframe will have one row per ID.}

\author{Levi Waldron and Markus Riester}




\examples{
## Takes too long to run for the first time in an example, because
## the sqlite file containing all GEO metadata has to be downloaded:
if( interactive() ){
    ids <- c("GSE100", "GSE1000", "PMID100", "PMID1000")
    geoLookup(ids)
}
}
