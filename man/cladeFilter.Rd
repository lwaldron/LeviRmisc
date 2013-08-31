\name{cladeFilter}
\alias{cladeFilter}
\title{Filter rows of a table of relative abundances}
\description{Provides several useful options for non-specific feature reduction
of a bug abundance table.
Function by Levi Waldron.}
\usage{cladeFilter(obj, terminal.nodes.only = FALSE, clustering.reduction = FALSE, 
    cor.options = list(method = "pearson"), cutree.options = list(h = 0.1), 
    clusterSelectFun = mean, genus.or.family.only = FALSE, remove.unclassified = TRUE, 
    remove.unnamed.genus.or.higher = TRUE, required.level = "p__", 
    discretize.cutpoints = NULL, discretize.labels = NULL, min.abd = 1e-04, 
    min.samp = 0.1, asinsqrt = TRUE)}
\arguments{
  \item{obj}{Relative abundance table with features as rows, samples as columns.}
  \item{terminal.nodes.only}{Keep terminal nodes only?  Terminal nodes have no child nodes present in the table.}
  \item{clustering.reduction}{Use clustering to reduce dimensionality?  Clustering is performed
by cutree(hclust(as.dist(1-cor(t(obj), cor.options))),
cutree.options).}
  \item{cor.options}{If using clustering to reduce the features, these arguments will be passed to cor()}
  \item{cutree.options}{If using clustering to reduce the features, these arguments will
be passed to stats::cutree().  For example, the default h=0.1 will
remove features with correlation > 0.9.  Alternatively, k=20 could
be specified to always return 20 features.}
  \item{clusterSelectFun}{If using clustering to reduce the features, select the feature
with the maximum value of this function to select from each
cluster.}
  \item{genus.or.family.only}{Keep only genus or family levels, nothing higher, nothing lower}
  \item{remove.unclassified}{Get rid of anything labelled "Unclassified" at any level}
  \item{remove.unnamed.genus.or.higher}{If true, remove things like |c__, |o__, |f__, |g__ - unnamed
class, order, family, genus...}
  \item{required.level}{Keep only rows containing this string in the name, by default
require at least phylum-level resolution.}
  \item{discretize.cutpoints}{If discretize.cutpoints is a numeric vector, then bug abundances
will be discretized at these values.  A sensible setting, if you
want to try this, is c(0, 1e-100, 1e-4, 0.01, 0.25), with
discretize.labels equal to c("zero", "very low", "low", "medium",
"high").}
  \item{discretize.labels}{A vector of labels for discretized data, with length 1 less than
the length of discretize.cutpoints.  A sensible setting is
discretize.cutpoints = c(0, 1e-100, 1e-4, 0.01, 0.25),
discretize.labels = c("zero", "very low", "low", "medium",
"high").}
  \item{min.abd}{Minimum abundance requirement for bugs, in at least min.samp
fraction of samples}
  \item{min.samp}{Minimum fraction of samples with a value of min.abd.}
  \item{asinsqrt}{perform asin(sqrt(obj)) ?}
}

\value{a cleaned-up version of the input matrix of bug abundances.}

\author{Levi Waldron and Markus Riester}





