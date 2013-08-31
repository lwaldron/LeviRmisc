cladeFilter <- function  #Filter rows of a table of relative abundances
### Provides several useful options for non-specific feature reduction
### of a bug abundance table.
### Function by Levi Waldron.
(obj,
### Relative abundance table with features as rows, samples as columns.
 terminal.nodes.only=FALSE,
### Keep terminal nodes only?  Terminal nodes have no child nodes present in the table.
 clustering.reduction=FALSE,
### Use clustering to reduce dimensionality?  Clustering is performed
### by cutree(hclust(as.dist(1-cor(t(obj), cor.options))),
### cutree.options).
 cor.options=list(method="pearson"),
### If using clustering to reduce the features, these arguments will be passed to cor()
 cutree.options=list(h=0.1),
### If using clustering to reduce the features, these arguments will
### be passed to stats::cutree().  For example, the default h=0.1 will
### remove features with correlation > 0.9.  Alternatively, k=20 could
### be specified to always return 20 features.
 clusterSelectFun=mean,
### If using clustering to reduce the features, select the feature
### with the maximum value of this function to select from each
### cluster.
 genus.or.family.only=FALSE, 
### Keep only genus or family levels, nothing higher, nothing lower
 remove.unclassified=TRUE,
### Get rid of anything labelled "Unclassified" at any level
 remove.unnamed.genus.or.higher=TRUE,
### If true, remove things like |c__, |o__, |f__, |g__ - unnamed
### class, order, family, genus...
 required.level="p__",
### Keep only rows containing this string in the name, by default
### require at least phylum-level resolution.
 discretize.cutpoints=NULL,
### If discretize.cutpoints is a numeric vector, then bug abundances
### will be discretized at these values.  A sensible setting, if you
### want to try this, is c(0, 1e-100, 1e-4, 0.01, 0.25), with
### discretize.labels equal to c("zero", "very low", "low", "medium",
### "high").
 discretize.labels=NULL,
### A vector of labels for discretized data, with length 1 less than
### the length of discretize.cutpoints.  A sensible setting is
### discretize.cutpoints = c(0, 1e-100, 1e-4, 0.01, 0.25),
### discretize.labels = c("zero", "very low", "low", "medium",
### "high").
 min.abd=1e-4,
### Minimum abundance requirement for bugs, in at least min.samp
### fraction of samples
 min.samp=0.1,
### Minimum fraction of samples with a value of min.abd.
 asinsqrt=TRUE
### perform asin(sqrt(obj)) ?
 ){
    if (asinsqrt && !is.null(discretize.cutpoints))
        warning("Since discretize.cutpoints is set, asinsqrt will not be performed.")
    obj <- as.matrix(obj)
    abd.pass <- apply(obj, 1, function(x){
        sum( x > min.abd, na.rm = TRUE ) > ( min.samp * length(x) )
    })
    obj <- obj[abd.pass, ]
    if(remove.unclassified)
        obj <- obj[!grepl("Unclassified", rownames(obj)), ]
    if(!is.null(required.level) & !is.na(required.level))
        obj <- obj[grepl(required.level, rownames(obj)), ]
    if(genus.or.family.only)
        obj <- obj[grepl("[fg]__[a-zA-Z]+$", rownames(obj)), ]
    if(remove.unnamed.genus.or.higher)
        obj <- obj[!grepl("\\|[pcofg]__$|\\|[pcofg]__\\|", rownames(obj)), ]
    if(terminal.nodes.only){
        ## For each rowname, see whether if is found within any other
        ## rowname.  grep.results is a matrix where each column is a
        ## rowname (clade), each row is where that clade greps to.
        grep.results <- sapply(1:nrow(obj), function(i){
            grepl(rownames(obj)[i], rownames(obj)[-i], fixed=TRUE)
        })
        ## Terminal nodes will have no grep matches:
        terminal.logical <- colSums(grep.results) == 0
        obj <- obj[terminal.logical, ]
    }
    if( clustering.reduction ){        
        cor.options$x <- t(obj)
        obj.dist <- as.dist( 1 - do.call(cor, cor.options) )
        obj.clust <- hclust(obj.dist)
        cutree.options$tree <- obj.clust
        obj.cutree <- do.call(cutree, cutree.options)
        clusterkeep.index <- sapply(unique(obj.cutree), function(x){
            ##clades with the same cluster ID:
            idx <- which(obj.cutree %in% x)
            ##If there are more than one, select the one with maximum
            ##mean.  If only one, return the singleton cluster:
            if(length(idx) > 1){
                select.val <- apply(obj[idx, ], 1, clusterSelectFun)
                return( idx[which.max( select.val )] )
            }else{
                return( idx )
            }
        })
        obj <- obj[clusterkeep.index, ]
    }
    if (!is.null(discretize.cutpoints)){
        obj.discrete <- t( apply(obj, 1, cut, breaks=discretize.cutpoints, labels=discretize.labels, include.lowest=TRUE, ordered_result=TRUE) )
        dimnames(obj.discrete) <- dimnames(obj)
        obj <- obj.discrete
    }
    if(asinsqrt && is.null(discretize.cutpoints))
        obj <- asin(sqrt(obj))
    return(obj)
### a cleaned-up version of the input matrix of bug abundances.
}
