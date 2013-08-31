expandProbesets <- function ### expand featureNames of an ExpressionSet that are 2+ delimited names.
### If a single probeset is collapsed to multiple ids, for example
### ZNRF2///MIR550-1, then this will create an ExpressionSet with single
### ids. ZNRF2 and MIR550-1 will be two identical rows in the exprs().
### Useful for accessing genes. If ids are not unique, the first instance is
### chosen. Priority is given however to probesets with single ids (i.e. if
### there is a "ZNRF2" and a "ZNRF2///MIR550-1" probe, the uncollapsed
### ExpressionSet will have the first probe ("ZNRF2") and the second
### ("MIR550-1"), not the second probe twice).  Function by Markus Riester.
(
eset,
### ExpressionSet to uncollapse.
sep=";"
### The string that separates multiple probes
) {
    x <- lapply(featureNames(eset), function(x) strsplit(x, sep)[[1]])
    # putting the single id probes on top, so that duplicated() will pick them
    # later in case ids are not unique
    eset <- eset[order(sapply(x, length)),]
    x <- lapply(featureNames(eset), function(x) strsplit(x, sep)[[1]])

    idx <- unlist(sapply(1:length(x), function(i) rep(i, length(x[[i]]))))
    xx <- !duplicated(unlist(x))
    idx <- idx[xx]
    x   <- unlist(x)[xx]
    eset <- eset[idx,]
    featureNames(eset) <- x
    eset
}
