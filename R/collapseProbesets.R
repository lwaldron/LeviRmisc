collapseProbesets <- function ### Wrapper for jetset or possibly other
### methods for collapsing probesets to genes.  Create a tab separated
### file for the phenoData slot. Used for Level 1 Affymetrix data.
### Note that the jetset package must be installed but currently
### available only from http://www.cbs.dtu.dk/biotools/jetset/.
### Function by Markus Riester.
(eset, 
### ExpressionSet with probe sets ids as features. Requires as valid
### annotation slot for the mapping to gene symbols (rma/frma normalized
### ExpressionSets have that slot).
method=c("jetset")
### The collapsing method.  "jetset" is a wrapper for the Jetset package.
) {
    method <- match.arg(method)
    switch(method,
        "jetset" = .collapse.jetset(eset))
### ExpressionSet with probe sets mapped to gene symbols.
}

.collapse.jetset <- function(eset) {
    require(jetset)
    require(annotate)
    ss <- getSYMBOL(featureNames(eset), annotation(eset))
    ss <- ss[!is.na(ss)]
    ss <- unique(ss)
    ps <- jmap(annotation(eset), symbol = ss)
    ps <- ps[!is.na(ps)]
    eset <- eset[ps,]
    featureNames(eset) <- names(ps)
    eset
}
