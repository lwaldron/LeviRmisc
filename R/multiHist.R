multiHist <- structure(function ### Plot one or more histograms on the same axes using lines
### If obj is a vector, this function acts like graphics::hist, but
### only sketches a line along the top of the histogram, rather than
### plotting bars.  If obj is a matrix or dataframe, it creates a line
### for each column.
(obj,
### A numeric/integer vector, dataframe, or matrix.  If obj is a
### dataframe or matrix, a histogram line will be created for each
### column.
lty=1:ncol(obj),
### lty for each histogram line.
lwd=rep(1, ncol(obj)),
### lwd for each histogram line.
col=rep(1, ncol(obj)),
### col for each histogram line.
...
### Extra arguments passedon to graphics::hist
){
    if(is(obj, "vector"))
        obj <- data.frame(x=obj)
    hist.out <- lapply(1:ncol(obj), function(i){
        hist(obj[, i], plot=FALSE, ...)
    })
    hist.args <- list(...)
    if(!"ylim" %in% names(hist.args))
        hist.args$ylim <- c(0, max(sapply(hist.out, function(x) max(x$counts))))
    if(!"xlim" %in% names(hist.args))
        hist.args$xlim <- range(sapply(hist.out, function(x) x$breaks))
    if(!"main" %in% names(hist.args))
        hist.args$main <- ""
    if(!"xlab" %in% names(hist.args))
        hist.args$xlab <- ""
    hist.args$x <- obj[, 1]
    hist.args$border <- "white"
    output <- do.call(hist, hist.args)
    for (i in 1:length(hist.out))
        lines( hist.out[[i]]$breaks,
              c(hist.out[[i]]$counts,tail(hist.out[[i]]$counts,1)),
              type='s', lwd=lwd[i], lty=lty[i], col=col[i])
    invisible( output )
### Invisibly returns the output of hist() obj (vector) or the first
### column of obj (matrix or dataframe).
}, ex=function(){
    x <- matrix(rnorm(2000), ncol=2)
    x[, 2] <- x[, 2] - 2
    multiHist(x)
    multiHist(x[, 1])
    multiHist(round(x[, 1] * 1000), xlab="x*1000", main="Integer input")
})
