adjP <- structure(function #user-friendly wrapper to the multtest package to adjust for multiple hypothesis testing
###Uses the multtest package with a friendlier interface.  x is your
###vector of raw p values, and the output is the corrected vector with
###the order maintained.
### Function by Levi Waldron.
(x,
### raw p-values
 proc="BH",
### procedure for correcting for multiple testing, "BH" by default for Benjamini-Hochberg.  See ?mt.rawp2adjp from multtest for options
 ...
### additional arguments passed to mt.rawp2adjp
 ){
  output <- mt.rawp2adjp(x,proc=proc,...)
  output <- output$adjp[order(output$index),]
  rownames(output) <- names(x)
  return(output)
### a matrix containing columns for raw and adjusted p-values
},ex=function(){
    rawp <- c(0.01, 0.05, 0.1, 0.2, 0.3)
    adjP(rawp)
})

