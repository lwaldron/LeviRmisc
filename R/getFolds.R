getFolds <- structure(function #split N samples into nfolds folds.
### Convenient function for cross-validation
### Function by Levi Waldron.
(N,
### number of samples
 nfolds
### number of folds
 ){
    evenly <- nfolds * N%/%nfolds
    extras <- N - evenly
    groupsize <- evenly/nfolds
    evenly.folds <- rep(1:nfolds,groupsize)
    evenly.folds <- sample(evenly.folds,size=length(evenly.folds))
    extra.folds <- sample(1:nfolds,size=extras)
    folds <- c(evenly.folds,extra.folds)
    return(folds)
### integer vector indicating to which fold each sample is assigned.
},ex=function(){
    getFolds(5, 3)
})

