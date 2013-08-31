fold <- structure(function  #fold - also known variously as reduce, accumulate, compress, or inject
### http://en.wikipedia.org/wiki/Fold_%28higher-order_function%29
### shamelessly copied from this post by Hadley Wickam:
### https://stat.ethz.ch/pipermail/r-help/2007-April/130462.html
(x,
### a list of vectors
 fun
### a function to apply recursively to x
 ){
    if (length(x) == 1) return(fun(x))
    accumulator <- fun(x[[1]], x[[2]])
    if (length(x) == 2) return(accumulator)
    for(i in 3:length(x)) {
        accumulator <- fun(accumulator, x[[i]])
    }
    accumulator
### result of x fold fun.
},ex=function(){
    a <- list(c(1,3,5), c(1,3), c(1, 2, 5, 6))
   fold(a, intersect) 
})
