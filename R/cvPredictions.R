cvPredictions <- structure(function  #cvPredictions - get cross-validated predictions
### Given a fitted object for which a "predict" method exists (eg from
### lm() or coxph()), make cross-validated predictions.  Note that
### sample order is maintained, but sample names are not.  In the
### output, a sample name of 4.1 for example, means fold 4, sample 1.
### Function by Markus Riester.
(fit,
### fit object as returned by lm(), glm(), coxph(), and probably
### others for which a predict() method exists.
 data,
### a dataframe containing data with which to make cross-validated
### predictions.  Generally should be the same dataframe used to
### create fit.
 y=data$y,
### response variable, by default the column "y" in data.
 ...
### additional variables passed on to cvTools::cvFolds
 ){
    folds <- cvFolds(nrow(data), ...)
    yhat <- cvFit(fit, y=y, data=data, folds=folds, cost=function(y, yhat) yhat )
    folds.order <- folds$subset[order(folds$which)]
    yhat$cv[order(folds.order)]
### returns the output of predict(), on left-out samples from
### cross-validation.  In the output, a sample name of 4.1, for
### example, means fold 4 sample 1.
},ex=function(){
    ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
    trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    mydata <- data.frame(group  = gl(2,10,20, labels=c("Ctl","Trt")),
                         weight = c(ctl, trt))
    lm.D9 <- lm(weight ~ group, data=mydata)
    ##non cross-validated predictions:
    predict(lm.D9)
    ##cross-validated predictions.  
    cvPredictions(lm.D9, data=mydata, y=mydata$weight)
})
