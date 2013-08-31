\name{cvPredictions}
\alias{cvPredictions}
\title{cvPredictions - get cross-validated predictions}
\description{Given a fitted object for which a "predict" method exists (eg from
lm() or coxph()), make cross-validated predictions.  Note that
sample order is maintained, but sample names are not.  In the
output, a sample name of 4.1 for example, means fold 4, sample 1.
Function by Markus Riester.}
\usage{cvPredictions(fit, data, y = data$y, ...)}
\arguments{
  \item{fit}{fit object as returned by lm(), glm(), coxph(), and probably
others for which a predict() method exists.}
  \item{data}{a dataframe containing data with which to make cross-validated
predictions.  Generally should be the same dataframe used to
create fit.}
  \item{y}{response variable, by default the column "y" in data.}
  \item{\dots}{additional variables passed on to cvTools::cvFolds}
}

\value{returns the output of predict(), on left-out samples from
cross-validation.  In the output, a sample name of 4.1, for
example, means fold 4 sample 1.}

\author{Levi Waldron and Markus Riester}




\examples{
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
mydata <- data.frame(group  = gl(2,10,20, labels=c("Ctl","Trt")),
                     weight = c(ctl, trt))
lm.D9 <- lm(weight ~ group, data=mydata)
##non cross-validated predictions:
predict(lm.D9)
##cross-validated predictions.  
cvPredictions(lm.D9, data=mydata, y=mydata$weight)
}
