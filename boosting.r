myPlot<-function(myDataFrame){
  lastCol<-ncol(myDataFrame)
  matplot(myDataFrame$x,myDataFrame[,c(2,lastCol)],pch=16,xlab="X",ylab="Data and Predictor")
}

simpleLearner<-function(mySignal){
  model<-Mclust(mySignal,G=2)
  model$parameters$mean[model$classification]
}

suppressWarnings(library(mclust))
set.seed(1180)
x=seq(0,2*pi,by=.01)
sigmaEps<-.1
eps<-rnorm(length(x),0,sigmaEps)
signal<-sin(x)+eps
data<-data.frame(x=x,signal=signal)
plot(data)


data$learner1 = simpleLearner(data$signal)
myPlot(data)
data$learner2 = data$learner1 + simpleLearner(data$signal - data$learner1)
myPlot(data)
data$learner3 = data$learner2 + simpleLearner(data$signal - data$learner2)
myPlot(data)
data$learner4 = data$learner3 + simpleLearner(data$signal - data$learner3)
myPlot(data)
data$learner5 = data$learner4 + simpleLearner(data$signal - data$learner4)
myPlot(data)
sds = c(sd(data$signal), 
        sd(data$signal - data$learner1), 
        sd(data$signal - data$learner2), 
        sd(data$signal - data$learner3), 
        sd(data$signal - data$learner4),
        sd(data$signal - data$learner5))
plot(sds)
