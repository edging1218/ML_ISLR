set.seed(8394756)
Epsilon<-rnorm(500,0,1)
X<-rnorm(500*500,0,2)
dim(X)<-c(500,500)
colnames(X)<-paste0("X",1:500)
slopesSet<-runif(500,1,3)
Y<-sapply(2:500,function(z) 1+X[,1:z]%*%slopesSet[1:z]+Epsilon)
m10<-lm(Y~.,data=data.frame(Y=Y[,9],X[,1:10]))
suppressWarnings(library(relaimpo))
(metrics10<-calc.relimp(m10, type = c("lmg", "first", "last","betasq", "pratt")))
(metrics10.lmg.rank<-metrics10@lmg.rank)
orderedPedictors<-X[,1:10][,order(metrics10.lmg.rank)]
originalR2.10<-sapply(2:10,function(z) summary(lm(Y~.,data=data.frame(Y=Y[,9],X[,1:z])))$r.squared)
improvedR2.10<-sapply(2:10,function(z) summary(lm(Y~.,data=data.frame(Y=Y[,9],orderedPedictors[,1:z])))$r.squared)
matplot(2:10,cbind(originalR2.10,improvedR2.10),type="l",lty=1,lwd=2,col=c("black","red"),
        main="Improvement of Fit with Number of Predictors",
        xlab="Number of Predictors",ylab="Determination Coefficient")
legend("bottomright",legend=c("Original","Improved"),lty=1,lwd=2,col=c("black","red"))

#PCR
prcomp10 = prcomp(X[,1:10])
print(summary(prcomp10))
factorLoadings = prcomp10$rotation
factorScores = prcomp10$x
factors10Data<-data.frame(Y=Y[,9],factorScores)
m10.PCA<-lm(Y~.,data=factors10Data)
(metrics.PCA <- calc.relimp(m10.PCA, type = c("lmg", "first", "last","betasq", "pratt")))
c(sum.PCA.lmg=sum(metrics.PCA@lmg),
  sum.PCA.first=sum(metrics.PCA@first),
  sum.PCA.last=sum(metrics.PCA@last),
  m10.R2=summary(m10)$r.squared)
(first.PCA.rank<-metrics.PCA@first.rank)
orderedFactors<-factorScores[,order(first.PCA.rank)]
orderedLoadings<-factorLoadings[,order(first.PCA.rank)]
orderedPCA.R2<-sapply(2:10,function(z) summary(lm(Y~.,data=data.frame(Y=Y[,9],orderedFactors[,1:z])))$r.squared)
matplot(2:10,cbind(originalR2.10,improvedR2.10,orderedPCA.R2),type="l",lty=1,lwd=2,col=c("black","red","blue"),
        main="Improvement of Fit with Number of Predictors",xlab="Number of Predictors",ylab="Determination Coefficient")
legend("bottomright",legend=c("Original","Improved","PCA"),lty=1,lwd=2,col=c("black","red","blue"))
coefficients.m10.PCA = m10.PCA$coefficients[-1][order(first.PCA.rank)]
restoredSlopes<-orderedLoadings%*%coefficients.m10.PCA
cbind(restoredSlopes,head(slopesSet,10))

prcomp490 = prcomp(X[,1:491])originalR2<-sapply(300:400,function(z) summary(lm(Y~.,data=data.frame(Y=data$Y, data[,2:z])))$r.squared)
#print(summary(prcomp490))
factorLoadings = prcomp490$rotation
factorScores = prcomp490$x
factors490Data<-data.frame(Y=Y[,490],factorScores)
print('Start fitting...')
m490.PCA<-lm(Y~.,data=factors490Data)
print('Calcuate relative importance...')
(metrics.PCA <- calc.relimp(m490.PCA, type = c("lmg", "first", "last","betasq", "pratt")))
(first.PCA.rank<-metrics.PCA@first.rank)
orderedFactors<-factorScores[,order(first.PCA.rank)]
orderedLoadings<-factorLoadings[,order(first.PCA.rank)]
print('Start fitting with ordered predictors...')
orderedPCA.R2<-sapply(2:491,function(z) summary(lm(Y~.,data=data.frame(Y=Y[,490],orderedFactors[,1:z])))$r.squared)
