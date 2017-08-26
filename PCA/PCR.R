dataPath<-"/scratch/midway/yezhou/machine_learning_02"
# data 500 * 492 has the first column as Y and all other columns as predictors
data <- read.table(paste(dataPath,"test_sample.csv",sep="/"),header=T)
originalR2<-sapply(3:492,function(z) summary(lm(Y~.,data=data.frame(Y=data$Y, data[,2:z])))$r.squared)
origs = cbind(3:492, originalR2)
# N.orig denotes the smallest number of regressors making determination coefficient greater than 0.9.
N.orig = origs[origs[,2]>0.9, 1][1]

#PCR
#PCA of predictors
prcomp490 = prcomp(data[,2:492])
#Extract loadings and factors
factorLoadings = prcomp490$rotation
factorScores = prcomp490$x
factors490Data<-data.frame(Y=data$Y,factorScores)
print('Start fitting...')
#Linear regression fitting with meta-features
m490.PCA<-lm(Y~.,data=factors490Data)
print('Calcuate relative importance...')
suppressMessages(library(relaimpo))
(metrics.PCA <- calc.relimp(m490.PCA, type = "first"))
(first.PCA.rank<-metrics.PCA@first.rank)
#Reorder the loadings and factors by the order of relative importance
orderedFactors<-factorScores[,order(first.PCA.rank)]
orderedLoadings<-factorLoadings[,order(first.PCA.rank)]
print('Start fitting with ordered predictors...')
orderedPCA.R2<-sapply(3:492,function(z) summary(lm(Y~.,data=data.frame(Y=data$Y,orderedFactors[,1:z])))$r.squared)
pca = cbind(3:492, orderedPCA.R2)
# N.PCA denotes the smallest number of regressors making determination coefficient greater than 0.9.
# PCA_R is the determination coefficient corresponding to N.PCA
N.PCA = pca[pca[,2]>0.9, 1][1]
PCA_R = pca[pca[,2]>0.9, 1][2]