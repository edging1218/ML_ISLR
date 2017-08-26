suppressWarnings(library(HistData))
suppressWarnings(library(e1071))
#suppressWarnings(library(kable))
data(GaltonFamilies)
head(GaltonFamilies)
set.seed(0)
Data = GaltonFamilies[sample(nrow(GaltonFamilies)),
                      c("midparentHeight","childHeight","gender")]
Gender = as.character(Data$gender)
ggplot(Data, aes(x=midparentHeight, y=childHeight, color=Gender)) + 
  geom_point(shape=16)+scale_color_hue(l=65, c=100)+ 
  scale_color_manual(values=c("orange", "blue"))
testInd = sample(nrow(Data), trunc(nrow(Data)/3))
xTrain = Data[-testInd,]
xTest = Data[testInd,]
svmTuned <- tune.svm(gender~., data = Data, gamma = 10^(-4:-1), cost = 5*(1:4))
summary(svmTuned)
svmTuned$best.parameters
plot(svmTuned$best.model,xTrain)
predict <- predict(svmTuned$best.model, xTest[,!names(Data)=="gender"])
classAgreement(table(pred = predict, true = xTest[,"gender"]))$diag
