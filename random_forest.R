suppressWarnings(library(ISLR))
suppressWarnings(library(knitr))

data(Hitters)
# Remove incomplete cases 
Hitters <- na.omit(Hitters)
# log transform Salary to make it a bit more normally distributed
Hitters$Salary <- log(Hitters$Salary)
kable(head(Hitters,3))

suppressWarnings(library(randomForest))
set.seed(0)
rfSalary <- randomForest(Salary~.,ntree=500, data=Hitters,importance=TRUE)
# From the error ~ tree number curve, 200 should be enough.
print(rfSalary)
varImpPlot(rfSalary, main="Variable Importance")
plot(rfSalary)
# mtryVec, the number of variables chosen to split the tree.
mtryVec<-1:19
set.seed(0)
mtryQuality<-t(sapply(mtryVec,
                      function(z) c(mse=tail(randomForest(Salary~.,
                                                          ntree=200,
                                                          data=Hitters,
                                                          importance=T,mtry=z)$mse,1),
                                    rsq=tail(randomForest(Salary~.,
                                                          ntree=200,
                                                          data=Hitters,
                                                          importance=T,
                                                          mtry=z)$rsq,1)
                      )
)
)
plot(mtryQuality[,1],type="b")
plot(mtryQuality[,2],type="b")
set.seed(0)
# Choose the mxtry corresponding to the max r, which is 6.
rfSalary200 <- randomForest(Salary~., ntree=200, data=Hitters,importance=F,mxtry=6)
print(rfSalary200)

MSE =mean((Hitters$Salary-predict(rfSalary200))^2)

