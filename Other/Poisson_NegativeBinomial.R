library('rpart')
library('rpart.plot')
library('MASS')

#import data
dataPath <- "~/Documents/Lecture/MachineLearning/ML_PA_SM17_Yuri/HW/hw5"
data <- read.csv(paste(dataPath,"test_sample.csv",sep="/"),header=T)

#regression tree
set.seed(1)
sfit <- rpart(Y ~ ., data = data, method ='poisson', control = rpart.control(cp = 0))
bestcp = sfit$cptable[which.min(sfit$cptable[,'xerror']), 'CP']
prunedTree = prune(sfit, bestcp)
prp(prunedTree)

#negative binomial regression
nbfit = glm.nb(Y ~., data = data)

# Calculate the Rmse for two methods
rmse <- function(x) sqrt(mean(x^2))
treeRmse = rmse(resid(prunedTree))
nbRmse = rmse(nbfit$residuals)
options(digits=4)
print('MSE')
print(c(Tree = treeRmse, Nb = nbRmse))

#Make prediction
newdata = rbind(data, c(0, 4, 4, 4))
last = nrow(newdata)
preTree = predict(prunedTree, newdata = newdata[last,2:4])
pred0Tree = dpois(0, lambda = preTree)

mu = predict(nbfit, newdata[last, 2:4], type='response')
pred0nb = dnbinom(0, mu=mu, size=nbfit$theta)

print('Predicted mean')
print(c(Tree = preTree, Nb = mu))
print('Zero prediction')
print(c(Tree = pred0Tree, Nb = pred0nb))




