suppressWarnings(library(caret))
suppressWarnings(library(rpart))

# Create rectangular class domain with some randomness
N = 1000
xPos = 0.2
yMinPos = 0.2
yMaxPos = 0.7
newData = data.frame(x=runif(N),y=runif(N))
newData$type = with(newData,ifelse(x>xPos & y>yMinPos & y<yMaxPos,
                                   'Positive', 'Negative'))

n = N/10
newData$type[1:n] = c('Positive', 'Negative')[1+rbinom(n, 1, 0.5)]
newData$type = factor(newData$type)
newData = newData[sample(nrow(newData)),] 
qplot(x=x,y=y,data=newData, color=type)

# logistic regression
# Use cross validation (caret) to check the predictive quality
modelFormula = formula('type ~ x + y')
logrFit <- glm(modelFormula, family=binomial("logit"),data=newData)
print(summary(logrFit))
ctrl <- trainControl(method = "cv", number = 10)
logrTrain <- train(modelFormula, data=newData,
                   method = 'glm', trControl = ctrl)
summary(logrTrain)

# Use classification tree to fit the data
treeFit <- rpart(modelFormula, data=newData)
printcp(treeFit)
treeTrain <- train(modelFormula, method="rpart",data=newData,
                   trControl = ctrl)

#svm
svmFitIso <- train(modelFormula, data=newData,
                   method = "svmRadial",
                   tuneGrid = data.frame(.C = rep(c(8,16,24), 3),
                                         .sigma = rep(10^(-2:0), each = 3)), 
                   trControl = ctrl,
                   preProc = c("center", "scale"))
optimal_svm <- svm(modelFormula, data = newData, gamma = 1, cost = 24)
plot(optimal_svm, newData)
# Compare the two methods
print(list(tree = treeTrain$results, logistic = logrTrain$results, svm = svmFitIso$results))
print(logrTrain$results)
