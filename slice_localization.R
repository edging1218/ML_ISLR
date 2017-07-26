suppressWarnings(library ('pls'))
suppressWarnings(library ('glmnet'))
suppressWarnings(library ('rpart'))
suppressWarnings(library ('rpart.plot'))
#import data
dataPath <- "~/Documents/Lecture/MachineLearning/ML_PA_SM17_Yuri/HW/project"
data <- read.csv(paste(dataPath,"slice_localization_data.csv",sep="/"),header=T)
coln = colnames(data)
head(coln)
tail(coln)
print(length(coln))

#define predictors and response 
pred = data[,c(-1, -386)]
Y = data[, 386]
# look at the distribution of Y
head(Y)
plot(Y)
summary(Y)
hist(Y)
# linear regression
newdata = data.frame(Y=Y, pred)
lmr = lm(Y~., data=newdata)
# Select predictors significant with 5%
coef_lm = coefficients(summary(lmr))
criteria = coef_lm[,4] < 0.05
# Always keep Y
criteria[0] = TRUE
reducedData = newdata[,criteria]
# linear regression with reduced data
lmReduced = lm(Y~., data=reducedData)
#Examine the residual of the fit
hist(lmReduced$residuals)
qqnorm(lmReduced$residuals)
qqline(lmReduced$residuals)
print('Summary for linear regression:')
res.lm = c(AIC=AIC(lmReduced), 
        Rsq = summary(lmReduced)$r.squared, 
        adjRsq = summary(lmReduced)$adj.r.squared, 
        MSE = mean(lmReduced$residuals^2), 
        PredNum = ncol(reducedData)-1)
print(res.lm)

# PCR

# Lasso regression model
# Use cross validation to find the optimal lambda for Lasso regression
cv.out=cv.glmnet(x=data.matrix(pred),y=Y,alpha=1)
plot(cv.out)
(bestlam =cv.out$lambda.min)
# Fit the data with optimal lambda
out=glmnet(x=as.matrix(pred),y=Y,alpha=1,lambda=bestlam)
lasso.mse = mean(deviance(out))
lasso.df = out$df
lasso.rsq = out$dev.ratio
res.lasso = c(AIC=0, 
           Rsq = out$dev.ratio, 
           MSE = mean(deviance(out)), 
           PredNum = out$df)
print(res.lasso)

# Regression tree
tree = rpart(Y~., data=newdata)
print(tree$cptable)
prp(tree, extra=101, # display the number of observations that fall in the node
    branch=.5, # change angle of branch lines
    shadow.col="gray", # shadows under the leaves
    branch.lty=3, # draw branches using dotted lines
    split.cex=1.2, # make the split text larger than the node text
    split.prefix="is ", # put "is " before split text
    split.suffix="?", # put "?" after split text
    split.box.col="lightgray", # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5,
    nn=TRUE)

tree.mse = mean(resid(prunedTree)^2)
tree.Rsq = 1 - sum(resid(tree)^2) /sum((Y - mean(Y))^2) 
length(tree$variable.importance)

