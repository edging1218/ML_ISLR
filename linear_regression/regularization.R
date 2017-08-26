# Generate data set
set.seed(8394756)
Epsilon<-rnorm(500,0,1)
X<-rnorm(500*500,0,2)
dim(X)<-c(500,500)
colnames(X)<-paste0("X",1:500)
slopesSet<-runif(500,1,3)
Y<-sapply(2:500,function(z) 1+X[,1:z]%*%slopesSet[1:z]+Epsilon)

# Get linear model with 10 predictors
m10<-lm(Y~.,data=data.frame(Y=Y[,9],X[,1:10]))
suppressWarnings(library('glmnet'))
# Ridge regression with 10 predictors
ridge10=glmnet(x=X[,1:10], y = Y[,9], alpha = 0, nlambda = 200, lambda.min.ratio=.0001, standardize = F)
set.seed(1)
# Separate dataset to the training set and test set with 1:1 ratio
train = sample(1:nrow(X[,1:10]), nrow(X[,1:10])/2)
test = (-train)
y.test = Y[test, 9]
set.seed(5)
# Perform cross-validation on the training set and pick the best lambda
cv.out = cv.glmnet(x = X[train, 1:10], Y[train, 9], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
# Use the best lambda to predict for the test set
ridge.pred=predict(ridge10,s=bestlam,newx=X[test,1:10]) 
# Calculate the mean squared error
(ridge.MSE<-mean((ridge.pred-y.test)^2))

# Compare with least squares linear model
train.m10 = lm(Y~., data=data.frame(Y = Y[train, 9], X[train, 1:10]))
lm.pred = predict(train.m10, newdata = as.data.frame(X[test, 1:10]))
(lm.MSE = mean((lm.pred - y.test)^2))

# Fit lasso regression
lasso10=glmnet(x=data.matrix(X[train,1:10]),y=Y[train,9],alpha=1,nlambda=100,lambda.min.ratio=.0001)
plot(lasso10)
abline(h=0)
set.seed(1)
cv.out=cv.glmnet(x=X[train,1:10],y=Y[train,9],alpha=1)
plot(cv.out)
(bestlam =cv.out$lambda.min)
lasso.pred=predict(lasso10,s=bestlam,newx=X[test,1:10])
(lasso.MSE<-mean((lasso.pred -y.test)^2) )

# Fit the entire data and compare the coefficients
out=glmnet(x=X[,1:10],y=Y[,9],alpha=1,nlambda=100,lambda.min.ratio=.0001)
lasso.coef=predict(out,type="coefficients",s=bestlam)
print(cbind(Lasso=lasso.coef,Lm=as.vector(m10$coefficients),Actual=c(1,slopesSet[1:10])))