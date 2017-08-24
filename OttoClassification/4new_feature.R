suppressWarnings(library(dummies))
suppressWarnings(library(e1071))
suppressWarnings(library(xgboost))
suppressWarnings(library(caret))


dataPath <- "~/Documents/Lecture/MachineLearning/ML_PA_SM17_Yuri/HW/project2"
train <- read.csv(paste(dataPath,"train_sample.csv",sep="/"),header=T)
test <- read.csv(paste(dataPath,"test_sample.csv",sep="/"),header=T)

MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  if (!is.matrix(pred)) pred<-(as.matrix(pred))
  if (!is.matrix(act)) act<-(as.matrix(act))
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  #normalize rows
  ll = sum(act*log(sweep(pred, 1, rowSums(pred), FUN="/")))
  ll = -ll/nrow(act)
  return(ll);
}

clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}

ncol = ncol(train)
nrow = nrow(train)
set.seed(13)
testInd = sample(nrow, nrow/3)
x = train[, -ncol]
y = train$target
xtest = test[, -1]


f1 = rowSums(x == 0)
f2 = rowSums(x != 0)
f3 = rowSums(x)

testf1 = rowSums(xtest == 0)
testf2 = rowSums(xtest != 0)
testf3 = rowSums(xtest)

cluster = kmeans(train, 30, iter.max = 30)
f4 = cluster$cluster

new_x = cbind(x, n1 = f1, n2 = f2, n3 = f3, n4 = f4)
xTrain = new_x[-testInd,]
xTest = new_x[testInd,]
xTrain[] = lapply(xTrain, as.numeric)
xTest[] = lapply(xTest, as.numeric)
xgbTrain = data.matrix(xTrain)
xgbTest = data.matrix(xTest)
yTrain = as.factor(y[-testInd])
yTest = y[testInd]
yTrain = as.integer(yTrain)-1
yTest = as.integer(yTest)-1
numClasses = max(yTrain) + 1

train_IndMat<-dummy.data.frame(data=as.data.frame(yTrain), 
                               sep="_", verbose=F, 
                               dummy.class="ALL")
target_IndMat<-dummy.data.frame(data=as.data.frame(yTest), 
                                sep="_", verbose=F, 
                                dummy.class="ALL")

xg.param <- list("objective" = "multi:softprob",
                 'eval_metric' = "mlogloss",
                 'num_class' = numClasses,
                 'eta' = 0.2,
                 'gamma' = 0.7,
                 'max.depth' = 5,
                 'min_child_weight' = 4
                 #'subsample' = 1,
                 #'colsample_bytree' = 1,
                 #'nthread' = 3
)
cv.nround <- 250
cv.nfold <- 5
set.seed(1)
(bst.cv = xgb.cv(param=xg.param, data = xgbTrain, label = yTrain, 
                 nfold = cv.nfold, nrounds = cv.nround,verbose=F))
opt.nround = which.min(bst.cv$evaluation_log$test_mlogloss_mean)
print(opt.nround)
bst = xgboost(param=xg.param, data = xgbTrain, label = yTrain, 
              nrounds=opt.nround,verbose=F)
xgbTrain.pred = matrix(predict(bst, xgbTrain), ncol = numClasses, byrow = TRUE)
xgbPred <- matrix(predict(bst, xgbTest), ncol = numClasses, byrow = TRUE)
print(MultiLogLoss(train_IndMat,xgbTrain.pred))
print(MultiLogLoss(target_IndMat,xgbPred))


testf4 = clusters(xtest, cluster$centers)
newtest = cbind(xtest, n1 = testf1, n2 = testf2, n3 = testf3, n4 = testf4)
newtest[] = lapply(newtest, as.numeric)
newtest = data.matrix(newtest)
res <- matrix(predict(bst, newtest), ncol = numClasses, byrow = TRUE)
write.table(res,"res.csv",quote=F,col.names = T,sep = ",")


new_x[] = lapply(new_x, as.numeric)
new_train = data.matrix(new_x)
y = as.integer(as.factor(train$target))-1
set.seed(1)
bst2 = xgboost(param=xg.param, data = new_train, label = y, 
                  nrounds=opt.nround,verbose=F)

res2 <- matrix(predict(bst2, newtest), ncol = numClasses, byrow = TRUE)
write.table(res2,"res2.csv",quote=F,col.names = T,sep = ",")

