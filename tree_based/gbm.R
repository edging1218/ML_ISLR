suppressWarnings(library('ggplot2'))
suppressWarnings(library('gbm'))
datapath = "~/Documents/Lecture/MachineLearning/ML_PA_SM17_Yuri/workshop/L7/"
dTrain = read.table(paste0(datapath,"isolet1234.data.gz"),
                    header=FALSE,
                    sep=',',
                    stringsAsFactors=FALSE,
                    blank.lines.skip=TRUE)
dTrain$isTest <- FALSE
dTest = read.table(paste0(datapath,"isolet5.data.gz"),
                   header=FALSE,sep=',',
                   stringsAsFactors=FALSE,blank.lines.skip=TRUE)
dTest$isTest <- TRUE
d <- rbind(dTrain,dTest)
dim(d)

# Make the response variable letters instead of letter number
d$V618 <- letters[d$V618]
vars <- colnames(d)[1:617]
# Extract only letters 'm' and 'n'
# The goal is to identify two letters 'm' and 'n'
d <- d[d$V618 %in% c('m','n'),,drop=FALSE]
dim(d)

# Add new boolean column 'isLetter' as T for n and F for m
yColumn <- 'isLetter'
d[,yColumn] <- d[,'V618']=='n'
head(d[,yColumn])

# Perform gradient boosting algorithm with gbm()
formula <- paste(yColumn,paste(vars,collapse=' + '),sep=' ~ ')
startTime=proc.time()
modelGBM <- suppressWarnings(
  gbm(as.formula(formula),
      data=d[!d$isTest,,drop=FALSE],
      distribution='bernoulli',
      n.trees=400,
      interaction.depth=3,
      shrinkage=0.05,
      bag.fraction=0.5,
      keep.data=T,
      cv.folds=5,
      verbose=F))
stopTime = proc.time()
(timeElapsed<-stopTime-startTime)

pretty.gbm.tree(modelGBM,1)
# Black is training set and green is for testing set
nTrees <- gbm.perf(modelGBM)
# Relative importance
relImport<-summary(modelGBM)

d$modelGBM <- predict(modelGBM,newdata=d,type='response',
                      n.trees=nTrees)
head(predict(modelGBM))
showPred<-cbind(Response=d$isLetter*1,Predict=d$modelGBM)
matplot(1:length(showPred[,1]),showPred[order(d$isLetter*1),],pch=1,ylab="Probability",xlab="Index")
