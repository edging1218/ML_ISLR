datapath = '~/Documents/Lecture/MachineLearning/ML_PA_SM17_Yuri/workshop/L5'
(tree.data<-read.csv(file=paste(datapath,"binary_splitting_data.csv",sep="/")))
oneSplit<-function(myData,myVariable,mySplit){
  # myData: matrix of data
  # myVariable: number of variable that needs split 1:ncol(myData)
  # mySplit: split level
  idx<-myData[,myVariable+1]<mySplit # separate variable by mySplit 
  subs1<-subset(myData,idx) # subset below mySplit
  subs2<-subset(myData,!idx) # subset above mySplit
  mean1<-mean(subs1$y) # mean of subset 1
  mean2<-mean(subs2$y) # mean of subset 2
  sum((subs1$y-mean1)^2)+sum((subs2$y-mean2)^2) # out: sum of sums of squares
}
selectSplit<-function(myData){
  # myData: data matrix including response in column 1
  x1<-sort(myData[,2]) # sort variable 1
  x2<-sort(myData[,3]) # sort variable 2
  nData<-length(myData[,1])-1 # number of splits
  
  # create splits
  splits<-data.frame(x1=sapply(1:nData,function(z) mean(c(x1[z],x1[z+1]))),
                     x2=sapply(1:nData,function(z) mean(c(x2[z],x2[z+1]))))
  
  # calculate sums of squares of all splits
  splits1.SS<-sapply(1:nData,function(z) oneSplit(myData,1,splits$x1[z]))
  splits2.SS<-sapply(1:nData,function(z) oneSplit(myData,2,splits$x2[z]))
  
  # out: sums of squares and splits
  list(SS=data.frame(splits1.SS=splits1.SS,splits2.SS=splits2.SS),
       splits=splits)
}

Split_tree = function(data){
  (node<-selectSplit(data))
  (splitVar<-ifelse(min(node[["SS"]][,"splits1.SS"])>min(node[["SS"]]["splits2.SS"]),2,1))
  (splitNum<-as.numeric(ifelse(splitVar==1,
                               which.min(node[["SS"]][,"splits1.SS"]),
                               which.min(node[["SS"]][,"splits2.SS"])))) 
  (splitLevel<-node[["splits"]][splitNum,splitVar])
  (Reg1<-subset(data, data[,splitVar+1]>=splitLevel))
  (Reg2<-subset(data, data[,splitVar+1]< splitLevel))
  return (list(reg1 = Reg1, reg2 = Reg2, var = splitVar, Level = splitLevel))
}


regs_1 = Split_tree(tree.data)
Reg1 = regs_1[['reg1']]
Reg2 = regs_1[['reg2']]
# Reg1 only have one point, thus is a leaf.
# Split R2 by creating regions
regs_2 = Split_tree(Reg2)
Reg3 = regs_2[['reg1']]
Reg4 = regs_2[['reg2']]
regs_3 = Split_tree(Reg2)





