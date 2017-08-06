head(USArrests)
states = row.names(USArrests)
USArrests.means<-apply(USArrests , 2, mean) 
USArrests.vars<-apply(USArrests , 2, var) 
rbind(USArrests.means,USArrests.vars)
pr.out=prcomp(USArrests , scale=TRUE)
names(pr.out)
#standardize the dataset to mean = 0 and std = 1
print(pr.out$center)
print(pr.out$scale)
#loading vectors
print(pr.out$rotation)
#score vectors
head(pr.out$x)
biplot(pr.out , scale=0)
matplot(pr.out$rotation,type="l",lty=1,lwd=3,col=c("black","red","blue","green"),xaxt = "n")
abline(h=0)
legend("bottomleft",legend=c("L1","L2","L3","L4"),lty=1,lwd=3,cex=.7,col=c("black","red","blue","green"))
axis(1,1:4,rownames(pr.out$rotation))

smry<-summary(pr.out)
print(smry$importance)
#Same as
#pr.var=pr.out$sdev ^2
#pve=pr.var/sum(pr.var)

#Dimension reduction, use the first two principal components to approximate the data
USArrests.approximations<-pr.out$x[,1:2]%*%t(pr.out$rotation[,1:2])
head(USArrests.approximations)
USArrests.scaled<-apply(USArrests,2,scale)
pairs(apply(USArrests.scaled,2,rank))
pairs(apply(USArrests.approximations,2,rank))
pr.out.residuals<-USArrests.scaled-USArrests.approximations
pairs(apply(pr.out.residuals,2,rank))