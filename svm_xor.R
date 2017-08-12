suppressWarnings(library(e1071))
dat<-rbind(c(1,1,1),
           c(2,1,0),
           c(2,2,1),
           c(1,2,0))
colnames(dat)<-c("X1","X2","Class")
dat<-as.data.frame(dat)
dat$Class<-as.factor(dat$Class)
plot(dat$X1,dat$X2,col="orange",pch=16,ylim=c(0.5,2.5),xlim=c(0.5,2.5))
points(dat$X1[dat$Class==1],dat$X2[dat$Class==1],col="blue",pch=16)
m1.linear<-svm(Class~.  , data=dat,kernel="linear")
m1.poly2<-svm(Class~.  , data=dat,kernel="polynomial",degree=2)
m1.poly3<-svm(Class~.  , data=dat,kernel="polynomial",degree=3)
m1.radial<-svm(Class~.  , data=dat,kernel="radial")
print(predict(m1.linear))
print(predict(m1.poly2))
print(predict(m1.poly3))
print(predict(m1.radial))

print('Only polynomial with degree 2 and radial made the right prediction.')
print('Polynomial function with higher degree does not necessarily work better.')