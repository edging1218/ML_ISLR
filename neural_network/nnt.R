# AND perceptron
suppressWarnings(library(neuralnet))
#suppressWarnings(library(boot))
(da<-data.frame(x1=c(0,0,1,1),x2=c(0,1,0,1),y=c(0,1,1,1)))
set.seed(14)
nnDa <- neuralnet(y~x1+x2,data=da, err.fct="sse",rep=2)
names(nnDa)
plot(nnDa,rep="best")
#nnDa$act.fct() is the activation function
#nnDa$err.fct() is the loss function
#nnDa$net.result is the y_hat
print(nnDa$net.result)
#nnDa$weights
#nnDa$result.matrix
#Function compute() like  predict().
compute(nnDa,da[,1:2])

#Force the coefficents 4 and 5 to be 0 and 1.
set.seed(14)
nnDa2 <- neuralnet(y~x1+x2,data=da, err.fct="sse",rep=10, exclude=c(4,5),
                  constant.weights = c(0,1),threshold=1e-20)
plot(nnDa2,rep="best")
print(nnDa2$net.result)
nnDa3 <- neuralnet(y~x1+x2,data=da, err.fct="sse", hidden = c(3))
plot(nnDa3,rep="best")
print(nnDa3$net.result)