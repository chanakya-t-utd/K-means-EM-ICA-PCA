## ICA
library (fastICA)

setwd("/000_UTD/Sem_3/3_MachineLearning/Project_4")
data1 = read.csv("datatraining.txt",header=TRUE)
data1<-data1[-1]
trainid1 = sample(1:nrow(data1),nrow(data1)/1.428)
testid1 = -trainid1
train1 = data1[trainid1,]
test1 = data1[testid1,]

data = read.csv("datatraining.txt",header=TRUE)
data<-data[-1]
data<-data[-6]

set.seed(50)

trainid = sample(1:nrow(data),nrow(data)/1.428)
testid = -trainid
train = data[trainid,]
test = data[testid,]


ica <- fastICA(train,5, alg.typ = "parallel", fun = "logcosh", alpha = 1,
                     method = "R", row.norm = FALSE, maxit = 200,
                     tol = 0.0001, verbose = TRUE)

library(neuralnet)
library(e1071)   
kurt = 0
kurt[1]<-kurtosis(ica$S[,1])
kurt[2]<-kurtosis(ica$S[,2])
kurt[3]<-kurtosis(ica$S[,3])
kurt[4]<-kurtosis(ica$S[,4])
kurt[5]<-kurtosis(ica$S[,5])

kurt
plot(kurt)

set.seed(2)
NN = neuralnet(as.numeric(Occupancy) ~ ica$S[,3] + ica$S[,4]+ica$S[,5],train1 ,
               hidden = 1 , linear.output = T, act.fct = 'logistic') #Change act.fct to vary differentiating function, hidden to layers

plot(NN)

