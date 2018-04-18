library (fastICA)
setwd("/000_UTD/Sem_3/3_MachineLearning/Project_4")
data = read.csv("HeartDisease_Dataset.csv",header=TRUE)
data1 = read.csv("HeartDisease_Dataset.csv",header=TRUE)
data = data [-14]
for (i in 1:303){
  if(data1$num..predicted.value.[i]>0)
  {data1$target[i]<-1}
  else 
  {data1$target[i]<-0}}
data1 = data1[-14]
data1$target<-as.numeric(data1$target)
set.seed(50)
trainid1 = sample(1:nrow(data1),nrow(data1)/1.428)
testid1 = -trainid1
train1 = data1[trainid1,]
test1 = data1[testid1,]

trainid = sample(1:nrow(data),nrow(data)/1.428)
testid = -trainid
train = data[trainid,]
test = data[testid,]


ica <- fastICA(train,13, alg.typ = "parallel", fun = "logcosh", alpha = 1,
               method = "R", row.norm = FALSE, maxit = 200,
               tol = 0.0001, verbose = TRUE)
library(e1071)   
kurt = 0
for (i in 1:13){
  kurt[i-1]<-kurtosis(ica$S[,i])}

plot(kurt)

kurt

#Neural Network
NN = neuralnet(as.numeric(target)~as.numeric(ica$S[,1])+ + as.numeric(ica$S[,3]) +as.numeric(ica$S[,4])+
                as.numeric(ica$S[,5])+as.numeric(ica$S[,6])+
                as.numeric(ica$S[,7])+as.numeric(ica$S[,8])+as.numeric(ica$S[,9])+as.numeric(ica$S[,10])+
                as.numeric(ica$S[,11])+as.numeric(ica$S[,12]),train1,
               hidden = 2 , linear.output = T, act.fct = 'logistic') #12.39 error for train


plot(NN) # 19.98
