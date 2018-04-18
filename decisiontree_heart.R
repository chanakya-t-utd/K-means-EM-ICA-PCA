library(rpart)
library(tree)
library(rpart.plot)
library(caret)
setwd("/000_UTD/Sem_3/3_MachineLearning/Project_4")
data = read.csv("HeartDisease_Dataset.csv",header=TRUE) #cp, thal, ca
data$cp<-as.factor(data$cp)
data$restecg<-as.factor(data$restecg)
data$slope<-as.factor(data$slope)
data$sex<-as.factor(data$sex)

for (i in 1:303){
  if(data$num..predicted.value.[i]>0)
  {data$target[i]<-1}
  else 
  {data$target[i]<-0}}
data$target<-as.factor(data$target)
data<-data[-14]
classs<-data$target
set.seed(50)

trainid = sample(1:nrow(data),nrow(data)/1.428)
testid = -trainid
train = data[trainid,]
test = data[testid,]
test_Class = classs[testid]

dt<-rpart(target~.,data = train,method ='class', parms = list(split = "information"), control = rpart.control(minsplit = 35, cp = 0.000005))
rpart.plot(dt,type=4,digits = 3,fallen.leaves = TRUE)

p1 <- predict(dt,test,type = 'class')
mean(p1 != test_Class) #miss classification = 24.1%

##Pruning to the tree for the best cp
plotcp(dt)
printcp(dt)

pruned_tree = prune(dt,cp = 0.011)
tree_pred_pruned = predict(pruned_tree, test, type = 'class')
mean(tree_pred_pruned != test_Class) # The new misclassification is 24.1%

#ANN

library(openxlsx)
library(dplyr)
library(dummies)
library(rpart)

setwd("/000_UTD/Sem_3/3_MachineLearning/Project_4")
dataset1 = read.csv("HeartDisease_Dataset.csv",header=TRUE) #cp, thal, ca

for (i in 1:303){
  if(dataset1$`num..predicted.value`[i]>0)
  {dataset1$target[i]<-1}
  else 
  {dataset1$target[i]<-0}}
dataset1$target<-as.numeric(dataset1$target)
data<-dataset1

samplesize = 0.7 * nrow(data)
set.seed(10)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

datatrain = data[ index, ]
datatest = data[ -index, ]

data <- data[, sapply(data, is.numeric)]

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

library(neuralnet)
trainNN = scaled[index , ]
testNN = scaled[-index , ]

set.seed(2)

NN = neuralnet(target~ca+thal+cp, trainNN,
               hidden = 2 , linear.output = T, act.fct = 'tanh') #12.39 error for train
plot(NN)

NN_test = neuralnet(target~ca+thal+cp,testNN, hidden = 2, linear.output = T, act.fct = 'tanh') # 4.5 error for test.
plot(NN_test)


