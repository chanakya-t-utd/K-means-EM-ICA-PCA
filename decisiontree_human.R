library(rpart)
library(tree)
library(rpart.plot)
library(caret)
setwd("/000_UTD/Sem_3/3_MachineLearning/Project_4")
data = read.csv("datatraining.txt",header=TRUE)
data<-data[-1]

data$Occupancy<-as.factor(data$Occupancy)
classs<-data$Occupancy
set.seed(50)

trainid = sample(1:nrow(data),nrow(data)/1.428)
testid = -trainid
train = data[trainid,]
test = data[testid,]
test_Class = classs[testid]

dt<-rpart(Occupancy~.,data = train,method ='class', parms = list(split = "information"), control = rpart.control(minsplit = 35, cp = 0.000005))
rpart.plot(dt,type=4,digits = 3,fallen.leaves = TRUE)

p1 <- predict(dt,test,type = 'class')
mean(p1 != test_Class) #miss classification = 1.1%

##Pruning to the tree for the best cp
plotcp(dt)
printcp(dt)

pruned_tree = prune(dt,cp = 0.011)
tree_pred_pruned = predict(pruned_tree, test, type = 'class')
mean(tree_pred_pruned != test_Class) # The new misclassification is 1.4%

##ANN

setwd("/000_UTD/Sem_3/3_MachineLearning/Project_4")

# Loading data

data = read.table("datatraining.txt",header=TRUE,sep=",")

samplesize = 0.7 * nrow(data)
set.seed(10)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

datatrain = data[ index, ]
datatest = data[ -index, ]

data <- data[, sapply(data, is.numeric)]

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))
#-----------------------------------------------------------------------------------------------------------------------------------
#Basic Neural Network Implementation

library(neuralnet)

trainNN = scaled[index , ]
testNN = scaled[-index , ]

set.seed(2)
NN = neuralnet(Occupancy ~ Temperature + Light + CO2, trainNN,
               hidden = 3 , linear.output = T, act.fct = 'logistic') #Change act.fct to vary differentiating function, hidden to layers
plot(NN)



