install.packages("openxlsx")
library(openxlsx)
library(mclust)
#dataset1<-read.csv("C:/Users/Meera/Desktop/UTD/Sem3/Machine_Learning/datatraining.txt", header = TRUE, sep=',',stringsAsFactors = FALSE)
dataset1 <- read.xlsx("C:/Users/Meera/Desktop/UTD/Sem3/Machine_Learning/HeartDisease_Dataset.xlsx", sheet = "Raw", colNames = TRUE, detectDates = TRUE)
for (i in 1:303){
  if(dataset1$`num.(predicted.value)`[i]>0)
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

set.seed(10)


model1 <- Mclust(scaled)

# Enter '0' to exit the plot menu
plot(model1)

summary(model1)

#Kmeans and EM ReRun
install.packages("openxlsx")
library(openxlsx)
dataset1<-read.csv("C:/Users/Meera/Desktop/UTD/Sem3/Machine_Learning/datatraining.txt", header = TRUE, sep=',',stringsAsFactors = FALSE)
#dataset1 <- read.xlsx("C:/Users/Meera/Desktop/UTD/Sem3/Machine_Learning/HeartDisease_Dataset.xlsx", sheet = "Raw", colNames = TRUE, detectDates = TRUE)
for (i in 1:303){
  if(dataset1$`num.(predicted.value)`[i]>0)
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


fit <- prcomp(scaled, cor=TRUE)
fit
plot(fit)
biplot(fit, scale = 0)
#compute standard deviation of each principal component
std_dev <- fit$sdev

#compute variance
pr_var <- std_dev^2

#check variance 
pr_var

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

loadings<- as.data.frame(fit$rotation[])

set.seed(10)


model1 <- Mclust(loadings)

# Enter '0' to exit the plot menu
plot(model1)

summary(model1)
