install.packages("openxlsx")
library(openxlsx)
dataset1 <- read.xlsx("C:/Users/Sudha Thyagarajan/Desktop/Machine Learning/Project4/Datasets/HeartDisease_dataset.xlsx", sheet = "Raw", colNames = TRUE, detectDates = TRUE)
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

#K-Means Rerun
mydata <- loadings
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:13) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:13, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

km2=kmeans(scaled,15,nstart=20)
km2

#Neural Network Rerun
library(neuralnet)
axes <- predict(fit, newdata = datatrain)
trainNN <- cbind(datatrain, axes)

NN = neuralnet(target~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11, trainNN,
               hidden = 2 , linear.output = T, act.fct = 'tanh') #12.39 error for train
plot(NN)

axes <- predict(fit, newdata = datatest)
testNN <- cbind(datatest, axes)

