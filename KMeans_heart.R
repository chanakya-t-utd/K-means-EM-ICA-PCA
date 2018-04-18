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

set.seed(10)
km1=kmeans(scaled,2,nstart=20)
km1
plot(scaled, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)


mydata <- scaled
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:30) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

km2=kmeans(scaled,11,nstart=20)
km2
install.packages("cluster")
library(cluster) 
clusplot(scaled, km2$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

plot(scaled, col =(km2$cluster +1) , main="K-Means result with 10 clusters", pch=20, cex=2)
