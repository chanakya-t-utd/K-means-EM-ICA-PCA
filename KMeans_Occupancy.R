# import the txt files
roomTr <- read.csv("C:/Users/Sudha Thyagarajan/Desktop/Machine Learning/Project4/Datasets/datatraining.txt", header = TRUE, sep=',',stringsAsFactors = FALSE)
roomTe1 <- read.csv("C:/Users/Sudha Thyagarajan/Desktop/Machine Learning/Project4/Datasets/datatest.txt", header = TRUE, sep=',',stringsAsFactors = FALSE)
roomTe2 <- read.csv("C:/Users/Sudha Thyagarajan/Desktop/Machine Learning/Project4/Datasets/datatest2.txt", header = TRUE, sep=',',stringsAsFactors = FALSE)
room <- rbind(roomTr, roomTe1, roomTe2)

# removing Date
room <- room[-1]
set.seed(20)
kmeans.result <- kmeans(room, 2, nstart = 20)
kmeans.result
centers <- kmeans.result$centers[kmeans.result$cluster, ] # "centers" is a data frame of 3 centers but the length of iris dataset so we can canlculate distance difference easily.
distances <- sqrt(rowSums((room - centers)^2))
outliers <- order(distances, decreasing=T)[1:10]
print(outliers) # these rows are 5 top outliers

print(room[outliers,])
plot(room, col =(kmeans.result$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

mydata <- room
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

set.seed(7)
km2 = kmeans(room, 6, nstart=100)
km2


table(km2$cluster, room$Occupancy)

library(cluster) 
#Plotting th clusters
clusplot(room, km2$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
plot(room, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)
