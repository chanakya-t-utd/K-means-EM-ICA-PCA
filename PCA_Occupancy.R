roomTr <- read.csv("C:/Users/Sudha Thyagarajan/Desktop/Machine Learning/Project4/Datasets/datatraining.txt", header = TRUE, sep=',',stringsAsFactors = FALSE)
roomTe1 <- read.csv("C:/Users/Sudha Thyagarajan/Desktop/Machine Learning/Project4/Datasets/datatest.txt", header = TRUE, sep=',',stringsAsFactors = FALSE)
roomTe2 <- read.csv("C:/Users/Sudha Thyagarajan/Desktop/Machine Learning/Project4/Datasets/datatest2.txt", header = TRUE, sep=',',stringsAsFactors = FALSE)
room <- rbind(roomTr, roomTe1, roomTe2)

# removing Date
room <- room[-1]
fit <- prcomp(room, cor=TRUE)
plot(fit)
biplot(fit, scale = 0)

#compute standard deviation of each principal component
std_dev <- fit$sdev
#compute variance
pr_var <- std_dev^2

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


#K-Means Re-Run
mydata <- loadings
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:4) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:4, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

km2=kmeans(scaled,4,nstart=20)
km2


#NeuralNet Re-Run
library(neuralnet)
axes <- predict(fit, newdata = roomTr)
trainNN <- cbind(roomTr, axes)

set.seed(2)
NN = neuralnet(Occupancy ~ PC1+PC2, trainNN,
               hidden = 3 , linear.output = T, act.fct = 'tanh') #Change act.fct to vary differentiating function, hidden to layers
plot(NN)

axes <- predict(fit, newdata = roomTe1)
testNN <- cbind(roomTe1, axes)
