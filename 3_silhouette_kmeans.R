#Problem 3
#a) Generate a simulated data set with 20 observations in each of three classes (i.e. 60
#observations total) and 50 variables.
set.seed(1)
a <- c(rep(1,20), rep(2,20), rep(3,20))

d1 <- matrix(rnorm(20*50, mean = 0), nrow = 20)
d2 <- matrix(rnorm(20*50, mean= 1), nrow = 20)
d3 <- matrix(rnorm(20*50, mean= 2), nrow = 20)
data <- rbind(d1,d2,d3)

data
pdata <- prcomp(data, center = T, scale = T)
plot(pdata)
biplot(pdata)
plot(pdata$x[,1:3], col = a, pch = 16)

#b) Perform k-means clustering of the observations with K=3. Using the rand index and
#adjusted rand index, assess how well do the clusters that you obtained in K-means
#clustering compare to the true labels?

datak <- kmeans(data, centers = 3, nstart = 20)
table(datak$cluster, a)
plot(pdata$x[,1:3], col = datak$cluster, pch = 17, main = "Kmean cluster 3")


#c) Using silhouette plots, select the optimal number of clusters.
library("cluster")

?silhouette
#k=2
datak2 <- kmeans(data, centers = 2, nstart = 20)
plot(silhouette(datak2$cluster, dist = dist(data), col = a))
plot(pdata$x[,1:3], col = datak2$cluster, pch = 17, main = "Kmean cluster 2")

#k=3
plot(silhouette(datak$cluster, dist = dist(data), col = a))

#k=4
datak4 <- kmeans(data, centers = 4, nstart = 20)
plot(silhouette(datak4$cluster, dist = dist(data), col = a))
plot(pdata$x[,1:3], col = datak4$cluster, pch = 17, main = "Kmean cluster 4")

#k=5
datak5 <- kmeans(data, centers = 5, nstart = 20)
plot(silhouette(datak5$cluster, dist = dist(data), col = a))
plot(pdata$x[,1:3], col = datak5$cluster, pch = 17, main = "Kmean cluster 5")


#d) Using the gap statistics, select the optimal number of clusters.
gap <- clusGap(data, kmeans, nstart = 20, K.max = 10, B = 100)
plot(gap, main = "Gap Statistics")

