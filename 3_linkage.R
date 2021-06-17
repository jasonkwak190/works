#Question 2
rm(list = ls())
#a)
m <- as.dist(matrix(c(0, 0.3, 0.4, 0.7, 
                      0.3, 0, 0.5, 0.8, 
                      0.4, 0.5, 0, 0.45, 
                      0.7, 0.8, 0.45, 0), nrow = 4, ncol = 4, byrow = TRUE))
plot(hclust(m, method = "complete"), main = "Complete Linkage")

#b)
plot(hclust(m, method = "single"), main = "Single Linkage")

#c) Suppose that we cut the dendrogram obtained in (a) such that two clusters result. 
# Which observations are in each cluster?
# -> For the fisrt cluster dendrogram at value of 0.7, we obtained clusters of (1,2) and (3,4).

#d) For the second cluster, we have obtained (4), (3,(1,2))

#e) It is mentioned in the chapter that at each fusion in the dendrogram, the position of the 
# two clusters being fused can be swapped without changing the meaning of the dendrogram.
# Draw a dendrogram that is equivalent to the dendrogram in (a), for which two or more of the
# leaves are repositioned, but for which the meaning of the dendrogram is the same.
plot(hclust(m, method = "complete"), labels = c(2,1,4,3), main = "Repositioned Complete Linkage")
