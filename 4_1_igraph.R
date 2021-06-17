#Question 1
#install.packages("gRim")
library(gRbase)
library(gRain)
library(ggm)
library(gRim)
library(bnlearn)
library(igraph)
library(Rgraphviz)

data(cad1, package = "gRbase")
head(cad1)
?cad1
cad.bn <- hc(cad1)
net <- as(amat(cad.bn), "graphNEL")

plot(net)

names(cad1)

block <- c(1, 3, 3, 4, 4, 4, 4, 1, 2, 1, 1, 1, 3, 2)
blm <- matrix(0, nrow = 14, ncol = 14)
rownames(blm) <- names(cad1)
colnames(blm) <- names(cad1)

for (b in 2:4){
  blm[block == b, block < b] <- 1
}

blackL <- data.frame(get.edgelist(as(blm, "igraph")))
names(blackL) <- c("from", "to")

cad.bn2 <- hc(cad1, blacklist = blackL)
net.contr <- as(amat(cad.bn2), "graphNEL")

plot(net.contr)
