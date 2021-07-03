pendigits
#install.packages("nsprcomp")

#sacle data
d = scale(pendigits)

#compute PCs
pc_ex <- prcomp(d, center = FALSE, scale = FALSE)
plot(pc_ex)

names(pc_ex)

#percent of variation explained
pc_var <- (pc_ex$sdev)^2
per_var_exp <- (pc_var/sum(pc_var))*100
barplot(per_var_exp, main = "PC variance explained", ylab = "% variation explained", xlab = "PCs")

pc_var/(sum(pc_var))

#Color them by class
my_col <- rep("black", length(d[,1]))
cl = c(0,1,2,3,4,5,6,7,8,9)

cc1 = which(cl<3)
my_col[cc1] = "red"
cc2 = which(cl<6 & cl>2)
my_col[cc2] = "blue"
cc3 = which(cl>5)
my_col[cc3] = "green"

my_col

#plot the score of PC1 and PC2 and PC3
#install.packages("plotly")
library(plotly)
head(round(abs(pc_ex$x)))
idx = round(abs(pc_ex$x))
pc_ex$rotation
plot_ly(x=idx[,1], y=idx[,2], z=idx[,3], type = "scatter3d", mode = "markers", color = my_col)



########################
head(d)
pendigits
index <- sample(1:length(pendigits[,1]), 2/3*length(pendigits[,1]))
train <- iris[index, ]
test <- iris[-index, ]
cl = c(0,100)
knn(train, test, cl= cl , k=1)
library(class)
?knn
