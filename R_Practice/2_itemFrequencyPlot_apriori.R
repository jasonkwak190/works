# (2) (20 points) Consider the Boston Housing Data. This data can be accessed in the
# MASS package (available through CRAN).
# > library(MASS)
# > data(Boston)

#rm(list = ls())
#install.packages("arules")
library(arules)
library(MASS)
dim(Boston)
names(Boston)
summary(Boston)
Boston
my_data <- Boston
my_data
# a) Visualize the data using histograms of the different variables in the data set.
# Transform the data into a binary incidence matrix, and justify the choices you
# make in grouping categories.
?Boston
dim(Boston)
#visualize data
par(mfrow=c(2,2))
hist(my_data$crim)
hist(my_data$zn)
hist(my_data$indus)
hist(my_data$chas) # 1 or 0

par(mfrow=c(2,2))
hist(my_data$nox)
hist(my_data$rm)
hist(my_data$age)
hist(my_data$dis)

par(mfrow=c(2,2))
hist(my_data$rad)
hist(my_data$tax)
hist(my_data$ptratio)
hist(my_data$black)

par(mfrow=c(1,2))
hist(my_data$lstat)
hist(my_data$medv)

#
Boston[["crim"]] <- ordered(cut(Boston[["crim"]], c(0,10,20,50), labels = c("smallcrime", "mediumcrime", "largecrime")))
Boston[["zn"]] <- ordered(cut(Boston[["zn"]], c(0,30,60,100), labels = c("smallft", "mediumft", "largeft")))
Boston[["indus"]] <- ordered(cut(Boston[["indus"]], c(0,30,60,100), labels = c("smallnonbus", "mediumnonbus", "largenonbus")))
Boston[["chas"]] <- NULL
Boston[["nox"]] <- ordered(cut(Boston[["nox"]], c(0.3, 0.5, 0.65, 0.9), labels = c("smallnx", "mediumnx", "largenx")))
Boston[["rm"]] <- ordered(cut(Boston[["rm"]], c(3, 5, 7, 9), labels = c("smallroom", "mediumroom", "largeroom")))
Boston[["age"]] <- ordered(cut(Boston[["age"]], c(0,30,60,100), labels = c("young", "old", "elder")))
Boston[["dis"]] <- ordered(cut(Boston[["dis"]], c(1,4,8,13), labels = c("near", "not_near", "far")))
Boston[["rad"]] <- ordered(cut(Boston[["rad"]], c(0,8,26), labels = c("Acc", "NotAcc")))
Boston[["tax"]] <- ordered(cut(Boston[["tax"]], c(150,300,500,750), labels = c("lowtax", "mediumtax", "largetax")))
Boston[["ptratio"]] <- ordered(cut(Boston[["ptratio"]], c(12,16,19,22), labels = c("lowtcher", "mediumtcher", "hightcher")))
Boston[["black"]] <- ordered(cut(Boston[["black"]], c(0,200,400), labels = c("lowblack", "highblack")))
Boston[["lstat"]] <- ordered(cut(Boston[["lstat"]], c(0,15,30,40), labels = c("lowpop", "mediumpop", "largepop")))
Boston[["medv"]] <- ordered(cut(Boston[["medv"]], c(5,30,60,100), labels = c("smallmdv", "mediummdv", "largemdv")))

indice_matrix <- as(Boston, "transactions")

summary(indice_matrix)

# b) Visualize the data using the itemFrequencyPlot in the ¡°arules¡± package.
# Apply the apriori algorithm (Do not forget to specify parameters in your write up).
par(mfrow=c(1,1))
ifp <- itemFrequencyPlot(indice_matrix, support = 0.01, cex.names = 0.8)
summary(ifp)

apr <- apriori(indice_matrix, parameter = list(support = 0.07, confidence = 0.8))
summary(apr)


# c) A student is interested is a low crime area, but wants to be as close to the city
# as possible (as measured by ¡°dis¡±). What can you advise on this matter
# through the mining of association rules?
#close <- subset(apr, subset = rhs %in% "dis=near" & lift > 1.2)
#lowcrime <- subset(apr, subset = rhs %in% "crim=smallcrime" & lift > 1.2)

#summary(close)
#summary(lowcrime)

inspect(head(subset(apr, subset=rhs %in% "dis=near"),n = 5,by="confidence"))
inspect(head(subset(apr, subset=rhs %in% "crim=smallcrime"),n = 5,by="confidence"))

inspect(head(subset(apr, subset=rhs %in% "dis=near"),n = 5,by="lift"))
inspect(head(subset(apr, subset=rhs %in% "crim=smallcrime"),n = 5,by="lift"))

#   d) A family is moving to the area, and has made schooling a priority. They want
# schools with low pupil-teacher ratios. What can you advise on this matter
# through the mining of association rules?
#   Extra Credit: Use a regression model to solve part d. Are you results
# comparable? Which provides an easier interpretation? When would
# regression be preferred, and when would association models be preferred? 
Boston$ptratio
apr1 <- apriori(indice_matrix, parameter = list(support = 0.001, confidence = 0.8))
inspect(head(subset(apr1, subset=rhs %in% "ptratio=lowtcher"),n = 5,by="lift"))

lowpupil
summary(lowpupil)

set.seed(100)
index = sample(1:nrow(my_data), nrow(my_data)*0.7)
test = my_data[index,]
train = my_data[-index,]
my_data$ptratio

dim(train)
dim(test)
lmpupil <- lm(train$ptratio~., train)
summary(lmpupil)

