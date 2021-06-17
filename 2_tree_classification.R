# 1) (20 points Modified Exercise 14.4 in ESL)
# Cluster the marketing data of Table 14.1 (ESL) using a classification tree. This
# data is in the ISLR package, and also available on UB learns.
# Specifically, generate a reference sample of the same size of the training set.
# This can be done in a couple of ways, e.g., (i) sample uniformly for each variable,
# or (ii) by randomly permuting the values within each variable independently. Build
# a classification tree to the training sample (class 1) and the reference sample
# (class 0) and describe the terminal nodes having highest estimated class 1
# probability. Compare the results to the results near Table 14.1 (ESL), which were
# derived using PRIM.
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
marketing
head(marketing)
dim(marketing)
summary(marketing)

#Split the data
set.seed(1)

my_marketing <- na.omit(marketing) #omit NAs
my_marketing <- as.data.frame(my_marketing)
my_marketing$Class <- 0

my_marketing
hist(my_marketing$Income)
hist(my_marketing$Sex)
hist(my_marketing$Marital)
hist(my_marketing$Age)
hist(my_marketing$Edu)
hist(my_marketing$Occupation)
hist(my_marketing$Lived)
hist(my_marketing$DualIncome)
hist(my_marketing$Household)
hist(my_marketing$Householdu18)
hist(my_marketing$Status)
hist(my_marketing$HomeType)
hist(my_marketing$Ethnic)
hist(my_marketing$Language)


##############################################################################################
#name each variables
#Income/Sex/ Marital/Age/Edu/Occupation/Lived/Dual_Income/Household/Householdu18/Status/Home_Type/Ethnic 
# marketing$Income       #1~9
# marketing$Sex          #1~2
# marketing$Marital      #1~5
# marketing$Age          #1~6
# marketing$Edu          #1~6
# marketing$Occupation   #1~9
# marketing$Lived        #1~5
# marketing$Dual_Income  #1~3
# marketing$Household    #1~9
# marketing$Householdu18 #0~9
# marketing$Status       #1~3
# marketing$Home_Type    #1~5
# marketing$Ethnic       #1~8
# marketing$Language     #1~3

my_marketing[["Income"]] <- ordered(cut(my_marketing[["Income"]], c(0,3,6,9), labels = c("low", "med", "high")))
my_marketing[["Sex"]] <- ordered(cut(my_marketing[["Sex"]], c(0,1,2), labels = c("man", "woman")))
my_marketing[["Marital"]] <- ordered(cut(my_marketing[["Marital"]], c(0, 2, 4, 5), labels = c("not married", "solo", "married")))
my_marketing[["Age"]] <- ordered(cut(my_marketing[["Age"]], c(0,3,5,7), labels = c("young", "med", "old")))
my_marketing[["Edu"]] <- ordered(cut(my_marketing[["Edu"]], c(0,2,4,6), labels = c("low", "med", "high")))
my_marketing[["Occupation"]] <- ordered(cut(my_marketing[["Occupation"]], c(0,3,6,9), labels = c("no job", "have job", "retired")))
my_marketing[["Lived"]] <- ordered(cut(my_marketing[["Lived"]], c(0,2,4,5), labels = c("low", "med", "high")))
my_marketing[["DualIncome"]] <- NULL
my_marketing[["Household"]] <- ordered(cut(my_marketing[["Household"]], c(0,3,6,9), labels = c("low", "med", "high")))
my_marketing[["Householdu18"]] <- NULL
my_marketing[["Status"]] <- ordered(cut(my_marketing[["Status"]], c(0,1,2,3), labels = c("low", "med", "high")))
my_marketing[["HomeType"]] <- NULL
my_marketing[["Ethnic"]] <- ordered(cut(my_marketing[["Ethnic"]], c(0,3,6,9), labels = c("low", "med", "high")))
my_marketing[["Language"]] <- NULL

sample0 <- my_marketing
sample1 <- my_marketing



for(i in 1:ncol(sample1)){
  sample1[,i] = sample(sample1[,i], nrow(sample1), replace = T)
}

sample1$Class <- 1


#sample1

combine_data <- rbind(sample0, sample1)
dim(combine_data)

for(i in 1:ncol(combine_data)){
  combine_data[,i] = as.factor(as.character(combine_data[,i]))
}

tree <- rpart(Class~., data = combine_data, method = "class", control = rpart.control(maxdepth = 4, minsplit = 9, xval = 10, cp = 0))
tree
tree$cptable
tree$control
plot(tree, uniform = T, compress = T, margin = 0.02)
text(tree, use.n = T)
plotcp(tree)
prp(tree)
