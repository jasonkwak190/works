# Consider the ¡°College¡± dataset in the package ¡°ISLR¡±.
rm(list = ls())
library("ISLR")
head(College)
class(College) #data frame

# a) Use the function summary() to produce a numerical summary of the variables
# in the dataset.
summary(College)

# b) Use Pairs() to produce a scatterplot of the continuous variables in the data set.
pairs(College)

# c) Create a new qualitative variable called ¡°Elite¡±, by ¡°binning¡± the variable
# ¡°Top10perc¡±. We are going to divide universities into two groups based on
# whether or not the proportion of students coming from the two 10% of their
# high school exceeds 50%. Add this variable to your dataset.
College$Elite <- College$Top10perc
College$Elite <- College$Elite > 50
College$Elite

# d) Use the table function to figure out how many Elite schools there are.
table(College$Elite)

# e) Use the table function to figure out how many of the Elite schools are private.
table(College$Elite, College$Private)

# f) Do elite schools tend to have higher graduation rates?
table(College$Elite, College$Grad.Rate)
plot(College$Elite, College$Grad.Rate, xlab = "Elite", ylab = "Grad Rate")
# Answer : Yes, in the plot, elite school students(1.0) are focused in 60 ~ 100% range.

# (2) (R programming + Data Processing ??? 20 points)
# This exercise uses the ¡°Auto¡± dataset in the package ¡°ISLR¡±.
head(Auto)
# a) Remove missing values from the data.
na.omit(Auto)

# b) What variables are numerical (continuous) or factors (categorical)?
class(Auto$mpg) #data composed of numbers are numeric
class(Auto$name)#data composed of certain numbers or letters are factors

# c) Report the mean and standard deviation for each continuous variable in the
# data.
summary(Auto)

# d) Remove the 5th through 55th observation. What is the range, mean and
# standard deviation?
#Auto
new_Auto <- Auto[-c(5:54),]
summary(new_Auto) 

# e) In the full Auto dataset, are there any variables you would consider removing,
# or representing differently? Why?
## I would remove name variable since it is a factor. Other variables are numerical, 
## so it would be much better to handle the data with only numerical values.

# f) In the full Auto dataset, graphically explore the relationships between the
# variables in the data set.
pairs(Auto)

# g) In the full Auto dataset, consider the variable mpg. You are going to create a
# new categorical variable for MPG, which has the categories: {low, med,
# high}. Call this variable ¡°my_mpg¡±, and create a new_Auto dataset, which
# contains all of the Auto variables, and your new variable ¡°my_mpg¡±. Save
# the dataset as an *.RData file and submit it with your assignment.
a <-Auto$mpg

?cut
#Split manually using low, median, 3rd Qu., Max
my_mpg <- cut(a, breaks = c(9, 22.75, 29, 46.6), labels = c("low", "med", "high"))

#Split by letting R to break the points
my_mpg2 <- cut(a, breaks = 3, labels = c("low", "med", "high"))

new_Auto <- Auto
new_Auto$my_mpg <- my_mpg
new_Auto #attached my_mpg to new_Auto
