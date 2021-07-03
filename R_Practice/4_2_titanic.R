#Question 2
rm(list = ls())
library(bnlearn)
library(Rgraphviz)

my_data <- read.csv("C:/Users/Win10/Downloads/titanic.csv")
names(my_data)
str(my_data)

names(my_data)
my_data$Age[my_data$Age >15] <- 'Adult'
my_data$Age[my_data$Age != 'Adult'] <- 'Child'

#use as.factor
for(x in seq(1,6)){
  my_data[,x]<-as.factor(my_data[,x])
}
str(my_data)

#erase the NA
my_data <- my_data[complete.cases(my_data), ]  # Rows are removed where NA are present

#double-check
sapply(my_data, function(x){
  sum(is.na(x))
})

#plot
my_data <- data.frame(my_data)
my_dag <- hc(my_data)
plot(my_dag)
bn_fit <- bn.fit(my_dag, data = my_data)
graphviz.plot(bn_fit)


# Prob of women and child survival
cpquery(bn_fit, (Survived == 1), (Sex == 'female'))
cpquery(bn_fit, (Survived == 1), (Age =='Child'))

# What characteristics/demographics are more likely in surviving passengers?
cpquery(bn_fit, (Survived == 1), (Age =='Child' & Sex=='female' & Pclass == 1))
cpquery(bn_fit, (Survived == 1), (Age =='Child' & Sex=='female' & Pclass == 2))
cpquery(bn_fit, (Survived == 1), (Age =='Child' & Sex=='female' & Pclass == 3))
cpquery(bn_fit, (Survived == 1), (Age =='Child' & Sex=='male' & Pclass == 1))
cpquery(bn_fit, (Survived == 1), (Age =='Child' & Sex=='male' & Pclass == 2))
cpquery(bn_fit, (Survived == 1), (Age =='Child' & Sex=='male' & Pclass == 3))

cpquery(bn_fit, (Survived == 1), (Age =='Adult' & Sex=='female' & Pclass == 1))
cpquery(bn_fit, (Survived == 1), (Age =='Adult' & Sex=='female' & Pclass == 2))
cpquery(bn_fit, (Survived == 1), (Age =='Adult' & Sex=='female' & Pclass == 3))
cpquery(bn_fit, (Survived == 1), (Age =='Adult' & Sex=='male' & Pclass == 1))
cpquery(bn_fit, (Survived == 1), (Age =='Adult' & Sex=='male' & Pclass == 2))
cpquery(bn_fit, (Survived == 1), (Age =='Adult' & Sex=='male' & Pclass == 3))

# What characteristics/demographics are more likely in passengers that perished?
cpquery(bn_fit, (Survived == 0), (Age =='Child' & Sex=='female' & Pclass == 1))
cpquery(bn_fit, (Survived == 0), (Age =='Child' & Sex=='female' & Pclass == 2))
cpquery(bn_fit, (Survived == 0), (Age =='Child' & Sex=='female' & Pclass == 3))
cpquery(bn_fit, (Survived == 0), (Age =='Child' & Sex=='male' & Pclass == 1))
cpquery(bn_fit, (Survived == 0), (Age =='Child' & Sex=='male' & Pclass == 2))
cpquery(bn_fit, (Survived == 0), (Age =='Child' & Sex=='male' & Pclass == 3))

cpquery(bn_fit, (Survived == 0), (Age =='Adult' & Sex=='female' & Pclass == 1))
cpquery(bn_fit, (Survived == 0), (Age =='Adult' & Sex=='female' & Pclass == 2))
cpquery(bn_fit, (Survived == 0), (Age =='Adult' & Sex=='female' & Pclass == 3))
cpquery(bn_fit, (Survived == 0), (Age =='Adult' & Sex=='male' & Pclass == 1))
cpquery(bn_fit, (Survived == 0), (Age =='Adult' & Sex=='male' & Pclass == 2))
cpquery(bn_fit, (Survived == 0), (Age =='Adult' & Sex=='male' & Pclass == 3))

# Prob of Rose survived, (1st class & female & Adult)
cpquery(bn_fit, (Survived == 1), (Sex == 'female' & Pclass==1 & Age=='Adult'))

# Prob of Jack died, (3rd class & male & adult)
cpquery(bn_fit, (Survived == 0), (Sex == 'male' & Pclass==3 & Age=='Adult'))