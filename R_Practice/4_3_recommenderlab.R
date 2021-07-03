# Question 4
library(recommenderlab)
data(MovieLense)
head(MovieLense)
dim(MovieLense) #943 X 1664

image(MovieLense[1:100, 1:100])
getRatingMatrix(MovieLense[1:100, 1:100])

#Create realRatingMatrix
R <- as(MovieLense, "realRatingMatrix")

#Normalize data
N <- normalize(MovieLense)
image(N[1:100, 1:100], main = "Normalized Data")

#De-normalize
N_de <- denormalize(N)

#Binary Matrix
N_bin <- binarize(N_de, minRating = 4)
image(N_bin[1:100, 1:100], main = "Binarized ratings")

length(unique(getRatings(N))) #3658
length(unique(rowCounts(N)))  #279
length(unique(colCounts(N)))  #272

hist(getRatings(N), breaks = 100, main = "Histogram of normalized ratings")
hist(rowCounts(N), breaks = 100, main = "Ratings given by users")
hist(colCounts(N), breaks = 100, main = "Count of ratings per movie")

#Recommender system
recommender_pop <- Recommender(R[943:1], method = "POPULAR")
names(getModel(recommender_pop))

#create Top 10
recommend_10 <- predict(recommender_pop, R[940:942], n=10)
recommend_10
as(recommend_10, "list")

#extract sublists
recommend_3 <- bestN(recommend_10, n = 3)
as(recommend_3, "list") 

ratings <- predict(recommender_pop, R[940:942], type = "ratings")
ratings
as(ratings, "matrix")[,1:10]

predict_ratings <- predict(recommender_pop, R[940:942], type = "ratingMatrix")
predict_ratings
as(predict_ratings, "matrix")[,1:10]

evaluation <- evaluationScheme(N, method = "split", given = 3, train = 0.8, goodRating = 4)
evaluation

userbased_model <- Recommender(getData(evaluation,"train"), "UBCF")
userbased_model

P <- predict(userbased_model, getData(evaluation, "known"), type = "ratings")
error <- rbind(UBCF = calcPredictionAccuracy(P, getData(evaluation, "unknown")))
error


#Question5

e <- evaluationScheme(R, method = "cross", k = 5, given = 3, goodRating = 4)
e

userbased_e <- Recommender(getData(e, "train"), "UBCF")


pred_rate <- predict(userbased_e, getData(e, "known"), type = "ratings")
as(pred_rate,"matrix")[1:10,1:5]
getRatingMatrix(pred_rate)[1:10,1:5]

pred_err <- rbind(UBCF = calcPredictionAccuracy(pred_rate, getData(e, "unknown")))
pred_err
ubcf <- evaluate(e, method = "UBCF", n=c(1,3,6,10,13,15))
avg(ubcf)
plot(ubcf)
