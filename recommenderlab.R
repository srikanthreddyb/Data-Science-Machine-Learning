#Load the package and prepare a dataset
# install.packages("recommenderlab")
library("recommenderlab")

# https://www.rdocumentation.org/packages/recommenderlab/versions/0.2-1
# recommenderlab accepts data in form of realRatingMatrix
# https://www.rdocumentation.org/packages/recommenderlab/versions/0.2-1/topics/realRatingMatrix

data("MovieLense")
#The format of MovieLense is an object of class "realRatingMatrix"

## look at the first few ratings of the first user
head(as(MovieLense[1,], "list")[[1]])

## Histogram for the movies and users
# Count of ratings per user (how many ratings have each user provided)
rowCounts(MovieLense)
hist(rowCounts(MovieLense))

# Count of ratings per movie (how many ratings does each movie has)
colCounts(MovieLense)
hist(colCounts(MovieLense))

### use only users with more than 100 ratings
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]
MovieLense100
# Lets visualize the reduced data in the form of a matrix
Movie_matrix100 <- as(MovieLense100, "matrix")

#Train a UBCF recommender using a small training set
train <- MovieLense100[1:50]
rec <- Recommender(train, method = "UBCF")
rec

#Create top-N recommendations for new users (users 101 and 102)
pre <- predict(rec, MovieLense100[101:102], n = 10)
pre

as(pre, "list")
