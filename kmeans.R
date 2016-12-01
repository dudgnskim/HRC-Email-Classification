setwd("C:/Users/Edward/Desktop/S7/Stat 154/Final Project")
train <- read.csv("Final_Features.csv", header = TRUE)
library(stats)
?kmeans
cluster1 <- kmeans(features[ind.cv[[1]],-1], 5, nstart = 50)
table(cluster1$cluster, features[ind.cv[[1]],1])

cv.5 <- rep(NA,5)
nobs <- nrow(train)
ind.cv <- split(sample(1:nobs, replace = FALSE), f = rep(1:5, each = nobs/5))

cv1.train <- train[-ind.cv[[1]],]
cv2.train <- train[-ind.cv[[2]],]
cv3.train <- train[-ind.cv[[3]],]
cv4.train <- train[-ind.cv[[4]],]
cv5.train <- train[-ind.cv[[5]],]



table(cluster1$cluster, train[ind.cv[[1]],1])
