#install.packages("randomForest")
library(randomForest)
#setwd('~/coding/stat154coding/154proj/')

features <- read.csv("data/train/Final_Features.csv", header = TRUE)
testfeatures <- read.csv("Final_Test_Features.csv", header = TRUE)
cv.5 <- rep(NA,5)
nobs <- nrow(features)
ind.cv <- split(sample(1:nobs, replace = FALSE), f = rep(1:5, each = nobs/5))

cv1.train <- features[-ind.cv[[1]],]
cv2.train <- features[-ind.cv[[2]],]
cv3.train <- features[-ind.cv[[3]],]
cv4.train <- features[-ind.cv[[4]],]
cv5.train <- features[-ind.cv[[5]],]

trial0 <- rfcv(features[,-1], features$HRC.V1, 5)

fit.0 <- randomForest(as.factor(cv1.train$HRC.V1) ~ ., data = cv1.train, ntree = n.tree)


yhat.v <- c()
ypred <- data.frame(NA,NA)
MSE <- data.frame(c(1:10),c(1:10))
for (k in 1:10) {
  yhat.v = predict(randomForest(as.factor(cv1.train$HRC.V1) ~ ., data = cv1.train, ntree = k*50)
                   , features[ind.cv[[1]],-1])
  MSE[k,1] <- k*50
  MSE[k,2] <- sum(yhat.v != features[ind.cv[[1]],1]) / nrow(features[ind.cv[[1]],])
}


colnames(MSE) <- c("Number of Trees", "MSE")
plot(MSE)
MSE$k[which.min(MSE$MSE)]

testpredictions <- data.frame(email = testfeatures$V1, prediction = c(rep(0, nrow(testfeatures))))

prediction1 <- predict(randomForest(as.factor(cv1.train$HRC.V1) ~ ., data = cv1.train, ntree = 50)
         , testfeatures)

testpredictions$prediction <- prediction1

