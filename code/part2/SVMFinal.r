library(e1071)
library(caret)

features <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
svmsample <- sample(1:3505, 1500, replace = FALSE)
svm1 <- features[svmsample,]

svm1a <- svm(as.factor(sender) ~., data = svm1, scale = FALSE, kernel = "radial")
pred1a <- predict(svm1a,features[-svmsample,-ncol(cv1.train)])
table(pred1a, features[-svmsample,ncol(cv1.train)])

svm1b <- svm(as.factor(sender) ~., data = svm1, scale = FALSE, kernel = "linear")
pred2a <- predict(svm2,features[-svmsample,-ncol(cv1.train)])
table(pred2a, features[-svmsample,ncol(cv1.train)])

svm3 <- svm(as.factor(sender) ~., data = svm1, scale = FALSE, kernel = "polynomial")
pred3a <- predict(svm3,features[-svmsample,-ncol(cv1.train)])
table(pred3a, features[-svmsample,ncol(cv1.train)])


svm_tune <- tune(svm, train.x=svm1[,-ncol(svm1)], train.y=as.factor(svm1[,ncol(svm1)]), scale = FALSE, kernel="linear", 
                 ranges=list(cost=.1, gamma=.5), tunecontrol = tune.control(cross = 5))
print(svm_tune)
svm_tune$performances
