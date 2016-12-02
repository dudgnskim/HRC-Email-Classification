svm1 <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
svm_tune <- tune(svm1, train.x=svm1[,-ncol(svm1)],
 train.y=as.factor(svm1[,ncol(svm1)]), scale = FALSE, kernel="linear",
 ranges=list(cost=c(.1,1,10), gamma=c(.5,1,2)),
 tunecontrol = tune.control(cross = 5))
print(svm_tune)
