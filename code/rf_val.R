####### RF OOB plot generater ######
library(randomForest)
final_tr <- read.csv(file.choose(), header = TRUE)
x.tr <- final_tr[,-ncol(final_tr)]

# OOB errors
err_1 <- vector()
err_2 <- vector()
err_3 <- vector()

# Confusion matrix
cm1 = matrix(NA)
cm2 = matrix(NA)
cm3 = matrix(NA)

nTree = c(50, 75, 100, 150, 200, 300)
set.seed(1234)
## nTree OOB
# m = sqrt(p)
for (nt in nTree) {
  rf = randomForest(sender ~ ., data = final_tr, ntree = nt, mtry = max(1, floor(sqrt(ncol(x.tr)))))
  err_1 = append(err_1, tail(rf$err.rate[,1],1))
  cm1 = rf$confusion
}

# m = 1/2
for (nt in nTree) {
  rf = randomForest(sender ~ ., data = final_tr, ntree = nt, mtry = max(1, floor(ncol(x.tr)/2)))
  err_2 = append(err_1, tail(rf$err.rate[,1],1))
  cm2 = rf$confusion
}

# m = 1/3
for (nt in nTree) {
  rf = randomForest(sender ~ ., data = final_tr, ntree = nt, mtry = max(1, floor(ncol(x.tr)/3)))
  err_3 = append(err_1, tail(rf$err.rate[,1],1))
  cm3 = rf$confusion
}

err_mat <- rbind(err_1, err_2, err_3)
colnames(err_mat) <- nTree

# Print OOB error matrix
print(err_mat)

# Confusion matrix
# m = sqrt(p)
print(cm1)

# m = p/2
print(cm2)

# m = p/3
print(cm3)