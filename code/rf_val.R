####### RF OOB plot generater ######
library(randomForest)
final_tr <- read.csv(file.choose(), header = TRUE)
x.tr <- final_tr[,-ncol(final_tr)]

# Model list
models_sq <- list()
models_half <- list()
models_third <- list()

# OOB errors
err_1 <- vector()
err_2 <- vector()
err_3 <- vector()

# Confusion matrix
cm1 = list()
cm2 = list()
cm3 = list()

nTree = c(50, 75, 100, 150, 200, 300)
set.seed(1234)
## nTree OOB
# m = sqrt(p)
for (i in 1:length(nTree)) {
  rf = randomForest(as.factor(sender) ~ ., data = final_tr, ntree = nTree[i], type = "classification",
                    mtry = max(1, floor(sqrt(ncol(x.tr)))))
  models_sq = append(models_sq, list(rf))
  err_1 = append(err_1, tail(rf$err.rate[,1],1))
  cm1 = append(cm1, list(rf$confusion))
}

# m = 1/2
for (i in 1:length(nTree)) {
  rf = randomForest(as.factor(sender) ~ ., data = final_tr, type = "classification",
                    ntree = nTree[i], mtry = max(1, floor(ncol(x.tr)/2)))
  models_half = append(models_half, list(rf))
  err_2 = append(err_2, tail(rf$err.rate[,1],1))
  cm2 = append(cm2, list(rf$confusion))
}

# m = 1/3
for (i in 1:length(nTree)) {
  rf = randomForest(as.factor(sender) ~ ., data = final_tr, type = "classification",
                    ntree = nTree[i], mtry = max(1, floor(ncol(x.tr)/3)))
  models_third = append(models_third, list(rf))
  err_3 = append(err_3, tail(rf$err.rate[,1],1))
  cm3 = append(cm3, list(rf$confusion))
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