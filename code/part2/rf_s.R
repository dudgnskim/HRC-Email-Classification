# Load the data
library(randomForest)
final_tr <- read.csv('../../data/train/Final_Features.csv', header = T, stringsAsFactors = F)
y.tr <- final_tr[,ncol(final_tr)]

# k-fold (k = 5)
k = 5
cv.5 <- rep(NA,5)
nobs <- nrow(final_tr)
ind.cv <- split(sample(1:nobs, replace = FALSE), f = rep(1:5, each = nobs/5))
# Run CV
print(paste0(k, " fold Cross-validation for best model"))
rf_func <- list()
pred_err_hat <- rep(NA, k)

#data *REMOVE FOR SUBMISSION*
cv1.train <- final_tr[-ind.cv[[1]],]
cv2.train <- final_tr[-ind.cv[[2]],]
cv3.train <- final_tr[-ind.cv[[3]],]
cv4.train <- final_tr[-ind.cv[[4]],]
cv5.train <- final_tr[-ind.cv[[5]],]

#response_var *REMOVE FOR SUBMISSION*
y1.train <- y.tr[-ind.cv[[1]]]
y2.train <- y.tr[-ind.cv[[2]]]
y3.train <- y.tr[-ind.cv[[3]]]
y4.train <- y.tr[-ind.cv[[4]]]
y5.train <- y.tr[-ind.cv[[5]]]

#CV1
time = proc.time()
rfcv1 <- randomForest(as.factor(y1.train) ~ ., data = cv1.train,
                      type = "classification", ntree = 200, mtry = max(1, floor(ncol(cv1.train)/3)))
print(proc.time() - time)
save(rfcv1, file = '../../../rfcv1.RData')

#CV2
time = proc.time()
rfcv1 <- randomForest(as.factor(y2.train) ~ ., data = cv2.train,
                      type = "classification", ntree = 200, mtry = max(1, floor(ncol(cv2.train)/3)))
print(proc.time() - time)
save(rfcv2, file = '../../../rfcv2.RData')

#CV3
time = proc.time()
rfcv1 <- randomForest(as.factor(y3.train) ~ ., data = cv3.train,
                      type = "classification", ntree = 200, mtry = max(1, floor(ncol(cv3.train)/3)))
print(proc.time() - time)
save(rfcv3, file = '../../../rfcv3.RData')

#CV4
time = proc.time()
rfcv4 <- randomForest(as.factor(y4.train) ~ ., data = cv4.train[,-which(colnames(cv4.train) %in% 'break.')],
                      type = "classification", ntree = 200,
                      mtry = max(1, floor(ncol(cv4.train[,-which(colnames(cv4.train) %in% 'break.')])/3)))
print(proc.time() - time)
save(rfcv4, file = '../../../rfcv4.RData')

#CV5
time = proc.time()
rfcv1 <- randomForest(as.factor(y5.train) ~ ., data = cv5.train,
                      type = "classification", ntree = 200, mtry = max(1, floor(ncol(cv5.train)/3)))
print(proc.time() - time)
save(rfcv5, file = '../../../rfcv5.RData')

for (i in 1:k) {
  print(paste0("Running ", i, "th randomForest model"))
  # Time each CV
  time <- proc.time()
  mtry_cv <- max(1, floor(ncol(final_tr[-ind.cv[[i]],])/3))
  rf_cv <- randomForest(as.factor(final_tr[-ind.cv[[i]],]$sender) ~ ., data = final_tr[-ind.cv[[i]],],
                        type = "classification", ntree = opt_nTree, mtry = mtry_cv)
  rf_func <- append(rf_func, list(rf_cv))
  pred_cv = predict(rf_cv, final_tr[ind.cv[[i]],-ncol(final_tr)])
  pred_err_cv = sum(pred_cv != final_tr[ind.cv[[i]], ncol(final_tr)]) / nrow(final_tr[ind.cv[[i]],])
  pred_err_hat[i] <- pred_err_cv
  print(paste0(i, "th randomForest model has the Error rate of ", pred_err_cv))
  # Stop the clock
  proc.time() - time
}
print(paste0("CV finished"))
save(rf_func, pred_err_hat, file = "../../data/rf_models.RData")

#rf_func[[i]] to call out the saved functions
best_rf = rf_func[[which.min(pred_err_hat)]]
save(best_rf, file = "../../data/best_rf.RData")

# Prediction on Test set
final_test = read.csv('../../data/test/final_test_features.csv', header = T, stringsAsFactors = F)
test_pred = as.numeric(predict(rfcv4, final_test))

# Save the test prediction
save(test_pred, file = "../../predictions/predict.txt")