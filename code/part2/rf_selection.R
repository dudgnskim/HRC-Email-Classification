#### Select the RandomForest Model from the OOB validated data ####
set.seed(1234)
load('../../data/rf_final.RData')
rownames(err_mat) <- c("p=sqrt(p)", "p=1/2", "p=1/3", "avg_error")

# Choose optimal nTree and mtry value from OOB validation
opt_nTree <- as.numeric(colnames(err_mat)[which(err_mat[4,] %in% min(err_mat[4,]))])
print(paste0("From the OOB validation, we conclude that ", opt_nTree,
             " is the optimal number of nTree"))

opt_mtry <- rownames(err_mat)[which.min(apply(err_mat, 1, mean))]

print(paste0("From the OOB validation, we conclude that ", opt_mtry,
             " is the optimal number of mtry"))
#opt_mtry <- names(err_mat[,which(err_mat[4,] %in% min(err_mat[4,]))]
#                  )[which.min(err_mat[,which(err_mat[4,] %in% min(err_mat[4,]))])]

print(paste0("optimal values = nTree: ", opt_nTree, ", mtry: ", opt_mtry))


# Since we know the optimal hyperparameters of the RandomForest model, we conduct 5-fold CV to find the best model.
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
for (i in 1:k) {
  print(paste0("Running ", i, "th randomForest model"))
  # Time each CV
  time <- proc.time()
  mtry_cv <- max(1, floor(ncol(final_tr[-ind.cv[[i]],])/3))
  rf_cv <- randomForest(as.factor(final_tr[-ind.cv[[i]],]$sender) ~ ., data = final_tr[-ind.cv[[i]],],
                          type = "classification", ntree = opt_nTree, mtry = mtry_cv)
  rf_func <- append(rf_func, list(rf_cv))
  pred_cv = predict(rf_func, final_tr[ind.cv[[i]],-ncol(final_tr)])
  pred_err_cv = sum(pred_cv != final_tr[ind.cv[[i]], ncol(final_tr)]) / nrow(final_tr[ind.cv[[i]],])
  pred_err_hat[i] <- pred_err_cv
  print(paste0(i, "th randomForest model has the Error rate of ", pred_err_cv))
  # Stop the clock
  proc.time() - time
}

#rf_func[[i]] to call out the saved functions
best_rf = rf_func[[which.min(pred_err_hat)]]

# Prediction on Test set
final_test = read.csv('../../data/test/final_test_features.csv', header = T, stringsAsFactors = F)
test_pred = as.numeric(predict(best_rf, final_test))

# Save the best_rf and test prediction
save(best_rf, file = "../../data/best_rf.RData")
save(test_pred, file = "../../predictions/predict.txt")
