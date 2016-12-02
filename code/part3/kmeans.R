### 0. Library
pkg = c("dplyr")
new.pkg = pkg[!(pkg %in% installed.packages()[,"Package"])]
if (length(new.pkg)) {install.packages(new.pkg,dependencies = TRUE)}
sapply(pkg,require,character.only = TRUE)

### 1. Load Data
features_tr <- read.csv('../../data/train/Final_Features.csv', header = TRUE)
# DATA IMPORT FROM RF needed
load('../../data/rf_sample.RData')
# rf <- load('../../data/rf_data.RData')
hund <- sort(fit.0$importance, decreasing = T)[1:100]
rftophund <- rownames(fit.0$importance)[which(fit.0$importance %in% hund)]

### 2. Function to prepare data matrix for kmeans
prep_dm = function(data.frame, predictor_names) {
  # Converting data.frame to data.matrix filtered by predictor names given
  df = data.frame
  top_hundred_filtered_df <- df[,which(colnames(df) %in% predictor_names)]
  dm <- data.matrix(top_hundred_filtered_df)
  
  # Normalize dm
  for(z in 1:nrow(dm)){
	  row <- dm[z,]
	  dm[z,] <- row / sqrt(row %*% row)
  }
  
  # Remove NAs if there exist
  if (sum(apply(dm, c(1,2), is.na)) > 0) {
    dm <- na.omit(dm)
  }
  return(dm)
}

### 3. Prepare data for K-means
# Prepped data
dm_full <- prep_dm(features_tr, rftophund)
y.tr <- features_tr[,ncol(features_tr)]

# k-fold CV data (for quick-running one segment)
set.seed(1234)
k = 5 # Number of Folds
num_clusters <- 5 # Number of response variable classes
nobs <- nrow(dm_full)
ind.cv <- split(sample(1:nobs, replace = FALSE), f = rep(1:k, each = nobs/k))

#data *REMOVE FOR SUBMISSION*
#cv1.train <- dm_full[-ind.cv[[1]],]
#cv2.train <- dm_full[-ind.cv[[2]],]
#cv3.train <- dm_full[-ind.cv[[3]],]
#cv4.train <- dm_full[-ind.cv[[4]],]
#cv5.train <- dm_full[-ind.cv[[5]],]

#response_var *REMOVE FOR SUBMISSION*
#y1.train <- y.tr[-ind.cv[[1]]]
#y2.train <- y.tr[-ind.cv[[2]]]
#y3.train <- y.tr[-ind.cv[[3]]]
#y4.train <- y.tr[-ind.cv[[4]]]
#y5.train <- y.tr[-ind.cv[[5]]]

### 4. Run k-fold CV
l = 9 # Number of multiple of nstart from 20 to 100 (by 10).
err = t(data.frame(rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l)))
withins <- t(data.frame(rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l)))
rownames(err) <- c("CV1","CV2","CV3","CV4","CV5")
colnames(err) <- c(seq(20, 100, by = 10))
rownames(withins) <- c("CV1", "CV2", "CV3", "CV4", "CV5")
colnames(withins) <- c(seq(20,100,by=10))
print(paste0(k, " fold Cross-validation for nstart from 20 to 100"))
for (i in 1:k) {
  dm_cv <- dm_full[-ind.cv[[i]],]
  y_cv <- y.tr[-ind.cv[[i]]]
  for (j in 1:l) {
    kc <- kmeans(dm_cv, centers = num_clusters, nstart = (10+j*10))
    dt_cv <- as.matrix(table(kc$cluster, y_cv))
    err[i,j] = sum(diag(dt_cv)) / sum(dt_cv)
    withins[i,j] = sum(kc$withinss)
    print(paste0("CV for ", rownames(err)[i], ", nstart: ", colnames(err)[j], " completed"))
  }
}

print(err)
print(withins)

# Find the lowest error rate & withins
avg_err <- vector()
avg_w <- vector()
for (o in 1:ncol(err)) {
  avg_err[o] = mean(err[,o])
  avg_w[o] = mean(withins[,o])
}
# Optinal nstart value from CV
opt_nstart_err = which(avg_err %in% min(avg_err)) * 10 + 10
opt_nstart_wit = which(avg_w %in% min(avg_w)) * 10 + 10

print(paste0("Optimal nstart for error is ", opt_nstart_err[1], ", ",
             "Optimal nstart for withinss is ", opt_nstart_wit[1]))


### 5. Run K-means with the best hyperparameter on full data
# Kmeans for Error
km_final_err <- kmeans(dm_full, centers = num_clusters, nstart = opt_nstart_err[1])
dte_full <- table(km_final_err$cluster, y.tr)
kfe_err <- sum(diag(dte_full)) / sum(dte_full)
print(paste0("Kmeans error rate: ", kfe_err))

# Kmeans for Withinss
km_final_wit <- kmeans(dm_full, centers = num_clusters, nstart = opt_nstart_wit[1])
dtw_full <- table(km_final_wit$cluster, y.tr)
kfw_dist <- km_final_wit$withinss
kfw_err <- sum(diag(dtw_full)) / sum(dtw_full)
print(paste0("Length of the prediction vector: ", length(km_final_wit$cluster)))
print(paste0("Kmeans error rate with withins distance: ", kfw_err))
print(paste0("Within cluster distance: ", kfw_dist))

##
#save(fit.0, file = "../../../rf_sample.RData")

# Run kmeans on Test set
# Load Final_features_test.csv for prediction
ff_test <- read.csv('../../data/test/final_test_features.csv', header = T)
dm_test <- prep_dm(ff_test, rftophund)

# CV for test set
k = 4
l = 9
test_w <- t(data.frame(rep(NA,l),rep(NA,l),rep(NA,l),rep(NA,l)))
rownames(test_w) <- c("CV1", "CV2", "CV3", "CV4")
colnames(test_w) <- c(seq(20,100,by=10))
nobs_t = nrow(dm_test)
ind.cv_t <- split(sample(1:nobs_t, replace = FALSE), f = rep(1:k, each = nobs_t/k))
print(paste0(k, " fold Cross-validation for nstart from 20 to 100"))
for (i in 1:k) {
  dm_cv <- dm_test[-ind.cv_t[[i]],]
  for (j in 1:l) {
    kc <- kmeans(dm_cv, centers = num_clusters, nstart = (10+j*10))
    test_w[i,j] = sum(kc$withinss)
    print(paste0("CV for ", rownames(test_w)[i], ", nstart: ", colnames(test_w)[j], " completed"))
  }
}
# Selecting optimal nstart for the test set
avg_wt <- vector()
for (o in 1:ncol(test_w)) {
  avg_wt[o] = mean(test_w[,o])
}
# Optinal nstart value from CV
opt_nstart_t = which(avg_wt %in% min(avg_wt)) * 10 + 10
print(paste0("Optimal nstart for withinss test is ", opt_nstart_t[1]))

# Two models
km_test_1 <- kmeans(dm_test, centers = num_clusters, nstart = opt_nstart_wit[1])
km_test_2 <- kmeans(dm_test, centers = num_clusters, nstart = opt_nstart_t[1])

# Acc
pred_test <- read.table('../../predictions/predict.txt')
pred_test <- pred_test[,1]
acc_1 <- sum(km_test_1$cluster == pred_test) / length(pred_test)
acc_2 <- sum(km_test_2$cluster == pred_test) / length(pred_test)

# Comparison
print(paste0(acc_1, "% match for nstart from training optimal withinss"))
print(paste0(acc_2, "% match for nstart from test optimal withinss"))
