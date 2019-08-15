data <- read.csv("Loan-imp-val-50k.csv")

#Removing rows with nas
data <- na.omit(data)
colSums(is.na(data))
drops <- c("X.1", "X")
data <- data[ , !(names(data) %in% drops)]


#Creating NAs by random for the 5 variables that have NAs on the original data
na_columns <- data[, c(57, 62, 21, 54, 78)]
na_columns <- as.data.frame(lapply(na_columns, function(cc) cc[sample(c(TRUE, NA), prob = c(0.98, 0.02), size = length(cc), replace = TRUE)]))
colSums(is.na(na_columns))

#Concatenating again
na_data <- data
na_data["mo_sin_old_il_acct"] <- na_columns["mo_sin_old_il_acct"]
na_data["dti"] <- na_columns["dti"]
na_data["bc_util"] <- na_columns["bc_util"]
na_data["percent_bc_gt_75"] <- na_columns["percent_bc_gt_75"]
na_data["mths_since_recent_bc"] <- na_columns["mths_since_recent_bc"]

colSums(is.na(na_data)) #NAs included
rm(na_columns)

#Imputation methods

#Mean or median imputation
mean_imp <- na_data

mean_imp$mo_sin_old_il_acct[is.na(mean_imp$mo_sin_old_il_acct)] <- median(na_data$mo_sin_old_il_acct, na.rm = T)
mean_imp$mths_since_recent_bc[is.na(mean_imp$mths_since_recent_bc)] <- median(na_data$mths_since_recent_bc, na.rm = T)
mean_imp$dti[is.na(mean_imp$dti)] <- median(na_data$dti, na.rm = T)
mean_imp$bc_util[is.na(mean_imp$bc_util)] <- median(na_data$bc_util, na.rm = T)
mean_imp$percent_bc_gt_75[is.na(mean_imp$percent_bc_gt_75)] <- median(na_data$percent_bc_gt_75, na.rm = T)

colSums(is.na(mean_imp))

#MICE imputation
mice_imp <- na_data

require(mice)

mice_imp2 <- subset(mice_imp, select = c("mo_sin_old_il_acct","mths_since_recent_bc","dti", "bc_util", "percent_bc_gt_75", "int_rate", "grade", "annual_inc", "loan_status", "mo_sin_rcnt_tl", "revol_util", "installment", "verification_status", "home_ownership", "purpose", "delinq_2yrs", "open_acc", "revol_bal", "total_pymnt", "acc_now_delinq"))
mice_imputed <- mice(mice_imp2, m = 2, method = "pmm", maxit = 15, seed = 125)

completedData <- complete(mice_imputed,1)
rm(list = c("mice_imp2", "mice_imputed"))
mice_imp["mo_sin_old_il_acct"] <- completedData["mo_sin_old_il_acct"]
mice_imp["dti"] <- completedData["dti"]
mice_imp["bc_util"] <- completedData["bc_util"]
mice_imp["percent_bc_gt_75"] <- completedData["percent_bc_gt_75"]
mice_imp["mths_since_recent_bc"] <- completedData["mths_since_recent_bc"]
rm(completedData)
colSums(is.na(mice_imp))

#KNN imputation
#SAme variables as MICE
knn_data <- na_data
knn_data <- subset(knn_data, select = c("mo_sin_old_il_acct","mths_since_recent_bc","dti", "bc_util", "percent_bc_gt_75", "int_rate", "grade", "annual_inc", "loan_status", "mo_sin_rcnt_tl", "revol_util", "installment", "verification_status", "home_ownership", "purpose", "delinq_2yrs", "open_acc", "revol_bal", "total_pymnt", "acc_now_delinq"))
library(VIM)
knn_imp <- kNN(knn_data, variable = colnames(knn_data), k = 7, dist_var = colnames(knn_data), numFun = median, metric = euclidean)
knn_imp <- knn_imp[, 1:20]
rm(knn_data)

#knn with k=300
knn_data3 <- na_data
knn_data3 <- subset(knn_data3, select = c("mo_sin_old_il_acct","mths_since_recent_bc","dti", "bc_util", "percent_bc_gt_75", "int_rate", "grade", "annual_inc", "loan_status", "mo_sin_rcnt_tl", "revol_util", "installment", "verification_status", "home_ownership", "purpose", "delinq_2yrs", "open_acc", "revol_bal", "total_pymnt", "acc_now_delinq"))
library(VIM)
knn_imp3 <- kNN(knn_data3, variable = colnames(knn_data3), k = 222, dist_var = colnames(knn_data3), numFun = median, metric = euclidean)
knn_imp3 <- knn_imp3[, 1:20]
rm(knn_data3)

#knn with k=1
knn_data1 <- na_data
knn_data1 <- subset(knn_data1, select = c("mo_sin_old_il_acct","mths_since_recent_bc","dti", "bc_util", "percent_bc_gt_75", "int_rate", "grade", "annual_inc", "loan_status", "mo_sin_rcnt_tl", "revol_util", "installment", "verification_status", "home_ownership", "purpose", "delinq_2yrs", "open_acc", "revol_bal", "total_pymnt", "acc_now_delinq"))
library(VIM)
knn_imp1 <- kNN(knn_data1, variable = colnames(knn_data1), k = 1, dist_var = colnames(knn_data1), numFun = median, metric = euclidean)
knn_imp1 <- knn_imp1[, 1:20]
rm(knn_data1)



#Evaluating results
keeps <- c("mo_sin_old_il_acct", "mths_since_recent_bc", "dti", "bc_util", "percent_bc_gt_75")
data2 <- subset(data, select = keeps, drop = FALSE)
knn_imp <- subset(knn_imp, select = keeps, drop = FALSE)
mean_imp <- subset(mean_imp, select = keeps, drop = FALSE)
mice_imp <- subset(mice_imp, select = keeps, drop = FALSE)
knn_imp1 <- subset(knn_imp1, select = keeps, drop = FALSE)
knn_imp3 <- subset(knn_imp3, select = keeps, drop = FALSE)

#MERGE
data2["mo_sin_old_il_acct_mean"] <- mean_imp["mo_sin_old_il_acct"]
data2["dti_mean"] <- mean_imp["dti"]
data2["bc_util_mean"] <- mean_imp["bc_util"]
data2["percent_bc_gt_75_mean"] <- mean_imp["percent_bc_gt_75"]
data2["mths_since_recent_bc_mean"] <- mean_imp["mths_since_recent_bc"]

data2["mo_sin_old_il_acct_mice"] <- mice_imp["mo_sin_old_il_acct"]
data2["dti_mice"] <- mice_imp["dti"]
data2["bc_util_mice"] <- mice_imp["bc_util"]
data2["percent_bc_gt_75_mice"] <- mice_imp["percent_bc_gt_75"]
data2["mths_since_recent_bc_mice"] <- mice_imp["mths_since_recent_bc"]

data2["mo_sin_old_il_acct_knn"] <- knn_imp["mo_sin_old_il_acct"]
data2["dti_knn"] <- knn_imp["dti"]
data2["bc_util_knn"] <- knn_imp["bc_util"]
data2["percent_bc_gt_75_knn"] <- knn_imp["percent_bc_gt_75"]
data2["mths_since_recent_bc_knn"] <- knn_imp["mths_since_recent_bc"]

data2["mo_sin_old_il_acct_knn3"] <- knn_imp3["mo_sin_old_il_acct"]
data2["dti_knn3"] <- knn_imp3["dti"]
data2["bc_util_knn3"] <- knn_imp3["bc_util"]
data2["percent_bc_gt_75_knn3"] <- knn_imp3["percent_bc_gt_75"]
data2["mths_since_recent_bc_knn3"] <- knn_imp3["mths_since_recent_bc"]

data2["mo_sin_old_il_acct_knn1"] <- knn_imp1["mo_sin_old_il_acct"]
data2["dti_knn1"] <- knn_imp1["dti"]
data2["bc_util_knn1"] <- knn_imp1["bc_util"]
data2["percent_bc_gt_75_knn1"] <- knn_imp1["percent_bc_gt_75"]
data2["mths_since_recent_bc_knn1"] <- knn_imp1["mths_since_recent_bc"]

rm(list = c("knn_imp", "mean_imp", "mice_imp", "knn_imp1", "knn_imp3"))

#Getting RMSE scores by performing a linear regression
rmse <- matrix(nrow = 6, ncol = 5)
r_sq <- matrix(nrow = 6, ncol = 5)

rownames(rmse) <- c("dti", "mo_sin_old_il_acct", "mths_since_recent_bc", "bc_util", "percent_bc_gt_75", "Column mean")
colnames(rmse) <- c("knn", "mean", "mice", "knn 222", "knn1")
rownames(r_sq) <- c("dti", "mo_sin_old_il_acct", "mths_since_recent_bc", "bc_util", "percent_bc_gt_75", "Column mean")
colnames(r_sq) <- c("knn", "mean", "mice", "knn 222", "knn1")

dti_knn <- lm(dti ~ dti_knn, data = data2)
predict <- predict(dti_knn, new_data = data2$dti_knn)
true <- data2$dti

rmse[1,1] <- sqrt(mean((predict - true)**2))
r_sq[1,1] <- summary(dti_knn)$r.squared

mo_knn <- lm(mo_sin_old_il_acct ~ mo_sin_old_il_acct_knn, data = data2)
predict <- predict(mo_knn, new_data = data2$mo_sin_old_il_acct_knn)
true <- data2$mo_sin_old_il_acct
rmse[2,1] <- sqrt(mean((predict - true)**2))
r_sq[2,1] <- summary(mo_knn)$r.squared


mt_knn <- lm(mths_since_recent_bc ~ mths_since_recent_bc_knn, data = data2)
predict <- predict(mt_knn, new_data = data2$mths_since_recent_bc_knn)
true <- data2$mths_since_recent_bc
rmse[3,1] <- sqrt(mean((predict - true)**2))
r_sq[3,1] <- summary(mt_knn)$r.squared


bc_knn <- lm(bc_util ~ bc_util_knn, data = data2)
predict <- predict(bc_knn, new_data = data2$bc_util_knn)
true <- data2$bc_util
rmse[4,1] <- sqrt(mean((predict - true)**2))
r_sq[4,1] <- summary(bc_knn)$r.squared


percent_knn <- lm(percent_bc_gt_75 ~ percent_bc_gt_75_knn, data = data2)
predict <- predict(percent_knn, new_data = data2$percent_bc_gt_75_knn)
true <- data2$percent_bc_gt_75
rmse[5,1] <- sqrt(mean((predict - true)**2))
r_sq[5,1] <- summary(percent_knn)$r.squared


dti_mean <- lm(dti ~ dti_mean, data = data2)
predict <- predict(dti_mean, new_data = data2$dti_mean)
true <- data2$dti

rmse[1,2] <- sqrt(mean((predict - true)**2))
r_sq[1,2] <- summary(dti_mean)$r.squared

mo_mean <- lm(mo_sin_old_il_acct ~ mo_sin_old_il_acct_mean, data = data2)
predict <- predict(mo_mean, new_data = data2$mo_sin_old_il_acct_mean)
true <- data2$mo_sin_old_il_acct
rmse[2,2] <- sqrt(mean((predict - true)**2))
r_sq[2,2] <- summary(mo_mean)$r.squared


mt_mean <- lm(mths_since_recent_bc ~ mths_since_recent_bc_mean, data = data2)
predict <- predict(mt_mean, new_data = data2$mths_since_recent_bc_mean)
true <- data2$mths_since_recent_bc
rmse[3,2] <- sqrt(mean((predict - true)**2))
r_sq[3,2] <- summary(mt_mean)$r.squared


bc_mean <- lm(bc_util ~ bc_util_mean, data = data2)
predict <- predict(bc_mean, new_data = data2$bc_util_mean)
true <- data2$bc_util
rmse[4,2] <- sqrt(mean((predict - true)**2))
r_sq[4,2] <- summary(bc_mean)$r.squared


percent_mean <- lm(percent_bc_gt_75 ~ percent_bc_gt_75_mean, data = data2)
predict <- predict(percent_mean, new_data = data2$percent_bc_gt_75_mean)
true <- data2$percent_bc_gt_75
rmse[5,2] <- sqrt(mean((predict - true)**2))
r_sq[5,2] <- summary(percent_mean)$r.squared


dti_mice <- lm(dti ~ dti_mice, data = data2)
predict <- predict(dti_mice, new_data = data2$dti_mice)
true <- data2$dti

rmse[1,3] <- sqrt(mean((predict - true)**2))
r_sq[1,3] <- summary(dti_mice)$r.squared


mo_mice <- lm(mo_sin_old_il_acct ~ mo_sin_old_il_acct_mice, data = data2)
predict <- predict(mo_mice, new_data = data2$mo_sin_old_il_acct_mice)
true <- data2$mo_sin_old_il_acct
rmse[2,3] <- sqrt(mean((predict - true)**2))
r_sq[2,3] <- summary(mo_mice)$r.squared


mt_mice <- lm(mths_since_recent_bc ~ mths_since_recent_bc_mice, data = data2)
predict <- predict(mt_mean, new_data = data2$mths_since_recent_bc_mice)
true <- data2$mths_since_recent_bc
rmse[3,3] <- sqrt(mean((predict - true)**2))
r_sq[3,3] <- summary(mt_mice)$r.squared


bc_mice <- lm(bc_util ~ bc_util_mice, data = data2)
predict <- predict(bc_mice, new_data = data2$bc_util_mice)
true <- data2$bc_util
rmse[4,3] <- sqrt(mean((predict - true)**2))
r_sq[4,3] <- summary(bc_mice)$r.squared


percent_mice <- lm(percent_bc_gt_75 ~ percent_bc_gt_75_mice, data = data2)
predict <- predict(percent_mice, new_data = data2$percent_bc_gt_75_mice)
true <- data2$percent_bc_gt_75
rmse[5,3] <- sqrt(mean((predict - true)**2))
r_sq[5,3] <- summary(percent_mice)$r.squared

dti_knn3 <- lm(dti ~ dti_knn3, data = data2)
predict <- predict(dti_knn3, new_data = data2$dti_knn3)
true <- data2$dti

rmse[1,4] <- sqrt(mean((predict - true)**2))
r_sq[1,4] <- summary(dti_knn3)$r.squared

mo_knn3 <- lm(mo_sin_old_il_acct ~ mo_sin_old_il_acct_knn3, data = data2)
predict <- predict(mo_knn3, new_data = data2$mo_sin_old_il_acct_knn3)
true <- data2$mo_sin_old_il_acct
rmse[2,4] <- sqrt(mean((predict - true)**2))
r_sq[2,4] <- summary(mo_knn3)$r.squared


mt_knn3 <- lm(mths_since_recent_bc ~ mths_since_recent_bc_knn3, data = data2)
predict <- predict(mt_knn3, new_data = data2$mths_since_recent_bc_knn3)
true <- data2$mths_since_recent_bc
rmse[3,4] <- sqrt(mean((predict - true)**2))
r_sq[3,4] <- summary(mt_knn3)$r.squared


bc_knn3 <- lm(bc_util ~ bc_util_knn3, data = data2)
predict <- predict(bc_knn3, new_data = data2$bc_util_knn3)
true <- data2$bc_util
rmse[4,4] <- sqrt(mean((predict - true)**2))
r_sq[4,4] <- summary(bc_knn3)$r.squared


percent_knn3 <- lm(percent_bc_gt_75 ~ percent_bc_gt_75_knn3, data = data2)
predict <- predict(percent_knn3, new_data = data2$percent_bc_gt_75_knn3)
true <- data2$percent_bc_gt_75
rmse[5,4] <- sqrt(mean((predict - true)**2))
r_sq[5,4] <- summary(percent_knn3)$r.squared


dti_knn3 <- lm(dti ~ dti_knn3, data = data2)
predict <- predict(dti_knn3, new_data = data2$dti_knn3)
true <- data2$dti

rmse[1,5] <- sqrt(mean((predict - true)**2))
r_sq[1,5] <- summary(dti_knn3)$r.squared

mo_knn3 <- lm(mo_sin_old_il_acct ~ mo_sin_old_il_acct_knn3, data = data2)
predict <- predict(mo_knn3, new_data = data2$mo_sin_old_il_acct_knn3)
true <- data2$mo_sin_old_il_acct
rmse[2,5] <- sqrt(mean((predict - true)**2))
r_sq[2,5] <- summary(mo_knn3)$r.squared


mt_knn3 <- lm(mths_since_recent_bc ~ mths_since_recent_bc_knn3, data = data2)
predict <- predict(mt_knn3, new_data = data2$mths_since_recent_bc_knn3)
true <- data2$mths_since_recent_bc
rmse[3,5] <- sqrt(mean((predict - true)**2))
r_sq[3,5] <- summary(mt_knn3)$r.squared


bc_knn3 <- lm(bc_util ~ bc_util_knn3, data = data2)
predict <- predict(bc_knn3, new_data = data2$bc_util_knn3)
true <- data2$bc_util
rmse[4,5] <- sqrt(mean((predict - true)**2))
r_sq[4,5] <- summary(bc_knn3)$r.squared


percent_knn3 <- lm(percent_bc_gt_75 ~ percent_bc_gt_75_knn3, data = data2)
predict <- predict(percent_knn3, new_data = data2$percent_bc_gt_75_knn3)
true <- data2$percent_bc_gt_75
rmse[5,5] <- sqrt(mean((predict - true)**2))
r_sq[5,5] <- summary(percent_knn3)$r.squared

dti_knn1 <- lm(dti ~ dti_knn1, data = data2)
predict <- predict(dti_knn1, new_data = data2$dti_knn1)
true <- data2$dti

rmse[1,5] <- sqrt(mean((predict - true)**2))
r_sq[1,5] <- summary(dti_knn1)$r.squared

mo_knn1 <- lm(mo_sin_old_il_acct ~ mo_sin_old_il_acct_knn1, data = data2)
predict <- predict(mo_knn1, new_data = data2$mo_sin_old_il_acct_knn1)
true <- data2$mo_sin_old_il_acct
rmse[2,5] <- sqrt(mean((predict - true)**2))
r_sq[2,5] <- summary(mo_knn1)$r.squared


mt_knn1 <- lm(mths_since_recent_bc ~ mths_since_recent_bc_knn1, data = data2)
predict <- predict(mt_knn1, new_data = data2$mths_since_recent_bc_knn1)
true <- data2$mths_since_recent_bc
rmse[3,5] <- sqrt(mean((predict - true)**2))
r_sq[3,5] <- summary(mt_knn1)$r.squared


bc_knn1 <- lm(bc_util ~ bc_util_knn1, data = data2)
predict <- predict(bc_knn1, new_data = data2$bc_util_knn1)
true <- data2$bc_util
rmse[4,5] <- sqrt(mean((predict - true)**2))
r_sq[4,5] <- summary(bc_knn1)$r.squared


percent_knn1 <- lm(percent_bc_gt_75 ~ percent_bc_gt_75_knn1, data = data2)
predict <- predict(percent_knn1, new_data = data2$percent_bc_gt_75_knn1)
true <- data2$percent_bc_gt_75
rmse[5,5] <- sqrt(mean((predict - true)**2))
r_sq[5,5] <- summary(percent_knn1)$r.squared

rmse[6,] <- colMeans(rmse, na.rm = TRUE)
r_sq[6,] <- colMeans(r_sq, na.rm = TRUE)

#Saving table for the report
rmse <- as.data.frame(rmse)
r_sq <- as.data.frame(r_sq)

write.csv(rmse, file = "rmse.csv")
write.csv(r_sq, file = "r_sq.csv")

#k-s testto check if the distribution remains

ks.test(data2$dti, data2$dti_knn3)
ks.test(data2$mo_sin_old_il_acct, data2$mo_sin_old_il_acct_knn3)
ks.test(data2$mths_since_recent_bc, data2$mths_since_recent_bc_knn3)
ks.test(data2$bc_util, data2$bc_util_knn3)
ks.test(data2$percent_bc_gt_75, data2$percent_bc_gt_75_knn3)



