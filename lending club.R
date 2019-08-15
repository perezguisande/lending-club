library(data.table)
#data2 <- fread("loan.csv")

#data2 <- as.data.frame(data2)
#colnames(data2)

#Subset of the data
set.seed(50)
#data2 <- data2[sample(nrow(data2), 75000), ]
#write.csv(data, file = "Loan-subset-75k.csv")
data2 <- read.csv("Loan-subset-50k.csv")

#Missing values
colSums(is.na(data2))

#Removing columns with more than 80% NA

data2 <- data2[, -which(colMeans(is.na(data2)) > 0.2)]
colSums(is.na(data2))

#Drop rows
library(tidyr)
data2 <- data2 %>% drop_na(avg_cur_bal)
colSums(is.na(data2))

#Column drop. Many NAs or no values (but no NAs). Also variables on hardship and settlement. Their presence indicates a loan with problems
drops <- c("mths_since_recent_inq", "desc", "verification_status_joint", "sec_app_earliest_cr_line", "hardship_type", "hardship_flag", "hardship_start_date", "hardship_end_date", "payment_plan_start_date", "hardship_loan_status", "debt_settlement_date",
"debt_settlement_flag", "settlement_status", "settlement_date", "debt_settlement_flag_date", "hardship_status", "hardship_reason")
data2 <- data2[ , !(names(data2) %in% drops)]




#Na replacing
#Replace with 0's
setDT(data2)
data2$bc_open_to_buy[is.na(data2$bc_open_to_buy)] <- 0
data2$num_tl_120dpd_2m[is.na(data2$num_tl_120dpd_2m)] <- 0
#Replace with 100's
data2$pct_tl_nvr_dlq[is.na(data2$pct_tl_nvr_dlq)] <- 100
data2 <- as.data.frame(data2)

#saving csv for imputation valuation
#write.csv(data2, file = "Loan-imp-val-50k.csv")


#knn with k=300
knn_data <- as.data.frame(data2)
knn_data <- subset(knn_data, select = c("mo_sin_old_il_acct","mths_since_recent_bc","dti", "bc_util", "percent_bc_gt_75", "int_rate", "grade", "annual_inc", "loan_status", "mo_sin_rcnt_tl", "revol_util", "installment", "verification_status", "home_ownership", "purpose", "delinq_2yrs", "open_acc", "revol_bal", "total_pymnt", "acc_now_delinq", "revol_util"))
library(VIM)
knn_imp <- kNN(knn_data, variable = colnames(knn_data), k = 222, dist_var = colnames(knn_data), numFun = median, metric = euclidean)
rm(knn_data)

#Merge data
data2["mo_sin_old_il_acct"] <- knn_imp["mo_sin_old_il_acct"]
data2["dti"] <- knn_imp["dti"]
data2["bc_util"] <- knn_imp["bc_util"]
data2["percent_bc_gt_75"] <- knn_imp["percent_bc_gt_75"]
data2["mths_since_recent_bc"] <- knn_imp["mths_since_recent_bc"]
data2["revol_util"] <- knn_imp["revol_util"]

#MICE
#mice_imp <- data2

#require(mice)

#mice_imp2 <- subset(mice_imp, select = c("mo_sin_old_il_acct","mths_since_recent_bc","dti", "bc_util", "percent_bc_gt_75", "int_rate", "grade", "annual_inc", "loan_status", "mo_sin_rcnt_tl", "revol_util", "installment", "verification_status", "home_ownership", "purpose", "delinq_2yrs", "open_acc", "revol_bal", "total_pymnt", "acc_now_delinq"))
#mice_imputed <- mice(mice_imp2, m = 2, method = "pmm", maxit = 15, seed = 125)

#completedData <- complete(mice_imputed,1)
#rm(list = c("mice_imp2", "mice_imputed"))
#mice_imp["mo_sin_old_il_acct"] <- completedData["mo_sin_old_il_acct"]
#mice_imp["dti"] <- completedData["dti"]
#mice_imp["bc_util"] <- completedData["bc_util"]
#mice_imp["percent_bc_gt_75"] <- completedData["percent_bc_gt_75"]
#mice_imp["mths_since_recent_bc"] <- completedData["mths_since_recent_bc"]
#mice_imp["revol_util"] <- completedData["revol_util"]

#Saving csv
#write.csv(data2, file = "Loan-mice-pmm-1.csv")
write.csv(data2, file = "Loan-knn-222-50k.csv")
#write.csv(mice_imp, file = "loan-mice-50k.csv")





#