library(data.table)
library(plyr)   
library(dplyr)
library(caret)
library(rsample)
library(tidyverse)
library(DMwR)

data2 <- fread("prepared_data_nocurrent-50k-adrate-2.csv")
data2 <- as.data.frame(data2)

drops <- c("V1")
data2 <- select(data2, -drops)


set.seed(482)
data_split <- initial_split(data2, prop = .696531473)
data_split_muchless <- initial_split(data2, prop = 1/25)
train <- training(data_split)
test  <- testing(data_split)
#test <- test[sample(nrow(test), 500), ]


train_control <- trainControl(method = "cv", number = 5)




model <- glm(default ~.,family=binomial(link='logit'),data=train)
summary(model)



#consufion matrix
pred <- predict(model, newdata = test)
preds <- ifelse(pred < 0.25, 0, 1)
preds <- as.factor(preds)
real <- as.factor(test$default)
confusionMatrix(preds, real)

library(pROC)
roc_curve <- roc(response = real, predictor = as.numeric(preds)-1, type = "response")
plot(roc(test$default, (as.numeric(predict(model, test, type = "response"))-1)),
     col="black", lwd=3, main="Figure 3.1: ROC curve example")
result.coords <- coords(roc_curve, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)
#h measure
library(hmeasure)
h_baseline_log <- misclassCounts(real, preds)
print(h_baseline_log$metrics, digits = 3)

#Removing non-significant variables

#drops <- c("total_acc", "total_il_high_credit_limit", "tot_cur_bal", "total_bal_ex_mort", "tot_hi_cred_lim", "total_bc_limit", "total_rev_hi_lim", "revol_bal", "installment", "bc_open_to_buy", "revol_util", "bc_util", "int_rate")
#data2 <- select(data2, -drops)
#data2 <- data2 %>% select(-contains("addr_state"))

#Handling imbalanced data
#library(ROSE)
#train <- ovun.sample(default~., train, method="both", 1000, p=0.5,
                #   subset=options("subset")$subset,
                 #  na.action=options("na.action")$na.action, seed = 42)
#train <- train$data

#write.csv(train, "train.csv")
#write.csv(test, "test.csv")

#train <- read.csv("train.csv")
#test <- read.csv("test.csv")

library(DMwR)
train$default <- as.factor(train$default)
train <- SMOTE(default~., train, perc.over = 100, k = 222, perc.under = 200)

#Saving splits
#write.csv(train, "train_sampled.csv")
#write.csv(test, "test_data.csv")
train <- train[complete.cases(train),]


library(glm2)
model2 <- glm2(default ~.,family=binomial(link='logit'),data=train)



train$default <- as.factor(train$default)



library(jtools)
summ(model2)


#consufion matrix
pred2 <- predict(model2, newdata = test)
preds2 <- ifelse(pred2 < 0.25, 0, 1)
preds2 <- as.factor(preds2)
real <- as.factor(test$default)
confusionMatrix(preds2, real)
line <- runif(8173, min = 0.5, max = 0.5)

library(pROC)
roc_log <- roc(response = as.vector(test$default), predictor = pred2, auc = T)
line <- runif(158, min = 0.5, max = 0.5)
roc_line <- roc(response = as.vector(test$default), predictor = line, auc = T)
ggroc(list(Logistic = roc_log, Random = roc_line))

scores <- data.frame(base=pred,base_over=pred2)
library(Laurae)
get.max_f1(pred2, test$default)
results <- HMeasure(test$default,scores, threshold = 0.5)
summary(results)
results$metrics


#knn classifier (with oversampled data)

library(class)
drops <- c("default")

y_train <- train$default
x_train <- dplyr::select(train, -drops)

y_test <- test$default
x_test <- dplyr::select(test, -drops)

y_train <- factor(y_train)
y_test <- factor(y_test)

#saving for stacking
#write.csv(x_train, "x_train.csv")
#write.csv(x_test, "x_test.csv")
#write.csv(y_train, "y_train.csv")
#write.csv(y_test, "y_test.csv")


#finding best k
hyper_grid <- expand.grid(
  k = seq(1, 301, by = 10)
)
library(pROC)

#for(i in 1:nrow(hyper_grid)) {

#train model
#model <- knn(

 # train = x_train,
#  test = x_test,
#  cl = y_train,
 # k = hyper_grid$k[i]
#  )
#hyper_grid$auc[i] <- auc(response = real, predictor = (as.numeric(model)-1), type = "response")
#}

#hyper_grid[which.max(hyper_grid$auc),]
#best k is 291

y_train <- revalue(y_train, c("0"="paid", "1"="default"))
y_test <- revalue(y_test, c("0"="paid", "1"="default"))

control <- trainControl(
  method = "cv",
  number = 5,
  allowParallel = F,
  savePredictions = "all",
  returnData = F,
  verboseIter = F,
  classProbs = T
)
knn_grid <- expand.grid(k =291)

model_knn <- caret::train(
  x = x_train,
  y = y_train,
  trControl = control,
  tuneGrid = knn_grid,
  method = "knn",
  metric = "Kappa"
)
y_test <- revalue(y_test, c("paid"="0", "default"="1"))
y_train <- revalue(y_train, c("paid"="0", "default"="1"))

preds_knn <- predict(model_knn, newdata = x_test, type = "prob")
preds_knn <- preds_knn$default
get.max_f1(preds_knn, test$default)


line <- runif(8268, min = 0.5, max = 0.5)
roc_line <- roc(response = as.vector(test$default), predictor = line, auc = T)
roc_knn <- roc(response = as.vector(test$default), predictor = preds_knn)
ggroc(list(Logistic = roc_log, Random = roc_line, kNN = roc_knn))

scores <- data.frame(base_over=pred2, knn = preds_knn)
results <- HMeasure(test$default,scores, severity.ratio = 0.125, threshold = c(0.35,0.4421769))
summary(results)
results$metrics

#Naive Bayes
library(e1071)

model_nb = naiveBayes(default~., data=train)
print(model_nb)

pred_nb <- predict(model_nb, newdata = test, type = "raw")
pred_nb <- pred_nb[,2]
preds_nb <- ifelse(pred_nb < 0.5, 0, 1)
preds2_nb <- as.factor(preds_nb)
real <- as.factor(test$default)
confusionMatrix(preds2_nb, as.factor(y_test))
auc_NB <- auc(response = as.factor(y_test), predictor = as.numeric(preds2_nb), type = "response")
get.max_f1(pred_nb, test$default)

scores <- data.frame(base=pred,base_over=pred2, knn = (as.numeric(model_knn)-1), NB = pred_nb)
results <- HMeasure(test$default,scores, severity.ratio = 0.125, threshold = c(0.35, -0.215, 0.5, 0.37609))
summary(results)
results$metrics

roc_nb <- roc(response = as.vector(test$default), predictor = preds_nb)
ggroc(list(Logistic = roc_log, Random = roc_line, NB = roc_nb))

#SVM with CAret
#Toooo slow
#control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#set.seed(333)
#library(doParallel)
#registerDoParallel(cores = 5)
#model_svm_linear <- train(default~., data = train, method = "svmLinear", trControl = control, allowParallel=TRUE)


#Random Forest
library(randomForest) 
library(ranger)   

#finding best parameters

hyper_grid <- expand.grid(
  num.trees = c(550, 600, 700, 900),
  mtry       = c(4, 3),
  node_size  = 7,
  sampe_size = c(.632)
)

# total number of combinations
nrow(hyper_grid)

#for(i in 1:nrow(hyper_grid)) {

 
 #model <- ranger(
#   formula         = default ~ ., 
 #   data            = train, 
  #  num.trees       = hyper_grid$num.trees[i],
  #  mtry            = hyper_grid$mtry[i],
#    min.node.size   = hyper_grid$node_size[i],
#   sample.fraction = hyper_grid$sampe_size[i],
 #   seed            = 123
#  )
# preds_rf <- predict(model, data = test, type = "response")
# preds_rf <- as.numeric(preds_rf$predictions)
#  hyper_grid$auc[i] <- auc(response = real, predictor = preds_rf, type = "response")
#}
#hyper_grid[which.max(hyper_grid$auc),]

#Best tune mtry3 nodesize7 sampe size0.7 ntree500


#hyper_grid %>% 
 #   dplyr::arrange(auc) %>%
#    tail(10)


#RF with caret

set.seed(18)
drops <- c("default")
y_train <- train$default
x_train <- dplyr::select(train, -drops)

y_test <- test$default
x_test <- dplyr::select(test, -drops)

y_train <- as.factor(y_train)
y_test <- as.factor(y_test)

control <- trainControl(
  method = "cv",
  number = 5,
  allowParallel = F,
  savePredictions = "all",
  returnData = F,
  verboseIter = F,
  classProbs = T
)


rf_grid <- expand.grid(
  mtry = 3,
  min.node.size = 7,
  splitrule = "gini")
y_train <- revalue(y_train, c("0"="paid", "1"="default"))
y_test <- revalue(y_test, c("0"="paid", "1"="default"))

model_rf <- caret::train(
  x = x_train,
  y = y_train,
  trControl = control,
  tuneGrid = rf_grid,
  method = "ranger",
  verbose = TRUE,
  metric = "Kappa",
  importance = "impurity"
)

y_test <- revalue(y_test, c("paid"="0", "default"="1"))
y_train <- revalue(y_train, c("paid"="0", "default"="1"))


preds_rf <- predict(model_rf, newdata = x_test, type = "prob")
preds_rf <- as.numeric(preds_rf[,2])
get.max_f1(preds_rf, test$default)

scores <- data.frame(base=pred,base_over=pred2, knn = preds_knn, NB = pred_nb, RF = preds_rf)
results <- HMeasure(test$default,scores, severity.ratio = 0.25, threshold = c(0.35, -0.215, 0.5, 0.37609, 0.3911))
summary(results)
results$metrics

rm(data2)
rm(h_baseline_log)
rm(h_baseline_log_over)
rm(hyper_grid)
rm(roc_curve)
rm(roc_curve2)
rm(train_control)

roc_rf <- roc(response = as.vector(test$default), predictor = preds_rf)
ggroc(list(Logistic = roc_log, Random = roc_line, RF = roc_rf))

#Variable importance
rf_imp <- varImp(model_rf, scale = FALSE)
plot(rf_imp, top = 15)

#Bagging decission trees
#library(adabag)
#train_ne$default <- as.factor(train_ne$default)
#control <- rpart.control(cp=0.01)
#model_bag <- bagging(default~., data = train_ne, par = T, control = control, mfinal = 4)

#preds_bag <- predict(model_bag, newdata = test_ne, type = "response")
#preds_bag <- as.numeric(preds_bag$class)

#auc_adabag <- auc(as.factor(y_test_ne), preds_bag, type = "response")

#scores <- data.frame(base=pred,base_over=pred2, knn = (as.numeric(model_knn)-1), NB = pred_nb, RF = preds_rf, BG = preds_bag )
#results <- HMeasure(test$default,scores, severity.ratio = 0.25, threshold = 0.5)
#summary(results)
#results$metrics


#Extreme Gradient Boosting
library(xgboost)
x_train = xgb.DMatrix(as.matrix(train %>% select(-default)))
y_train <- as.factor(train$default)
y_test <- as.factor(test$default)
x_test <- xgb.DMatrix(as.matrix(test %>% select(-default)))

dtrain <- xgb.DMatrix(as.matrix(train), label = train$default)

control <- trainControl(
  method = "cv",
  number = 5,
  allowParallel = T,
  savePredictions = "all",
  returnData = F,
  verboseIter = F
)



best <- expand.grid(
  nrounds = 1000,
  max_depth = 10,
  colsample_bytree = 0.5,
  eta = 0.01,
  gamma = 1,
  min_child_weight = 0.2,
  subsample = 0.1
)
train_control <- caret::trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = FALSE,
  savePredictions = "final"
)

y_train <- revalue(y_train, c("0"="paid", "1"="default"))
y_test <- revalue(y_test, c("0"="paid", "1"="default"))
xgb_model <- caret::train(
  x = x_train,
  y = y_train,
  trControl = train_control,
  tuneGrid = best,
  method = "xgbTree",
  verbose = TRUE,
  metric = "ROC"
)

y_test <- revalue(y_test, c("paid"="0", "default"="1"))

plot(varImp(xgb_model), top = 15)

preds_xg <- predict(xgb_model, newdata = x_test, type = "prob")
preds_xg <- as.numeric(preds_xg[,2])
get.max_f1(preds_xg, test$default)

roc_xgb <- roc(response = as.vector(test$default), predictor = preds_xg, auc = T)
ggroc(list(Logistic = roc_log, Random = roc_line, XGB = roc_rf))


scores <- data.frame(base=pred,base_over=pred2, knn = preds_knn, NB = pred_nb, RF = preds_rf,  XGB = preds_xg)
results <- HMeasure(y_test,scores, severity.ratio = 0.125, threshold = c(0.35, -0.215, 0.5, 0.37609, 0.3911, 0.3851668))
summary(results)
results$metrics



#Gradient Boosting Machine

#y_train <- train$default
#x_train <- dplyr::select(train, -drops)

#y_test <- test$default
#x_test <- dplyr::select(test, -drops)

#y_train <- factor(y_train)
#y_test <- factor(y_test)
#y_train <- revalue(y_train, c("0"="paid", "1"="default"))
#y_test <- revalue(y_test, c("0"="paid", "1"="default"))
#results$metrics
#control <- trainControl(
 # method = "cv",
#  number = 3,
 # allowParallel = F,
 # returnData = F,
#  verboseIter = F,
#  savePredictions = "final",
#  classProbs = TRUE                                                          # set to TRUE for AUC to be computed
#)

#parameters <- expand.grid(
#  ntrees = c(500,1000),
# max_depth = c(5,10),
#  col_sample_rate = c(0.5, 0.7),
#  min_rows = 2,
#  learn_rate = c(0.25, 0.75)
#)

#parameters_best <- expand.grid(
#  ntrees = 250,
#  max_depth = 3,
#  col_sample_rate = 0.2,
#  min_rows = 1,
#  learn_rate = 0.1
#)

#library(h2o)
#h2o.init()
#gbm_model <- caret::train(
#  x = x_train,
#  y = y_train,
#  trControl = control,
#  tuneGrid = parameters_best,
#  method = "gbm_h2o",
#  verbose = TRUE,
#  metric = "auc"
#)

#best is 1000   10    0.7   1 0.25


y_test <- revalue(y_test, c("paid"="0", "default"="1"))


#preds_gbm <- predict(gbm_model, newdata = x_test, type = "prob")
#preds_gbm <- as.numeric(preds_gbm[,2])
#get.max_f1(preds_gbm, test$default)


c1 <- get.max_f1(pred2, test$default)
c2 <- get.max_f1(preds_knn, test$default)
c3<- get.max_f1(preds_nb, test$default)
c4 <- get.max_f1(preds_rf, test$default)
c5 <- get.max_f1(preds_xg, test$default)



scores <- data.frame(logistic=pred2, knn = preds_knn, NBayes = pred_nb,RF = preds_rf, XGB = preds_xg)
results <- HMeasure(y_test,scores, severity.ratio = 0.125,  threshold = c(c1[2],c2[2], c3[2],c4[2],c5[2]))
summary(results)
results$metrics


#Precision - recall curves
PRcurve(preds_rf, y_test)
PRcurve(preds_xg, y_test)
PRcurve(preds_gbm, y_test)

#LogLoss
library(MLmetrics)

LogLoss(preds_rf, as.numeric(y_test) - 1)
LogLoss(preds_xg, as.numeric(y_test) - 1)
LogLoss(preds_gbm, as.numeric(y_test) - 1)
LogLoss(pred2, as.numeric(y_test) - 1)
LogLoss((as.numeric(model_knn)-1), as.numeric(y_test) - 1)



roc_xgb <- roc(response = as.vector(test$default), predictor = preds_xg, auc = T)
ggroc(list(Logistic = roc_log, Random = roc_line))
