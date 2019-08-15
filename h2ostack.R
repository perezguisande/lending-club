#H2o stack
library(h2o)  
h2o.init()  
h2o.removeAll() 

library(data.table)
library(plyr)
library(dplyr)
library(caret)
library(rsample)
library(tidyverse)


data2 <- h2o.importFile("prepared_data_nocurrent-50k-adrate-2.csv")

data_split <- h2o.splitFrame(data=data2, ratios=0.7, seed = 45)
data_split_muchless <- h2o.splitFrame(data=data2, ratios=c(1/25, 1/50, 23.5/50), seed = 45)
train <- data_split[[1]]
test <- data_split[[2]]


y <- "default"
x <- setdiff(names(train), y)

train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

#parameters for much less gbm:  col_sample_rate 0.2 learn_rate 0.01 max_depth 3 min_rows 3ntrees 100sample_rate0.5
#gbm for large dataset  ntrees = 250,max_depth = 3,min_rows = 1,learn_rate = 0.1,nfolds = 3col_sample_rate = 0.2,sample_rate = 0.5

my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train,
                  distribution = "AUTO",
                  ntrees = 250,
                  max_depth = 3,
                  min_rows = 1,
                  learn_rate = 0.1,
                  nfolds = 3,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1, balance_classes = T,
                  col_sample_rate = 0.2,
                  sample_rate = 0.5
                  )

my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train,
                          ntrees = 200,
                          nfolds = 3,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1, balance_classes = T,
                          max_depth = 2, mtries = 3, sample_rate = 0.5)

my_dl <- dl1<-h2o.deeplearning(x= x, y= y, activation="Maxout",training_frame = train, hidden=c(157,75,1),epochs=100,input_dropout_ratio=0.25,
                               nfolds = 3, fold_assignment = "Modulo",keep_cross_validation_predictions = TRUE, balance_classes = T, l1 = 1e-04, l2 = 1e-04,
                               rho = 0.99) 

#parameters for much less gbm:  col_sample_rate 0.2 learn_rate 0.01 max_depth 3 min_rows 3ntrees 100sample_rate0.5


my_glm <- h2o.glm(x = x,
                      y = y,
                      training_frame = train,
                      nfolds = 3,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE,
                      balance_classes = T,
                      family = "binomial")

my_nb <- h2o.naiveBayes(x = x,
                      y = y,
                      training_frame = train,
                      nfolds = 3,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE,
                      seed = 1)
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "asdf",
                                base_models = list(my_gbm, my_rf, my_glm, my_nb),
                                metalearner_algorithm = "glm")

ensemble_nn <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "asdf",
                                base_models = list(my_gbm, my_rf, my_dl, my_glm),
                                metalearner_algorithm = "deeplearning")


perf <- h2o.performance(ensemble, newdata = test)
perf_dl <- h2o.performance(my_dl, newdata = test)
perf_rf <- h2o.performance(my_rf, newdata = test)
perf_gbm <- h2o.performance(my_gbm, newdata = test)
perf_nn <- h2o.performance(ensemble_nn, newdata = test)
h2o.auc(perf)


#dl_path <- h2o.saveModel(object = my_dl, path = getwd(), force = T)
#rf_path <- h2o.saveModel(object = my_rf, path = getwd(), force = T)
#gbm_path <- h2o.saveModel(object = my_gbm, path = getwd(), force = T)
#glm_path <- h2o.saveModel(object = my_glm, path = getwd(), force = T)
#en_path <- h2o.saveModel(object = ensemble, path = getwd(), force = T)
#try_path <- h2o.saveModel(object = my_glm, path = getwd(), force = T)
#load model
#my_dl <- h2o.loadModel("C:\\Users\\Usuario\\Documents\\Universidad\\MSc\\SummerProject\\lending-club-loan-data\\DeepLearning_model_R_1565649055319_4091")
#my_rf <- h2o.loadModel("C:\\Users\\Usuario\\Documents\\Universidad\\MSc\\SummerProject\\lending-club-loan-data\\DRF_model_R_1565649055319_3465")
#my_gbm <- h2o.loadModel("C:\\Users\\Usuario\\Documents\\Universidad\\MSc\\SummerProject\\lending-club-loan-data\\GBM_model_R_1565649055319_2884")
#my_glm <- h2o.loadModel("C:\\Users\\Usuario\\Documents\\Universidad\\MSc\\SummerProject\\lending-club-loan-data\\GLM_model_R_1565649055319_4231")
#ensemble <- h2o.loadModel("C:\\Users\\Usuario\\Documents\\Universidad\\MSc\\SummerProject\\lending-club-loan-data\\asdf")


pred_dl <- h2o.predict(my_dl, newdata = test)
pred_dl <- pred_dl[,3]

pred_gbm <- h2o.predict(my_gbm, newdata = test)
pred_gbm <- pred_gbm[,3]

pred_en <- h2o.predict(ensemble, newdata = test)
pred_en <- pred_en[,3]

pred_en <- as.vector(pred_en)
pred_gbm <- as.vector(pred_gbm)
pred_dl <- as.vector(pred_dl)

pred_en_nn <- h2o.predict(ensemble_nn, newdata = test)
pred_en_nn <- pred_en_nn[,3]

pred_en_nn <- as.vector(pred_en_nn)

pred_log <- h2o.predict(my_glm, newdata = test)
pred_log <- pred_log[,3]

pred_log <- as.vector(pred_log)



library(hmeasure)
scores <- data.frame(GBM = pred_gbm, NN = pred_dl, Ensemble = pred_en, Ensemble_nn = pred_en_nn)
results <- HMeasure(as.vector(test$default),scores, severity.ratio = 0.125,  threshold = c(0.203665599105636,0.222937860919032,0.204142949578486, 0.234378390672705))
summary(results)
results$metrics

h2o.confusionMatrix(perf_nn)


write.csv(pred_en, "pred_en.csv")
write.csv(pred_dl, "pred_dl.csv")
write.csv(pred_gbm, "pred_gbm.csv")

write.csv(pred_en_nn, "pred_en_nn")

h2o.varimp_plot(my_gbm, num_of_features = 15)
h2o.varimp_plot(my_dl, num_of_features = 15)

library(pROC)
library(ggplot2)
roc <- roc(response = as.vector(test$default), predictor = pred_dl, auc = T)
roc2 <- roc(response = as.vector(test$default), predictor = pred_gbm, auc = T)
roc3 <- roc(response = as.vector(test$default), predictor = pred_en, auc = T)
roc4 <- roc(response = as.vector(test$default), predictor = pred_log, auc = T)
ggroc(list(Neuralnet = roc, GBM = roc2, Ensemble = roc3, Logistic = roc_log))

#roc_log <- roc(response = as.vector(test$default), predictor = pred_log, auc = T)
line <- runif(26175, min = 0.5, max = 0.5)
roc_line <- roc(response = as.vector(test$default), predictor = line, auc = T)

ggroc(list(Logistic = roc_log, GBM = roc2, Random = roc_line))
ggroc(list(Logistic = roc_log, NeuralNet = roc, Random = roc_line))
ggroc(list(Logistic = roc_log, Ensemble = roc3, Random = roc_line), legacy.axes = T)
h2o.auc(perf_gbm)

#Importances
h2o.varimp_plot(my_gbm, num_of_features = 15)
h2o.varimp_plot(my_dl, num_of_features = 15)
