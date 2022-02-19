#Stacking prediction model for the risk of prolonged ICU LoS 
# Author: Dr. Igor Tona Peres, Professor of Industrial Engineering, PUC-Rio
# igor.peres@puc-rio.br
# Last updated: 02/19/2021
library(caret)
library(tidyverse)
library(caretEnsemble)

#Load your dataset
load("data.RData")

#Predictors of stacking model
predictors = read.csv('predictors.csv')
predictors = predictors[,2]

data = data%>%
  select(predictors,UnitLengthStay_trunc)

#Splitting dataset into training and testing
set.seed(998)
inTraining <- createDataPartition(data$UnitLengthStay_trunc,
                                  p = .8, list = FALSE)
training <- data[ inTraining,]
testing  <- data[-inTraining,]

training = training%>%
  mutate(Desfecho_internacao = if_else(UnitnLengthStay_trunc<14,"Baixo","Alto"))
training$UnitLengthStay_trunc = NULL

testing = testing%>%
  mutate(Desfecho_internacao = if_else(UnitLengthStay_trunc<14,"Baixo","Alto"))
testing$UnitLengthStay_trunc = NULL

#summary function
fiveStats = function(...)c(multiClassSummary(...),brierScore(...)) 

#basic parameter tuning

fitControl <- trainControl(## 5-fold CV
  method = "cv", number = 5, verboseIter = TRUE,returnData = FALSE,trim = TRUE,
  savePredictions = "final", classProbs = TRUE, summaryFunction = fiveStats)

model_list_complete <- caretList(
  x=training[,-ncol(training)],
  y= training$Desfecho_internacao,
  trControl=fitControl,
  metric="AUC",
  tuneList=list(
    lr = caretModelSpec(method="glm"),
    rf=caretModelSpec(method="ranger", tuneGrid=data.frame(.mtry = c(5:10),
                                                           .min.node.size = c(5:10),
                                                           .splitrule =  c("gini","extratrees","hellinger")))))

save(model_list_complete,file="model_list_complete.RData")

gbmGrid = expand.grid(interaction.depth = c(5,10,15,20),
                      n.trees = c(300,400),
                      shrinkage = 0.01,
                      n.minobsinnode = 20)

Stacking_lr_rf_OptGBM_RiskModel  <- caretStack(
  model_list_complete, 
  trControl=fitControl,
  metric="AUC",
  method="gbm",
  tuneGrid = gbmGrid)

save(Stacking_lr_rf_OptGBM_RiskModel,file="Stacking_lr_rf_OptGBM_RiskModel.RData")

model = Stacking_lr_rf_OptGBM_RiskModel

#Model Perfomance
library(MLmetrics)
library(ModelMetrics)

Observed = data.frame(Observed=testing$Desfecho_internacao)
Observed$Observed = if_else(Observed$Observed=="Alto",1,0)
Predicted = data.frame(Predicted = predict(model,newdata=testing,type="prob"))

brier = mean((Predicted$Predicted-Observed$Observed)^2)
brier
auc(predicted = Predicted$Predicted, actual = Observed$Observed)
ppv(predicted = Predicted$Predicted, actual = Observed$Observed)
npv(predicted = Predicted$Predicted, actual = Observed$Observed)
sensitivity(predicted = Predicted$Predicted, actual = Observed$Observed)
specificity(predicted = Predicted$Predicted, actual = Observed$Observed)

#Calibration Belt

library(givitiR)
comparacao = cbind(Observed,Predicted)
cb <- givitiCalibrationBelt( comparacao$Observed, comparacao$Predicted,devel = "external")
plot(cb, main = "Calibration Belt for Updated Stacking model",
     xlab = "Predicted probability of prolonged stay",
     ylab = "Observed prolonged stay"
     )