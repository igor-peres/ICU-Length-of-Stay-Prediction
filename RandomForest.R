#Random Forest Prediction model for ICU LoS 
# Author: Dr. Igor Tona Peres, PhD in Industrial Engineering, PUC-Rio
# igor.peres.puc@gmail.com
# Last updated: 27/07/2021

library(caret)
library(tidyverse)

# load('training.RData')
# load('testing.RData')

#Loading the predictors selected in Recursive Feature Elimination
load('rfe_result.RData')

predictors = rfe_result$optVariables
training = training%>%
  select(predictors,UnitLengthStay)

testing = testing%>%
  select(predictors,UnitLengthStay)

#basic parameter tuning
fitControl <- trainControl(## 5-fold CV
  method = "cv", number = 5, verboseIter = TRUE,returnData = FALSE,trim = TRUE)

#RF
library(ranger)
set.seed(476)
Grid = expand.grid(mtry = c(5:10),
                      min.node.size = c(5:10),
                      splitrule =  c("variance","extratrees","maxstat","beta")
                      )

rf <- train(x=training[,-ncol(training)],
                  y= training$UnitLengthStay,
                  tuneGrid = Grid,
                  method="ranger",
                  metric="RMSE",
                  trControl = fitControl)
save(rf,file="rf.RData")

# Predicted$Predicted[Predicted$Predicted<0] = 0
# Predicted$Predicted[Predicted$Predicted>21] = 21

Erro = RMSE(y_pred = Predicted$Predicted, y_true = Observed$Observed)
MAE = MAE(y_pred = Predicted$Predicted, y_true = Observed$Observed)
R2 = R2_Score(y_pred = Predicted$Predicted, y_true = Observed$Observed)
