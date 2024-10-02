#Stacking prediction model for Numeric ICU LoS 
#Updated 2024 Model with synthetic dataset for testing
# Author: Dr. Igor Tona Peres, Professor of Industrial Engineering, PUC-Rio
# igor.peres@puc-rio.br
# Last updated: 09/18/2024

library(caret)
library(tidyverse)
library(caretEnsemble)
library(MLmetrics)

#Load your dataset
load("data.RData")

#Predictors for stacking model
predictors = read.csv('predictors.csv')
predictors = predictors[,2]


data = data%>%
  select(predictors,UnitLengthStay_trunc)


#Basic parameter tuning
fitControl <- trainControl(## 5-fold CV
  method = "cv", number = 5, verboseIter = TRUE,returnData = FALSE,trim = TRUE,
  savePredictions = "final")

model_list_complete <- caretList(
  x=training[,-ncol(training)],
  y= training$UnitLengthStay_trunc,
  trControl=fitControl,
  metric="RMSE",
  tuneList=list(
    lm = caretModelSpec(method="lm"),
    rf=caretModelSpec(method="ranger", tuneGrid=data.frame(.mtry=c(5:10),
                                                           .splitrule =  "variance",
                                                           .min.node.size=5))
  )
)

save(model_list_complete,file="SLOS_model_list.RData")

rfGrid = expand.grid(mtry = 2,
                     min.node.size = c(5,10,15,20),
                     splitrule =  c("variance","extratrees","maxstat")
)

SLOS_model  <- caretStack(
  model_list_complete, 
  trControl=fitControl,
  metric="RMSE",
  method="ranger",
  tuneGrid = rfGrid)

save(SLOS_model,file="SLOS_model.RData")
