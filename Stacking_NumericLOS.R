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

#basic parameter tuning
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

save(model_list_complete,file="model_list_complete.RData")

rfGrid = expand.grid(mtry = 2,
                   min.node.size = c(5,10,15,20),
                   splitrule =  c("variance","extratrees","maxstat")
)

Stacking_lm_rf_OptRF_NumericLOS  <- caretStack(
  model_list_complete, 
  trControl=fitControl,
  metric="RMSE",
  method="ranger",
  tuneGrid = rfGrid)

save(Stacking_lm_rf_OptRF_NumericLOS,file="Stacking_lm_rf_OptRF_NumericLOS.RData")

#Model Performance

model = Stacking_lm_rf_OptRF_NumericLOS

library(MLmetrics)

Observed = data.frame(Observed=testing$UnitLengthStay_trunc)
Predicted = data.frame(Predicted = predict(model,newdata=testing))

Erro = RMSE(y_pred = Predicted$Predicted, y_true = Observed$Observed)
Erro
MAE = MAE(y_pred = Predicted$Predicted, y_true = Observed$Observed)
MAE
R2 = R2_Score(y_pred = Predicted$Predicted, y_true = Observed$Observed)
R2

comparison = as.data.frame(cbind(Observed,Predicted))

ggplot(aes(x=Predicted,y=Observed),data=comparison)+ggtitle("Calibration for Updated Stacking model")+
  geom_smooth()+
  geom_segment(aes(x=0,y=0,xend=21,yend=21))+
  xlab("Predicted ICU length of stay")+
  ylab("Observed ICU length of stay")

