#Stacking prediction model for the Numeric ICU LoS 
#External validating the original stacking model
# Author: Dr. Igor Tona Peres, Professor of Industrial Engineering, PUC-Rio
# igor.peres@puc-rio.br
# Last updated: 04/25/2024
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

#Model Performance
load("Stacking_NumericLOS_model.RData")

Observed = data.frame(Observed=data$UnitLengthStay_trunc)
Predicted = data.frame(Predicted = predict(model,newdata=data))

Erro = RMSE(pred =  Predicted$Predicted, obs = Observed$Observed)
Erro
MAE = MAE(pred = Predicted$Predicted, obs = Observed$Observed)
MAE
R2 = R2(pred = Predicted$Predicted, obs = Observed$Observed)
R2

comparison = as.data.frame(cbind(Observed,Predicted))

ggplot(aes(x=Predicted,y=Observed),data=comparison)+ggtitle("Calibration for Updated Stacking model")+
  geom_smooth()+
  geom_segment(aes(x=0,y=0,xend=21,yend=21))+
  xlab("Predicted ICU length of stay")+
  ylab("Observed ICU length of stay")

