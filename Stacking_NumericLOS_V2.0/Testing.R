#Stacking prediction model for the Numeric ICU LoS 
#Testing the model with your dataset, or the synthetic dataset
# Author: Dr. Igor Tona Peres, Professor of Industrial Engineering, PUC-Rio
# igor.peres@puc-rio.br
# Last updated: 09/18/2024

library(caret)
library(tidyverse)
library(caretEnsemble)
library(MLmetrics)

#Load your dataset
testing = read.csv("Synthetic_TestingData.csv")

load("SLOS_model.RData")

#model = Stacking_lm_rf_OptRF_NumericLOS
model = SLOS_model

Observed = data.frame(Observed=testing$UnitLengthStay_trunc)
Predicted = data.frame(Predicted = predict(model,newdata=testing))

Erro = RMSE(y_pred = Predicted$Predicted, y_true = Observed$Observed)
Erro
#4.025009
#4.023174

MAE = MAE(y_pred = Predicted$Predicted, y_true = Observed$Observed)
MAE
#2.645971
#2.686037

R2 = R2_Score(y_pred = Predicted$Predicted, y_true = Observed$Observed)
R2
#0.1669913
#0.174175

comparison = as.data.frame(cbind(Observed,Predicted))

ggplot(aes(x=Predicted,y=Observed),data=comparison)+ggtitle("Calibration for Updated Stacking model")+
  geom_smooth()+
  geom_segment(aes(x=0,y=0,xend=21,yend=21))+
  xlab("Predicted ICU length of stay")+
  ylab("Observed ICU length of stay")

#SLOS Analysis
##############################################################
df_model_pred = 
  testing%>%
  select(observed = UnitLengthStay_trunc)%>%
  mutate(predicted = predict(model,newdata=testing))%>%
  mutate(predicted = case_when(
    predicted < 0 ~ 0,
    predicted > 21 ~ 21,
    TRUE ~ predicted
  ))

df_unit_slos = df_model_pred%>%
  bind_cols(testing%>%select(UnitCode))%>%
  group_by(UnitCode)%>%
  summarise(admissoes = n(),
            soma_los_obs = sum(observed),
            soma_los_esp = sum(predicted))%>%
  ungroup()%>%
  mutate(SLOS = soma_los_obs/soma_los_esp)%>%
  na.omit()
df_unit_slos

#General SLOS
df_model_pred%>%summarise(SLOS_geral = sum(observed)/sum(predicted))
#1.03984
#1.006443

#R2 gropued by units
R2 = R2_Score(df_unit_slos$soma_los_esp,df_unit_slos$soma_los_obs)
R2
#0.9078752
#0.9108478

#Plot SLOS Units - Predicted x Observed
plot_SLOS_obs_prev = df_unit_slos%>%
  ggplot()+
  geom_point(aes(x=soma_los_esp,y=soma_los_obs),color="gray40")+
  geom_smooth(aes(x=soma_los_esp,y=soma_los_obs))+
  geom_abline(aes(intercept=0,slope=1),linetype="dashed")+
  labs(x= "Sum of predicted ICU LoS",y="Sum of observed ICU LoS",title="Grouped LoS per Unit (days)")+
  theme_bw()
plot_SLOS_obs_prev

#Funnel plots
library(ems)

# Getting the cross-sectional arguments to use in funnel



# Analysis of proportions
f1 <- funnel(unit = df_unit_slos$UnitCode, 
             y = df_unit_slos$SLOS,
             y.type = "SRU",
             o = df_unit_slos$soma_los_obs, 
             e = df_unit_slos$soma_los_esp, 
             theta = sum(df_unit_slos$soma_los_obs) / sum(df_unit_slos$soma_los_esp),
             n = df_unit_slos$admissoes, 
             method = "normal", option = "rate", plot = F, direct = T)
f1
plot(f1, main = "Funnel plot for SLOSR",ylim = c(0.2,1.8), xlim = c(0,2700),
     ylab = "Standardized Length of Stay Ratio (SLOSR)", xlab="Number of ICU admissions")

slos_plot = as.data.frame(f1$tab)

summary(slos_plot$SRU)

#Min.    1st Qu.  Median  Mean   3rd Qu.  Max.
#0.5431  0.8794  1.0321  1.0534  1.1686  5.2973

#Min.    1st Qu.  Median  Mean   3rd Qu.  Max.
#0.5334  0.8667  1.0220  1.0209  1.1638  1.8070