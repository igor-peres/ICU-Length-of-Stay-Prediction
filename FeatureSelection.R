#Feature Selection for ICU LoS Prediction 
# Author: Dr. Igor Tona Peres, PhD in Industrial Engineering, PUC-Rio
# igor.peres.puc@gmail.com
# Last updated: 27/07/2021

library(caret)

# load('training.RData')
# load('testing.RData')

#basic parameter tuning
fitControl <- trainControl(## 5-fold CV
  method = "cv", number = 5, verboseIter = TRUE,returnData = FALSE,trim = TRUE)

#Normalization
preproc = preProcess(training[,-90],method = c("range"))
training_norm = predict(preproc,training)
testing_norm = predict(preproc,testing)


#Recursive feature elimination using Treebag functions
set.seed(420)
subsets = c(10:30,35,40,45,50,60,80)
rfe_control = rfeControl(functions=treebagFuncs,
                         method = "cv",
                         number = 5,
                         returnResamp = "all",
                         verbose = T)
rfe_result = rfe(training_norm[,-90],
                 training_norm[,90],
                 sizes=subsets,
                 metric="RMSE",
                 rfeControl = rfe_control)

print(rfe_result)
predictors(rfe_result)
plot(rfe_result,type=c("g","o"))
head(rfe_result$optVariables)
head(varImp(rfe_result))
save(rfe_result,file="rfe_result_treebag.RData")


#Recursive feature elimination using Random Forests functions
set.seed(100) 
subsets = c(10:30,40,60)
rfe_control = rfeControl(functions=rfFuncs,
                         method = "cv",
                         number = 5,
                         verbose = T,
                         saveDetails = F,
                         returnResamp = "none")
rfe_result = rfe(training[,-90],
                 training[,90],
                 sizes=subsets,
                 metric="RMSE",
                 rfeControl = rfe_control)
save(rfe_result,file="rfe_result_RF.RData")


rfe_result$results
rfe_result$fit
head(rfe_result$optVariables)
head(varImp(rfe_result))
head(rfe_result$variables)



