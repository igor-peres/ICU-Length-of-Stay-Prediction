#Data Preprocessing for ICU LoS Prediction
# Author: Dr. Igor Tona Peres, PhD in Industrial Engineering, PUC-Rio
# igor.peres.puc@gmail.com
# Last updated: 27/07/2021

#load("data.RData")

#Data Splitting
if(!require(caret)) {install.packages("caret"); library(caret) }
if(!require(DescTools)) {install.packages("DescTools"); library(DescTools) }
if(!require(mice)) {install.packages("mice"); library(mice) }

set.seed(998)
inTraining <- createDataPartition(data$UnitLengthStay,
                                  p = .8, list = FALSE)
training <- data[ inTraining,]
testing  <- data[-inTraining,]


#Identifying and Removing Zero and Near Zero variance features
nzv = nearZeroVar(training, saveMetrics = T, freqCut = 100/2)
nzv["Variaveis"] = row.names(nzv)
descritiva_nzv = nzv%>%
  filter(nzv==T)%>%
  select(Variaveis,freqRatio,percentUnique)
retirados_nzv = descritiva_nzv$Variaveis

training = training %>%
  select(.,-retirados_nzv_ajus)
testing = testing %>%
  select(.,-retirados_nzv_ajus)

# Identifying and Removing Correlated Predictors (for numeric features)
training_pre_numeric = training %>%
  select_if(., is.numeric)
training_pre_numeric$UnitLengthStay = NULL
descrCor <-  cor(training_pre_numeric, 
                 use="pairwise.complete.obs")

corrplot.mixed(descrCor, tl.pos = "lt")

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
retirados_cor = colnames(training_pre_numeric[,highlyCorDescr])
training_pre_numeric = 
  training_pre_numeric[,-highlyCorDescr]

testing_pre_numeric = testing %>%
  select_if(., is.numeric)
testing_pre_numeric$UnitLengthStay = NULL
testing_pre_numeric = 
  testing_pre_numeric[,-highlyCorDescr]


# Identifying and Removing Correlated Predictors (for categorical features)
training_pre_factor = training %>%
  select_if(., is.factor)
cramer_tab = PairApply(training_pre_factor,
                       CramerV, symmetric = TRUE)
cramer_tab[which(is.na(cramer_tab[,])==T)] = 0

corrplot.mixed(cramer_tab, tl.pos = "lt")

highlyCorCateg <- findCorrelation(cramer_tab, cutoff = 0.5)
retirados_categ = colnames(training_pre_factor[,highlyCorCateg])
training_pre_factor = training_pre_factor %>%
  select(.,-retirados_categ)

testing_pre_factor = testing %>%
  select_if(., is.factor)
testing_pre_factor = testing_pre_factor %>%
  select(.,-retirados_categ)

training = cbind(training_pre_numeric,training_pre_factor, training$UnitLengthStay)
training$UnitLengthStay = training$`training$UnitLengthStay`
training$`training$UnitLengthStay` = NULL

testing = cbind(testing_pre_numeric,testing_pre_factor, testing$UnitLengthStay)
testing$UnitLengthStay = testing$`testing$UnitLengthStay`
testing$`testing$UnitLengthStay` = NULL


#MICE Imputation
training_imp = training
testing_imp = testing

  #training
set.seed(100)
predictormatrix = quickpred(training_imp,
                          include = c("UnitLengthStay"),
                          exclude = NULL,
                          mincor = 0.3)
imp_gen = mice(data = training_imp,
               predictorMatrix = predictormatrix,
               m=1,
               maxit = 5,
               diagnostics=TRUE)

imp_data = mice::complete(imp_gen,1)
training_imp = imp_data
summary(training_imp)


  #testing
set.seed(100)
predictormatrix = quickpred(testing_imp,
                            include = c("UnitLengthStay"),
                            exclude = NULL,
                            mincor = 0.3)
imp_gen_test = mice(data = testing_imp,
               predictorMatrix = predictormatrix,
               m=1,
               maxit = 5,
               diagnostics=TRUE)
imp_data_test = mice::complete(imp_gen_test,1)
testing_imp = imp_data_test
summary(testing_imp)


#Final preprocessed dataset
save(training_imp,file = "training.RData")
save(testing_imp,file = "testing.RData")

