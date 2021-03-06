if(Sys.info()['user']=="aden") { setwd("~/Dropbox/School/MS Exam/") } else setwd("~/MS\ Exam/")
library(TeachingDemos)
library(gdata)
library(data.table)
library(caret)
library(doMC)
library(lubridate)
library(Metrics)
library(geosphere)
library(ggplot2)
library(gam)
library(pROC)
library(parallel)

load("data/bigTrain2011_2013_1MosquitoPerRow.rdata")
bigTrain2011_2013$WnvPresent = bigTrain2011_2013$Date = bigTrain2011_2013$NumMosquitos = bigTrain2011_2013$NearbySprays = bigTrain2011_2013$Trap = NULL

twoClassSummaryCustom = function(data, lev = NULL, model = NULL) {
  rocObject <- try(pROC::roc(data$obs, data[, lev[1]]), silent = TRUE)
  rocAUC <- ifelse(class(rocObject)[1] == "try-error", NA, rocObject$auc) 
  sens = sensitivity(data[, "pred"], data[, "obs"], lev[1])
  spec = specificity(data[, "pred"], data[, "obs"], lev[2])
  ppv = posPredValue(data[,"pred"], reference = data[,"obs"])
  BalancedAccuracy = mean(c(sens, spec))
  F1 = 2 * (ppv * sens) / (ppv+sens)
  out <- c(F1, BalancedAccuracy, rocAUC, sens, spec)
  names(out) <- c("F1", "BalancedAccuracy", "AUC", "Sens", "Spec")
  out
}

CVctrl = trainControl(method='cv', number=5, classProbs=TRUE, summaryFunction=twoClassSummaryCustom)
registerDoMC(detectCores())

SprayModel = formula(.outcome ~ .)
SprayModelLM = formula(.outcome ~ . + as.factor(Species) + as.factor(Year) + as.factor(Month) -Month -Cool -Species -Year)

fitGLM = train(SprayModelLM, data = bigTrain2011_2013, trControl=CVctrl, metric="AUC", method = "glm", family='binomial')

GLMNet_Grid = expand.grid(alpha=c(seq(0, .1, .001), seq(.9, 1,.001)), lambda=seq(0,.1,.001))
fitGLMNet_F1 = train(SprayModelLM, data = bigTrain2011_2013, method = "glmnet", trControl = CVctrl, metric = 'F1', preProc=c("center","scale"), tuneGrid = GLMNet_Grid)
fitGLMNet_BA = train(SprayModelLM, data = bigTrain2011_2013, method = "glmnet", trControl = CVctrl, metric = 'BalancedAccuracy', preProc=c("center","scale"), tuneGrid = GLMNet_Grid)
fitGLMNet_AUC = train(SprayModelLM, data = bigTrain2011_2013, method = "glmnet", trControl = CVctrl, metric = 'AUC', preProc=c("center","scale"), tuneGrid = GLMNet_Grid)

RF_Grid = expand.grid(mtry=seq(1, 30, 2))
fitRF_F1 = train(SprayModel, data = bigTrain2011_2013, method = "rf",  trControl = CVctrl, metric = 'F1', importance=TRUE, preProc=c("center","scale"), tuneGrid = RF_Grid)
fitRF_BA = train(SprayModel, data = bigTrain2011_2013, method = "rf",  trControl = CVctrl, metric = 'BalancedAccuracy', importance=TRUE, preProc=c("center","scale"), tuneGrid = RF_Grid)
fitRF_AUC = train(SprayModel, data = bigTrain2011_2013, method = "rf",  trControl = CVctrl, metric = 'AUC', importance=TRUE, preProc=c("center","scale"), tuneGrid = RF_Grid)

GBM_Grid = expand.grid(n.trees = seq(1, 300, 50), interaction.depth = 2:3, shrinkage=seq(0, 1, .1), n.minobsinnode = seq(10, 50, 10))
fitGBM_F1 = train(SprayModel, data = bigTrain2011_2013, method = "gbm", trControl = CVctrl, metric = 'F1', preProc=c("center","scale"), verbose=FALSE)
fitGBM_BA = train(SprayModel, data = bigTrain2011_2013, method = "gbm", trControl = CVctrl, metric = 'BalancedAccuracy', preProc=c("center","scale"), tuneGrid = GBM_Grid, verbose=FALSE)
fitGBM_AUC = train(SprayModel, data = bigTrain2011_2013, method = "gbm", trControl = CVctrl, metric = 'AUC', preProc=c("center","scale"), tuneGrid = GBM_Grid, verbose=FALSE)

Ada_Grid = expand.grid(iter=150, maxdepth=1:3, nu=seq(0, 1, .1))
fitAda_F1 = train(SprayModel, data = bigTrain2011_2013, method = "ada", trControl = CVctrl, metric= 'F1', preProc=c("center","scale"), tuneGrid = Ada_Grid)
fitAda_BA = train(SprayModel, data = bigTrain2011_2013, method = "ada", trControl = CVctrl, metric= 'BalancedAccuracy', preProc=c("center","scale"), tuneGrid = Ada_Grid)
fitAda_AUC = train(SprayModel, data = bigTrain2011_2013, method = "ada", trControl = CVctrl, metric= 'AUC', preProc=c("center","scale"), tuneGrid = Ada_Grid)
save(list = ls()[grep("fit", ls())], file="SprayFits_NoSpray.Rdata")