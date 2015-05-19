library(pROC)
library(TeachingDemos)
library(caret)

load("data/bigTrain_1MosquitoPerRow.rdata")
load("data/bigTrain2011_2013_1MosquitoPerRow.rdata")

load("out/AllFits.Rdata")
pdf(file="out/ROC_AllYears_Weather.pdf")
plot(roc(bigTrain$.outcome ~ predict(fitGLMNet_AUC, bigTrain, type='prob')$Present), col='blue', xlim=c(1,0), main="ROC Curves for Weather Only (2007-2013). glmnet (blue), GLM (red),\n RandomForest (orange), GradientBoosting (green), AdaBoost (brown)")
plot(roc(bigTrain$.outcome ~ predict(fitGLM, bigTrain, type='prob')$Present), col='red', add=TRUE) #GLM ROC
plot(roc(bigTrain$.outcome ~ predict(fitRF_AUC, bigTrain, type='prob')$Present), col='orange', add=TRUE) #RandomForest ROC
plot(roc(bigTrain$.outcome ~ predict(fitGBM_AUC, bigTrain, type='prob')$Present), col='green', add=TRUE) #GBM ROC
plot(roc(bigTrain$.outcome ~ predict(fitAda_AUC, bigTrain, type = "prob")$Present), col='brown', add=TRUE) #Ada ROC
dev.off()

fitAllResults = data.frame(BestF1 = c(fitGLM[[4]]$F1[which.max(fitGLM[[4]]$F1)],
                                      fitGLMNet_F1[[4]]$F1[which.max(fitGLMNet_F1[[4]]$F1)],
                                      fitRF_F1[[4]]$F1[which.max(fitRF_F1[[4]]$F1)],
                                      fitGBM_F1[[4]]$F1[which.max(fitGBM_F1[[4]]$F1)],
                                      fitAda_F1[[4]]$F1[which.max(fitAda_F1[[4]]$F1)]),
                           BestF1SD = c(fitGLM[[4]]$F1SD[which.max(fitGLM[[4]]$F1SD)],
                                        fitGLMNet_F1[[4]]$F1SD[which.max(fitGLMNet_F1[[4]]$F1SD)],
                                        fitRF_F1[[4]]$F1SD[which.max(fitRF_F1[[4]]$F1SD)],
                                        fitGBM_F1[[4]]$F1SD[which.max(fitGBM_F1[[4]]$F1SD)],
                                        fitAda_F1[[4]]$F1SD[which.max(fitAda_F1[[4]]$F1SD)]),
                           BestAUC = c(fitGLM[[4]]$AUC[which.max(fitGLM[[4]]$AUC)],
                                       fitGLMNet_AUC[[4]]$AUC[which.max(fitGLMNet_AUC[[4]]$AUC)],
                                       fitRF_AUC[[4]]$AUC[which.max(fitRF_AUC[[4]]$AUC)],
                                       fitGBM_AUC[[4]]$AUC[which.max(fitGBM_AUC[[4]]$AUC)],
                                       fitAda_AUC[[4]]$AUC[which.max(fitAda_AUC[[4]]$AUC)]),
                           BestAUCSD = c(fitGLM[[4]]$AUCSD[which.max(fitGLM[[4]]$AUCSD)],
                                         fitGLMNet_AUC[[4]]$AUCSD[which.max(fitGLMNet_AUC[[4]]$AUCSD)],
                                         fitRF_AUC[[4]]$AUCSD[which.max(fitRF_AUC[[4]]$AUCSD)],
                                         fitGBM_AUC[[4]]$AUCSD[which.max(fitGBM_AUC[[4]]$AUCSD)],
                                         fitAda_AUC[[4]]$AUCSD[which.max(fitAda_AUC[[4]]$AUCSD)]),
                           BestBalancedAccuracy = c(fitGLM[[4]]$BalancedAccuracy[which.max(fitGLM[[4]]$BalancedAccuracy)],
                                                    fitGLMNet_BA[[4]]$BalancedAccuracy[which.max(fitGLMNet_BA[[4]]$BalancedAccuracy)],
                                                    fitRF_BA[[4]]$BalancedAccuracy[which.max(fitRF_BA[[4]]$BalancedAccuracy)],
                                                    fitGBM_BA[[4]]$BalancedAccuracy[which.max(fitGBM_BA[[4]]$BalancedAccuracy)],
                                                    fitAda_BA[[4]]$BalancedAccuracy[which.max(fitAda_BA[[4]]$BalancedAccuracy)]),
                           BestBalancedAccuracySD = c(fitGLM[[4]]$BalancedAccuracySD[which.max(fitGLM[[4]]$BalancedAccuracySD)],
                                                      fitGLMNet_BA[[4]]$BalancedAccuracySD[which.max(fitGLMNet_BA[[4]]$BalancedAccuracySD)],
                                                      fitRF_BA[[4]]$BalancedAccuracySD[which.max(fitRF_BA[[4]]$BalancedAccuracySD)],
                                                      fitGBM_BA[[4]]$BalancedAccuracySD[which.max(fitGBM_BA[[4]]$BalancedAccuracySD)],
                                                      fitAda_BA[[4]]$BalancedAccuracySD[which.max(fitAda_BA[[4]]$BalancedAccuracySD)]))
rownames(fitAllResults) = c("GLM", "GLM Elastic Net", "RandomForest", "Gradient Boosting", "AdaBoost")

txtStart(file = "out/AllTrain_Confusion.txt")
confusionMatrix(fitGLM)
confusionMatrix(fitGLMNet_F1); confusionMatrix(fitGLMNet_BA); confusionMatrix(fitGLMNet_AUC)
confusionMatrix(fitRF_F1); confusionMatrix(fitRF_BA); confusionMatrix(fitRF_AUC)
confusionMatrix(fitGBM_F1); confusionMatrix(fitGBM_BA); confusionMatrix(fitGBM_AUC)
confusionMatrix(fitAda_F1); confusionMatrix(fitAda_BA); confusionMatrix(fitAda_AUC)
txtStop()






load("out/SprayFits_NoSpray.Rdata")
pdf(file="out/ROC_SprayYears_WeatherOnly.pdf")
plot(roc(bigTrain2011_2013$.outcome ~ predict(fitGLMNet_AUC, bigTrain2011_2013, type='prob')$Present), col='blue', xlim=c(1,0), main="ROC Curves for Weather Only (2011, 2013). glmnet (blue), GLM (red),\n RandomForest (orange), GradientBoosting (green), AdaBoost (brown)")
plot(roc(bigTrain2011_2013$.outcome ~ predict(fitGLM, bigTrain2011_2013, type='prob')$Present), col='red', add=TRUE) #GLM ROC
plot(roc(bigTrain2011_2013$.outcome ~ predict(fitRF_AUC, bigTrain2011_2013, type='prob')$Present), col='orange', add=TRUE) #RandomForest ROC
plot(roc(bigTrain2011_2013$.outcome ~ predict(fitGBM_AUC, bigTrain2011_2013, type='prob')$Present), col='green', add=TRUE) #GBM ROC
plot(roc(bigTrain2011_2013$.outcome ~ predict(fitAda_AUC, bigTrain2011_2013, type = "prob")$Present), col='brown', add=TRUE) #Ada ROC
dev.off()

fitSprayResults_NoSpray = data.frame(BestF1 = c(fitGLM[[4]]$F1[which.max(fitGLM[[4]]$F1)],
                                                fitGLMNet_F1[[4]]$F1[which.max(fitGLMNet_F1[[4]]$F1)],
                                                fitRF_F1[[4]]$F1[which.max(fitRF_F1[[4]]$F1)],
                                                fitGBM_F1[[4]]$F1[which.max(fitGBM_F1[[4]]$F1)],
                                                fitAda_F1[[4]]$F1[which.max(fitAda_F1[[4]]$F1)]),
                                     BestF1SD = c(fitGLM[[4]]$F1SD[which.max(fitGLM[[4]]$F1SD)],
                                                  fitGLMNet_F1[[4]]$F1SD[which.max(fitGLMNet_F1[[4]]$F1SD)],
                                                  fitRF_F1[[4]]$F1SD[which.max(fitRF_F1[[4]]$F1SD)],
                                                  fitGBM_F1[[4]]$F1SD[which.max(fitGBM_F1[[4]]$F1SD)],
                                                  fitAda_F1[[4]]$F1SD[which.max(fitAda_F1[[4]]$F1SD)]),
                                     BestAUC = c(fitGLM[[4]]$AUC[which.max(fitGLM[[4]]$AUC)],
                                                 fitGLMNet_AUC[[4]]$AUC[which.max(fitGLMNet_AUC[[4]]$AUC)],
                                                 fitRF_AUC[[4]]$AUC[which.max(fitRF_AUC[[4]]$AUC)],
                                                 fitGBM_AUC[[4]]$AUC[which.max(fitGBM_AUC[[4]]$AUC)],
                                                 fitAda_AUC[[4]]$AUC[which.max(fitAda_AUC[[4]]$AUC)]),
                                     BestAUCSD = c(fitGLM[[4]]$AUCSD[which.max(fitGLM[[4]]$AUCSD)],
                                                   fitGLMNet_AUC[[4]]$AUCSD[which.max(fitGLMNet_AUC[[4]]$AUCSD)],
                                                   fitRF_AUC[[4]]$AUCSD[which.max(fitRF_AUC[[4]]$AUCSD)],
                                                   fitGBM_AUC[[4]]$AUCSD[which.max(fitGBM_AUC[[4]]$AUCSD)],
                                                   fitAda_AUC[[4]]$AUCSD[which.max(fitAda_AUC[[4]]$AUCSD)]),
                                     BestBalancedAccuracy = c(fitGLM[[4]]$BalancedAccuracy[which.max(fitGLM[[4]]$BalancedAccuracy)],
                                                              fitGLMNet_BA[[4]]$BalancedAccuracy[which.max(fitGLMNet_BA[[4]]$BalancedAccuracy)],
                                                              fitRF_BA[[4]]$BalancedAccuracy[which.max(fitRF_BA[[4]]$BalancedAccuracy)],
                                                              fitGBM_BA[[4]]$BalancedAccuracy[which.max(fitGBM_BA[[4]]$BalancedAccuracy)],
                                                              fitAda_BA[[4]]$BalancedAccuracy[which.max(fitAda_BA[[4]]$BalancedAccuracy)]),
                                     BestBalancedAccuracySD = c(fitGLM[[4]]$BalancedAccuracySD[which.max(fitGLM[[4]]$BalancedAccuracySD)],
                                                                fitGLMNet_BA[[4]]$BalancedAccuracySD[which.max(fitGLMNet_BA[[4]]$BalancedAccuracySD)],
                                                                fitRF_BA[[4]]$BalancedAccuracySD[which.max(fitRF_BA[[4]]$BalancedAccuracySD)],
                                                                fitGBM_BA[[4]]$BalancedAccuracySD[which.max(fitGBM_BA[[4]]$BalancedAccuracySD)],
                                                                fitAda_BA[[4]]$BalancedAccuracySD[which.max(fitAda_BA[[4]]$BalancedAccuracySD)]))
rownames(fitSprayResults_NoSpray) = c("GLM", "GLM Elastic Net", "RandomForest", "Gradient Boosting", "AdaBoost")

txtStart(file = "SprayTrain_NoSpray_Confusion.txt")
confusionMatrix(fitGLM)
confusionMatrix(fitGLMNet_F1); confusionMatrix(fitGLMNet_BA); confusionMatrix(fitGLMNet_AUC)
confusionMatrix(fitRF_F1); confusionMatrix(fitRF_BA); confusionMatrix(fitRF_AUC)
confusionMatrix(fitGBM_F1); confusionMatrix(fitGBM_BA); confusionMatrix(fitGBM_AUC)
confusionMatrix(fitAda_F1); confusionMatrix(fitAda_BA); confusionMatrix(fitAda_AUC)
txtStop()



load("out/SprayFits.Rdata")
pdf(file="out/ROC_SprayYears_SprayWeather.pdf")
plot(roc(bigTrain2011_2013$.outcome ~ predict(fitGLMNet_AUC, bigTrain2011_2013, type='prob')$Present), col='blue', xlim=c(1,0), main="ROC Curves for Weather and Spray (2011, 2013). glmnet (blue), GLM (red),\n RandomForest (orange), GradientBoosting (green), AdaBoost (brown)")
plot(roc(bigTrain2011_2013$.outcome ~ predict(fitGLM, bigTrain2011_2013, type='prob')$Present), col='red', add=TRUE) #GLM ROC
plot(roc(bigTrain2011_2013$.outcome ~ predict(fitRF_AUC, bigTrain2011_2013, type='prob')$Present), col='orange', add=TRUE) #RandomForest ROC
plot(roc(bigTrain2011_2013$.outcome ~ predict(fitGBM_AUC, bigTrain2011_2013, type='prob')$Present), col='green', add=TRUE) #GBM ROC
plot(roc(bigTrain2011_2013$.outcome ~ predict(fitAda_AUC, bigTrain2011_2013, type = "prob")$Present), col='brown', add=TRUE) #Ada ROC
dev.off()

fitSprayResults = data.frame(BestF1 = c(fitGLM[[4]]$F1[which.max(fitGLM[[4]]$F1)],
                                        fitGLMNet_F1[[4]]$F1[which.max(fitGLMNet_F1[[4]]$F1)],
                                        fitRF_F1[[4]]$F1[which.max(fitRF_F1[[4]]$F1)],
                                        fitGBM_F1[[4]]$F1[which.max(fitGBM_F1[[4]]$F1)],
                                        fitAda_F1[[4]]$F1[which.max(fitAda_F1[[4]]$F1)]),
                             BestF1SD = c(fitGLM[[4]]$F1SD[which.max(fitGLM[[4]]$F1SD)],
                                          fitGLMNet_F1[[4]]$F1SD[which.max(fitGLMNet_F1[[4]]$F1SD)],
                                          fitRF_F1[[4]]$F1SD[which.max(fitRF_F1[[4]]$F1SD)],
                                          fitGBM_F1[[4]]$F1SD[which.max(fitGBM_F1[[4]]$F1SD)],
                                          fitAda_F1[[4]]$F1SD[which.max(fitAda_F1[[4]]$F1SD)]),
                             BestAUC = c(fitGLM[[4]]$AUC[which.max(fitGLM[[4]]$AUC)],
                                         fitGLMNet_AUC[[4]]$AUC[which.max(fitGLMNet_AUC[[4]]$AUC)],
                                         fitRF_AUC[[4]]$AUC[which.max(fitRF_AUC[[4]]$AUC)],
                                         fitGBM_AUC[[4]]$AUC[which.max(fitGBM_AUC[[4]]$AUC)],
                                         fitAda_AUC[[4]]$AUC[which.max(fitAda_AUC[[4]]$AUC)]),
                             BestAUCSD = c(fitGLM[[4]]$AUCSD[which.max(fitGLM[[4]]$AUCSD)],
                                           fitGLMNet_AUC[[4]]$AUCSD[which.max(fitGLMNet_AUC[[4]]$AUCSD)],
                                           fitRF_AUC[[4]]$AUCSD[which.max(fitRF_AUC[[4]]$AUCSD)],
                                           fitGBM_AUC[[4]]$AUCSD[which.max(fitGBM_AUC[[4]]$AUCSD)],
                                           fitAda_AUC[[4]]$AUCSD[which.max(fitAda_AUC[[4]]$AUCSD)]),
                             BestBalancedAccuracy = c(fitGLM[[4]]$BalancedAccuracy[which.max(fitGLM[[4]]$BalancedAccuracy)],
                                                      fitGLMNet_BA[[4]]$BalancedAccuracy[which.max(fitGLMNet_BA[[4]]$BalancedAccuracy)],
                                                      fitRF_BA[[4]]$BalancedAccuracy[which.max(fitRF_BA[[4]]$BalancedAccuracy)],
                                                      fitGBM_BA[[4]]$BalancedAccuracy[which.max(fitGBM_BA[[4]]$BalancedAccuracy)],
                                                      fitAda_BA[[4]]$BalancedAccuracy[which.max(fitAda_BA[[4]]$BalancedAccuracy)]),
                             BestBalancedAccuracySD = c(fitGLM[[4]]$BalancedAccuracySD[which.max(fitGLM[[4]]$BalancedAccuracySD)],
                                                        fitGLMNet_BA[[4]]$BalancedAccuracySD[which.max(fitGLMNet_BA[[4]]$BalancedAccuracySD)],
                                                        fitRF_BA[[4]]$BalancedAccuracySD[which.max(fitRF_BA[[4]]$BalancedAccuracySD)],
                                                        fitGBM_BA[[4]]$BalancedAccuracySD[which.max(fitGBM_BA[[4]]$BalancedAccuracySD)],
                                                        fitAda_BA[[4]]$BalancedAccuracySD[which.max(fitAda_BA[[4]]$BalancedAccuracySD)]))
rownames(fitSprayResults) = c("GLM", "GLM Elastic Net", "RandomForest", "Gradient Boosting", "AdaBoost")

txtStart(file = "out/SprayTrain_Confusion.txt")
confusionMatrix(fitGLM)
confusionMatrix(fitGLMNet_F1); confusionMatrix(fitGLMNet_BA); confusionMatrix(fitGLMNet_AUC)
confusionMatrix(fitRF_F1); confusionMatrix(fitRF_BA); confusionMatrix(fitRF_AUC)
confusionMatrix(fitGBM_F1); confusionMatrix(fitGBM_BA); confusionMatrix(fitGBM_AUC)
confusionMatrix(fitAda_F1); confusionMatrix(fitAda_BA); confusionMatrix(fitAda_AUC)
txtStop()

BestAUC_NoCV = data.frame(Weather_AllYears= Weather_AllYears_AUC,
                          Weather_SprayYears = Weather_SprayYears_AUC,
                          SprayWeather_SprayYears = SprayWeather_SprayYears_AUC)
BestAUC = data.frame(Weather_AllYears=fitAllResults$BestAUC, 
                     SprayWeather_SprayYears = fitSprayResults_NoSpray$BestAUC, 
                     Weather_SprayYears=fitSprayResults$BestAUC)
BestBA = data.frame(Weather_AllYears=fitAllResults$BestBalancedAccuracy, 
                     SprayWeather_SprayYears = fitSprayResults_NoSpray$BestBalancedAccuracy, 
                     Weather_SprayYears=fitSprayResults$BestBalancedAccuracy)
rownames(BestAUC_NoCV) = rownames(BestAUC) = rownames(BestBA) = c("GLM", "GLM ElasticNet", "RandomForest", "Gradient Boosting", "AdaBoost")


print(xtable(round(BestAUC_NoCV, 3)))
print(xtable(round(BestAUC, 3)))
print(xtable(round(BestBA, 3)))

#Obtained from the plots above.
Weather_AllYears_AUC = c(0.7961, 0.7965, 0.8547, 0.9329, 0.8741)
Weather_SprayYears_AUC = c(0.8195, 0.8196, 0.9267, 0.9271, 0.9234)
SprayWeather_SprayYears_AUC = c(0.8215, 0.8216, 0.9307, 0.9643, 0.9268)
AUCMatrix = data.frame(Weather_AllYears = Weather_AllYears_AUC,
                       Weather_SprayYears = Weather_SprayYears_AUC,
                       SprayWeather_SprayYears = SprayWeather_SprayYears_AUC)
rownames(AUCMatrix) = c("GLM", "GLM Elastic Net", "RandomForest", "Gradient Boosting", "AdaBoost")
