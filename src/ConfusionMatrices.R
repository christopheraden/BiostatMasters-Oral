library(xtable)
Method = rep(c("GLM", "ElasticNet", "RandomForest", "GradientBoosting", "AdaBoost"), times=3)
Data = rep(c("AllYears, Weather", "SprayYears, Weather", "SprayYears, SprayWeather"), each=5)

#AUC
AllFitClassification = c(89.1, 10.5, .1, .3, 89.1, 10.6, .1, .1, 88.3, 4, .9, 6.7, 87.8, 6.2, 1.5, 4.6, 88.4, 9.2, .8, 1.6)
SprayYrWeatherClassification = c(86.1, 12.5, .5, .9, 86.2, 12.7, .4, .7, 85, 3.2, 1.6, 10.2, 84.4, 5, 2.2, 8.4, 84.6, 8.8, 2, 4.6)
SprayYrWeatherSprayClassification = c(86.1, 12.4, .5, 1, 86.1, 12.5, .5, .9, 85.1, 3.3, 1.5, 10.1, 84.5, 5, 2.1, 8.4, 84.5, 9, 2.1, 4.4)
Confusions = matrix(c(AllFitClassification, SprayYrWeatherClassification, SprayYrWeatherSprayClassification), ncol=4, byrow=TRUE)
colnames(Confusions) = c("True Neg", "False Neg", "False Pos", "True Pos")
print(xtable(data.frame(Method, Data, Confusions)), include.rownames=FALSE)

#F1
AllFitClassification = c(89.1, 10.5, .1, .3, 89.1, 10.5, .1, .2, 88.3, 4, 1, 6.8, 87.9, 6.3, 1.4, 4.5, 88.4, 9.1, .8, 1.7)
SprayYrWeatherClassification = c(86.1, 12.5, .5, .9, 86.5, 12.9, 0, .5, 85.2, 3.4, 1.4, 10, 85.3, 10.5, 1.3, 2.9, 84.6, 8.8, 2, 4.7)
SprayYrWeatherSprayClassification = c(86.1, 12.4, .5, 1, 86.4, 12.8, .2, .6, 85.1, 3.3, 1.5, 10.1, 85.4, 10.2, 1.2, 3.2, 84.5, 8.7, 2.1, 4.7)
Confusions = matrix(c(AllFitClassification, SprayYrWeatherClassification, SprayYrWeatherSprayClassification), ncol=4, byrow=TRUE)
F1 = sapply(1:nrow(Confusions), function(i) {
  sens = Confusions[i, 4] / (Confusions[i, 2] + Confusions[i, 4])
  ppv = Confusions[i, 4] / (Confusions[i, 3] + Confusions[i, 4])
  round(2 * sens * ppv / (ppv + sens), 3)
})
Confusions = data.frame(Confusions, F1)
colnames(Confusions) = c("True Neg", "False Neg", "False Pos", "True Pos", "F1")
print(xtable(data.frame(Method, Data, Confusions)), include.rownames=FALSE)

#BA
AllFitClassification = c(89.1, 10.5, .1, .3, 89.1, 10.5, .1, .2, 88.2, 3.9, 1, 6.8, 87.8, 6.6, 1.5, 4.1, 88.4, 9.1, .8, 1.6)
SprayYrWeatherClassification = c(86.1, 12.5, .5, .9, 86, 12.5, .5, .9, 85, 3.2, 1.5, 10.2, 84.5, 5.2, 2.1, 8.3, 84.6, 8.8, 2, 4.6)
SprayYrWeatherSprayClassification = c(86.1, 12.4, .5, 1, 86.1, 12.5, .5, .9, 84.9, 3.2, 1.7, 10.2, 84.5, 5, 2.1, 8.4, 84.5, 9, 2.1, 4.4)
Confusions = matrix(c(AllFitClassification, SprayYrWeatherClassification, SprayYrWeatherSprayClassification), ncol=4, byrow=TRUE)
BA = sapply(1:nrow(Confusions), function(i) 
  round(mean(c( (Confusions[i, 1] / (Confusions[i, 1] + Confusions[i, 3])),
  (Confusions[i, 4] / (Confusions[i, 2] + Confusions[i, 4])))), 3))
Confusions = data.frame(Confusions, BA)
colnames(Confusions) = c("True Neg", "False Neg", "False Pos", "True Pos", "BA")
print(xtable(data.frame(Method, Data, Confusions)), include.rownames=FALSE)
