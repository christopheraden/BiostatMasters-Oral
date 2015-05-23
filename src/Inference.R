if(Sys.info()['user']=="aden") { setwd("~/Dropbox/School/MS Exam/") } else setwd("~/MS\ Exam/")
library(car)
library(MASS)
library(pROC)
library(glmnet)
library(doMC)
library(parallel)
library(mgcv)
library(LogisticDx)
library(genridge)
library(geepack)

#2011 & 2013 years, with Sprays.
load("data/bigTrain2011_2013_1MosquitoPerRow.rdata")
train = bigTrain2011_2013; rm(bigTrain2011_2013)
train$WnvPresent = train$.outcome; train$.outcome = NULL
n = nrow(train)
n_eff = sample(1:n, .8*n)
holdout = setdiff(1:n, n_eff)

par(mfrow=1:2)
truehist(train$NearbySprays, xlab="Number of Nearby Sprays", main="Histogram of Nearby Sprays", prob = FALSE)
truehist(train$NearbySprays[train$NearbySprays>0], xlab="Number of Nearby Nonzero Sprays", main="Histogram of Nearby Nonzero Sprays", prob=FALSE)
spineplot(WnvPresent~NearbySprays, xlim=c(0, 1), data=train, main="", xlab="Nearby Sprays")
cdplot(WnvPresent~NearbySprays, data=train[train$NearbySprays>0,], main="", xlab="Nearby Sprays > 0")

SprayEffectCts = glm(WnvPresent ~ . -Species -Trap -Block -Tmax -Tmin -Cool -Date + as.factor(Month) -Month + as.factor(Year) -Year + Latitude * Longitude + as.factor(Species) + NearbySprays, data=train, family='binomial')
SprayEffectDich =glm(WnvPresent ~ . -Species -Trap -Block -Tmax -Tmin -Cool -Date + as.factor(Month) -Month + as.factor(Year) -Year + Latitude * Longitude + as.factor(Species) -NearbySprays + as.factor(NearbySprays>0),data=train, family='binomial')
summary(SprayEffectCts)
summary(SprayEffectDich)

ridgeGCV = function(Lambda, X=train[n_eff,]){
  log(genridge::ridge(as.numeric(WnvPresent)-1 ~ . - Species - Trap - Block - Tmax - Tmin - Cool - 
              Date + as.factor(Month) - Month + as.factor(Year) - Year + 
              Latitude * Longitude + as.factor(Species) + as.factor(NearbySprays > 0) -NearbySprays, 
            data=X, lambda=Lambda, family='binomial')$GCV)
}
lambda_opt = optimize(f = ridgeGCV, lower=0, upper=1E4)$minimum

SprayEffectCtsNoRidge = genridge::ridge(as.numeric(WnvPresent)-1 ~ . - Species - Trap - Block - Tmax - Tmin - Cool - 
                                Date + as.factor(Month) - Month + as.factor(Year) - Year + 
                                Latitude * Longitude + as.factor(Species) + NearbySprays, 
                              data=train[n_eff,], lambda=0, family='binomial')
SprayEffectDichNoRidge = genridge::ridge(as.numeric(WnvPresent)-1 ~ . - Species - Trap - Block - Tmax - Tmin - Cool - 
                                 Date + as.factor(Month) - Month + as.factor(Year) - Year + 
                                 Latitude * Longitude + as.factor(Species) -NearbySprays + as.factor(NearbySprays > 0), 
                               data=train[n_eff,], lambda=0, family='binomial')
SprayEffectDichRidge = genridge::ridge(as.numeric(WnvPresent)-1 ~ . - Species - Trap - Block - Tmax - Tmin - Cool - 
                               Date + as.factor(Month) - Month + as.factor(Year) - Year + 
                               Latitude * Longitude + as.factor(Species) + as.factor(NearbySprays > 0) -NearbySprays, 
                             data=train[n_eff,], lambda=lambda_opt, family='binomial')

VIFs = data.frame(t(vif.ridge(SprayEffectCtsNoRidge)),
                  t(vif.ridge(SprayEffectDichNoRidge)), 
                  t(vif.ridge(SprayEffectDichRidge)))
colnames(VIFs) = c("NoRidgeContinuous", "NoRidgeDiscrete", "RidgingDiscrete(lambda=3.596)")

train$Month = as.factor(train$Month)
train$Year = as.factor(train$Year)
train$Species = as.factor(train$Species)

X.design = model.matrix(~ . -WnvPresent -Trap -Cool -Block -Date + Latitude * Longitude -NearbySprays + as.factor(NearbySprays > 0)-1, data=train[n_eff,])
Y.response = as.numeric(train$WnvPresent[n_eff])-1

glmNoReg = glm(WnvPresent~.-Trap-Cool -Block -Date -Tmax - Tmin + Latitude * Longitude -NearbySprays + as.factor(NearbySprays > 0), data=train[n_eff,], family="binomial")
summary(glmNoReg)
vif(glmNoReg)

registerDoMC(detectCores())

CV = cv.glmnet(X.design, Y.response, family='binomial', alpha=.9, parallel = TRUE)
glmnetFit = glmnet(x = X.design, y = Y.response, lambda = CV$lambda.1se, family='binomial', alpha = .9)
coef(glmnetFit, CV$lambda.1se) #Best model, chosen by Lasso

train$isSpecies1247 = factor(train$Species %in% c("1", "2", "4", "7"))
train$anyNearbySprays = factor(train$NearbySprays > 0)

glmHoldout = glm(WnvPresent ~ isSpecies1247 + Latitude*Longitude-Latitude + Year + Month + Tmax + Heat + StnPressure + ResultSpeed + anyNearbySprays, data = train[holdout,], family='binomial')
summary(glmHoldout)
vif(glmHoldout)

gamHoldout = gam(WnvPresent ~ isSpecies1247 + te(Longitude, Latitude) + Year + Month + s(Tmax) + Heat +s(StnPressure) +s(ResultSpeed) +anyNearbySprays, data = train[holdout,], family='binomial')
summary(gamHoldout)
par(mfrow=c(1,1))
plot(gamHoldout, select = 1, scheme = 2, main="Heatmap of Longitude and Latitude vs. WNv Incidence")

glmHoldout_RandomEffect <- geese(as.numeric(WnvPresent)-1 ~ isSpecies1247 + ns(Longitude, 3) + Year + Month + Tmax + Heat + StnPressure + ResultSpeed + anyNearbySprays,
                        id = Trap, data = train[holdout,], family = binomial, corstr="exchangeable", scale.fix= TRUE)
summary(glmHoldout_RandomEffect)
