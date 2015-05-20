if(Sys.info()['user']=="aden") { setwd("~/Dropbox/School/MS Exam/") } else setwd("~/MS\ Exam/")
library(data.table)
library(caret)
library(doMC)
library(lubridate)
library(Metrics)
library(ggmap)
library(vcd)

load("data/bigTrain.rdata")
cdplot(WnvPresent~NumMosquitos, data=bigTrain, main="Conditional Probability of Number of Mosquitos on West Nile Presence")

load("data/bigTrain_1MosquitoPerRow.rdata")
mosaic(bigTrain$WnvPresent~bigTrain$Species, main="Mosaic Plot of Mosquito Species vs. WNv Presence")

par(mfrow=1:2)
cdplot(WnvPresent~Latitude, data=bigTrain, main=NULL)
cdplot(WnvPresent~Longitude, data=bigTrain, main=NULL)

#Species of Mosquito on Presence of WNV
with(bigTrain, chisq.test(WnvPresent, Species, simulate.p.value = TRUE, B=1E4)) #An association, not surprising.

#Back to better labeling scheme.
bigTrain$Species = factor(bigTrain$Species, labels = c("Erraticus", "Pipiens","Pipiens/Restuans", "Restuans", "Salinarius", "Tarsal", "Territans"))

#Data within Traps may be correlated, by nature of trap's location
with(bigTrain, table(WnvPresent, Trap))
chisq.test(bigTrain$WnvPresent, bigTrain$Trap, simulate.p.value = TRUE, B=1E4)
#Could use as poor man's location effect?
TrapLogit = glm(WnvPresent ~ Trap + Species, data=bigTrain, family=binomial)
summary(TrapLogit)