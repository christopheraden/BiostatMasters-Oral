if(Sys.info()['user']=="aden") { setwd("~/Dropbox/School/MS Exam/") } else setwd("~/MS\ Exam/")
library(data.table)
library(caret)
library(doMC)
library(parallel)
library(gdata)

load("out/AllFits.Rdata")
load("data/BigTest.Rdata")

# test = fread("data/test.csv")
# test$Species[test$Species=="UNSPECIFIED CULEX"] = NA
# load("data/bigTrain_1MosquitoPerRow.rdata")
# weather = fread("data/weather.csv")
# weather[weather=="M"] = NA
# weather$Date = as.Date(weather$Date)
# weather$Tmax = as.numeric(weather$Tmax)
# weather$Tmin = as.numeric(weather$Tmin)
# weather$Tavg = as.numeric(weather$Tavg)
# weather$Tmax = as.numeric(weather$Tmax)
# weather$DewPoint = as.numeric(weather$DewPoint)
# weather$WetBulb = as.numeric(weather$WetBulb)
# weather$Heat = as.numeric(weather$Heat)
# weather$Cool = as.numeric(weather$Cool)
# weather$DewPoint = as.numeric(weather$DewPoint)
# weather$PrecipTotal[which(weather$PrecipTotal == "  T")] = .005
# weather$PrecipTotal = as.numeric(weather$PrecipTotal)
# weather$StnPressure = as.numeric(weather$StnPressure)
# weather$SeaLevel = as.numeric(weather$SeaLevel)
# weather$ResultSpeed = as.numeric(weather$ResultSpeed)
# weather$ResultDir = as.numeric(weather$ResultDir)
# weather$AvgSpeed = as.numeric(weather$AvgSpeed)
# weather$Depart = weather$Sunrise = weather$Sunset = weather$Depth = weather$Water1 = weather$SnowFall = NULL
# weatherAgg = as.data.table(aggregate(weather[,!"CodeSum", with=FALSE], by = list(weather$Date), FUN = mean, na.rm=TRUE))
# weatherAgg$Date = NULL
# WAnames = names(weatherAgg); WAnames[1] = "Date"
# setnames(weatherAgg, names(weatherAgg), WAnames)
# 
# test$Date = as.Date(test$Date)
# test$Species = factor(test$Species, labels = c("Erraticus", "Pipiens","Pipiens.Restuans", "Restuans", "Salinarius", "Tarsal", "Territans"))
# test$Species = as.integer(test$Species)
# test$Trap = factor(test$Trap)
# test$Year = year(test$Date)
# test$Month = month(test$Date)
# test$Date = as.Date(test$Date)
# 
# numNAs = sum(is.na(test$Species))
# NAspecies = sample(1:7, size = numNAs, replace=TRUE, prob = table(bigTrain$Species))
# test[is.na(test$Species), ':='(Species = NAspecies)]
# 
# bigTest = merge(test, weatherAgg, by="Date")
# bigTest$Date = bigTest$Address = bigTest$Street = bigTest$AddressNumberAndStreet = bigTest$AddressAccuracy = bigTest$Station = NULL
# save(bigTest, file="BigTest.Rdata")
# keep(list = "bigTest", sure=TRUE)

bigTestPred = bigTest
bigTestPred$Id = NULL
TestRFAUC = abs(as.integer(predict(fitRF_AUC, newdata = bigTestPred))-1)
TestRFBA = abs(as.integer(predict(fitRF_BA, newdata = bigTestPred))-1)
TestRFF1 = abs(as.integer(predict(fitRF_F1, newdata = bigTestPred))-1)

TestAdaAUC = abs(as.integer(predict(fitAda_AUC, newdata = bigTestPred))-1)
TestAdaBA = abs(as.integer(predict(fitAda_BA, newdata = bigTestPred))-1)
TestAdaF1 = abs(as.integer(predict(fitAda_F1, newdata = bigTestPred))-1)

TestGBMAUC = abs(as.integer(predict(fitGBM_AUC, newdata = bigTestPred))-1)
TestGBMBA = abs(as.integer(predict(fitGBM_BA, newdata = bigTestPred))-1)
TestGBMF1 = abs(as.integer(predict(fitGBM_F1, newdata = bigTestPred))-1)

write.csv( data.frame(Id=bigTest$Id, WnvPresent = TestRFAUC), file="TestRFAUC.csv")
write.csv( data.frame(Id=bigTest$Id, WnvPresent = TestRFBA), file="TestRFBA.csv")
write.csv( data.frame(Id=bigTest$Id, WnvPresent = TestRFF1), file="TestRFF1.csv")

write.csv( data.frame(Id=bigTest$Id, WnvPresent = TestAdaAUC), file="TestAdaAUC.csv")
write.csv( data.frame(Id=bigTest$Id, WnvPresent = TestAdaBA), file="TestAdaBA.csv")
write.csv( data.frame(Id=bigTest$Id, WnvPresent = TestAdaF1), file="TestAdaF1.csv")

write.csv( data.frame(Id=bigTest$Id, WnvPresent = TestGBMAUC), file="TestGBMAUC.csv")
write.csv( data.frame(Id=bigTest$Id, WnvPresent = TestGBMBA), file="TestGBMBA.csv")
write.csv( data.frame(Id=bigTest$Id, WnvPresent = TestGBMF1), file="TestGBMF1.csv")