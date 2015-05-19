if(Sys.info()['user']=="Aden") { setwd("~/Dropbox/School/MS Exam/") } else setwd("~/MS\ Exam/")
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
library(geosphere)

train = fread("data/train.csv")
train$WnvPresent = factor(train$WnvPresent, labels=c("NotPresent", "Present"))
train$Species = factor(train$Species, labels = c("Erraticus", "Pipiens","Pipiens.Restuans", "Restuans", "Salinarius", "Tarsal", "Territans"))
train$Species = as.integer(train$Species)
train$Trap = factor(train$Trap)
train$Year = year(train$Date)
train$Month = month(train$Date)
train$Date = as.Date(train$Date)
train$.outcome = train$WnvPresent

weather = fread("data/weather.csv")
weather[weather=="M"] = NA
weather$Date = as.Date(weather$Date)
weather$Tmax = as.numeric(weather$Tmax)
weather$Tmin = as.numeric(weather$Tmin)
weather$Tavg = as.numeric(weather$Tavg)
weather$Tmax = as.numeric(weather$Tmax)
weather$DewPoint = as.numeric(weather$DewPoint)
weather$WetBulb = as.numeric(weather$WetBulb)
weather$Heat = as.numeric(weather$Heat)
weather$Cool = as.numeric(weather$Cool)
weather$DewPoint = as.numeric(weather$DewPoint)
weather$PrecipTotal[which(weather$PrecipTotal == "  T")] = .005
weather$PrecipTotal = as.numeric(weather$PrecipTotal)
weather$StnPressure = as.numeric(weather$StnPressure)
weather$SeaLevel = as.numeric(weather$SeaLevel)
weather$ResultSpeed = as.numeric(weather$ResultSpeed)
weather$ResultDir = as.numeric(weather$ResultDir)
weather$AvgSpeed = as.numeric(weather$AvgSpeed)
weather$Depart = weather$Sunrise = weather$Sunset = weather$Depth = weather$Water1 = weather$SnowFall = NULL

weatherAgg = as.data.table(aggregate(weather[,!"CodeSum", with=FALSE], by = list(weather$Date), FUN = mean, na.rm=TRUE))
weatherAgg$Date = NULL
WAnames = names(weatherAgg); WAnames[1] = "Date"
setnames(weatherAgg, names(weatherAgg), WAnames)
bigTrain = merge(train, weatherAgg, by="Date")
bigTrain$Address = bigTrain$Street = bigTrain$AddressNumberAndStreet = bigTrain$AddressAccuracy = bigTrain$Station = NULL

#Add in Spraying/Weather Effects:
spray = fread("data/spray.csv")
spray$Date = as.Date(spray$Date)
bigTrain$Date = as.Date(bigTrain$Date)

bigTrain2011_2013 = bigTrain[year(bigTrain$Date) %in% c(2011, 2013), ]
uniqueSprays = unique(spray[, .(Date, Latitude, Longitude)])

sprayTrapDist = distm(cbind(bigTrain2011_2013$Longitude, bigTrain2011_2013$Latitude),
                      cbind(uniqueSprays$Longitude, uniqueSprays$Latitude),
                      fun = distHaversine) * 0.000621371 #Convert Haversine dist to Mi
whichDists = lapply(1:nrow(bigTrain2011_2013), function(r) which(sprayTrapDist[r,] < 2))
recentlySprayedCloseBy = lapply(1:nrow(bigTrain2011_2013), function(i)
  which(bigTrain2011_2013$Date[i] - uniqueSprays[whichDists[[i]] , Date] < 7 & 
          bigTrain2011_2013$Date[i] - uniqueSprays[whichDists[[i]] , Date] >= 0))
bigTrain2011_2013$NearbySprays = sapply(recentlySprayedCloseBy, length)

keep(bigTrain, bigTrain2011_2013, sure = TRUE)
save(bigTrain, file="data/bigTrain.rdata")
save(bigTrain2011_2013, file="data/bigTrain2011_2013.rdata")

bigTrain = bigTrain[rep(seq_len(nrow(bigTrain)), times=bigTrain$NumMosquitos),]
bigTrain2011_2013 = bigTrain2011_2013[rep(seq_len(nrow(bigTrain2011_2013)), times=bigTrain2011_2013$NumMosquitos),]
bigTrain$NumMosquitos = bigTrain2011_2013$NumMosquitos = NULL
save(bigTrain, file="data/bigTrain_1MosquitoPerRow.rdata")
save(bigTrain2011_2013, file="data/bigTrain2011_2013_1MosquitoPerRow.rdata")