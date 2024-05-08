library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('prophet')
library('rugarch')

library('tidyverse')
library('lubridate')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
#source("/mnt/hollandr/config.r")

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lookback <- 500 #days
forecastDays <- 20 

indexDf <- sqlQuery(lcon, "select px_close, time_stamp from VIX_HISTORY")
iXts <- xts(indexDf[,1], indexDf[,2])
names(iXts) <- c("VIX")

startDate <- first(index(iXts))
endDate <- last(index(iXts))

garch_spec <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0))) #GARCH(1,1) is good enough. https://archive.aessweb.com/index.php/5002/article/view/2072

forecastStatsDf <- data.frame(TIME_STAMP="", PROPHET=0.0, GARCH_11=0.0, LOCF=0.0, AVG=0.0)
for(ii in (lookback+1):(nrow(iXts) - forecastDays)){
	trainData <- coredata(iXts[(ii-lookback):ii])
	fcastVix <- iXts[(ii+1):(ii+forecastDays)]
	actVix <- coredata(fcastVix)[,1]
	
	#predict prophet
	trainDf <- data.frame(ds = index(iXts[(ii-lookback):ii]), y=trainData)
	colnames(trainDf) <- c('ds', 'y')
	lModel1 <- prophet(trainDf, daily.seasonality=FALSE, yearly.seasonality=FALSE)

	future <- data.frame(ds = c(trainDf$ds, index(fcastVix)))
	nextDf <- predict(lModel1, future)
	predVix1 <- tail(nextDf$yhat, forecastDays)
	
	rmseProp <- sqrt(mean((actVix - predVix1)^2))
	
	#predict garch
	lModel2 <- ugarchfit(spec = garch_spec, data = trainData)
	
	predVix2 <- ugarchforecast(lModel2, n.ahead = 20)
	predVix2 <- as.numeric(predVix2@forecast$series)
	rmseGarch11 <- sqrt(mean((actVix - predVix2)^2))
	
	#locf
	predVix3 <- rep(as.numeric(xts::last(fcastVix)), forecastDays)
	rmseLocf <- sqrt(mean((actVix - predVix3)^2))
	
	#avg
	predVixAvg <- unlist(lapply(1:forecastDays, function(X) (predVix1[X] + predVix2[X] + predVix3[X])/3))
	rmseAvg <- sqrt(mean((actVix - predVixAvg)^2))
	
	#save results
	forecastStatsDf <- rbind(forecastStatsDf, c(toString(index(xts::last(fcastVix))), rmseProp, rmseGarch11, rmseLocf, rmseAvg))
}

forecastStatsDf <- forecastStatsDf[-1,]
forecastStatsDf$TIME_STAMP <- as.Date(forecastStatsDf$TIME_STAMP)
forecastStatsDf$PROPHET <- as.numeric(forecastStatsDf$PROPHET)
forecastStatsDf$GARCH_11 <- as.numeric(forecastStatsDf$GARCH_11)
forecastStatsDf$LOCF <- as.numeric(forecastStatsDf$LOCF)
forecastStatsDf$AVG <- as.numeric(forecastStatsDf$AVG)

save(forecastStatsDf, file=sprintf("%s/forecastStatsDf.Rdata", reportPath))

########################################

