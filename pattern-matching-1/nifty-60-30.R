source("/mnt/hollandr/config.R")
#source("d:/stockviz/r/config.r")

reportPath<-"."
#reportPath <- "\\\\siberia\\R\\intraday\\hedged01"

library('RODBC')
library('RPostgres')
library('e1071')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')

library('foreach')
library("doFuture")

#registerDoParallel(4)
registerDoFuture()
plan(multisession, workers = 2)

options(stringsAsFactors = FALSE)
options("scipen"=100)
options(future.globals.maxSize= 10*1024^3)

baseDate <- as.Date("1990-01-01", tz='UTC')
startTime <- hms("09:15:00")
endTime <- hms("15:30:00")

drag<-0.02/100

startDate <- as.Date("2015-01-01")

print("fetching price...")

pcon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)
niftyDf <- dbGetQuery(pcon, "select c as cl, tick_stamp from zd_index_bars where symbol = $1 and time_stamp >= $2", params=list('NIFTY 50', startDate))

dbDisconnect(pcon)

niftyDf2 <- niftyDf
niftyDf2$tick <- as.POSIXct(baseDate + seconds(niftyDf$tick_stamp))
niftyDf2$tod <- hms(format(niftyDf2$tick, "%H:%M:%S"))

niftyDf2 <- niftyDf2 %>% filter(tod >= startTime & tod <= endTime) %>% select(cl, tick) 

npXts <- xts(niftyDf2$cl, niftyDf2$tick)
dates <- unique(as.Date(index(npXts)))

#one minute returns. ignore first 15min and last 30min
np01 <- npXts/stats::lag(npXts, 1)-1 
np01["T09:00/T09:30", ] <- NA
np01["T15:00/T15:30", ] <- NA

np01 <- na.omit(np01)

#60-minute boundaries
seq60min <- format(seq(from=as.POSIXct("2013-01-01 09:30:00", tz="GMT"), to=as.POSIXct("2013-01-01 15:00:00", tz="GMT"),  by='60 min'), '%H:%M')

#look-back days
lbDays <- 200

#dates <- dates[1:202]

euclidDist <- function(x1, x2){
	c1 <- cumprod(1 + x1)
	c2 <- cumprod(1 + x2)
	tryCatch({
		edist <- stats::dist(t(data.frame(a=coredata(c1), b=coredata(c2))))
		return(edist)
	}, error=function(x) NA)
}

print(Sys.time())

allStatsDf <- foreach(matchSegIndex=1:(length(seq60min)-1), .combine=rbind.data.frame) %dopar% {
#for(matchSegIndex in 1:(length(seq60min)-1)){

	segStatsDf <- data.frame(DATE = "", SEGMENT = 0, AVG_LB_RET=0.0, MEDIAN_LB_RET=0.0, SD_LB_RET=0.0, ACTUAL_RET=0.0)
	for(lb in (lbDays+1):length(dates)){
	
		#string of returns to match
		np01seg <- np01[toString(dates[lb]),][paste0('T', seq60min[matchSegIndex], '/T', seq60min[matchSegIndex+1]),]
		
		#next 30-min segment boundary
		next30min <- format(as.POSIXct(paste("2013-01-01", seq60min[matchSegIndex+1]), tz = "GMT") + lubridate::minutes(30), '%H:%M')
		
		#next 30-min return
		actualNext30minRet <- as.numeric(Return.cumulative(np01[toString(dates[lb]),][paste0('T', seq60min[matchSegIndex + 1], '/T', next30min),]))

		#for all look-back dates, calculate distaince
		statsDf <- data.frame(DATE = "", LB_DATE="", SEGMENT = 0, DIST=0.0)
		for(i in (lb-lbDays):(lb-1)){
			lbseg <- np01[toString(dates[i]),][paste0('T', seq60min[matchSegIndex], '/T', seq60min[matchSegIndex+1]),]
			
			statsDf <- rbind(statsDf, c(toString(dates[lb]), toString(dates[i]), matchSegIndex, euclidDist(np01seg, lbseg)))
		}
		
		statsDf <- statsDf[-1,]
		statsDf$DIST <- as.numeric(statsDf$DIST)
		nearestMatches <- head(statsDf[order(statsDf$DIST),], lbDays/4)
		
		#calcuate the next 30-min return for the nearest matches
		nearestMatches$NEXT_30_RET <- NA

		for(j in 1:nrow(nearestMatches)){
			nearestMatches$NEXT_30_RET[j] <- as.numeric(Return.cumulative(np01[nearestMatches$LB_DATE[j], ][paste0('T', seq60min[matchSegIndex+1], '/T', next30min),]))
		}
		
		segStatsDf <- rbind(segStatsDf, c(toString(dates[lb]), matchSegIndex, mean(nearestMatches$NEXT_30_RET), median(nearestMatches$NEXT_30_RET), sd(nearestMatches$NEXT_30_RET), actualNext30minRet))
	}
	
	segStatsDf <- segStatsDf[-1,]
	segStatsDf
}


allStatsDf$SEGMENT <- as.numeric(allStatsDf$SEGMENT)
allStatsDf$AVG_LB_RET <- as.numeric(allStatsDf$AVG_LB_RET)
allStatsDf$MEDIAN_LB_RET <- as.numeric(allStatsDf$MEDIAN_LB_RET)
allStatsDf$SD_LB_RET <- as.numeric(allStatsDf$SD_LB_RET)
allStatsDf$ACTUAL_RET <- as.numeric(allStatsDf$ACTUAL_RET)

#print(allStatsDf)

save(allStatsDf, file=sprintf("%s/pattern-1.Rda", reportPath))
