library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('lubridate')
library('reshape2')
library('viridis')
library('ggpubr')
library('ggthemes')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")

pdf(NULL)
reportPath <- "D:/StockViz/public/blog/allocation vs tactical"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

eqPct <- 0.5

indexName <- "NIFTY 50 TR"
endDate <- as.Date("2020-05-31")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp <= '%s'", indexName, endDate))
eqXts <- xts(pDf[,2], pDf[,1])
names(eqXts) <- c("EQUITY")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, tri from index_ccil_tenor where index_name='0_5' and time_stamp <= '%s'", endDate))
bndXts <- xts(pDf[,-1], pDf[,1])
names(bndXts) <- c("BOND")

######################################
#rolling returns

eqRets <- monthlyReturn(eqXts)
bndRets <- monthlyReturn(bndXts)

eqRets <- eqRets[-1,]
bndRets <- bndRets[-1,]

dateRange <- paste0(first(index(bndRets)), "/")

allRets <- merge(eqRets[dateRange], bndRets[dateRange])
allRets[,2] <- na.locf(allRets[,2], fromLast=T)
allRets <- na.omit(allRets)

dateRange <- paste0("/", first(index(bndRets))-1)
rndBndRets <- xts(rnorm(nrow(eqRets[dateRange]), mean=mean(coredata(bndRets)), sd=sd(coredata(bndRets))), index(eqRets[dateRange]))

allMonthlyRets <- rbind(allRets, merge(eqRets[dateRange], rndBndRets))
names(allMonthlyRets) <- c('EQ.RET', 'BND.RET')

#SMA
smaLb <- 50 #days
eqRets <- weeklyReturn(eqXts)
bndRets <- weeklyReturn(bndXts)

eqRets <- eqRets[-1,]
bndRets <- bndRets[-1,]

dateRange <- paste0(first(index(bndRets)), "/")

allRets <- merge(eqRets[dateRange], bndRets[dateRange])
allRets[,2] <- na.locf(allRets[,2], fromLast=T)
allRets <- na.omit(allRets)

dateRange <- paste0("/", first(index(bndRets))-1)
rndBndRets <- xts(rnorm(nrow(eqRets[dateRange]), mean=mean(coredata(bndRets)), sd=sd(coredata(bndRets))), index(eqRets[dateRange]))

allRets <- rbind(allRets, merge(eqRets[dateRange], rndBndRets))
names(allRets) <- c('EQ.RET', 'BND.RET')

allXts <- merge(eqXts, SMA(eqXts, smaLb), allRets[,1], allRets[,2]) #1,2,3,4
allXts <- na.omit(allXts)
allXts <- merge(allXts, stats::lag(allXts[,3], -1), stats::lag(allXts[,4], -1)) #5,6
allXts <- na.omit(allXts)

btest <- merge(ifelse(allXts[,1] > allXts[,2], allXts[,5], allXts[,6]))
btest <- na.omit(merge(btest, stats::lag(btest, 1))[,2])

######################################

allocRet <- allMonthlyRets[,'EQ.RET']*eqPct + allMonthlyRets[,'BND.RET']*(1-eqPct)
monthlyRets <- rollapply(allocRet, 12*10, Return.annualized)
weeklyRets <- rollapply(btest, 10*52, Return.annualized)

######################################

yearlyEquityRets <- yearlyReturn(eqXts)
yearlyBondRets <- yearlyReturn(bndXts)

yearlyEquityRets <- yearlyEquityRets[-1,]
yearlyBondRets <- yearlyBondRets[-1,]

allYearlyRets <- merge(yearlyEquityRets, yearlyBondRets)
allYearlyRets[,2] <- na.locf(allYearlyRets[,2], fromLast=T)
allYearlyRets <- na.omit(allYearlyRets)

names(allYearlyRets) <- c('EQ.RET', 'BND.RET')

######################################

allocRet <- allYearlyRets[,'EQ.RET']*eqPct + allYearlyRets[,'BND.RET']*(1-eqPct)
yearlyRets <- rollapply(allocRet, 10, Return.annualized)

allocRets <- 100*merge(na.omit(monthlyRets), na.omit(yearlyRets), na.omit(weeklyRets))

toPlot<-data.frame(allocRets)
names(toPlot) <- c('MONTHLY', 'ANNUAL', 'SMA')
toPlot$Y <- year(index(allocRets))
toPlot$Y <- factor(toPlot$Y, levels=unique(toPlot$Y))

ggplot(toPlot, aes(x=Y)) +
	theme_economist() +
	geom_violin(aes(y=MONTHLY), color='darkgreen', fill='green') +
	geom_violin(aes(y=SMA), color='darkred', fill='brown', alpha=0.5) +
	geom_point(aes(y=ANNUAL), color='red', size=2) +
	labs(y='return (%)', x='', color='', fill='', 
		title = sprintf("Rolling %.0f/%.0f %s/Short-term Bonds Allocation vs. SMA %d", 100*eqPct, 100*(1-eqPct), indexName, smaLb), 
		subtitle="monthly vs. annual rebalance; 10-year annualized returns")
		
ggsave(sprintf("%s/%s-bonds.rolling-return.%.0f.allocation.vs.SMA-%d.png", reportPath, indexName, 100*eqPct, smaLb), width=16, height=8, units="in")	

######################################

#plot actual monthly vs. annual rebalance cumulative returns

eqRets <- monthlyReturn(eqXts)
bndRets <- monthlyReturn(bndXts)

eqRets <- eqRets[-1,]
bndRets <- bndRets[-1,]

dateRange <- paste0(first(index(bndRets)), "/")

allRets <- merge(eqRets[dateRange], bndRets[dateRange])
allRets[,2] <- na.locf(allRets[,2], fromLast=T)
allRets <- na.omit(allRets)

names(allRets) <- c('EQ.RET', 'BND.RET')

allocRet.monthly <- merge(allRets[,'EQ.RET']*eqPct + allRets[,'BND.RET']*(1-eqPct),
							allRets[,'EQ.RET'], 
							allRets[,'BND.RET'])

names(allocRet.monthly) <- c('ALLOC.GROSS', 'EQ', 'BND')
##							
eqRets <- yearlyReturn(eqXts)
bndRets <- yearlyReturn(bndXts)

smaRets <- unlist(lapply(unique(year(index(btest[dateRange]))), function(x) as.numeric(Return.cumulative(btest[paste0(x[[1]])]))))

allRets <- merge(eqRets[dateRange], bndRets[dateRange], xts(smaRets, index(bndRets[dateRange])))
allRets[,2] <- na.locf(allRets[,2], fromLast=T)
allRets[,3] <- na.locf(allRets[,3], fromLast=T)
allRets <- na.omit(allRets)

names(allRets) <- c('EQ.RET', 'BND.RET', 'SMA.RET')

allocRet.annual <- merge(allRets[,'EQ.RET']*eqPct + allRets[,'BND.RET']*(1-eqPct), 
							ifelse(allRets[,'EQ.RET'] > 0, 0.85*allRets[,'EQ.RET'], allRets[,'EQ.RET'])*eqPct + ifelse(allRets[,'BND.RET'] > 0, 0.7*allRets[,'BND.RET'], allRets[,'BND.RET'])*(1-eqPct), 
							ifelse(allRets[,'EQ.RET'] > 0, 0.90*allRets[,'EQ.RET'], allRets[,'EQ.RET'])*eqPct + ifelse(allRets[,'BND.RET'] > 0, 0.7*allRets[,'BND.RET'], allRets[,'BND.RET'])*(1-eqPct), 
							allRets[,'EQ.RET'], 
							allRets[,'BND.RET'])

names(allocRet.annual) <- c('ALLOC.GROSS', 'ALLOC.NET-ST', 'ALLOC.NET-LT', 'EQ', 'BND')

allocRet.annual2 <- merge(  allRets[,'SMA.RET'],
							ifelse(allRets[,'SMA.RET'] > 0, 0.85*allRets[,'SMA.RET'], allRets[,'SMA.RET']),
							ifelse(allRets[,'EQ.RET'] > 0, 0.85*allRets[,'EQ.RET'], allRets[,'EQ.RET'])*eqPct + ifelse(allRets[,'BND.RET'] > 0, 0.7*allRets[,'BND.RET'], allRets[,'BND.RET'])*(1-eqPct), 
							allRets[,'EQ.RET'])

names(allocRet.annual2) <- c('SMA.GROSS', 'SMA.NET-ST', 'ALLOC.NET-ST', 'EQ')

allocRet.annual3 <- merge(  allRets[,'EQ.RET']*eqPct + allRets[,'BND.RET']*(1-eqPct), 
							ifelse(allRets[,'SMA.RET'] > 0, 0.85*allRets[,'SMA.RET'], allRets[,'SMA.RET']),
							allRets[,'EQ.RET'])

names(allocRet.annual3) <- c('ALLOC.GROSS', 'SMA.NET-ST', 'EQ')
allocRet.annual3 <- allocRet.annual3*100

##							

toPlot <- data.frame(allocRet.annual3)
toPlot$Y <- year(index(allocRet.annual3))
toPlot <- melt(toPlot, id='Y')
toPlot$Y <- factor(toPlot$Y, levels=unique(toPlot$Y))

ggplot(toPlot, aes(x=Y, y=value, fill=variable)) +
	theme_economist() +
	scale_fill_viridis(discrete = TRUE) +
	geom_bar(stat="identity", position=position_dodge()) +
	labs(y='return (%)', x='', color='', fill='', title=sprintf("%s/Short-term Bonds SMA %d vs. %.0f/%.0f Allocation", indexName, 100*eqPct, 100*(1-eqPct), smaLb)) +
	annotate("text", x=1, y=min(toPlot$value, na.rm=T), label = "@StockViz", hjust=0, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/%s-bonds.annual-return.%.0f.allocation.vs.SMA-%d.png", reportPath, indexName, 100*eqPct, smaLb), width=16, height=8, units="in")
	

Common.PlotCumReturns(allocRet.monthly, 
			sprintf("%.0f/%.0f %s/Short-term Bonds Allocation", 100*eqPct, 100*(1-eqPct), indexName), 
			"monthly rebalance", 
			sprintf("%s/%s-bonds.cumulative-return-actual.monthly rebalance.%.0f.allocation.png", reportPath, indexName, 100*eqPct))
			
Common.PlotCumReturns(allocRet.annual, 
			sprintf("%.0f/%.0f %s/Short-term Bonds Allocation", 100*eqPct, 100*(1-eqPct), indexName), 
			"annual rebalance", 
			sprintf("%s/%s-bonds.cumulative-return-actual.annual rebalance.%.0f.allocation.png", reportPath, indexName, 100*eqPct))
			
Common.PlotCumReturns(allocRet.annual2, 
			sprintf("%s/Short-term Bonds SMA %d vs. %.0f/%.0f Allocation", indexName, 100*eqPct, 100*(1-eqPct), smaLb), 
			"annual rebalance", 
			sprintf("%s/%s-bonds.cumulative-return-actual.annual rebalance.%.0f.allocation.SMA%d.png", reportPath, indexName, 100*eqPct, smaLb))


			