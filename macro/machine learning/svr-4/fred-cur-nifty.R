library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
#library("fUnitRoots")
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')
#library('ggrepel')
#library('dplyr')
library('e1071')
library('Metrics')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/public/blog/macro/machine learning/svr-1"

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("2005-01-01")
endDate<-as.Date("2018-10-31")

index1<-"DTWEXB"
degree1<-8

index2<-"DTWEXM"
degree2<-4

kernelSvm<-'polynomial'

usdDf1<-sqlQuery(lconUs, sprintf("select val, time_stamp from FRED_OBSERVATION 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and series_id=(select id from FRED_SERIES where series_id='%s')", startDate, endDate, index1))
usdXts1<-xts(usdDf1$val, as.Date(usdDf1$time_stamp))

usdDf2<-sqlQuery(lconUs, sprintf("select val, time_stamp from FRED_OBSERVATION 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and series_id=(select id from FRED_SERIES where series_id='%s')", startDate, endDate, index2))
usdXts2<-xts(usdDf2$val, as.Date(usdDf2$time_stamp))

niftyDf<-sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index 
							where index_name='nifty 50' 
							and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
niftyXts<-xts(niftyDf$px_close, as.Date(niftyDf$time_stamp))							

#########

allXts<-merge(usdXts1, usdXts2, niftyXts)
allXts[,1]<-na.locf(allXts[,1])
allXts[,2]<-na.locf(allXts[,2])
allXts<-na.omit(allXts)

weeklyXts<-merge(weeklyReturn(allXts[,1]), weeklyReturn(allXts[,2]), weeklyReturn(allXts[,3]))
names(weeklyXts)<-c('USD1', 'USD2', 'NIFTY')

rXts<-merge(weeklyXts[,1], rollapply(weeklyXts[,1], 2, Return.cumulative), rollapply(weeklyXts[,1], 5, Return.cumulative), rollapply(weeklyXts[,1], 10, Return.cumulative),	
	weeklyXts[,2], rollapply(weeklyXts[,2], 2, Return.cumulative), rollapply(weeklyXts[,2], 5, Return.cumulative), rollapply(weeklyXts[,2], 10, Return.cumulative),
	stats::lag(weeklyXts[,3], -1))
	
ipNames1<-c('USD1_1', 'USD1_2', 'USD1_5', 'USD1_10')
ipNames2<-c('USD2_1', 'USD2_2', 'USD2_5', 'USD2_10')
names(rXts)<-c(ipNames1, ipNames2, 'N_L1')

rXts<-na.omit(rXts)
tSetBoundary1<-as.integer(nrow(rXts)*0.7)

trainingSet<-rXts[1:tSetBoundary1,] #The sample of data used to fit the model.
testSet<-rXts[(1+tSetBoundary1):nrow(rXts),] #use for evaluating the final model

svecModel1.formula<-as.formula(paste("N_L1 ~", paste(ipNames1, collapse=" + ")))
svecModel2.formula<-as.formula(paste("N_L1 ~", paste(ipNames2, collapse=" + ")))

svecModel1.fit<-svm(svecModel1.formula, trainingSet, kernel=kernelSvm, degree=degree1)
svecModel1.preds<-predict(svecModel1.fit, testSet[, ipNames1])

svecModel2.fit<-svm(svecModel2.formula, trainingSet, kernel=kernelSvm, degree=degree2)
svecModel2.preds<-predict(svecModel2.fit, testSet[, ipNames2])

pdf(NULL)

svecModel1.actpred<-data.frame(testSet[,'N_L1'], PREDICTED=svecModel1.preds)
svecModel2.actpred<-data.frame(testSet[,'N_L1'], PREDICTED=svecModel2.preds)

retCum<-data.frame(svecModel1.actpred$N_L1)
retCum<-cbind(retCum, ifelse(svecModel1.actpred$PREDICTED > 0, svecModel1.actpred$N_L1, 0))
retCum<-cbind(retCum, ifelse(svecModel1.actpred$PREDICTED > 0, svecModel1.actpred$N_L1, -svecModel1.actpred$N_L1))

retCum<-cbind(retCum, ifelse(svecModel2.actpred$PREDICTED > 0, svecModel2.actpred$N_L1, 0))
retCum<-cbind(retCum, ifelse(svecModel2.actpred$PREDICTED > 0, svecModel2.actpred$N_L1, -svecModel2.actpred$N_L1))

retCum<-cbind(retCum, ifelse(svecModel1.actpred$PREDICTED > 0 & svecModel2.actpred$PREDICTED > 0, svecModel2.actpred$N_L1, 0))
retCum<-cbind(retCum, ifelse(svecModel1.actpred$PREDICTED > 0 & svecModel2.actpred$PREDICTED > 0, svecModel2.actpred$N_L1, ifelse(svecModel1.actpred$PREDICTED < 0 & svecModel2.actpred$PREDICTED < 0, -svecModel2.actpred$N_L1, 0)))

retCumXts<-xts(retCum, index(testSet))
names(retCumXts)<-c('BH', 'L1', 'LS1', 'L2', 'LS2', 'L', 'LS')

Common.PlotCumReturns(retCumXts, sprintf("SVM %s + %s /NIFTY", index1, index2), sprintf("%s/%s+%s.NIFTY.cumulative.png", reportPath, index1, index2))

for(j in 1:ncol(retCumXts)){
	tdn<-table.Drawdowns(retCumXts[,j], 10)
	tdn<-tdn[tdn$Depth < -0.05,]
	tdn$Depth<-round(100*tdn$Depth, 2)
	
	tt1<-arrangeGrob(tableGrob(tdn, rows=NULL, theme=tableTheme), ncol=1, top=textGrob(sprintf("%s + %s /NIFTY (%s) Drawdowns", index1, index2, names(retCumXts)[j]), gp=gpar(fontsize=12, fontface ='bold', fontfamily='Segoe UI')), bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

	ggsave(sprintf("%s/%s+%s.NIFTY.%s.drawdowns.png", reportPath, index1, index2, names(retCumXts)[j]), tt1, width=7, height=nrow(tdn)*0.5, units='in')
}