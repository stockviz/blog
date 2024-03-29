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

#reportPath <- "."
reportPath <- "D:/Dropbox/StockViz/machine learning NIFTY"

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")
source("D:/StockViz/public/blog/common/misc.R")

executeKernal<-function(ds, validationSet, suffix=""){
	svecModel.fit<-svm(svecModel.formula, trainingSet, kernel=kernelSvm, degree=ds)
	svecModel.preds<-predict(svecModel.fit, validationSet[, ipNames])

	svecModel.actpred<-data.frame(validationSet[,'N_L1'], PREDICTED=svecModel.preds)

	plotStart<-min(index(validationSet))
	plotEnd<-max(index(validationSet))
		
	svecModel.actpred$COLOR<-ifelse((svecModel.actpred$N_L1 > 0 & svecModel.actpred$PREDICTED > 0) | (svecModel.actpred$N_L1 < 0 & svecModel.actpred$PREDICTED < 0), 'grey', 'black')
	ggplot(svecModel.actpred, aes(x=N_L1, y=PREDICTED)) + 
		theme_economist() +
		geom_point(color=svecModel.actpred$COLOR) +
		labs(x = "actual", y="predicted", fill="", color="", title=sprintf("SVM %s/NIFTY (%d)", usdIndex, ds), subtitle=sprintf("Actual vs. Predicted [%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=max(svecModel.actpred$N_L1), y=min(svecModel.actpred$PREDICTED), label = "@StockViz", hjust=0, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.6)
	ggsave(sprintf("%s/%s.NIFTY.%d.actual.vs.predicted.%s.png", reportPath, usdIndex, ds, suffix), width=12, height=6, units="in")
	
	svecModel.rmse<-rmse(validationSet[,'N_L1'], svecModel.preds)
	svecModel.bias<-bias(validationSet[,'N_L1'], svecModel.preds)

	svecModel.ape<-data.frame(validationSet[,'N_L1'], ape(validationSet[,'N_L1'], svecModel.preds))
	names(svecModel.ape)<-c('ACTUAL', 'APE')

	ggplot(svecModel.ape, aes(x=ACTUAL,y=APE)) + 
		theme_economist() +
		geom_point() +
		labs(x = "actual", y="ape", fill="", color="", title=sprintf("SVM %s/NIFTY", usdIndex), subtitle=sprintf("Absolute Percent Error [%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=max(svecModel.ape$ACTUAL), y=min(svecModel.ape$APE), label = "@StockViz", hjust=+1, vjust=1, col="white", cex=6, fontface = "bold", alpha = 0.6)
	ggsave(sprintf("%s/%s.NIFTY.%d.absolute.pct.error.%s.png", reportPath, usdIndex, ds, suffix), width=12, height=6, units="in")
	
	retCum<-data.frame(svecModel.actpred$N_L1)
	retCum<-cbind(retCum, ifelse(svecModel.actpred$PREDICTED > 0, svecModel.actpred$N_L1, 0))
	retCum<-cbind(retCum, ifelse(svecModel.actpred$PREDICTED > 0, svecModel.actpred$N_L1, -svecModel.actpred$N_L1))
	retCumXts<-xts(retCum, index(validationSet))
	names(retCumXts)<-c('BH', 'L', 'LS')
	
	Common.PlotCumReturns(retCumXts, sprintf("NIFTY Trend + Macro"), sprintf("%s/%s.NIFTY.%d.cumulative.%s.png", reportPath, usdIndex, ds, suffix))
	
	return(list(100*Return.cumulative(retCumXts$BH), c(round(100*Return.cumulative(retCumXts$L), 2), round(100*Return.cumulative(retCumXts$LS), 2))))
}

runKernalLoop<-function(degrees, validationSet, suffix){
	buyNhold<-0.0
	kernReturns<-data.frame(DEGREE=0, LONG_ONLY=0.0, LONG_SHORT=0.0)
	for(ds in degrees){
		rets<-executeKernal(ds, validationSet, suffix)
		buyNhold<-rets[1]
		kernReturns<-rbind(kernReturns, c(ds, rets[[2]]))
	}
	kernReturns<-kernReturns[-1,]

	tt2<-textGrob(sprintf("Buy & Hold: %.2f%%", buyNhold), gp=gpar(fontsize=10, fontfamily='Segoe UI')) 
	tt1<-arrangeGrob(grobs=list(tableGrob(kernReturns, rows=NULL, theme=tableTheme), tt2), ncol=1, top=textGrob(sprintf("SVM %s/NIFTY (%s)", usdIndex, suffix), gp=gpar(fontsize=14, fontface ='bold', fontfamily='Segoe UI')), bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

	ggsave(sprintf("%s/%s.NIFTY.cumulative.%s.png", reportPath, usdIndex, suffix), tt1, width=3.5, height=nrow(kernReturns)*0.8, units='in')
	
	return(list(buyNhold, kernReturns))
}

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("2000-01-01")
endDate<-as.Date("2018-10-31")
kernelSvm<-'polynomial'
degreeSvm<-4:4

usdIndex<-'DTWEXM'
usdDf<-sqlQuery(lconUs, sprintf("select val, time_stamp from FRED_OBSERVATION 
							where time_stamp >= '%s' and time_stamp <= '%s' 
							and series_id=(select id from FRED_SERIES where series_id='%s')", startDate, endDate, usdIndex))
usdXts<-xts(usdDf$val, as.Date(usdDf$time_stamp))

niftyDf<-sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index 
							where index_name='nifty 50' 
							and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
niftyXts<-xts(niftyDf$px_close, as.Date(niftyDf$time_stamp))							

#########

allXts<-merge(usdXts, niftyXts)
allXts[,1]<-na.locf(allXts[,1])
allXts<-na.omit(allXts)

weeklyXts<-merge(weeklyReturn(allXts[,1]), weeklyReturn(allXts[,2]))
names(weeklyXts)<-c('USD', 'NIFTY')

rXts<-merge(weeklyXts[,1], rollapply(weeklyXts[,1], 2, Return.cumulative), rollapply(weeklyXts[,1], 5, Return.cumulative), rollapply(weeklyXts[,1], 10, Return.cumulative),	stats::lag(weeklyXts[,2], -1))
ipNames<-c('USD_1', 'USD_2', 'USD_5', 'USD_10')
names(rXts)<-c(ipNames, 'N_L1')

rXts<-na.omit(rXts)
tSetBoundary1<-as.integer(nrow(rXts)*0.6)
tSetBoundary2<-tSetBoundary1 + as.integer(nrow(rXts)*0.2)

trainingSet<-rXts[1:tSetBoundary1,] #The sample of data used to fit the model.
validationSet<-rXts[(1+tSetBoundary1):tSetBoundary2,] #use for tuning
testSet<-rXts[(1+tSetBoundary2):nrow(rXts),] #use for evaluating the final model

svecModel.formula<-as.formula(paste("N_L1 ~", paste(ipNames, collapse=" + ")))
pdf(NULL)

#run the loop and use the best kernel to train the trainingSet and run it on the testSet
allKernalRets<-runKernalLoop(degreeSvm, validationSet, 'validation')
bh<-allKernalRets[[1]]
krets<-allKernalRets[[2]]
krets$LONG_SHORT<-as.numeric(krets$LONG_SHORT)
successDegrees<-krets[krets$LONG_SHORT >= krets$LONG_SHORT[1],1]

################

rets<-runKernalLoop(successDegrees, testSet, 'test')