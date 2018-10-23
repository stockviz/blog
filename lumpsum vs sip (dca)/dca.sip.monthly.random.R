library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')

options("scipen"=100)
options(stringsAsFactors = FALSE)
reportPath <- "."
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

numIterations<-10000
#indexName<-'NIFTY 50'

analyzeIndex<-function(indexName){
	dataDf<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_CLOSE from BHAV_INDEX where index_name='%s'", indexName))
	dataXts<-xts(dataDf$PX_CLOSE, as.Date(dataDf$TIME_STAMP))
	
	#compute monthly price series and remove the first and last rows
	monthlyXts<-to.monthly(dataXts)
	monthlyXts<-monthlyXts[-1]
	monthlyXts<-monthlyXts[-nrow(monthlyXts)]
	
	#calculate SIP on real data
	numInstallments<-nrow(monthlyXts)
	realSipReturn<-as.numeric(sum(1/monthlyXts[,4])*last(monthlyXts[,4])/numInstallments-1)
	annualizedRealSipReturn<-100*((1+realSipReturn)^(12/numInstallments)-1) #annualized real sip/dca returns
	
	#compute monthly returns and remove the first and last rows
	monthlyRetXts<-monthlyReturn(dataXts)
	monthlyRetXts<-monthlyRetXts[-1]
	monthlyRetXts<-monthlyRetXts[-nrow(monthlyRetXts)]
	
	numInstallments<-nrow(monthlyRetXts)
	realCumReturn<-100*Return.annualized(monthlyRetXts) #annualized real lumpsum returns
	
	monthlyRet<-as.numeric(monthlyRetXts)
	simCumReturns<-c()
	
	#simulate
	for(i in 1:numIterations){
		simRet<-sample(monthlyRet)
		pxSeries<-cumprod(1+simRet)
		simCumReturns<-c(simCumReturns, sum(1/pxSeries)*last(pxSeries)/numInstallments-1)
	}
	
	firstDate<-as.Date(first(index(monthlyXts[,1])))
	lastDate<-as.Date(last(index(monthlyXts[,1])))
	
	toPlot<-data.frame(RET=100*((1+simCumReturns)^(12/numInstallments)-1))
	
	pdf(NULL)
	ggplot(toPlot, aes(RET)) +
		theme_economist() +
		geom_histogram(aes(y=..density..), binwidth=0.25, colour="black", fill="lightblue") +
		geom_density(alpha=.2, fill="#FF6666") +
		geom_vline(aes(xintercept=annualizedRealSipReturn), colour="blue", size=1, linetype='dashed', alpha=0.5) +
		geom_label(aes(x=annualizedRealSipReturn, label=sprintf("actual sip: %.2f%%", annualizedRealSipReturn), y=0.1), colour="blue") +
		geom_vline(aes(xintercept=realCumReturn), colour="red", size=1, linetype='dashed', alpha=0.5) +
		geom_label(aes(x=realCumReturn, label=sprintf("lumpsum: %.2f%%", realCumReturn), y=0.15), colour="red") +
		labs(y='density', x='annualized sip returns', color='', title=indexName, subtitle=sprintf("randomized monthly sip/dca[%s:%s]", firstDate, lastDate)) +
		annotate("text", x=max(toPlot$RET), y=0, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)
		
	ggsave(sprintf("%s/%s.monthly-sip-random.png", reportPath, indexName), dpi=600, width=12, height=6, units="in")
}

analyzeIndex('NIFTY 50')
analyzeIndex('NIFTY MIDCAP 100')
analyzeIndex('NIFTY SMLCAP 100')