library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('ggrepel')
library('reshape2')

#msciIds<-c(990100, 891800) #WORLD, EM
#msciType<-'G'
#startDate<-as.Date("1987-12-31")
#endDate<-as.Date("2019-02-28")

args <- commandArgs(TRUE)

arg.msciIds<-as.numeric(unlist(strsplit(args[1], ",")))
arg.msciType<-args[2]
arg.startDate<-as.Date(args[3])
arg.endDate<-as.Date(args[4])

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")
source("D:/StockViz/public/blog/common/msci.R")

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

retMonthly<-xts()
retAnnual<-xts()
indexNames<-c()
for(i in 1:length(arg.msciIds)){
	pXts<-Common.DownloadMsci(arg.msciIds[i], arg.msciType, arg.startDate, arg.endDate)
	pXts<-Common.NormalizeMonthlyDates(pXts)
	monthlyReturnXts<-diff(pXts)/stats::lag(pXts,1)
	monthlyReturnXts<-na.omit(monthlyReturnXts)
	
	retMonthly<-merge(retMonthly, monthlyReturnXts)
	retAnnual<-merge(retAnnual, annualReturn(pXts))
	
	indexName<-sqlQuery(lconUs2, sprintf("select INDEX_NAME from MSCI_META where INDEX_CODE=%d", arg.msciIds[i]))[[1]]
	indexNames<-c(indexNames, indexName)
}

names(retMonthly)<-indexNames
names(retAnnual)<-indexNames
retAnnual<-retAnnual[-1,]
retAnnual<-retAnnual[-nrow(retAnnual),]
retAnnual<-na.omit(retAnnual)

Common.PlotCumReturns(retMonthly, "MSCI Index Returns (Gross)", sprintf("%s/MSCI.%s.cumulative.%s.%s.png", reportPath, paste(indexNames, collapse="-"), arg.startDate, arg.endDate))

retAnnualDf<-data.frame(retAnnual*100)
retAnnualDf$Y<-year(index(retAnnual))
retAnnualDf$Y<-factor(retAnnualDf$Y, levels=retAnnualDf$Y)

toPlot<-melt(retAnnualDf, id='Y')

pdf(NULL)	
ggplot(toPlot, aes(x=Y, y=value, fill=variable)) +
	theme_economist() +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text_repel(aes(label=round(value,2)), position = position_dodge(0.9)) +
	labs(x = "", y="returns(%)", fill="", color="", title="MSCI Index Returns (Gross)") +
	annotate("text", x=0, y=max(retAnnual), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.5)
ggsave(sprintf("%s/MSCI.%s.annual.%s.%s.png", reportPath, paste(indexNames, collapse="-"), arg.startDate, arg.endDate), width=12, height=8, units="in")