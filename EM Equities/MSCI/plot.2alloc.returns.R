library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('ggrepel')
library('reshape2')
library('dplyr')

arg.msciIds<-c(652601,652557) #WORLD, EM
arg.msciType<-'G'
arg.startDate<-as.Date("1994-05-31")
arg.endDate<-as.Date("2019-03-29")

#args <- commandArgs(TRUE)
#arg.msciIds<-as.numeric(unlist(strsplit(args[1], ",")))
#arg.msciType<-args[2]
#arg.startDate<-as.Date(args[3])
#arg.endDate<-as.Date(args[4])

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")
source("D:/StockViz/public/blog/common/msci.R")

lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 13 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

retMonthly<-xts()
indexNames<-c()
for(i in 1:length(arg.msciIds)){
	pXts<-Common.DownloadMsci(arg.msciIds[i], arg.msciType, arg.startDate, arg.endDate)
	pXts<-Common.NormalizeMonthlyDates(pXts)
	monthlyReturnXts<-diff(pXts)/stats::lag(pXts,1)
	monthlyReturnXts<-na.omit(monthlyReturnXts)
	
	retMonthly<-merge(retMonthly, monthlyReturnXts)
	
	indexName<-sqlQuery(lconUs2, sprintf("select INDEX_NAME from MSCI_META where INDEX_CODE=%d", arg.msciIds[i]))[[1]]
	indexNames<-c(indexNames, indexName)
}

names(retMonthly)<-indexNames

wts<-list(c(0,1), c(.25, .75), c(.5,.5), c(.75,.25), c(1,0))
scenNames<-sapply(wts, function(X) sprintf("%d/%d", 100*X[1], 100*X[2]))

scenarios<-NULL
for(i in 1:length(wts)){
	wt<-wts[[i]]
	scen<-as.xts(rowSums(t(t(retMonthly)*wt)))
	scenarios<-merge.xts(scenarios, scen)
}
names(scenarios)<-scenNames

Common.PlotCumReturns(scenarios, sprintf("MSCI %s Index Returns (Gross)", paste(indexNames, collapse="/")), sprintf("%s/MSCI.%s.cumulative.2alloc.%s.%s.png", reportPath, paste(indexNames, collapse="-"), arg.startDate, arg.endDate))

listOfAnnualReturns<-sapply(scenarios, function(X) data.frame(data.frame(M=as.numeric(X), T=index(scenarios)) %>% group_by(Y=year(T)) %>% summarize(A = 100*(prod(1+ M)-1))), simplify=F)
retAnnual<-Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Y"), listOfAnnualReturns)

retAnnual<-retAnnual[-1,] #ignore first year
retAnnual<-retAnnual[-nrow(retAnnual),] #ignore last year

retAnnual$Y<-factor(retAnnual$Y, levels=retAnnual$Y)
names(retAnnual)<-c('Y', scenNames)

toPlot<-melt(retAnnual, id='Y')

pdf(NULL)	
ggplot(toPlot, aes(x=Y, y=value, fill=variable)) +
	theme_economist() +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text_repel(aes(label=round(value,2)), position = position_dodge(0.9), alpha=0.8) +
	labs(x = "", y="returns(%)", fill="", color="", title=sprintf("MSCI %s Index Returns (Gross)", paste(indexNames, collapse="/"))) +
	annotate("text", x=0, y=max(retAnnual[, -1]), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.5)
ggsave(sprintf("%s/MSCI.%s.annual.2alloc.%s.%s.png", reportPath, paste(indexNames, collapse="-"), arg.startDate, arg.endDate), width=16, height=8, units="in")