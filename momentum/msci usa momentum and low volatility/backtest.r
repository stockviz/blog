library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('ggrepel')
library('grid')
library('gridExtra')
library('gtable')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/msci.R")
source("D:/StockViz/public/blog/common/misc.R")
reportPath <- "."

startDate<-as.Date('1994-06-30')
endDate<-as.Date('2019-03-31')

lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

baseIndex <- "USA MOMENTUM"
volIndex <- "USA MINIMUM VOLATILITY (USD)"

baseIndexId<-sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", baseIndex))[[1]]
volIndexId<-sqlQuery(lconUs2, sprintf("select index_code from MSCI_META where index_name='%s'", volIndex))[[1]]

baseXts<-Common.DownloadMsci(baseIndexId, 'G', startDate, endDate)
volXts<-Common.DownloadMsci(volIndexId, 'G', startDate, endDate)

baseXts<-Common.NormalizeMonthlyDates(baseXts)
volXts<-Common.NormalizeMonthlyDates(volXts)

#### cumulative monthly returns

monthlyReturnXts<-merge(monthlyReturn(baseXts), #1 
						monthlyReturn(volXts)) 	 #2

toPlot<-merge(monthlyReturnXts, monthlyReturnXts[,1]*0.5 + monthlyReturnXts[,2]*0.5, monthlyReturnXts[,1]*0.75 + monthlyReturnXts[,2]*0.25)					
names(toPlot)<-c(baseIndex, volIndex, "Portfolio 50/50", "Portfolio 75/25")

Common.PlotCumReturns(toPlot, sprintf("%s/%s", baseIndex, volIndex), "", sprintf("%s/%s-%s.cumulative.png", reportPath, baseIndex, volIndex), NULL)

#### annual returns

annualReturnXts<-merge(annualReturn(baseXts), #1 
						annualReturn(volXts)) 	 #2
annualReturnXts<-annualReturnXts[-1,]
annualReturnXts<-annualReturnXts[-nrow(annualReturnXts),]

toPlot<-merge(annualReturnXts, annualReturnXts[,1]*0.5 + annualReturnXts[,2]*0.5, annualReturnXts[,1]*0.75 + annualReturnXts[,2]*0.25)					
names(toPlot)<-c(baseIndex, volIndex, "Portfolio 50/50", "Portfolio 75/25")

sdDf<-data.frame(toPlot*100)
sdDf$T<-year(index(toPlot))
#sdDf$T<-factor(sdDf$T, levels=sdDf$T)
sdMelt<-melt(sdDf, id='T')

pdf(NULL)
ggplot(sdMelt, aes(x=T, y=value, fill=variable)) +
	theme_economist() +
	geom_bar(stat="identity", position=position_dodge()) +
	scale_x_continuous(labels=sdDf$T, breaks=sdDf$T) +
	geom_text_repel(aes(label= round(value, 2)), position = position_dodge(0.9)) +
	labs(x='', y='(%)', fill='', title=sprintf("Annual Returns", baseIndex, volIndex)) +
	annotate("text", x=max(sdDf$T), y=min(toPlot*100, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
ggsave(sprintf("%s/%s-%s.annual.png", reportPath, baseIndex, volIndex), width=16, height=8, units="in")


