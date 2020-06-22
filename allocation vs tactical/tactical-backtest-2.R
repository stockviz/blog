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

smaLb <- 50 #days

windowStep <- 4
rollingWindow <- 52*10 #weeks

indexName <- "NIFTY 50 TR"
startDate <- as.Date("2003-12-31")
endDate <- as.Date("2020-05-31")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
eqXts <- xts(pDf[,2], pDf[,1])

eqXts <- eqXts[-1,]
names(eqXts) <- c("EQUITY")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, tri, YTM from index_ccil_tenor where index_name='0_5' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
bndXts <- xts(pDf[,-1], pDf[,1])

bndXts <- bndXts[-1,]
names(bndXts) <- c("BOND", "YTM")

######################################

eqBndXts <- merge(eqXts, bndXts)
eqBndXts[,2] <- na.locf(eqBndXts[,2])
eqBndXts[,3] <- na.locf(eqBndXts[,3])
eqBndXts <- na.omit(eqBndXts)

allXts <- merge(eqXts, SMA(eqXts, smaLb), weeklyReturn(eqXts), weeklyReturn(eqBndXts[,2]))
allXts <- na.omit(allXts)

allXts <- merge(allXts, stats::lag(allXts[,3], -1), stats::lag(allXts[,4], -1))

btest <- merge(ifelse(allXts[,1] > allXts[,2], allXts[,5], allXts[,6]), ifelse(allXts[,1] > allXts[,2], allXts[,5], 0), ifelse(allXts[,1] > allXts[,2], 0, allXts[,6]))

sXts <- merge(ifelse(allXts[,1] > allXts[,2], allXts[,5], allXts[,6]), eqBndXts[,3])
sXts <- na.omit(sXts)

sharpeXts <- rollapply(sXts, rollingWindow, function(X) SharpeRatio.annualized(X[,1], ((as.numeric(X[1,2])/100+1)^(1/52))-1), by=windowStep, by.column = F) #as.numeric(X[1,2])
sharpeXts <- na.omit(sharpeXts)

bXts <- merge(btest, allXts[,5], allXts[,6])
names(bXts) <- c('TACTICAL', 'EQUITY.CONTRIB', 'BOND.CONTRIB', 'EQUITY', 'BOND')

rollingReturns <- rollapply(bXts, rollingWindow, Return.annualized, by=windowStep, by.column = T)
rollingReturns <- 100*na.omit(rollingReturns)

toPlot <- data.frame(rollingReturns)
toPlot$T <- index(rollingReturns)
toPlot <- melt(toPlot, id='T')
toPlot$sz <- (1+length(names(bXts)) - match(toPlot$variable, names(bXts)))/3

p1<-ggplot(toPlot, aes(x=T, y=value, color=variable)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line(aes(size=sz)) +
	scale_size_identity() +
	scale_color_viridis(discrete = TRUE)+
	scale_x_date(date_breaks = "3 months", date_labels = "%Y-%b", expand=c(0, 0)) + 
	labs(y='return (%)', x='', color='', fill='', title="")
	
toPlot2 <- data.frame(sharpeXts)
names(toPlot2) <- c('Sharpe')
toPlot2$T <- index(sharpeXts)

p2<-ggplot(toPlot2, aes(x=T, y=Sharpe)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_point() +
	scale_x_date(date_breaks = "3 months", date_labels = "%Y-%b", expand=c(0, 0)) + 
	labs(y='Sharpe', x='', color='', fill='', title="")
	
figure <- ggarrange(p1, p2, labels=c("Annualized Returns", "Annualized Sharpe"), ncol=1, nrow=2)	
	annotate_figure(figure, top = text_grob(sprintf("%d-Day SMA %s/Short-term Bonds Tactical", smaLb, indexName), face = "bold", size = 14, family='Segoe UI'), 
					right = text_grob(sprintf("Rolling %d-week window", rollingWindow), size = 12, family='Segoe UI', rot=90), 
					bottom = text_grob("@StockViz", face="bold", size=12, family="Segoe UI", color='grey'))
	
ggsave(sprintf("%s/%d-day tactical %s.rolling.png", reportPath, smaLb, indexName), width=16, height=16, units="in")	
