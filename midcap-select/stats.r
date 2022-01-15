library('RODBC')
library('quantmod')
library('lubridate')
library('tidyverse')
library('extrafont')
library('viridis')
library('ggthemes')
library('ggpubr')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "D:/StockViz/public/blog/midcap-select"
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

index1 <- "NIFTY MIDCAP 150"
index2 <- "NIFTY MIDCAP SELECT"

fromDt <- as.Date("2006-01-01")
toDt <- as.Date("2021-10-31")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s' order by time_stamp", index1, fromDt, toDt))
pXts1 <- xts(pDf[,2], pDf[,1])

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s' order by time_stamp", index2, fromDt, toDt))
pXts2 <- xts(pDf[,2], pDf[,1])

dailyRets <- merge(dailyReturn(pXts1), dailyReturn(pXts2))
weeklyRets <- merge(weeklyReturn(pXts1), weeklyReturn(pXts2))
monthlyRets <- merge(monthlyReturn(pXts1), monthlyReturn(pXts2))

dailyRets <- dailyRets[-1,]
weeklyRets <- weeklyRets[-1,]
monthlyRets <- monthlyRets[-1,]

names(dailyRets) <- c(index1, index2)
names(weeklyRets) <- c(index1, index2)
names(monthlyRets) <- c(index1, index2)

years <- unique(year(index(dailyRets)))

#i<-1
for(i in 1:length(years)){
	yrs <- toString(years[i])

	toPlot <- data.frame(dailyRets[yrs,])

	ggplot(toPlot, aes_string(gsub(' ', '.', index1), gsub(' ', '.', index2))) + 
		theme_economist() +  
		geom_point()+
		geom_smooth(method=lm) +
		stat_cor(method = "pearson") +
		labs(color='', fill='', title = "Index Correlation", subtitle=sprintf("daily returns %s", yrs)) +
		annotate("text", x=Inf, y=-Inf, label = "@StockViz", hjust='top', vjust='left', col="white", cex=6, fontface = "bold", alpha = 0.4)
	  
	ggsave(sprintf("%s/correlation.daily.%s.png", reportPath, yrs), width=12, height=6, units="in")	  
	
	
	toPlot <- data.frame(weeklyRets[yrs,])

	ggplot(toPlot, aes_string(gsub(' ', '.', index1), gsub(' ', '.', index2))) + 
		theme_economist() +  
		geom_point()+
		geom_smooth(method=lm) +
		stat_cor(method = "pearson") +
		labs(color='', fill='', title = "Index Correlation", subtitle=sprintf("weekly returns %s", yrs)) +
		annotate("text", x=Inf, y=-Inf, label = "@StockViz", hjust='top', vjust='left', col="white", cex=6, fontface = "bold", alpha = 0.4)
	  
	ggsave(sprintf("%s/correlation.weekly.%s.png", reportPath, yrs), width=12, height=6, units="in")	  
}

toPlot <- data.frame(monthlyRets)
ggplot(toPlot, aes_string(gsub(' ', '.', index1), gsub(' ', '.', index2))) + 
	theme_economist() +  
	geom_point()+
	geom_smooth(method=lm) +
	stat_cor(method = "pearson") +
	labs(color='', fill='', title = "Index Correlation", subtitle=sprintf("monthly returns [%s:%s]", fromDt, toDt)) +
	annotate("text", x=Inf, y=-Inf, label = "@StockViz", hjust='top', vjust='left', col="white", cex=6, fontface = "bold", alpha = 0.4)
  
ggsave(sprintf("%s/correlation.monthly.png", reportPath), width=12, height=6, units="in")	  
