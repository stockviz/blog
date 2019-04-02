library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('dplyr')

source("d:/stockviz/r/config.r")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 13 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDt<-as.Date(sqlQuery(lcon, "select min(TIME_STAMP) from INDEX_CONST_HISTORY")[[1]])
endDt<-as.Date(sqlQuery(lcon, "select max(TIME_STAMP) from INDEX_CONST_HISTORY")[[1]])

marketDates<-sqlQuery(lcon, sprintf("select distinct(time_stamp) from bhav_index where time_stamp >= '%s' and time_stamp <= '%s'", startDt, endDt))
marketDates[,1]<-as.Date(marketDates[,1])

result<-data.frame(T="", P = 0, N = 0)
for(i in 1:nrow(marketDates)){
	dt<-marketDates[i,1]
	compDt<-sqlQuery(lcon, sprintf("select min(time_stamp) from INDEX_CONST_HISTORY where time_stamp > '%s'", dt))[[1]]
	symbols<-sqlQuery(lcon, sprintf("select distinct(symbol) from INDEX_CONST_HISTORY where time_stamp = '%s'", compDt))[,1]
	
	countTotal<-sqlQuery(lcon, sprintf("select count(*) from RETURN_SERIES_ALL where time_stamp = '%s' and symbol in ('%s')", dt, paste(symbols, collapse="','")))[[1]]
	countPositive<-sqlQuery(lcon, sprintf("select count(*) from RETURN_SERIES_ALL where time_stamp = '%s' and symbol in ('%s') and daily_return > 0", dt, paste(symbols, collapse="','")))[[1]]
		
	result<-rbind(result, c(toString(dt), countPositive, countTotal))
}
result<-result[-1,]
result[,1]<-as.Date(result[,1])
result[,2]<-as.numeric(result[,2])
result[,3]<-as.numeric(result[,3])

result<-result[result[,3] != 0,]
result$YM<-format(result[,1], "%Y-%m")

result2<-data.frame(result %>% group_by(YM) %>% mutate(C = n()))
result2<-result2[result2$C > 10,]

result2$YM<-factor(result2$YM, levels=unique(result2$YM))
result2$YQ<-quarter(result2$T, with_year = T)
result2$YQ<-factor(result2$YQ, levels=unique(result2$YQ))

result2$Q<-quarter(result2$T, with_year = F)

pdf(NULL)
ggplot(result2, aes(x=YM, y=P, color=Q)) + 
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_boxplot() +
	guides(color=F, fill=F) +
	labs(x='', y='N', color='', title="Number of NIFTY 100 stocks with positive returns", subtitle=sprintf("[%s:%s]", result[1,1], result[nrow(result),1])) +
	annotate("text", x=length(unique(result2$YM)), y=0, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
ggsave(sprintf("%s/NIFTY100.gainers.png",reportPath), width=16, height=8, units="in")	
	