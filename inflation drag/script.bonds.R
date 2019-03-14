library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('lubridate')
library('reshape2')

options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
reportPath <- "."
lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

getSymbols("FPCPITOTLZGIND", src="FRED")
index(FPCPITOTLZGIND)<-as.Date(sprintf("%s-12-31", year(FPCPITOTLZGIND)))

createChart<-function(indexName, startDate){
	data<-sqlQuery(lcon, sprintf("select TIME_STAMP, TRI from INDEX_CCIL_TENOR where index_name='%s' and time_stamp >= '%s'", indexName, startDate))
	dataXts<-xts(data[,-1], as.Date(data[,1]))
	yXts<-yearlyReturn(dataXts)
	index(yXts)<-as.Date(sprintf("%s-12-31", year(yXts)))
	aXts<-na.omit(merge(FPCPITOTLZGIND, 100*yXts))
	names(aXts)<-c('INFLATION', 'GROSS')
	aXts$REAL<-aXts$GROSS-aXts$INFLATION

	rets<-data.frame(YEAR=year(index(aXts)), INFLATION=aXts$INFLATION, GROSS=aXts$GROSS, REAL=aXts$REAL)

	pdf(NULL)
	ggplot(data=melt(rets, id='YEAR'), aes(x=YEAR, y=value, fill=variable)) +
	  theme_economist() +
	  geom_bar(stat="identity", position=position_dodge()) + 
	  scale_x_continuous(breaks=rets$YEAR) +
	  geom_text(aes(label=round(value, 0)), vjust=1.6, color="black", position = position_dodge(0.9), size=2.5) +
	  ylab("(%)") + 
	  xlab("Year") +
	  labs(fill="", title=sprintf("%s Gross and Real Returns [%d - %d] @StockViz", indexName, year(index(first(aXts))), year(index(last(aXts))))) +
	  annotate("text", x=(year(index(last(aXts)))-1), y=max(aXts$GROSS), label=sprintf("Annualized: %.2f%%/%.2f%%", 100.0*Return.annualized(aXts$GROSS/100.0), 100.0*Return.annualized(aXts$REAL/100.0)), family='Segoe UI')
	  
	ggsave(sprintf("%s/%s.gross.real.returns.%d-%d.png", reportPath, indexName, year(index(first(aXts))), year(index(last(aXts)))), dpi=600, width=12, height=6, units="in")  
}

createChart('0_5',as.Date("2004-01-01"))
createChart('20_30',as.Date("2004-01-01"))
