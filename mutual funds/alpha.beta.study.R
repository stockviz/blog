library('RODBC')
library('ggplot2')
library('extrafont')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')
library('dplyr')

source("d:/stockviz/r/config.r")

options(stringsAsFactors = FALSE)
options("scipen"=100)

reportPath <- "."
mytheme <- ttheme_default(
		core = list(fg_params=list(fontfamily='Segoe UI', hjust=1, x=1)),
		colhead = list(fg_params=list(fontfamily='Segoe UI')),
		rowhead = list(fg_params=list(fontfamily='Segoe UI')))

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
		
launchCutoffDate<-Sys.Date() - 10*365

lookBack<-200 #in weeks

rfRates<-sqlQuery(lcon, "select time_stamp, ytm from INDEX_CCIL_TENOR where index_name='0_5' and time_stamp >= '2005-01-01'")
rfXts<-xts(rfRates[,-1], as.Date(rfRates[,1]))

benchMarkIndexName<-'NIFTY MIDCAP 100'
bench<-sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s' and time_stamp >= '2005-01-01'", benchMarkIndexName))
benchXts<-xts(bench[,-1], as.Date(bench[,1]))

createChart<-function(schemeCode, schemeName, dateRange=NA){
	print(schemeName)
	
	nav<-sqlQuery(lcon, sprintf("select as_of, nav from MF_NAV_HISTORY where scheme_code=%d", schemeCode))
	navXts<-xts(nav[,-1], as.Date(nav[,1]))

	allXts<-merge(benchXts, navXts)
	allXts<-na.trim(allXts, sides='left')
	allXts<-na.locf(allXts)
	
	allXts<-merge(allXts, rfXts)
	allXts[,3]<-na.locf(allXts[,3])
	
	allXts<-na.omit(allXts)

	allXts<-merge(allXts, weeklyReturn(allXts[,1]), weeklyReturn(allXts[,2]))
	allXts<-na.omit(allXts)
	
	cumRets1<-rollapply(allXts, lookBack, function(X) Return.cumulative(X[,4]), by.column = F)
	cumRets2<-rollapply(allXts, lookBack, function(X) Return.cumulative(X[,5]), by.column = F)
	betas<-rollapply(allXts, lookBack, function(X) CAPM.beta(X[,5], X[,4], as.numeric(first(X[,3]))/5200), by.column = F)
	alphas<-rollapply(allXts, lookBack, function(X) CAPM.alpha(X[,5], X[,4], as.numeric(first(X[,3]))/5200), by.column = F)
	ir<-rollapply(allXts, lookBack, function(X) InformationRatio(X[,5], X[,4]), by.column = F)

	toPlot<-na.omit(merge(100.0*cumRets1, 100.0*cumRets2, betas, alphas, ir))
	if(!is.na(dateRange)){
		print(dateRange)
		toPlot<-toPlot[dateRange,]
	}

	metricsDf<-data.frame(toPlot)
	metricsDf$time_stamp<-as.Date(row.names(metricsDf))
	names(metricsDf)<-c('index', 'mf', 'beta', 'alpha', 'ir', 'time_stamp')

	firstDate<-first(metricsDf$time_stamp)
	lastDate<-last(metricsDf$time_stamp)
	xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)

	plot1 <- metricsDf %>%
	  select(time_stamp, index, mf) %>%
	  melt(id='time_stamp') %>%
	  ggplot(aes(x = time_stamp, y=value, color=variable)) +
	  geom_line() +
	  ylab(sprintf("%d-week rolling returns", lookBack)) +
	  theme_economist() +
	  theme(axis.title.x = element_blank(),
			axis.text.x=element_blank(),
			axis.ticks.x=element_blank()) +
	  labs(color='', title=schemeName, subtitle=sprintf("%d-weeks rolling [%s:%s]", lookBack, firstDate, lastDate))

	plot2 <- metricsDf %>%
	  select(time_stamp, alpha) %>%
	  ggplot() +
	  geom_line(aes(x = time_stamp, y = alpha), size = 0.5, alpha = 0.75) +
	  ylab("alpha") +
	  theme_economist() +
	  theme(axis.title.x = element_blank(),
			axis.text.x=element_blank(),
			axis.ticks.x=element_blank())

	plot3 <- metricsDf %>%
	  select(time_stamp, beta) %>%
	  ggplot() +
	  geom_line(aes(x = time_stamp, y = beta), size = 0.5, alpha = 0.75) +
	  ylab("beta") +
	  theme_economist() +
	  scale_x_date(breaks = xAxisTicks) +
	  theme(axis.title.x = element_blank(),
			axis.text.x=element_blank(),
			axis.ticks.x=element_blank())

	plot4 <- metricsDf %>%
	  select(time_stamp, ir) %>%
	  ggplot() +
	  geom_line(aes(x = time_stamp, y = ir), size = 0.5, alpha = 0.75) +
	  ylab("ir") +
	  theme_economist() +
	  scale_x_date(breaks = xAxisTicks) +
	  theme(axis.title.x = element_blank()) +
	  annotate("text", x=lastDate, y=min(metricsDf$ir), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	  
	pdf(NULL)
	ggpg<-rbind(ggplotGrob(plot1), ggplotGrob(plot2), ggplotGrob(plot3), ggplotGrob(plot4), size = "first")
	#grid.newpage()
	#grid.draw(ggpg)
	
	ggsave(sprintf("%s/%s.%d.ABIR.%s.%s.png", reportPath, schemeName, lookBack, firstDate, lastDate), plot=ggpg, dpi=600, width=12, height=12, units="in")	
	return(metricsDf)
}

mutualFunds<-sqlQuery(lcon, "select N2.SCHEME_CODE, N1.SCHEME_NAME, min(N2.as_of) ST from MF_NAV_HISTORY N1, MF_NAV_HISTORY N2 
								where N1.AS_OF='2018-10-04'
								and N1.SCHEME_NAME like '%mid%cap%'
								and N1.SCHEME_NAME like '%growth%'
								and N1.SCHEME_NAME not like '%dividend%'
								and N1.SCHEME_NAME not like '%direct%'
								and N1.SCHEME_NAME not like '%institutional%'
								and N1.SCHEME_NAME not like '%large%'
								and N1.SCHEME_NAME not like '%small%'
								and N1.SCHEME_NAME not like '%tax%'
								and N1.SCHEME_CODE = n2.SCHEME_CODE
								group by N2.SCHEME_CODE, N1.SCHEME_NAME
								order by ST")

mutualFunds$ST<-as.Date(mutualFunds$ST)
mutualFunds<-mutualFunds[mutualFunds$ST <= launchCutoffDate,]

#mutualFunds<-mutualFunds[1:2,]

metrixDf<-NULL
for(i in 1:length(mutualFunds[,1])){
	mdf<-createChart(mutualFunds$SCHEME_CODE[i], mutualFunds$SCHEME_NAME[i])
	mdf$SCHEME_CODE <- mutualFunds$SCHEME_CODE[i]
	metrixDf<-rbind(metrixDf, mdf)
}

write.csv(metrixDf, sprintf('%s/mf.%d.abir.csv', reportPath, lookBack), row.names = F)
