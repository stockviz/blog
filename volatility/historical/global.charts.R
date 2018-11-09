library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library("fUnitRoots")
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')
library('ggrepel')
#library('dplyr')

options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."
#reportPath <- "D:/StockViz/report"

source("d:/stockviz/r/config.r")

plotVix<-function(vixXts, dateSubset=NULL){
	if(!is.null(dateSubset)){
		vixXts<-vixXts[dateSubset,]
	}
	volNames<-names(vixXts)
	
	volDf<-data.frame(vixXts)
	volDf$T<-as.Date(index(vixXts[,1]))
	plotStart<-min(volDf$T)
	plotEnd<-max(volDf$T)
	xAxisTicks<-seq(plotStart, plotEnd, length.out=10)
	pcity<-periodicity(vixXts)$scale
	
	#plot volatility time-series for all indices
	pdf(NULL)
	ggplot(melt(volDf, id='T'), aes(x=T, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		scale_x_date(breaks = xAxisTicks) +
		labs(x = "", y="", fill="", color="", title=sprintf("VIX (%s)", pcity), subtitle=sprintf("S&P 500, Nifty 50 [%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=plotEnd, y=max(vixXts), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/volatility.%s.%s.%s.png", reportPath, pcity, plotStart, plotEnd), width=12, height=6, units="in")
	
	#calcualte and plot density for all indices
	denList<-lapply(volNames, function(X) {
		d<-density(volDf[,X], na.rm=T)
		data.frame(X=d$x, Y=d$y)
	})
	
	denDf<-data.frame(Y=NA)
	lapply(denList, function(DEN) denDf<<-merge(denDf, DEN, by='Y', all=T)) 
	names(denDf)<-c("DENSITY", volNames)

	meltedDf<-melt(denDf, id='DENSITY')

	pdf(NULL)
	ggplot(meltedDf, aes(x=value, y=DENSITY, color=variable))+ 
		theme_economist() +
		geom_line() +
		labs(x = "vix", y="density", fill="", color="", title=sprintf("VIX (%s)", pcity), subtitle=sprintf("[%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=max(meltedDf$value, na.rm=T), y=max(meltedDf$DENSITY, na.rm=T), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/vix.density.%s.%s.%s.png", reportPath, pcity, plotStart, plotEnd), width=12, height=6, units="in")	
}

plotVolatility<-function(spyXts,nikkeiXts,niftyXts, lb, dateSubset=NULL){
	volXts<-merge(TTR::volatility(spyXts, calc="yang.zhang", n=lb), TTR::volatility(nikkeiXts, calc="yang.zhang", n=lb), TTR::volatility(niftyXts, calc="yang.zhang", n=lb))
	volXts<-na.omit(volXts)
	
	if(!is.null(dateSubset)){
		volXts<-volXts[dateSubset,]
	}

	volNames<-c('SP500', 'NK225', 'N50')
	names(volXts)<-volNames

	volDf<-data.frame(volXts)
	volDf$T<-as.Date(index(volXts[,1]))
	plotStart<-min(volDf$T)
	plotEnd<-max(volDf$T)
	xAxisTicks<-seq(plotStart, plotEnd, length.out=10)
	pcity<-periodicity(spyXts)$scale

	#plot volatility time-series for all indices
	pdf(NULL)
	ggplot(melt(volDf, id='T'), aes(x=T, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		scale_x_date(breaks = xAxisTicks) +
		labs(x = "", y="", fill="", color="", title=sprintf("Volatility (%d-%s)", lb, pcity), subtitle=sprintf("S&P 500, Nikkei 225, Nifty 50 [%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=plotEnd, y=max(volXts), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/volatility.%d-%s.%s.%s.png", reportPath, lb, pcity, plotStart, plotEnd), width=12, height=6, units="in")
	
	#calcualte and plot density for all indices
	denList<-lapply(volNames, function(X) {
		d<-density(volDf[,X])
		data.frame(X=d$x, Y=d$y)
	})
	
	denDf<-data.frame(Y=NA)
	lapply(denList, function(DEN) denDf<<-merge(denDf, DEN, by='Y', all=T)) 
	names(denDf)<-c("DENSITY", volNames)

	meltedDf<-melt(denDf, id='DENSITY')

	pdf(NULL)
	ggplot(meltedDf, aes(x=value, y=DENSITY, color=variable))+ 
		theme_economist() +
		geom_line() +
		labs(x = "volatility", y="density", fill="", color="", title=sprintf("Volatility (%d-%s)", lb, pcity), subtitle=sprintf("[%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=max(meltedDf$value, na.rm=T), y=max(meltedDf$DENSITY, na.rm=T), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/volatility.density.%d-%s.%s.%s.png", reportPath, lb, pcity, plotStart, plotEnd), width=12, height=6, units="in")	

	#plot each volatility time-series individually with an envelope
	for(i in 1:ncol(volXts)){
		seriesName<-toString(names(volXts)[i])
		vol1Xts<-volXts[,i]
		
		vol1Xts$MEAN<-rollapply(vol1Xts[,1], lb*2, mean)
		vol1Xts$SD<-rollapply(vol1Xts[,1], lb*2, sd)
		vol1Xts$avgmsd<-vol1Xts$MEAN - vol1Xts$SD
		vol1Xts$avgpsd<-vol1Xts$MEAN + vol1Xts$SD
		
		volDf<-data.frame(vol1Xts)
		volDf$T<-as.Date(index(vol1Xts[,1]))
		
		ggplot(volDf, aes(x=T)) +
			theme_economist() +
			geom_line(aes_string(y=seriesName)) +
			geom_ribbon(aes(ymin = avgmsd, ymax=avgpsd), fill='grey70', alpha=0.5) +
			scale_x_date(breaks = xAxisTicks) +
			labs(x = "", y="", fill="", color="", title=sprintf("%s Volatility (%d-%s)", seriesName, lb, pcity), subtitle=sprintf("[%s:%s]", plotStart, plotEnd)) +
			annotate("text", x=plotEnd, y=max(volXts), label = "@StockViz", hjust=1.1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
			
		ggsave(sprintf("%s/volatility.%s.%d-%s.%s.%s.png", reportPath, seriesName, lb, pcity, plotStart, plotEnd), width=12, height=6, units="in")
	}
}

rcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", dbserver, dbname, dbuser, dbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("1990-07-03")
endDate<-as.Date("2018-10-31")

ohlcNames<-c("Open","High","Low","Close")

#implied volatility

df<-sqlQuery(lconUs2, sprintf("select time_stamp, ac from BHAV_YAHOO where symbol='^VIX' and time_stamp >= '%s' and time_stamp <= '%s' and o > 0", startDate, endDate))
vixXts<-na.omit(xts(df[,-1], as.Date(df[,1])))

df<-sqlQuery(rcon, sprintf("select time_stamp, px_close from VIX_HISTORY where time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
vixXts<-merge(vixXts, na.omit(xts(df[,-1], as.Date(df[,1]))))

names(vixXts)<-c('SP500', 'N50')
plotVix(vixXts)
plotVix(vixXts, "2010/")

#historical volatility

df<-sqlQuery(lconUs2, sprintf("select time_stamp, O, H, L, C from BHAV_YAHOO where symbol='^GSPC' and time_stamp >= '%s' and time_stamp <= '%s' and o > 0", startDate, endDate))
spyXts<-na.omit(xts(df[,-1], as.Date(df[,1])))
names(spyXts)<-ohlcNames

df<-sqlQuery(lconUs2, sprintf("select time_stamp, O, H, L, C from BHAV_YAHOO where symbol='^N225' and time_stamp >= '%s' and time_stamp <= '%s' and o > 0", startDate, endDate))
nikkeiXts<-na.omit(xts(df[,-1], as.Date(df[,1])))
names(nikkeiXts)<-ohlcNames

df<-sqlQuery(lcon, sprintf("select time_stamp, px_open, px_high, px_low, px_close from BHAV_INDEX where index_name='nifty 50' and time_stamp >= '%s' and time_stamp <= '%s' and px_open > 0", startDate, endDate))
niftyXts<-na.omit(xts(df[,-1], as.Date(df[,1])))
names(niftyXts)<-ohlcNames

#daily volatility
plotVolatility(spyXts,nikkeiXts,niftyXts, 20)
plotVolatility(spyXts,nikkeiXts,niftyXts, 50)
plotVolatility(spyXts,nikkeiXts,niftyXts, 20, "2010/")
plotVolatility(spyXts,nikkeiXts,niftyXts, 50, "2010/")

#weekly volatility
spyWeeklyXts<-to.weekly(spyXts)
nikkeiWeeklyXts<-to.weekly(nikkeiXts)
niftyWeeklyXts<-to.weekly(niftyXts)
names(spyWeeklyXts)<-ohlcNames
names(nikkeiWeeklyXts)<-ohlcNames
names(niftyWeeklyXts)<-ohlcNames

plotVolatility(spyWeeklyXts,nikkeiWeeklyXts,niftyWeeklyXts, 10)
plotVolatility(spyWeeklyXts,nikkeiWeeklyXts,niftyWeeklyXts, 20)
plotVolatility(spyWeeklyXts,nikkeiWeeklyXts,niftyWeeklyXts, 10, "2010/")
plotVolatility(spyWeeklyXts,nikkeiWeeklyXts,niftyWeeklyXts, 20, "2010/")

