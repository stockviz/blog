library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('lubridate')
library('reshape2')
library('ggrepel')

options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
reportPath <- "."

rcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", dbserver, dbname, dbuser, dbpassword), case = "nochange", believeNRows = TRUE)
lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUS <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

plotDf1<-function(dataDf, ylabTxt, title, subtitle, fileName){
	dataXts<-xts(dataDf[,2], as.Date(dataDf[,1]))

	dataXts<-merge(dataXts, rollapply(dataXts[,1], 500, median))
	dataXts<-merge(dataXts, rollapply(dataXts[,1], 500, mean))
	dataXts<-merge(dataXts, rollapply(dataXts[,1], 500, sd))

	toPlotDf<-data.frame(dataXts)
	toPlotDf$time_stamp <- as.Date(index(dataXts[,1]))
	
	firstDate<-as.Date(first(index(dataXts[,1])))
	lastDate<-as.Date(last(index(dataXts[,1])))
	xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)
	
	names(toPlotDf)<-c('Y', 'MEDIAN', 'MEAN', 'SD', 'DAY')
	toPlotDf$SD1<-toPlotDf$MEAN+toPlotDf$SD
	toPlotDf$SD2<-toPlotDf$MEAN-toPlotDf$SD
	
	pdf(NULL)
	ggplot(data=toPlotDf, aes(x=DAY)) +
		theme_economist() +
		geom_line(aes(y=Y)) + 
		geom_line(aes(y=MEDIAN), color='darkgrey', linetype='dashed') + 
		geom_line(aes(y=SD1), color='darkgrey', linetype='dotted') + 
		geom_line(aes(y=SD2), color='darkgrey', linetype='dotted') + 
		scale_x_date(breaks = xAxisTicks) +
		labs(x='', y=ylabTxt, color='', title=title, subtitle=sprintf("%s [%s:%s]", subtitle, firstDate, lastDate)) +
		annotate("text", x=lastDate, y=min(subset(toPlotDf,select=-c(DAY, SD)), na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(fileName, dpi=600, width=12, height=6, units="in")
}

#x axis is a time series
#multiple y axis
plotDf2<-function(dataDf, ylabTxt, title, subtitle, fileName){
	firstDate<-min(dataDf$time_stamp)
	lastDate<-max(dataDf$time_stamp)
	xAxisTicks<-seq(from=firstDate, to=lastDate, length.out=10)
	
	plotDf<-melt(dataDf, id='time_stamp')
	plotDf$label<-ifelse(plotDf$time_stamp == max(plotDf$time_stamp), as.character(plotDf$variable), NA)
	
	pdf(NULL)
	ggplot(data=plotDf, aes(x=time_stamp, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		scale_x_date(breaks = xAxisTicks) +
		geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +
		guides(color = FALSE, fill = FALSE, group = FALSE) +
		labs(x='', y=ylabTxt, color='', title=title, subtitle=sprintf("%s [%s:%s]", subtitle, firstDate, lastDate)) +
		annotate("text", x=firstDate, y=min(plotDf$value, na.rm=T), label = "@StockViz", vjust=-1.1, col="white", cex=4, fontface = "bold", alpha = 0.8)
		
	ggsave(fileName, dpi=600, width=12, height=6, units="in")
}

#already melted
#x axis identifed as 'X'
plotDf3<-function(dataDf, ylabTxt, title, subtitle, fileName){
	pdf(NULL)
	ggplot(data=dataDf, aes(x=X, y=value, group=variable, color=variable)) +
		theme_economist() +
		geom_line() +
		geom_label_repel(aes(label = label), nudge_x = 2, na.rm = TRUE) +
		guides(color = FALSE, fill = FALSE, group = FALSE) +
		labs(x='', y=ylabTxt, color='', title=title, subtitle=subtitle) +
		annotate("text", x=max(dataDf$X, na.rm=T), y=min(dataDf$value, na.rm=T), label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(fileName, dpi=600, width=12, height=6, units="in")
}

doFred<-function(indexList, ylabTxt, title, subtitle, fileName){
		tsXts<-xts()
		for(i in indexList){
			id<-sqlQuery(lconUS, sprintf("select id from FRED_SERIES where series_id='%s'", i))[[1]]
			dataDf<-sqlQuery(lconUS, sprintf("select TIME_STAMP, VAL from FRED_OBSERVATION where series_id=%d", id))
			dXts<-xts(dataDf$VAL, as.Date(dataDf$TIME_STAMP))
			tsXts<-merge(tsXts, dXts)
		}
		tsXts<-na.omit(tsXts)
		names(tsXts)<-indexList
		tDf<-data.frame(tsXts)
		tDf$time_stamp<-as.Date(index(tsXts[,1]))
		plotDf2(tDf, ylabTxt, title, subtitle, fileName)
}

corporateUS<-c('BAMLC0A1CAAA', 'BAMLC0A2CAA','BAMLC0A3CA','BAMLC0A4CBBB')
corporateEM<-c('BAMLEM1BRRAAA2ACRPIOAS', 'BAMLEM2BRRBBBCRPIOAS','BAMLEM3BRRBBCRPIOAS','BAMLEM4BRRBLCRPIOAS')

doFred(corporateUS, 'spread', 'US Corporate', 'Option-Adjusted Spreads', sprintf("%s/US.CORPORATE.png", reportPath))
doFred(corporateEM, 'spread', 'EM Corporate', 'Option-Adjusted Spreads', sprintf("%s/EM.CORPORATE.png", reportPath))
doFred(c('BAMLC0A1CAAA', 'BAMLEM1BRRAAA2ACRPIOAS'), 'spread', 'US vs. EM AAA Corporate', 'Option-Adjusted Spreads', sprintf("%s/US-EM.AAA.CORPORATE.png", reportPath))

#------------------------------------

dataDf<-sqlQuery(lcon, "select a1.tenor_y+a1.tenor_m/12.0 TENOR, a1.VAL AAA, a2.VAL [ALL] from EUR_YIELD_CURVE a1, EUR_YIELD_CURVE a2
							where a1.curve_id='G_N_A' 
							and a2.curve_id='G_N_C' 
							and a1.time_stamp = a2.time_stamp
							and a1.tenor_m = a2.tenor_m
							and a1.tenor_y = a2.tenor_y
							and a1.time_stamp = '2018-10-15'")

meltedDf<-melt(dataDf, id='TENOR')
names(meltedDf)<-c('X', 'variable', 'value')
meltedDf$label<-ifelse(meltedDf$X == max(meltedDf$X), as.character(meltedDf$variable), NA)

plotDf3(meltedDf, 'yield', 'Euro Area Sovereign AAA rated vs. All', '2018-10-15', sprintf("%s/EUR.2018-10-15.png", reportPath))

#------------------------------------

dataYears<-sqlQuery(lcon, "select distinct datepart(year, time_stamp) from EUR_YIELD_CURVE")
dataYears<-sort(dataYears[,1])
dataYears<-tail(dataYears, 5)
ycLastDayDf<-NULL
for(dy in dataYears){
	lastDayOfYear<-as.Date(sprintf("%d-12-31", dy))
	dataLastDayOfYear<-sqlQuery(lcon, sprintf("select max(time_stamp) from EUR_YIELD_CURVE where time_stamp <= '%s'", lastDayOfYear))[[1]]
	ycLast<-sqlQuery(lcon, sprintf("select TIME_STAMP, tenor_y+tenor_m/12.0, val from EUR_YIELD_CURVE where time_stamp = '%s' and tenor_y <= 30 and curve_id='G_N_C'", dataLastDayOfYear))
	ycLastDayDf<-rbind(ycLastDayDf, ycLast)
}
ycLastDayDf$YEAR<-year(ycLastDayDf$TIME_STAMP)
ycLastDayDf<-subset(ycLastDayDf, select=-c(TIME_STAMP))
names(ycLastDayDf)<-c('X', 'value', 'variable')

ycLastDayDf$label<-ifelse(ycLastDayDf$X == max(ycLastDayDf$X), as.character(ycLastDayDf$variable), NA)
plotDf3(ycLastDayDf, 'yield', 'Euro Area Sovereign Yield Curves', sprintf('term structure over the years [%s:%s]', min(dataYears), max(dataYears)), sprintf("%s/EUR-ALL.png", reportPath))

#------------------------------------

dataYears<-sqlQuery(lcon, "select distinct datepart(year, time_stamp) from EUR_YIELD_CURVE")
dataYears<-sort(dataYears[,1])
dataYears<-tail(dataYears, 5)
ycLastDayDf<-NULL
for(dy in dataYears){
	lastDayOfYear<-as.Date(sprintf("%d-12-31", dy))
	dataLastDayOfYear<-sqlQuery(lcon, sprintf("select max(time_stamp) from EUR_YIELD_CURVE where time_stamp <= '%s'", lastDayOfYear))[[1]]
	ycLast<-sqlQuery(lcon, sprintf("select TIME_STAMP, tenor_y+tenor_m/12.0, val from EUR_YIELD_CURVE where time_stamp = '%s' and tenor_y <= 30 and curve_id='G_N_A'", dataLastDayOfYear))
	ycLastDayDf<-rbind(ycLastDayDf, ycLast)
}
ycLastDayDf$YEAR<-year(ycLastDayDf$TIME_STAMP)
ycLastDayDf<-subset(ycLastDayDf, select=-c(TIME_STAMP))
names(ycLastDayDf)<-c('X', 'value', 'variable')

ycLastDayDf$label<-ifelse(ycLastDayDf$X == max(ycLastDayDf$X), as.character(ycLastDayDf$variable), NA)
plotDf3(ycLastDayDf, 'yield', 'Euro Area Sovereign AAA rated Yield Curves', sprintf('term structure over the years [%s:%s]', min(dataYears), max(dataYears)), sprintf("%s/EUR-AAA.png", reportPath))

#------------------------------------

dataYears<-sqlQuery(rcon, "select distinct datepart(year, time_stamp) from ZERO_COUPON_CURVE")
dataYears<-sort(dataYears[,1])
dataYears<-tail(dataYears, 5)
ycLastDayDf<-NULL
for(dy in dataYears){
	lastDayOfYear<-as.Date(sprintf("%d-12-31", dy))
	dataLastDayOfYear<-sqlQuery(rcon, sprintf("select max(time_stamp) from ZERO_COUPON_CURVE where time_stamp <= '%s'", lastDayOfYear))[[1]]
	ycLast<-sqlQuery(rcon, sprintf("select * from ZERO_COUPON_CURVE where time_stamp = '%s' and maturity <= 30", dataLastDayOfYear))
	ycLastDayDf<-rbind(ycLastDayDf, ycLast)
}
ycLastDayDf$YEAR<-year(ycLastDayDf$TIME_STAMP)
ycLastDayDf<-subset(ycLastDayDf, select=-c(TIME_STAMP))
names(ycLastDayDf)<-c('X', 'value', 'variable')

ycLastDayDf$label<-ifelse(ycLastDayDf$X == max(ycLastDayDf$X), as.character(ycLastDayDf$variable), NA)
plotDf3(ycLastDayDf, 'yield', 'India Zero Coupon Yield Curves', sprintf('term structure over the years [%s:%s]', min(dataYears), max(dataYears)), sprintf("%s/IND.png", reportPath))
q()

#------------------------------------

dataYears<-sqlQuery(lcon, "select distinct datepart(year, time_stamp) from UST_YIELD_CURVE")
dataYears<-sort(dataYears[,1])
dataYears<-tail(dataYears, 5)
ycLastDayXts<-NULL
for(dy in dataYears){
	lastDayOfYear<-as.Date(sprintf("%d-12-31", dy))
	dataLastDayOfYear<-sqlQuery(lcon, sprintf("select max(time_stamp) from UST_YIELD_CURVE where time_stamp <= '%s'", lastDayOfYear))[[1]]
	ycLast<-sqlQuery(lcon, sprintf("select * from UST_YIELD_CURVE where time_stamp = '%s'", dataLastDayOfYear))
	ycLastXts<-xts(subset(ycLast, select=-c(TIME_STAMP)), as.Date(ycLast$TIME_STAMP))
	if(all(ycLastXts != 0) == F) next
	ycLastDayXts<-rbind(ycLastDayXts, ycLastXts)
}

ycLastDayXts$YEAR<-year(index(ycLastDayXts))
dataDf<-data.frame(ycLastDayXts)

meltedDf<-melt(dataDf, id='YEAR')
meltedDf$variable<-as.numeric(gsub('M', '0.', gsub('Y', '', meltedDf$variable)))
meltedDf$label<-ifelse(meltedDf$variable == max(meltedDf$variable), as.character(meltedDf$YEAR), NA)

names(meltedDf)<-c('variable', 'X', 'value', 'label')

plotDf3(meltedDf, 'yield', 'UST Yield Curves', sprintf('term structure over the years [%s:%s]', min(dataYears), max(dataYears)), sprintf("%s/UST.png", reportPath))

#------------------------------------

dataDf<-sqlQuery(lcon, "select time_stamp, y10-y2 from UST_YIELD_CURVE")
plotDf1(dataDf, 'spread', 'UST 2s10s', 'Spread between 10y and 2y yields', sprintf("%s/UST.2s10s.png", reportPath))

#------------------------------------

dataDf<-sqlQuery(lcon, "select time_stamp, y10 from UST_YIELD_CURVE")
plotDf1(dataDf, 'yield', 'UST 10y', '10y yield', sprintf("%s/UST.10s.png", reportPath))

#------------------------------------

dataDf<-sqlQuery(lcon, "select a1.time_stamp, a1.VAL AAA, a2.VAL [ALL] from EUR_YIELD_CURVE a1, EUR_YIELD_CURVE a2
							where a1.curve_id='G_N_A' and a1.tenor_y=10 and a1.tenor_m=0
							and a2.curve_id='G_N_C' and a2.tenor_y=10 and a2.tenor_m=0
							and a1.time_stamp = a2.time_stamp")
dataDf$time_stamp<-as.Date(dataDf$time_stamp)
plotDf2(dataDf, 'yield', 'Euro Area Sovereign AAA rated vs. All 10y', '10y yields', sprintf("%s/EUR.10s.png", reportPath))

# #------------------------------------
							
dataDf<-sqlQuery(lcon, "select a1.time_stamp, a1.VAL-a3.VAL AAA, a2.VAL-a4.VAL [ALL] from EUR_YIELD_CURVE a1, EUR_YIELD_CURVE a2, EUR_YIELD_CURVE a3, EUR_YIELD_CURVE a4
							where a1.curve_id='G_N_A' and a1.tenor_y=10 and a1.tenor_m=0
							and a2.curve_id='G_N_C' and a2.tenor_y=10 and a2.tenor_m=0
							and a3.curve_id='G_N_A' and a3.tenor_y=2 and a3.tenor_m=0
							and a4.curve_id='G_N_C' and a4.tenor_y=2 and a4.tenor_m=0
							and a1.time_stamp = a2.time_stamp
							and a1.time_stamp = a3.time_stamp
							and a1.time_stamp = a4.time_stamp")
dataDf$time_stamp<-as.Date(dataDf$time_stamp)
plotDf2(dataDf, 'spread', 'Euro Area Sovereign AAA rated vs. All 2s10s', 'Spread between 10y and 2y yields', sprintf("%s/EUR.2s10s.png", reportPath))

#------------------------------------

dataDf<-sqlQuery(rcon, "select a1.time_stamp, a1.yield from ZERO_COUPON_CURVE a1 where a1.maturity=10")
plotDf1(dataDf, 'yield', 'India Zero Coupon 10y', '10y yields', sprintf("%s/IND.10s.png", reportPath))

#------------------------------------
							
dataDf<-sqlQuery(rcon, "select a1.time_stamp, a1.yield - a2.yield from ZERO_COUPON_CURVE a1, ZERO_COUPON_CURVE a2
							where a1.maturity=10 
							and a2.maturity=2 
							and a1.time_stamp = a2.time_stamp")
plotDf1(dataDf, 'spread', 'India Zero Coupon 2s10s', 'Spread between 10y and 2y yields', sprintf("%s/IND.2s10s.png", reportPath))
