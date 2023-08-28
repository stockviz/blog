source("d:/stockviz/r/config.r")
reportPath<-"."

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('fOptions')

library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('viridis')
library('patchwork')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexName <- "NIFTY 50"
indexSym <- "NIFTY"

minExpDays <- 30

eodDt <- sqlQuery(lcon, "select max(time_stamp) from bhav_index")[[1]]

mainExp <- sqlQuery(lcon, sprintf("select distinct expiry_dt from BHAV_EQ_FUT where symbol='NIFTY' and expiry_dt > '%s' order by expiry_dt", eodDt))[,1]

targetExp <- min(mainExp[mainExp > Sys.Date() + minExpDays])

upx <- sqlQuery(lcon, sprintf("select px_close from bhav_index where index_name='%s' and time_stamp='%s'", indexName, eodDt))[[1]]

otmStrikes <- sqlQuery(lcon, sprintf("select strike_pr from BHAV_EQ_OPT where symbol='%s' and option_typ='CE' and time_stamp='%s' and expiry_dt = '%s' and strike_pr > %f order by strike_pr", 
				indexSym, eodDt, targetExp, upx))[,1]
				
otmStrike <- otmStrikes[otmStrikes %% 100 == 0][2]

otmPrem <- sqlQuery(lcon, sprintf("select px_close from BHAV_EQ_OPT where symbol='%s' and option_typ='CE' and time_stamp='%s' and expiry_dt = '%s' and strike_pr  = %f", 
				indexSym, eodDt, targetExp, otmStrike))[[1]]
				
tf <- 'c'
t2e <- as.numeric(difftime(targetExp, eodDt, units=c('days')))				
rate <- 5.0
iv <- GBSVolatility(price=otmPrem, TypeFlag=tf, S=upx, X=otmStrike, Time=t2e/365, r=rate/100, b=rate/100)				

t2edts <- seq(0, t2e-15, length.out=5)
t2esz <- seq(0.5, length.out=length(t2edts), by=0.25)
scenDf <- data.frame(T2E = 0.0, PX=0.0, PREM = 0.0, SZ=0.0)
for(i in 1:length(t2edts)){
	t2edt <- t2edts[i]
	for(px in seq(otmStrike*0.97, otmStrike*1.03, length.out=50)){
		gbs <- GBSCharacteristics(TypeFlag=tf, S=px, X=otmStrike, Time=t2edt/365, r=rate/100, b=rate/100, sigma=iv)
		
		scenDf <- rbind(scenDf, c(t2edt, px, gbs$premium - otmPrem, t2esz[i]))
	}
}

scenDf <- scenDf[-1,]
scenDf$T2E <- factor(scenDf$T2E, levels=unique(scenDf$T2E))

scenDf %>% {
	ggplot(., aes(x=PX, y=PREM, color=T2E)) +
		theme_economist() +
		geom_line(linewidth=.$SZ) +
		scale_color_viridis_d() +
		labs(x=sprintf("%s Level", indexName), y="Position P&L", title=sprintf("%s %d %s CE", indexSym, otmStrike, format(targetExp, '%b%Y')), subtitle=sprintf("asof: %s", eodDt)) +
		#geom_hline(yintercept = otmPrem, linetype="dashed", color = "red") +
		geom_vline(xintercept = otmStrike, linetype="dashed", color = "red") +
		#annotate('text', x=otmStrike*0.975, y=otmPrem, label = 'premium paid', vjust = -1, color='black') +
		annotate('text', x=otmStrike, y=0, label = 'long strike', hjust = -1, color='black', angle = 90) 
}

ggsave(sprintf("%s/%s.call.naked.png", reportPath, indexSym), width=12, height=6, units="in")

##################################

otmStrike2 <- otmStrike + 100
otmPrem2 <- sqlQuery(lcon, sprintf("select px_close from BHAV_EQ_OPT where symbol='%s' and option_typ='CE' and time_stamp='%s' and expiry_dt = '%s' and strike_pr  = %f", 
				indexSym, eodDt, targetExp, otmStrike2))[[1]]
				
scenDf <- data.frame(T2E = 0.0, PX=0.0, PREM = 0.0, SZ=0.0)
for(i in 1:length(t2edts)){
	t2edt <- t2edts[i]
	for(px in seq(otmStrike*0.97, otmStrike*1.03, length.out=50)){
		gbs <- GBSCharacteristics(TypeFlag=tf, S=px, X=otmStrike, Time=t2edt/365, r=rate/100, b=rate/100, sigma=iv)
		gbs2 <- GBSCharacteristics(TypeFlag=tf, S=px, X=otmStrike2, Time=t2edt/365, r=rate/100, b=rate/100, sigma=iv)
		
		scenDf <- rbind(scenDf, c(t2edt, px, gbs$premium - gbs2$premium - otmPrem + otmPrem2, t2esz[i]))
	}
}

scenDf <- scenDf[-1,]
scenDf$T2E <- factor(scenDf$T2E, levels=unique(scenDf$T2E))

scenDf %>% {
	ggplot(., aes(x=PX, y=PREM, color=T2E)) +
		theme_economist() +
		geom_line(linewidth=.$SZ) +
		scale_color_viridis_d() +
		labs(x=sprintf("%s Level", indexName), y="Position P&L", title=sprintf("%s %d %s CE", indexSym, otmStrike, format(targetExp, '%b%Y')), subtitle=sprintf("asof: %s", eodDt)) +
		#geom_hline(yintercept = otmPrem, linetype="dashed", color = "red") +
		geom_vline(xintercept = otmStrike, linetype="dashed", color = "red") +
		geom_vline(xintercept = otmStrike2, linetype="dashed", color = "orange") +
		annotate('text', x=otmStrike, y=0, label = 'long strike', hjust = -1, color='black', angle = 90) +
		annotate('text', x=otmStrike2, y=0, label = 'short strike', hjust = -1, color='black', angle = 90) 
}

ggsave(sprintf("%s/%s.call.spread.png", reportPath, indexSym), width=12, height=6, units="in")
