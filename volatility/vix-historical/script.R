source("d:/stockviz/r/config.r")

reportPath<-"."

library('RODBC')
library('quantmod')
library('feasts')
library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('patchwork')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=100)

volLb <- 20 #days
volCalcs <- c("close", "garman.klass", "parkinson", "rogers.satchell", "gk.yz", "yang.zhang")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexDf <- sqlQuery(lcon, "select px_close, time_stamp from VIX_HISTORY")
iXts <- xts(indexDf[,1], indexDf[,2])
names(iXts) <- c("vix")

startDate <- first(index(iXts))

indexDf <- sqlQuery(lcon, sprintf("select time_stamp, px_open, px_high, px_low, px_close from BHAV_INDEX where index_name = 'nifty 50' and time_stamp >= '%s'", startDate))
nXts <- xts(indexDf[,-1], indexDf[,1])
names(nXts) <- c('Open', 'High', 'Low', 'Close')

volXts <- do.call(merge.xts, lapply(volCalcs, function(X) volatility(nXts, n=volLb, calc=X)))
names(volXts) <- volCalcs

volXts$avg <- rowSums(volXts)/ncol(volXts)
volXts$avg <- 100*volXts$avg

lm_eqn <- function(frml, df){
	yStr <- as.character(frml)[2]
	xStr <- as.character(frml)[3]
    m <- lm(frml, df)
    eq <- substitute(italic(yStr) == a + b %.% italic(xStr)*","~~italic(r)^2~"="~r2, 
         list(a = format(unname(coef(m)[1]), digits = 2),
              b = format(unname(coef(m)[2]), digits = 2),
             r2 = format(summary(m)$r.squared, digits = 3),
			 yStr = yStr,
			 xStr = xStr))
    as.character(as.expression(eq));
}

plotRel <- function(lagDays){
	allXts <- merge(volXts, iXts)
	allXts$vix_lag <- stats::lag(allXts$vix, lagDays)
	allXts <- na.omit(allXts)
	
	allDf <- data.frame(allXts)
	allDf$T <- index(allXts)

	volSegs <- seq(1, nrow(allDf), by = 500)
	volSegs[length(volSegs)] <- nrow(allDf)

	for(i in 2:length(volSegs)){
		toPlot <- allDf[volSegs[i-1]:volSegs[i],]
		eqexp <- lm_eqn(formula("vix_lag ~ avg"), toPlot)
		
		ggplot(toPlot, aes(x=avg, y=vix_lag)) +
				theme_economist() +
				geom_point() +
				geom_smooth(method = "lm") +
				geom_text(aes(x = min(avg), y = max(vix_lag)), label = eqexp, parse=T, hjust='left', cex=5, col='darkgrey') +
				labs(x="Historical Volatility", y="VIX", title=sprintf("Historical vs. Implied Volatility (VIX) (%d-day lag)", lagDays), subtitle = sprintf("%s:%s", first(toPlot$T), last(toPlot$T))) +
				annotate("text", x=max(toPlot$avg), y=max(toPlot$vix_lag), label = "@StockViz", hjust='right', vjust='top', col="white", cex=6, fontface = "bold", alpha = 0.8) 
				
		ggsave(sprintf("%s/vix-historical.lag-%d.%d.png", reportPath, lagDays, i-1), width=12, height=6, units="in")
	}

	#magick.exe convert -delay 60 -loop 0 vix-historical.lag-%d.*.png vix-historical.lag-%d.gif
}

for(i in c(0, 5, 10, 20)){
	plotRel(i)
}