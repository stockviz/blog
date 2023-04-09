# based on https://ssrn.com/abstract=3505045

source("D:/stockviz/r/config.r")

library('RODBC')
library('tidyverse')
library('reshape2')
library('ggthemes')
library('viridis')
library('ggrepel')
library('quantmod')

reportPath <- "."
options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

pdf(NULL)

oiStDate <- as.Date("2020-05-01")
expiries <- sqlQuery(lcon, sprintf("select distinct EXPIRY_DT from bhav_eq_opt where symbol='NIFTY' and time_stamp >= '%s'", oiStDate))$EXPIRY_DT
tradingDays <- sqlQuery(lcon, sprintf("select distinct time_stamp from bhav_eq_opt where symbol='NIFTY' and time_stamp >= '%s'", oiStDate))$time_stamp

expiries <- sort(expiries)
tradingDays <- sort(tradingDays)

getStats <- function(varName){
	nearExpStat <- data.frame(asof = Sys.Date(), oiwks = 0.0, indexPx = 0.0)
	for(i in 1:length(tradingDays)){
		trday <- tradingDays[i]
		expday <- first(expiries[expiries > trday])

		oiData <- sqlQuery(lcon, sprintf("select option_typ, strike_pr, %s wtvar from bhav_eq_opt where symbol='NIFTY' and time_stamp = '%s' and expiry_dt = '%s'", varName, trday, expday))
		if(nrow(oiData) == 0){
			print(paste("no data for ", trday, "/", expday))
			next
		}
		spotPx <- sqlQuery(lcon, sprintf("select px_close from bhav_index where index_name='NIFTY 50' and time_stamp = '%s'", trday))[[1]]
		
		tempDf <- oiData %>% summarize(asof = trday, oiwks = sum(wtvar * (strike_pr/spotPx-1))/sum(wtvar), indexPx = spotPx) %>% as.data.frame()
		
		nearExpStat <- rbind(nearExpStat, tempDf)
	}

	nearExpStat <- nearExpStat[-1,]
	nearExpStat$oiwks <- nearExpStat$oiwks * 100

	nearExpStat$ret <- c(diff(log(nearExpStat$indexPx)), NA)
	nearExpStat$ret <- nearExpStat$ret * 100

	return(nearExpStat)
}

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

for(varName in c('OPEN_INTEREST', 'VAL_IN_LAKH')){
	toPlot <- getStats(varName)
	eqexp <- lm_eqn(formula("ret ~ oiwks"), toPlot)
	
	plotStart <- first(toPlot$asof)
	plotEnd <- last(toPlot$asof)
	
	ggplot(toPlot, aes(x=oiwks, y=ret)) +
		theme_economist() +
		geom_point() +
		geom_smooth(method = "lm") +
		geom_text(aes(x = min(oiwks, na.rm=T) + 1, y = max(ret, na.rm=T)), label = eqexp, parse=T, hjust='left') +
		labs(x = sprintf("oiwks (%s)", varName), y="ret (Returns %)", fill="", color="", title="Weighted strike-spot ratio vs. Returns", subtitle=sprintf("[%s:%s]", plotStart, plotEnd)) +
		annotate("text", x=max(toPlot$oiwks, na.rm=T), y=min(toPlot$ret, na.rm=T), label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/reg.%s.png", reportPath, varName), width=12, height=6, units="in")
}