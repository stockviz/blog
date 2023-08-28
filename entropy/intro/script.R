#read: https://cran.r-project.org/web/packages/RTransferEntropy/vignettes/transfer-entropy.html

source("d:/stockviz/r/config.r")
reportPath<-"."

library('RTransferEntropy')
library('RODBC')
library('RPostgres')
library('quantmod')

library('extrafont')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('ggpubr')

options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pcon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)

pxLb <- 1000

bhavDate <- sqlQuery(lcon, "select max(time_stamp) from BHAV_EQ_FUT")[[1]]
futSyms <- sqlQuery(lcon, sprintf("select distinct(symbol) from BHAV_EQ_FUT where time_stamp='%s'", bhavDate))[,1]

futNames <- c()
futRet <- NULL
for(sym in futSyms){
	pxDf <- dbGetQuery(pcon, sprintf("select c, date_stamp from eod_adjusted_nse where ticker=$1 order by date_stamp desc limit %d", pxLb), params=list(sym))
	if(pxDf[1,2] != bhavDate || nrow(pxDf) != pxLb) next
	
	pXts <- xts(pxDf[,1], pxDf[,2])
	
	futRet <- merge.xts(futRet, dailyReturn(pXts))
	futNames <- c(futNames, sym)
}

names(futRet) <- futNames

pxDf <- sqlQuery(lcon, sprintf("select top %d px_close, time_stamp from bhav_index where index_name='NIFTY 50' order by time_stamp desc", pxLb))
pXts <- xts(pxDf[,1], pxDf[,2])
nRet <- dailyReturn(pXts)

allRets <- merge(nRet, futRet)

allRets <- allRets[-1,]
allRets <- na.trim(allRets, side='right')
allRets <- na.fill(allRets, 0)

startDate <- first(index(allRets))
endDate <- last(index(allRets))

#################################################

syms <- names(allRets)
results <- data.frame(SYM="", FLOW_TO=0.0, FLOW_FROM=0.0, FLOW_TO_SE=0.0, FLOW_FROM_SE=0.0)
for(i in 2:ncol(allRets)){
	te <- transfer_entropy(allRets[,i], allRets[,1], quantiles = c(10, 90), quiet = T)
	ete <- coef(te)[1:2, 2]
	se <- coef(te)[1:2, 3]
	
	results <- rbind(results, c(syms[i], ete, se))
}

results <- results[-1,]

for(i in 2:ncol(results)){
	results[,i] <- as.numeric(results[,i])
}

#plot flows
  
toPlot <- results[order(results$FLOW_FROM), c('SYM', 'FLOW_TO', 'FLOW_FROM')]
toPlot$SYM <- factor(toPlot$SYM, levels = toPlot$SYM)
toPlot <- melt(toPlot, id='SYM')

ggplot(toPlot, aes(x = value, y = SYM, color = variable)) + 
	theme_economist() +
	labs(y = NULL, x = "Effective Transfer Entropy", color='', title="Shannon Entropy: Futures over NIFTY", subtitle=sprintf("%d-day lookback [%s:%s]", pxLb, startDate, endDate)) +
	geom_point() +
	annotate("text", y=1, x=max(toPlot$value, na.rm=T), label = "@StockViz", hjust='right', vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)
ggsave(sprintf("%s/shannon.%d.from.png", reportPath, pxLb), width=8, height=20, units="in")
  
toPlot <- results[order(results$FLOW_TO), c('SYM', 'FLOW_TO', 'FLOW_FROM')]
toPlot$SYM <- factor(toPlot$SYM, levels = toPlot$SYM)
toPlot <- melt(toPlot, id='SYM')

ggplot(toPlot, aes(x = value, y = SYM, color = variable)) + 
	theme_economist() +
	labs(y = NULL, x = "Effective Transfer Entropy", color='', title="Shannon Entropy: Futures over NIFTY", subtitle=sprintf("%d-day lookback [%s:%s]", pxLb, startDate, endDate)) +
	geom_point() +
	annotate("text", y=1, x=max(toPlot$value, na.rm=T), label = "@StockViz", hjust='right', vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)
  
ggsave(sprintf("%s/shannon.%d.to.png", reportPath, pxLb), width=8, height=20, units="in")        