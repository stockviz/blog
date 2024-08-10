library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('data.table')

library('tidyverse')
library('patchwork')
library('viridis')
library('ggthemes')

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

drag <- 0.02/100

indices <- c('NIFTY 50 TR', 'NIFTY NEXT 50 TR', 'NIFTY MIDCAP SELECT TR', 'NIFTY BANK TR')
for(indexName in indices){
	print(indexName)
	
	ndf<-sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s'", indexName))
	ndx<-xts(ndf$px_close, as.Date(ndf$time_stamp))
	dRet<-dailyReturn(ndx)
	dRet<-dRet[-1]
	names(dRet)<-c('RET')
	dRet$RET01 <- ifelse(dRet$RET > 0, 1, 0)
	dRet$RET_LAG_1 <- stats::lag(dRet$RET, -1)

	rle01<-rle(as.vector(coredata(dRet$RET01)))
	rle01df<-data.frame(unclass(rle01))

	ggplot(rle01df, aes(x=as.factor(lengths), group=as.factor(values), fill=as.factor(values))) +
		theme_economist() +
		geom_histogram(position='dodge', stat="count") +
		stat_count(binwidth = 1, geom = 'text', color = 'black', aes(label = paste0(round(..count../sum(..count..)*100, 2), '%')), position=position_dodge(width = 1), vjust = -0.2) +
		scale_fill_viridis_d() +
		labs(y='count', x='streak length (days)', fill='down/up', color='', title=sprintf("%s Return Streaks", indexName), subtitle=sprintf("%s:%s; N = %d", first(index(dRet)), last(index(dRet)), nrow(dRet)), caption = '@StockViz')
		
	ggsave(sprintf("%s/%s.streak.histogram.png", reportPath, indexName), width=14, height=6, units="in")

	dretDf <- as.data.frame(dRet)
	setDT(dretDf)[, continual := seq_len(.N), by = rleid(RET01)]

	dretXts <- xts(dretDf, index(dRet))
	maxStreak <- max(rle01df$lengths) - 1

	retsUp <- NULL
	for(i in 2:maxStreak){
		retsUp <- merge.xts(retsUp, ifelse(dretXts$continual == i & dretXts$RET01 == 1, dretXts$RET_LAG_1, 0))
	}

	retsDn <- NULL
	for(i in 2:maxStreak){
		retsDn <- merge.xts(retsDn, ifelse(dretXts$continual == i & dretXts$RET01 == 0, dretXts$RET_LAG_1, 0))
	}
	
	retsUp2 <- NULL
	for(i in 2:maxStreak){
		retsUp2 <- merge.xts(retsUp2, ifelse(dretXts$continual == i & dretXts$RET01 == 1, -dretXts$RET_LAG_1, 0))
	}
	
	retsDn2 <- NULL
	for(i in 2:maxStreak){
		retsDn2 <- merge.xts(retsDn2, ifelse(dretXts$continual == i & dretXts$RET01 == 0, -dretXts$RET_LAG_1, 0))
	}

	names(retsUp) <- sapply(2:maxStreak, \(x) paste0('STREAK_', sprintf('%02d', x)))
	annRetsUp <- Return.annualized(retsUp)
	
	names(retsDn) <- sapply(2:maxStreak, \(x) paste0('STREAK_', sprintf('%02d', x)))
	annRetsDn <- Return.annualized(retsDn)
	
	names(retsUp2) <- sapply(2:maxStreak, \(x) paste0('STREAK_', sprintf('%02d', x)))
	annRetsUp2 <- Return.annualized(retsUp2)
	
	names(retsDn2) <- sapply(2:maxStreak, \(x) paste0('STREAK_', sprintf('%02d', x)))
	annRetsDn2 <- Return.annualized(retsDn2)

	annRets <- data.frame(cbind(t(annRetsUp)*100, t(annRetsDn*100)))
	colnames(annRets) <- c('Buy1', 'Buy0')
	annRets$Streak <- rownames(annRets)
	annRets %>% pivot_longer(cols=-Streak) %>% mutate(Streak = as.factor(Streak)) %>%
		ggplot(aes(x=Streak, y = value, fill = name)) +
			theme_economist() +
			geom_bar(stat="identity", position=position_dodge()) +
			geom_text(aes(label=paste0(round(value, 2), '%')), vjust=-0.6, color="black",  position = position_dodge(0.9), size=3.5)+
			scale_fill_viridis_d() +
			labs(y='annualized returns (%)', x='', fill='BuyDown/BuyUp', color='', title=sprintf("%s Buy after N-day Streaks", indexName), subtitle=sprintf("%s:%s", first(index(dRet)), last(index(dRet))), 
				caption = '@StockViz')
			
	ggsave(sprintf("%s/%s.streak.buy.png", reportPath, indexName), width=12, height=6, units="in")
	
	annRets <- data.frame(cbind(t(annRetsUp2)*100, t(annRetsDn2*100)))
	colnames(annRets) <- c('Sell1', 'Sell0')
	annRets$Streak <- rownames(annRets)
	annRets %>% pivot_longer(cols=-Streak) %>% mutate(Streak = as.factor(Streak)) %>%
		ggplot(aes(x=Streak, y = value, fill = name)) +
			theme_economist() +
			geom_bar(stat="identity", position=position_dodge()) +
			geom_text(aes(label=paste0(round(value, 2), '%')), vjust=-0.6, color="black",  position = position_dodge(0.9), size=3.5)+
			scale_fill_viridis_d() +
			labs(y='annualized returns (%)', x='', fill='SellDown/SellUp', color='', title=sprintf("%s Sell after N-day Streaks", indexName), subtitle=sprintf("%s:%s", first(index(dRet)), last(index(dRet))), 
				caption = '@StockViz')
			
	ggsave(sprintf("%s/%s.streak.sell.png", reportPath, indexName), width=12, height=6, units="in")
	
	print("**********************************************")
		
}