library('RODBC')
library('quantmod')

library('lubridate')
library('scales')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexNames <- sqlQuery(lcon, "select distinct(MKT_CAP_CLASS) ts from MF_MKT_CAP_CLASS order by MKT_CAP_CLASS")[,1]
indexChangeDates <- sqlQuery(lcon, "select distinct(AS_OF) ts from MF_MKT_CAP_CLASS order by ts")[,1]

i <- 2
j <- 1

mktCapIQ <- NULL
for(i in 2:length(indexChangeDates)){
	for(j in 1:length(indexNames)){
		indexName <- indexNames[j]
		iDate <- indexChangeDates[i-1]
		iDateNext <- indexChangeDates[i]
		
		symSet <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName, iDate))[,1]
		
		illiqDf <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
											where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
											and r.symbol = p.symbol and r.time_stamp = p.time_stamp
											and p.series in ('EQ', 'BE')
											group by r.symbol", paste(symSet, collapse="','"), iDate, iDateNext))
		 
		mktCapIQ <- bind_rows(mktCapIQ, data.frame(IDATE = rep(as.character(iDate), nrow(illiqDf)), INAME = rep(indexName, nrow(illiqDf)), SYM = illiqDf$SYM, ILLIQ = illiqDf$ILLIQ, CT = illiqDf$CT))
	}
}

mktCapIQ %>% filter(CT >= 100) %>% group_by(IDATE, INAME) %>% summarize(A = median(ILLIQ*100000)) %>% {
	ggplot(., aes(x = factor(IDATE), y = A, color = factor(INAME))) +
		theme_economist() +
		geom_point(size=10) +
		scale_color_viridis_d() +
		scale_y_continuous(trans='log', labels = label_number(accuracy = 0.01)) +
		labs(x='', y='log illiquidity', color='', fill='', title="Illiquidity by Market-cap Classification", caption = "@StockViz")
}
ggsave(sprintf("%s/illiquidity-mkt-cap.simple.png", reportPath), width=16, height=8, units="in")	

#large caps

indexName <- "LARGE CAP"
largeCapIQEx <- tibble()
largeCapIQCo <- tibble()
for(i in 2:(length(indexChangeDates)-1)){
	iDate <- indexChangeDates[i-1]
	iDateNext <- indexChangeDates[i]
	iDateNext2 <- indexChangeDates[i + 1]
	
	oldSet <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName, iDate))[,1]
	newSet <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName, iDateNext))[,1]
	
	exits <- setdiff(oldSet, newSet)
	common <- intersect(oldSet, newSet)
	
	illiqBeforeEx <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(exits, collapse="','"), iDate, iDateNext))
	
	illiqAfterEx <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(exits, collapse="','"), iDateNext, iDateNext2))
										
	
	illiqBeforeCo <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(common, collapse="','"), iDate, iDateNext))
	
	illiqAfterCo <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(common, collapse="','"), iDateNext, iDateNext2))
	 
	illqEx <- illiqBeforeEx %>% inner_join(illiqAfterEx, by='SYM', suffix = c('.b', '.a')) %>% mutate(IDATE = iDateNext, INAME = indexName)
	illqCo <- illiqBeforeCo %>% inner_join(illiqAfterCo, by='SYM', suffix = c('.b', '.a')) %>% mutate(IDATE = iDateNext, INAME = indexName)
	largeCapIQEx <- bind_rows(largeCapIQEx, illqEx)
	largeCapIQCo <- bind_rows(largeCapIQCo, illqCo)
}

#midcap -> largecap
indexName <- "MID CAP"
indexName2 <- "LARGE CAP"
midCapIQEx <- tibble()
midCapIQCo <- tibble()
for(i in 2:(length(indexChangeDates)-1)){
	iDate <- indexChangeDates[i-1]
	iDateNext <- indexChangeDates[i]
	iDateNext2 <- indexChangeDates[i + 1]
	
	oldSet <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName, iDate))[,1]
	newSet <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName, iDateNext))[,1]
	newSet2 <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName2, iDateNext))[,1]
	
	exits <- intersect(oldSet, newSet2)
	common <- intersect(oldSet, newSet)
	
	illiqBeforeEx <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(exits, collapse="','"), iDate, iDateNext))
	
	illiqAfterEx <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(exits, collapse="','"), iDateNext, iDateNext2))
										
	
	illiqBeforeCo <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(common, collapse="','"), iDate, iDateNext))
	
	illiqAfterCo <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(common, collapse="','"), iDateNext, iDateNext2))
	 
	illqEx <- illiqBeforeEx %>% inner_join(illiqAfterEx, by='SYM', suffix = c('.b', '.a')) %>% mutate(IDATE = iDateNext, INAME = indexName)
	illqCo <- illiqBeforeCo %>% inner_join(illiqAfterCo, by='SYM', suffix = c('.b', '.a')) %>% mutate(IDATE = iDateNext, INAME = indexName)
	midCapIQEx <- bind_rows(midCapIQEx, illqEx)
	midCapIQCo <- bind_rows(midCapIQCo, illqCo)
}

#midcap -> smallcap
indexName <- "MID CAP"
indexName2 <- "SMALL CAP"
midCapIQEx2 <- tibble()
midCapIQCo2 <- tibble()
for(i in 2:(length(indexChangeDates)-1)){
	iDate <- indexChangeDates[i-1]
	iDateNext <- indexChangeDates[i]
	iDateNext2 <- indexChangeDates[i + 1]
	
	oldSet <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName, iDate))[,1]
	newSet <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName, iDateNext))[,1]
	newSet2 <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName2, iDateNext))[,1]
	
	exits <- intersect(oldSet, newSet2)
	common <- intersect(oldSet, newSet)
	
	illiqBeforeEx <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(exits, collapse="','"), iDate, iDateNext))
	
	illiqAfterEx <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(exits, collapse="','"), iDateNext, iDateNext2))
										
	
	illiqBeforeCo <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(common, collapse="','"), iDate, iDateNext))
	
	illiqAfterCo <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(common, collapse="','"), iDateNext, iDateNext2))
	 
	illqEx <- illiqBeforeEx %>% inner_join(illiqAfterEx, by='SYM', suffix = c('.b', '.a')) %>% mutate(IDATE = iDateNext, INAME = indexName)
	illqCo <- illiqBeforeCo %>% inner_join(illiqAfterCo, by='SYM', suffix = c('.b', '.a')) %>% mutate(IDATE = iDateNext, INAME = indexName)
	midCapIQEx2 <- bind_rows(midCapIQEx2, illqEx)
	midCapIQCo2 <- bind_rows(midCapIQCo2, illqCo)
}

#smallcap -> midcap
indexName <- "SMALL CAP"
indexName2 <- "MID CAP"
smallCapIQEx <- tibble()
smallCapIQCo <- tibble()
for(i in 2:(length(indexChangeDates)-1)){
	iDate <- indexChangeDates[i-1]
	iDateNext <- indexChangeDates[i]
	iDateNext2 <- indexChangeDates[i + 1]
	
	oldSet <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName, iDate))[,1]
	newSet <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName, iDateNext))[,1]
	newSet2 <- sqlQuery(lcon, sprintf("select symbol from MF_MKT_CAP_CLASS where MKT_CAP_CLASS = '%s' and AS_OF = '%s'", indexName2, iDateNext))[,1]
	
	exits <- intersect(oldSet, newSet2)
	common <- intersect(oldSet, newSet)
	
	illiqBeforeEx <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(exits, collapse="','"), iDate, iDateNext))
	
	illiqAfterEx <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(exits, collapse="','"), iDateNext, iDateNext2))
										
	
	illiqBeforeCo <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(common, collapse="','"), iDate, iDateNext))
	
	illiqAfterCo <- sqlQuery(lcon, sprintf("select r.symbol SYM, avg(abs(100*daily_return)/tot_trd_qty) ILLIQ, count(*) CT from PX_HISTORY p, RETURN_SERIES_ALL r 
										where r.symbol in ('%s') and r.time_stamp >= '%s' and r.time_stamp <= '%s'
										and r.symbol = p.symbol and r.time_stamp = p.time_stamp
										and p.series in ('EQ', 'BE')
										group by r.symbol", paste(common, collapse="','"), iDateNext, iDateNext2))
	 
	illqEx <- illiqBeforeEx %>% inner_join(illiqAfterEx, by='SYM', suffix = c('.b', '.a')) %>% mutate(IDATE = iDateNext, INAME = indexName)
	illqCo <- illiqBeforeCo %>% inner_join(illiqAfterCo, by='SYM', suffix = c('.b', '.a')) %>% mutate(IDATE = iDateNext, INAME = indexName)
	smallCapIQEx <- bind_rows(smallCapIQEx, illqEx)
	smallCapIQCo <- bind_rows(smallCapIQCo, illqCo)
}

plotFn <- function(exitTb, commonTb, plotTitle, plotFileName){
	exitIllq <- exitTb %>% select(-INAME) %>% filter(CT.a >= 100 & CT.b >= 100) %>% group_by(IDATE) %>% summarize(A = median(100*ILLIQ.a/ILLIQ.b))
	commonIllq <- commonTb %>% select(-INAME) %>% filter(CT.a >= 100 & CT.b >= 100) %>% group_by(IDATE) %>% summarize(A = median(100*ILLIQ.a/ILLIQ.b))

	commonIllq %>% inner_join(exitIllq, by = c('IDATE'), suffix = c('.1', '.2')) %>% rename(common = A.1, exit = A.2) %>% pivot_longer(cols = c(common, exit), names_to = 'IQR', values_to = 'A') %>% {
		ggplot(., aes(x = factor(IDATE), y = A, color = factor(IQR), group = factor(IDATE))) +
			theme_economist() +
			geom_point(size=10, position=position_jitterdodge(jitter.width=0.5)) +
			geom_line() +
			scale_color_viridis_d() +
			scale_y_continuous(labels = label_number(accuracy = 0.1)) +
			labs(x='', y='change in illiquidity (%)', color='', fill='', shape='', title=plotTitle, caption = "@StockViz")
	}
	ggsave(sprintf("%s/%s.simple.png", reportPath, plotFileName), width=16, height=8, units="in")	
}

plotFn(largeCapIQEx, largeCapIQCo, "Illiquidty Impact: Large-cap Exits", "illiquidity.large-cap-exit")
plotFn(midCapIQEx, midCapIQCo, "Illiquidty Impact: Midcap-cap to Large-cap Promotion", "illiquidity.mid-cap-to-large-cap")
plotFn(midCapIQEx2, midCapIQCo2, "Illiquidty Impact: Midcap-cap to Small-cap Demotion", "illiquidity.mid-cap-to-small-cap")
plotFn(smallCapIQEx, smallCapIQCo, "Illiquidty Impact: Small-cap to Mid-cap Promotion", "illiquidity.small-cap-to-midcap-cap")
