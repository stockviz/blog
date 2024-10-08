library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('viridis')
library('patchwork')

pdf(NULL)
reportPath <- "."
options(stringsAsFactors = FALSE)
options("scipen"=100)

source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

fomcDf <- read.table(sprintf("%s/fomc_dates.txt", reportPath), sep="\t", header=TRUE) #source: https://en.wikipedia.org/wiki/History_of_Federal_Open_Market_Committee_actions

fomcDf$DAY <- as.Date(fomcDf$DAY)
fomcDf$DISC_RATE <- as.numeric(fomcDf$DISC_RATE)

startDate <- min(fomcDf$DAY) - 100
endDate <- max(fomcDf$DAY) + 100

indexName <- "NIFTY BANK TR" #"NIFTY BANK TR" #"NIFTY 50 TR"

indexPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name = '%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
indexXts <- xts(indexPx[,2], indexPx[,1])
names(indexXts) <- c('INDEX')

dRet <- dailyReturn(indexXts)

fomcXts <- xts(fomcDf[,2], fomcDf[,1])
names(fomcXts) <- c('DISC_RATE')
fomcXts$CHG <- fomcXts$DISC_RATE - stats::lag(fomcXts$DISC_RATE, 1)


##############################################################
	
toPlot1 <- merge(indexXts, fomcXts$CHG)
toPlot1df <- data.frame(toPlot1)
toPlot1df$DATE_STAMP <- index(toPlot1)

toPlot2 <- na.omit(merge(indexXts, fomcXts))
toPlot2df <- data.frame(toPlot2)
toPlot2df$DATE_STAMP <- index(toPlot2)

ggplot(toPlot1df, aes(x=DATE_STAMP, y = INDEX)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line() +
	geom_point(data = toPlot2df, mapping = aes(color = DISC_RATE, size = CHG)) +
	scale_y_log10() +
	scale_fill_viridis() + 
	scale_color_viridis() +
	scale_x_date(breaks = "12 months", date_labels="%b-%Y") +
	labs(x = "", y=sprintf('log(%s)', indexName), fill="", color="rate", size="change", 
		title=sprintf("%s vs. FOMC Dates", indexName), subtitle=sprintf("%s:%s;", first(index(toPlot1)), last(index(toPlot1)))) 
		
ggsave(sprintf("%s/%s-index.fomc.png", reportPath, indexName), width=16, height=8, units="in")					

##############################################################

lbs <- c(5, 10, 20, 50)
fomcAnal <- data.frame(fomcXts)

for(lb in lbs){
	fomcAnal[, paste0('PRE_', lb)] <- NA
	fomcAnal[, paste0('POST_', lb)] <- NA
}

for(i in 1:nrow(fomcXts)){
	dt <- index(fomcXts)[i]
	for(lb in lbs){
		preRets <- tail(dRet[paste0('/', dt)], lb)
		if(nrow(preRets) > 0) fomcAnal[i, paste0('PRE_', lb)] <- Return.cumulative(preRets)
		
		postRets <- head(dRet[paste0(dt+1, '/')], lb)
		if(nrow(postRets) > 0) fomcAnal[i, paste0('POST_', lb)] <- Return.cumulative(postRets)
	}
}

fomcEffectDf <- fomcAnal
fomcEffectDf$DATE_STAMP <- index(fomcAnal)

write.csv(fomcEffectDf, file=sprintf("%s/%s.fomcEffect.returns.csv", reportPath, indexName))

for(lb in lbs){
	p1 <- fomcEffectDf %>% filter(CHG > 0) %>% select(c(DATE_STAMP, ends_with(paste0('_', lb)))) %>% rename_with(~str_remove(., paste0('_', lb))) %>% pivot_longer(-DATE_STAMP) %>%
		ggplot(., aes(x=factor(DATE_STAMP), y=100*value, fill=name)) + 
			theme_economist() +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			geom_bar(stat="identity", position=position_dodge()) +
			scale_fill_viridis_d() +
			labs(x = "", y='returns (%)', fill="", color="",  title="Hikes") 
				
	p2 <- fomcEffectDf %>% filter(CHG < 0) %>% select(c(DATE_STAMP, ends_with(paste0('_', lb)))) %>% pivot_longer(-DATE_STAMP) %>%
		ggplot(., aes(x=factor(DATE_STAMP), y=100*value, fill=name)) + 
			theme_economist() +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			geom_bar(stat="identity", position=position_dodge()) +
			scale_fill_viridis_d() +
			guides(fill='none') +
			labs(x = "", y='returns (%)', fill="", color="",  title="Cuts") 
				
	p3 <- fomcEffectDf %>% filter(CHG == 0) %>% select(c(DATE_STAMP, ends_with(paste0('_', lb)))) %>% pivot_longer(-DATE_STAMP) %>%
		ggplot(., aes(x=factor(DATE_STAMP), y=100*value, fill=name)) + 
			theme_economist() +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			geom_bar(stat="identity", position=position_dodge()) +
			scale_fill_viridis_d() +
			guides(fill='none') +
			labs(x = "", y='returns (%)', fill="", color="",  title='Holds') 

	p1 + p2 + p3 + plot_layout(ncol=1) + 
				plot_annotation(title = sprintf("%d-day %s Returns on FOMC Dates", lb, indexName), 
					subtitle = sprintf("[%s:%s]", first(index(fomcXts)), last(index(fomcXts))),
					caption='@StockViz') 

	ggsave(sprintf("%s/%s-index.returns.%d.png", reportPath, indexName, lb), width=12, height=6*3, units="in")					
}

for(lb in lbs){
	p1 <- fomcEffectDf %>% filter(CHG > 0) %>% select(ends_with(paste0('_', lb))) %>% rename_with(~str_remove(., paste0('_', lb))) %>%
		ggplot(., aes(x=100*PRE, y=100*POST, color=POST-PRE, size=POST-PRE)) + 
			theme_economist() +
			geom_point() +
			geom_hline(yintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			geom_vline(xintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			guides(color='none', size='none') +
			scale_color_viridis() +
			labs(x = "pre-FOMC returns", y='post-FOMC returns', fill="", color="",  title="Hikes") 
			
	p2 <- fomcEffectDf %>% filter(CHG < 0) %>% select(ends_with(paste0('_', lb))) %>% rename_with(~str_remove(., paste0('_', lb))) %>%
		ggplot(., aes(x=100*PRE, y=100*POST, color=POST-PRE, size=POST-PRE)) + 
			theme_economist() +
			geom_point() +
			geom_hline(yintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			geom_vline(xintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			guides(color='none', size='none') +
			scale_color_viridis() +
			labs(x = "pre-FOMC returns", y='post-FOMC returns', fill="", color="",  title="Cuts") 
			
	p3 <- fomcEffectDf %>% filter(CHG == 0) %>% select(ends_with(paste0('_', lb))) %>% rename_with(~str_remove(., paste0('_', lb))) %>%
		ggplot(., aes(x=100*PRE, y=100*POST, color=POST-PRE, size=POST-PRE)) + 
			theme_economist() +
			geom_point() +
			geom_hline(yintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			geom_vline(xintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			guides(color='none', size='none') +
			scale_color_viridis() +
			labs(x = "pre-FOMC returns", y='post-FOMC returns', fill="", color="",  title="Holds") 
			
	p1 + p2 + p3 + plot_layout(ncol=1) + 
				plot_annotation(title = sprintf("%d-day %s Returns around FOMC Dates", lb, indexName), 
					subtitle = sprintf("[%s:%s]", first(index(fomcXts)), last(index(fomcXts))),
					caption='@StockViz') 

	ggsave(sprintf("%s/%s-index.returns.pre-post.%d.png", reportPath, indexName, lb), width=12, height=6*3, units="in")					
}			

##############################################################

fomcAnal <- data.frame(fomcXts)

for(lb in lbs){
	fomcAnal[, paste0('PRE_', lb)] <- NA
	fomcAnal[, paste0('POST_', lb)] <- NA
}

for(i in 1:nrow(fomcXts)){
	dt <- index(fomcXts)[i]
	for(lb in lbs){
		preRets <- tail(dRet[paste0('/', dt)], lb)
		if(nrow(preRets) > 0) fomcAnal[i, paste0('PRE_', lb)] <- sd(preRets)
		
		postRets <- head(dRet[paste0(dt+1, '/')], lb)
		if(nrow(postRets) > 0) fomcAnal[i, paste0('POST_', lb)] <- sd(postRets)
	}
}

fomcEffectDf <- fomcAnal
fomcEffectDf$DATE_STAMP <- index(fomcAnal)

write.csv(fomcEffectDf, file=sprintf("%s/%s.fomcEffect.sd.csv", reportPath, indexName))

for(lb in lbs){
	p1 <- fomcEffectDf %>% filter(CHG > 0) %>% select(c(DATE_STAMP, ends_with(paste0('_', lb)))) %>% rename_with(~str_remove(., paste0('_', lb))) %>% pivot_longer(-DATE_STAMP) %>%
		ggplot(., aes(x=factor(DATE_STAMP), y=100*value, fill=name)) + 
			theme_economist() +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			geom_bar(stat="identity", position=position_dodge()) +
			scale_fill_viridis_d() +
			labs(x = "", y='sd x 100', fill="", color="",  title="Hikes") 
				
	p2 <- fomcEffectDf %>% filter(CHG < 0) %>% select(c(DATE_STAMP, ends_with(paste0('_', lb)))) %>% pivot_longer(-DATE_STAMP) %>%
		ggplot(., aes(x=factor(DATE_STAMP), y=100*value, fill=name)) + 
			theme_economist() +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			geom_bar(stat="identity", position=position_dodge()) +
			scale_fill_viridis_d() +
			guides(fill='none') +
			labs(x = "", y='sd x 100', fill="", color="",  title="Cuts") 
				
	p3 <- fomcEffectDf %>% filter(CHG == 0) %>% select(c(DATE_STAMP, ends_with(paste0('_', lb)))) %>% pivot_longer(-DATE_STAMP) %>%
		ggplot(., aes(x=factor(DATE_STAMP), y=100*value, fill=name)) + 
			theme_economist() +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			geom_bar(stat="identity", position=position_dodge()) +
			scale_fill_viridis_d() +
			guides(fill='none') +
			labs(x = "", y='sd x 100', fill="", color="",  title='Holds') 

	p1 + p2 + p3 + plot_layout(ncol=1) + 
				plot_annotation(title = sprintf("%d-day %s Volatility on FOMC Dates", lb, indexName), 
					subtitle = sprintf("[%s:%s]", first(index(fomcXts)), last(index(fomcXts))),
					caption='@StockViz') 

	ggsave(sprintf("%s/%s-index.sd.%d.png", reportPath, indexName, lb), width=12, height=6*3, units="in")					
}

for(lb in lbs){
	p1 <- fomcEffectDf %>% filter(CHG > 0) %>% select(ends_with(paste0('_', lb))) %>% rename_with(~str_remove(., paste0('_', lb))) %>%
		ggplot(., aes(x=100*PRE, y=100*POST, color=POST-PRE, size=POST-PRE)) + 
			theme_economist() +
			geom_point() +
			guides(color='none', size='none') +
			scale_color_viridis() +
			labs(x = "pre-FOMC sd", y='post-FOMC sd', fill="", color="",  title="Hikes") 
			
	p2 <- fomcEffectDf %>% filter(CHG < 0) %>% select(ends_with(paste0('_', lb))) %>% rename_with(~str_remove(., paste0('_', lb))) %>%
		ggplot(., aes(x=100*PRE, y=100*POST, color=POST-PRE, size=POST-PRE)) + 
			theme_economist() +
			geom_point() +
			guides(color='none', size='none') +
			scale_color_viridis() +
			labs(x = "pre-FOMC sd", y='post-FOMC sd', fill="", color="",  title="Cuts") 
			
	p3 <- fomcEffectDf %>% filter(CHG == 0) %>% select(ends_with(paste0('_', lb))) %>% rename_with(~str_remove(., paste0('_', lb))) %>%
		ggplot(., aes(x=100*PRE, y=100*POST, color=POST-PRE, size=POST-PRE)) + 
			theme_economist() +
			geom_point() +
			guides(color='none', size='none') +
			scale_color_viridis() +
			labs(x = "pre-FOMC sd", y='post-FOMC sd', fill="", color="",  title="Holds") 
			
	p1 + p2 + p3 + plot_layout(ncol=1) + 
				plot_annotation(title = sprintf("%d-day %s Volatility around FOMC Dates", lb, indexName), 
					subtitle = sprintf("[%s:%s]", first(index(fomcXts)), last(index(fomcXts))),
					caption='@StockViz') 

	ggsave(sprintf("%s/%s-index.sd.pre-post.%d.png", reportPath, indexName, lb), width=12, height=6*3, units="in")					
}			
