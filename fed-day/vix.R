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

indexName <- "INDIA VIX"

vixPx <- sqlQuery(lcon, sprintf("select time_stamp, px_close from VIX_HISTORY where time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
vixXts <- xts(vixPx[,2], vixPx[,1])
names(vixXts) <- c('VIX')

dRet <- dailyReturn(vixXts)

fomcXts <- xts(fomcDf[,2], fomcDf[,1])
names(fomcXts) <- c('DISC_RATE')
fomcXts$CHG <- fomcXts$DISC_RATE - stats::lag(fomcXts$DISC_RATE, 1)

toPlot <- merge(vixXts, fomcXts$DISC_RATE)
toPlot$DISC_RATE <- na.locf(toPlot$DISC_RATE)

toPlotDf <- data.frame(toPlot$DISC_RATE)
toPlotDf$DATE_STAMP <- index(toPlot)

ggplot(toPlotDf, aes(x = DATE_STAMP, y = DISC_RATE)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line() +
	scale_x_date(breaks = "12 months", date_labels="%b-%Y") +
	labs(x = "", y="Discount Rate (%)", title="FOMC Discount Rates", subtitle=sprintf("%s:%s;", first(index(toPlot)), last(index(toPlot)))) 
		
ggsave(sprintf("%s/fomc.png", reportPath), width=16, height=8, units="in")					

##############################################################
	
toPlot1 <- na.trim(merge(vixXts, fomcXts$CHG), side='left')
toPlot1df <- data.frame(toPlot1)
toPlot1df$DATE_STAMP <- index(toPlot1)

toPlot2 <- na.omit(merge(vixXts, fomcXts))
toPlot2df <- data.frame(toPlot2)
toPlot2df$DATE_STAMP <- index(toPlot2)

ggplot(toPlot1df, aes(x=DATE_STAMP, y = VIX)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line() +
	geom_point(data = toPlot2df, mapping = aes(color = DISC_RATE, size = CHG)) +
	scale_y_log10() +
	scale_fill_viridis() + 
	scale_color_viridis() +
	scale_x_date(breaks = "12 months", date_labels="%b-%Y") +
	labs(x = "", y=sprintf('log(%s)', indexName), fill="", color="rate", size="change", 
		title="INDIA VIX vs. FOMC Dates", subtitle=sprintf("%s:%s;", first(index(toPlot1)), last(index(toPlot1)))) 
		
ggsave(sprintf("%s/%s-index.fomc.png", reportPath, indexName), width=16, height=8, units="in")					

##############################################################

lbs <- c(5, 10, 20, 50)
fomcAnalXts <- fomcXts[paste0(first(index(vixXts)), '/'),]
fomcAnal <- data.frame(fomcAnalXts)

for(lb in lbs){
	fomcAnal[, paste0('PRE_', lb)] <- NA
	fomcAnal[, paste0('POST_', lb)] <- NA
}

for(i in 1:nrow(fomcAnalXts)){
	dt <- index(fomcAnalXts)[i]
	for(lb in lbs){
		preRets <- tail(dRet[paste0('/', dt)], lb)
		if(nrow(preRets) > 0) fomcAnal[i, paste0('PRE_', lb)] <- Return.cumulative(preRets)
		
		postRets <- head(dRet[paste0(dt+1, '/')], lb)
		if(nrow(postRets) > 0) fomcAnal[i, paste0('POST_', lb)] <- Return.cumulative(postRets)
	}
}

fomcEffectDf <- fomcAnal
fomcEffectDf$DATE_STAMP <- index(fomcAnalXts)

write.csv(fomcEffectDf, file=sprintf("%s/%s.fomcEffect.returns.csv", reportPath, indexName))

for(lb in lbs){
	p1 <- fomcEffectDf %>% filter(CHG > 0) %>% select(c(DATE_STAMP, ends_with(paste0('_', lb)))) %>% rename_with(~str_remove(., paste0('_', lb))) %>% pivot_longer(-DATE_STAMP) %>% 
		ggplot(aes(x=factor(DATE_STAMP), y=100*value, fill=name)) + 
			theme_economist() +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			geom_bar(stat="identity", position=position_dodge()) +
			scale_fill_viridis_d() +
			labs(x = "", y='change (%)', fill="", color="",  title="Hikes") 
				
	p2 <- fomcEffectDf %>% filter(CHG < 0) %>% select(c(DATE_STAMP, ends_with(paste0('_', lb)))) %>% pivot_longer(-DATE_STAMP) %>%
		ggplot(aes(x=factor(DATE_STAMP), y=100*value, fill=name)) + 
			theme_economist() +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			geom_bar(stat="identity", position=position_dodge()) +
			scale_fill_viridis_d() +
			guides(fill='none') +
			labs(x = "", y='change (%)', fill="", color="",  title="Cuts") 
				
	p3 <- fomcEffectDf %>% filter(CHG == 0) %>% select(c(DATE_STAMP, ends_with(paste0('_', lb)))) %>% pivot_longer(-DATE_STAMP) %>%
		ggplot(aes(x=factor(DATE_STAMP), y=100*value, fill=name)) + 
			theme_economist() +
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
			geom_bar(stat="identity", position=position_dodge()) +
			scale_fill_viridis_d() +
			guides(fill='none') +
			labs(x = "", y='change (%)', fill="", color="",  title='Holds') 

	p1 + p2 + p3 + plot_layout(ncol=1) + 
				plot_annotation(title = sprintf("%d-day %s change on FOMC Dates", lb, indexName), 
					subtitle = sprintf("[%s:%s]", first(index(vixXts)), last(index(vixXts))),
					caption='@StockViz') 

	ggsave(sprintf("%s/%s-index.change.%d.png", reportPath, indexName, lb), width=12, height=6*3, units="in")					
}

for(lb in lbs){
	p1 <- fomcEffectDf %>% filter(CHG > 0) %>% select(ends_with(paste0('_', lb))) %>% rename_with(~str_remove(., paste0('_', lb))) %>%
		ggplot(aes(x=100*PRE, y=100*POST, color=POST-PRE, size=POST-PRE)) + 
			theme_economist() +
			geom_point() +
			geom_hline(yintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			geom_vline(xintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			guides(color='none', size='none') +
			scale_color_viridis() +
			labs(x = "pre-FOMC change", y='post-FOMC change', fill="", color="",  title="Hikes") 
			
	p2 <- fomcEffectDf %>% filter(CHG < 0) %>% select(ends_with(paste0('_', lb))) %>% rename_with(~str_remove(., paste0('_', lb))) %>%
		ggplot(aes(x=100*PRE, y=100*POST, color=POST-PRE, size=POST-PRE)) + 
			theme_economist() +
			geom_point() +
			geom_hline(yintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			geom_vline(xintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			guides(color='none', size='none') +
			scale_color_viridis() +
			labs(x = "pre-FOMC change", y='post-FOMC change', fill="", color="",  title="Cuts") 
			
	p3 <- fomcEffectDf %>% filter(CHG == 0) %>% select(ends_with(paste0('_', lb))) %>% rename_with(~str_remove(., paste0('_', lb))) %>%
		ggplot(aes(x=100*PRE, y=100*POST, color=POST-PRE, size=POST-PRE)) + 
			theme_economist() +
			geom_point() +
			geom_hline(yintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			geom_vline(xintercept=0, linetype="dashed", color = "grey", linewidth=1) +
			guides(color='none', size='none') +
			scale_color_viridis() +
			labs(x = "pre-FOMC change", y='post-FOMC change', fill="", color="",  title="Holds") 
			
	p1 + p2 + p3 + plot_layout(ncol=1) + 
				plot_annotation(title = sprintf("%d-day %s Changes around FOMC Dates", lb, indexName), 
					subtitle = sprintf("[%s:%s]", first(index(vixXts)), last(index(vixXts))),
					caption='@StockViz') 

	ggsave(sprintf("%s/%s-index.change.pre-post.%d.png", reportPath, indexName, lb), width=12, height=6*3, units="in")					
}			

##############################################################
