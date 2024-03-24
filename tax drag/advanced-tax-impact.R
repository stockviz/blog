library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('lubridate')
library('tidyverse')
library('ggthemes')
library('viridis')
library('cowplot')

reportPath<-"."

pdf(NULL)

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indices <- c('NIFTY200 MOMENTUM 30 TR', 'NIFTY MIDCAP150 MOMENTUM 50 TR')

stDateStr1 <- '%s-04-01/%s-03-01'
stDateStr2 <- '%s-03-02/%s-03-14'

statsDf <- data.frame(INDEX = "", Y = 0, PRE_RET = 0.0, MAR_RET = 0.0)
statsDf2 <- tibble()
for(i in 1:length(indices)){
	indexName <- indices[i]
	pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '2005-01-01'", indexName))
	
	dRet <- dailyReturn(xts(pDf[,2], pDf[,1]))
	startDate <- min(pDf$time_stamp)
	endDate <- max(pDf$time_stamp)
	yrs <- sort(unique(year(index(dRet))))
	for(j in 2:length(yrs)){
		preRet <- Return.cumulative(dRet[sprintf(stDateStr1, yrs[j-1], yrs[j])])
		marRet <- Return.cumulative(dRet[sprintf(stDateStr2, yrs[j], yrs[j])])
		
		statsDf <- rbind(statsDf, c(indexName, yrs[j], 100*preRet, 100*marRet))
	}
	
	stedDays <- pDf %>% mutate(D = day(time_stamp), M = month(time_stamp), Y = year(time_stamp)) %>% filter(D >= 1 & D <= 14) %>% group_by(Y, M) %>% summarize(D1 = min(D), D2 = max(D)) 
	w2Rets <- pDf %>% mutate(D = day(time_stamp), M = month(time_stamp), Y = year(time_stamp)) %>% inner_join(stedDays, by=c('M', 'Y')) %>% filter(D == D1 | D == D2) %>% 
		mutate(P = ifelse(D == D1, "F", "L")) %>% select(px_close, P, M, Y) %>% 
		pivot_wider(id_cols=c(Y, M), names_from=c(P), values_from=c(px_close)) %>% mutate(RET = L/F - 1)
		
	statsDf2 <- bind_rows(statsDf2, w2Rets %>% mutate(INDEX = indexName) %>% select(INDEX, M, RET))
}

statsDf <- statsDf[-1,]

#write.csv(statsDf, sprintf("%s/advanced-tax-impact.momentum-tr.csv", reportPath), row.names = FALSE)

p1 <- statsDf %>% mutate(PRE_RET = round(as.numeric(PRE_RET), 2), MAR_RET = round(as.numeric(MAR_RET), 2), Y = factor(Y, levels=unique(Y)), INDEX = factor(INDEX, levels=unique(INDEX))) %>%
	ggplot(aes(x=PRE_RET, y=MAR_RET, color=Y, shape=INDEX)) +
		theme_economist() +
		geom_point(stroke=NA, size=5) +
		scale_color_viridis_d() +
		geom_hline(yintercept = 0, color = 'red') +
		geom_vline(xintercept = 0, color = 'red') +
		labs(y='March advance-tax weeks returns (%)', x='April - March returns (%)', color='', fill='', shape='', title='Momentum Index Returns', caption = '@StockViz')
		
color_legend <- cowplot::get_legend(p1 + guides(shape='none') + theme(legend.position = 'right'))

plot_grid(plotlist = list(p1 + guides(color='none'), color_legend), rel_widths =  c(20, 1)) + theme_economist()

ggsave(sprintf("%s/advanced-tax-impact.momentum-tr.png", reportPath), width=16, height=8, units="in")		

################## first 2 weeks of all months

ggplot(statsDf2, aes(x=factor(month.abb[M], levels=month.abb), y=100*RET, fill=INDEX, color=INDEX)) +
	theme_economist() +
	geom_violin(alpha=0.5) +
	geom_point(position = position_jitter(seed = 1, width = 0.2)) +
	scale_color_viridis_d() +
	scale_fill_viridis_d() +
	labs(y='First two week returns (%)', x='months', color='', fill='', title='Momentum Index First 2-weeks Returns', subtitle=sprintf("by month; %s:%s", startDate, endDate), caption = '@StockViz')
	
ggsave(sprintf("%s/months-2wks.momentum-tr.png", reportPath), width=16, height=8, units="in")			