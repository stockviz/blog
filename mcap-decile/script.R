library('RODBC')
library('RPostgres')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('igraph')
library('ggthemes')
library('patchwork')
library('viridis')

pdf(NULL)
reportPath <- "."
options(stringsAsFactors = FALSE)
options("scipen"=100)

source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
pcon <- dbConnect(RPostgres::Postgres(), host=ldbserver2, user= ldbuser2, password=ldbpassword2, dbname=ldbname2)

startDate <- as.Date("2020-01-15")
endDate <- sqlQuery(lcon, "select max(time_stamp) from EQUITY_MISC_INFO")[[1]]

indices <- c("NIFTY 100", "NIFTY MIDCAP 150", "NIFTY SMALLCAP 250", "NIFTY MICROCAP 250")
indexVals <- sqlQuery(lcon, sprintf("select * from INDEX_NSE_VALUATION where index_name in ('%s') and time_stamp >= '%s' and time_stamp <= '%s'", paste(indices, collapse="','"), '2021-05-11', endDate))

ggplot(indexVals, aes(x=TIME_STAMP, y=PE, color = INDEX_NAME)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	geom_line(linewidth = 1.5) +
	scale_color_viridis_d() +
	scale_x_date(breaks = "3 months", date_labels="%b-%Y") +
	labs(x = "", y="P/E ratio", fill="", color="", size="", 
		title="Index P/E ratio", subtitle=sprintf("%s:%s; source: NSE", min(indexVals$TIME_STAMP), max(indexVals$TIME_STAMP))) 

ggsave(sprintf("%s/index-pe.png", reportPath), width=12, height=6, units="in")			

earningsBegin1 <- as.Date('2020-01-01')
earningsBegin2 <- as.Date('2021-01-01')
earningsEnd1 <- as.Date('2024-01-01')
earningsEnd2 <- as.Date('2025-01-01')

mktCaps <- sqlQuery(lcon, sprintf("select s1.SYMBOL, s1.MKT_CAP_CR CAP_START, s2.MKT_CAP_CR*100 CAP_END from EQUITY_MISC_INFO s1, EQUITY_MISC_INFO s2
									where s1.SYMBOL = s2.SYMBOL
									and s1.TIME_STAMP = '%s'
									and s2.TIME_STAMP = '%s'", startDate, endDate))
		
mktCaps$DECILE_START <- ntile(mktCaps$CAP_START, 10)
mktCaps$DECILE_END <- ntile(mktCaps$CAP_END, 10)


mktCapFf <- sqlQuery(lcon, sprintf("select s1.SYMBOL, s1.MKT_CAP_CR, s1.FF_MKT_CAP_CR from EQUITY_MISC_INFO s1 where s1.TIME_STAMP = '%s'", startDate))
		
mktCapFf$DECILE_START <- ntile(mktCapFf$MKT_CAP_CR, 10)

ggplot() +
	theme_economist() +
	geom_bar(data = mktCapFf %>% group_by(DECILE_START) %>% summarize(FULL_FLOAT = sum(MKT_CAP_CR), FREE_FLOAT = sum(FF_MKT_CAP_CR)) %>% pivot_longer(-DECILE_START),
		mapping = aes(x=factor(DECILE_START), y = value, fill = name), stat="identity", position=position_dodge()) +
	scale_y_sqrt() + 
	geom_text(data = mktCapFf %>% group_by(DECILE_START) %>% summarize(FULL_FLOAT = sum(MKT_CAP_CR), PCT = 100*sum(FF_MKT_CAP_CR)/sum(MKT_CAP_CR)), 
		mapping = aes(x = factor(DECILE_START), y = FULL_FLOAT, label = sprintf("%.2f%%", PCT))) +
	scale_fill_viridis_d() +
	labs(x = "", y="sqrt market-cap (Rs. crores)", fill="", color="", size="", 
		title="Market-cap by Decile", subtitle=sprintf("2020 Jan; N = %d", nrow(mktCapFf)))

ggsave(sprintf("%s/decile-free.full-float.png", reportPath), width=12, height=6, units="in")	

mktCaps$EBIT_START <- NA
mktCaps$EBIT_END <- NA
for(i in 1:nrow(mktCaps)){
	sym <- mktCaps$SYMBOL[i]

	ebitaConsolBegin <- sqlQuery(lcon, sprintf("select avg(item_val) from FUNDA_XBRL where ITEM_KEY = 'EBIT' and PERIOD_TYPE='YEARLY' and IS_AUDITED=1 and IS_CONSOLIDATED=1 
										and symbol='%s' and period_end >= '%s' and period_end < '%s'", 
											sym, earningsBegin1, earningsBegin2))
	ebitaStdAlBegin <- sqlQuery(lcon, sprintf("select avg(item_val) from FUNDA_XBRL where ITEM_KEY = 'EBIT' and PERIOD_TYPE='YEARLY' and IS_AUDITED=1 and IS_CONSOLIDATED=0 
										and symbol='%s' and period_end >= '%s' and period_end < '%s'", 
											sym, earningsBegin1, earningsBegin2))
	ebitaBegin <- sqlQuery(lcon, sprintf("select avg(item_val) from FUNDA_XBRL where ITEM_KEY = 'EBIT' and PERIOD_TYPE='YEARLY' and IS_AUDITED=0 and IS_CONSOLIDATED=0 
										and symbol='%s' and period_end >= '%s' and period_end < '%s'", 
											sym, earningsBegin1, earningsBegin2))
	
	ebitaConsolEnd <- sqlQuery(lcon, sprintf("select avg(item_val) from FUNDA_XBRL where ITEM_KEY = 'EBIT' and PERIOD_TYPE='YEARLY' and IS_AUDITED=1 and IS_CONSOLIDATED=1 
										and symbol='%s' and period_end >= '%s' and period_end < '%s'", 
											sym, earningsEnd1, earningsEnd2))
	ebitaStdAlEnd <- sqlQuery(lcon, sprintf("select avg(item_val) from FUNDA_XBRL where ITEM_KEY = 'EBIT' and PERIOD_TYPE='YEARLY' and IS_AUDITED=1 and IS_CONSOLIDATED=0 
										and symbol='%s' and period_end >= '%s' and period_end < '%s'", 
											sym, earningsEnd1, earningsEnd2))
	ebitaEnd <- sqlQuery(lcon, sprintf("select avg(item_val) from FUNDA_XBRL where ITEM_KEY = 'EBIT' and PERIOD_TYPE='YEARLY' and IS_AUDITED=0 and IS_CONSOLIDATED=0 
										and symbol='%s' and period_end >= '%s' and period_end < '%s'", 
											sym, earningsEnd1, earningsEnd2))
	
	mktCaps$EBIT_START[i] <- ifelse(!is.na(ebitaConsolBegin[[1]]), ebitaConsolBegin[[1]], ifelse(!is.na(ebitaStdAlBegin[[1]]), ebitaStdAlBegin[[1]], ifelse(!is.na(ebitaBegin[[1]]) == 1, ebitaBegin[[1]], NA)))
	mktCaps$EBIT_END[i] <- ifelse(!is.na(ebitaConsolEnd[[1]]), ebitaConsolEnd[[1]], ifelse(!is.na(ebitaStdAlEnd[[1]]), ebitaStdAlEnd[[1]], ifelse(!is.na(ebitaEnd[[1]]) == 1, ebitaEnd[[1]], NA)))
}

mktCaps$EBIT_START <- as.numeric(mktCaps$EBIT_START)
mktCaps$EBIT_END <- as.numeric(mktCaps$EBIT_END)

write.csv(mktCaps, file=sprintf("%s/data.csv", reportPath), row.names = FALSE)

ebitChanges <- mktCaps %>% filter(DECILE_START >= 1 & DECILE_START <= 10 & DECILE_END >= 1 & DECILE_END <= 10 & !endsWith(SYMBOL, 'DVR')) %>% 
			group_by(DECILE_START) %>% 
			summarize(EBIT_BEGIN = sum(EBIT_START, na.rm = TRUE), EBIT_FINISH = sum(EBIT_END, na.rm = TRUE), CTR_BEGIN = sum(!is.na(EBIT_START)), CTR_END = sum(!is.na(EBIT_END))) %>%
			mutate(EBIT_CHG = 100*(EBIT_FINISH - EBIT_BEGIN)/ abs(EBIT_BEGIN))
			
p1 <- ebitChanges %>% select(DECILE_START, EBIT_BEGIN, EBIT_FINISH) %>% pivot_longer(cols = -DECILE_START) %>% 
				ggplot(aes(x=factor(DECILE_START), y=value/10000000000000000, fill=name)) +
					theme_economist() +
					geom_bar(stat="identity", position=position_dodge()) +
					scale_fill_viridis_d() +
					labs(x = "", y="EBIT (Rs. '000,000,000,000)", fill="", color="", size="", 
						title="Total EBIT by Decile", subtitle=sprintf("2020 Jan - 2024 August; N = %d", nrow(mktCaps))) 
			
p2 <- ebitChanges %>% select(DECILE_START, EBIT_CHG, CTR_BEGIN, CTR_END) %>% 
				ggplot(aes(x=factor(DECILE_START), y = EBIT_CHG, label = sprintf('%.2f%%\n(%d/%d)', EBIT_CHG, CTR_BEGIN, CTR_END))) +
				theme_economist() +
				geom_bar(stat="identity", fill = (viridis_pal()(2))[2]) +
				geom_text(vjust = 1) +
				scale_fill_viridis_d() +
				labs(x = "market-cap deciles", y="change (%)", fill="", color="", size="", 
					title="Changes in Total EBIT by Decile", subtitle=sprintf("2020 Jan - 2024 August; N = %d", nrow(mktCaps)), caption='@StockViz') 

p1 + p2 + plot_layout(nrow=2)		
ggsave(sprintf("%s/decile-ebit.png", reportPath), width=12, height=12, units="in")			

capChanges <- mktCaps %>% filter(DECILE_START >= 1 & DECILE_START <= 10 & DECILE_END >= 1 & DECILE_END <= 10) %>% 
			group_by(DECILE_START) %>% 
			summarize(CAP_BEGIN = sum(CAP_START, na.rm = TRUE), CAP_FINISH = sum(CAP_END, na.rm = TRUE), CTR_BEGIN = sum(!is.na(EBIT_START)), CTR_END = sum(!is.na(EBIT_END))) %>%
			mutate(CAP_CHG = 100*(CAP_FINISH/CAP_BEGIN - 1))
		
p1 <- capChanges %>% select(DECILE_START, CAP_BEGIN, CAP_FINISH) %>% pivot_longer(cols=-DECILE_START) %>%		
	ggplot(aes(x=factor(DECILE_START), y=value, fill=name)) +
		theme_economist() +
		geom_bar(stat="identity", position=position_dodge()) +
		scale_y_sqrt() + 
		scale_fill_viridis_d() +
		labs(x = "", y="sqrt market-cap (Rs. crores)", fill="", color="", size="", 
			title="Total Market-cap by Decile", subtitle=sprintf("2020 Jan - 2024 August; N = %d", nrow(mktCaps))) 
		
p2 <- capChanges %>% select(DECILE_START, CAP_CHG, CTR_BEGIN, CTR_END) %>% 
	ggplot(aes(x=factor(DECILE_START), y = CAP_CHG, label = sprintf('%.2f%%\n(%d/%d)', CAP_CHG, CTR_BEGIN, CTR_END))) +
	theme_economist() +
	geom_bar(stat="identity", fill = (viridis_pal()(2))[2]) +
	geom_text(vjust = 1) +
	scale_y_sqrt() + 
	scale_fill_viridis_d() +
	labs(x = "market-cap deciles", y="sqrt change (%)", fill="", color="", size="", 
		title="Changes in Total Market-cap by Decile", subtitle=sprintf("2020 Jan - 2024 August; N = %d", nrow(mktCaps)), caption='@StockViz') 

p1 + p2 + plot_layout(nrow=2)		
ggsave(sprintf("%s/decile-cap.png", reportPath), width=12, height=12, units="in")
		
capTransition <- mktCaps %>% filter(DECILE_START >= 1 & DECILE_START <= 10 & DECILE_END >= 1 & DECILE_END <= 10) %>% 
				filter(DECILE_END > DECILE_START) %>%
				group_by(DECILE_START, DECILE_END) %>%
				summarize(CNT = n())
				
gDf <- capTransition %>% select(DECILE_START, DECILE_END) %>% as.data.frame()
nDf <- data.frame(node = 1:10)
dGrph <- graph_from_data_frame(d = gDf, vertices = nDf, directed = TRUE)

png(sprintf("%s/cap-transitions.png", reportPath), width=1000, height=800, bg="white")
plot(dGrph, layout = layout_in_circle(dGrph),
			edge.label = capTransition$CNT, 
			edge.width=capTransition$CNT/mean(capTransition$CNT), 
			edge.curved=TRUE, 
			edge.arrow.size=0.5,
			edge.color = viridis_pal()(nrow(capTransition)),
			edge.label.family = 'sans',
			edge.label.color = 'black',
			vertex.label.family = 'sans', 
			vertex.label.font=2,
			vertex.color = viridis_pal(alpha=0.5)(10))
title('Market-cap Decile Transitions', sub = sprintf("2020 Jan - 2024 August; N = %d", nrow(mktCaps)), family='sans', cex.main=2)
mtext("@StockViz", side=4, col='grey')
dev.off()