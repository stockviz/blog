source("d:/stockviz/r/config.r")

reportPath<-"."

library('RODBC')
library('quantmod')
library('lubridate')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('viridis')

pdf(NULL)
options(stringsAsFactors = FALSE)
options("scipen"=100)

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

symbol <- "NIFTY"
indexName <- "NIFTY 50"
dte <- 5 #days
numStrikes <- 7
startDate <- as.Date("2020-05-01")
	
genData <- function(){
	iDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s'", indexName, startDate))
	iXts <- xts(iDf[,-1], iDf[,1])

	dates <- index(iXts)

	print(sprintf("%d dates", length(dates)))

	ivData <- data.frame(T = Sys.Date(), strike = 0, iv = 0.0)
	for(i in 1:length(dates)){
		d <- dates[i]
		#cat(paste(i, d, "..."))
		
		iLevel <- as.numeric(iXts[d])
		
		tryCatch({
			expiry <- sqlQuery(lcon, sprintf("select min(expiry_dt) from BHAV_EQ_OPT where symbol='%s' and time_stamp='%s' and expiry_dt >= '%s'", symbol, d, d + dte))[[1]]
			
			strikesUp <- sqlQuery(lcon, sprintf("select top %d strike_pr from BHAV_EQ_OPT where symbol='%s' and time_stamp='%s' and expiry_dt = '%s' and strike_pr >= %f  
													and convert(int, strike_pr) %% 100 = 0
													and option_typ = 'CE' order by strike_pr",
												numStrikes, symbol, d, expiry, iLevel))[,1]
													
			strikesDn <- sqlQuery(lcon, sprintf("select top %d strike_pr from BHAV_EQ_OPT where symbol='%s' and time_stamp='%s' and expiry_dt = '%s' and strike_pr < %f 
													and convert(int, strike_pr) %% 100 = 0 
													and option_typ = 'PE' order by strike_pr desc",
												numStrikes, symbol, d, expiry, iLevel))[,1]
												
			ivUp <- sqlQuery(lcon, sprintf("select strike, 100*iv as iv from EQ_OPTION_GREEKS where symbol='%s' and time_stamp='%s' and expiry_date = '%s' 
													and strike in (%s)
													and option_type = 'CE' order by strike desc",
												symbol, d, expiry, paste(strikesUp, collapse=",")))
												
			if(nrow(ivUp) == 0) next
												
			ivDn <- sqlQuery(lcon, sprintf("select strike, 100*iv as iv from EQ_OPTION_GREEKS where symbol='%s' and time_stamp='%s' and expiry_date = '%s' 
													and strike in (%s)
													and option_type = 'PE' order by strike desc",
												symbol, d, expiry, paste(strikesDn, collapse=",")))
												
			if(nrow(ivDn) == 0) next
												
			ivUp$strike <- seq(-numStrikes, -1, by=1)
			ivDn$strike <- seq(1, length.out=numStrikes, by=1)
			
			ivUp$T <- rep(d, nrow(ivUp))
			ivDn$T <- rep(d, nrow(ivDn))
			
			ivData <- rbind(ivData, rbind(ivUp, ivDn))
			
			rbind(ivUp, ivDn) %>% {
				ggplot(., aes(x=strike, y=iv)) +
					theme_economist() +
					theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
					geom_line() +
					scale_y_continuous(breaks = seq.int(as.integer(min(.$iv)), as.integer(max(.$iv)), by=5)) +
					scale_x_continuous(n.breaks=2*numStrikes+1) +
					labs(x="", y="", title=sprintf("%s Implied Volatility vs. Strikes", symbol), subtitle = sprintf("%s", d), caption='@StockViz') 
			}
			
			ggsave(sprintf("%s/%s.iv-strike.%04d.png", reportPath, symbol, i), width=12, height=6, units="in", dpi=150)
		}, error = function(e) { print(paste(e, ":", i, d, "failed!!!")) })
	}

	ivData <- ivData[-1,]
	save(ivData, file = sprintf("%s/%s.iv-strike.Rdata", reportPath, symbol))
}

#genData()
#magick.exe convert -monitor -delay 1 NIFTY.iv-strike.*.png NIFTY.iv-strike.gif
#q()

load(sprintf("%s/%s.iv-strike.Rdata", reportPath, symbol)) #ivData

ivDt <- ivData %>% pivot_wider(id_cols = c(T), names_from=strike, names_prefix = "S", values_from = iv)


ivDiffs <- (ivData %>% filter(strike > 0)) %>% inner_join(ivData %>% filter(strike < 0) %>% mutate(strike = -strike), by=join_by(T, strike)) %>% mutate(ivr = iv.x/iv.y) %>% select(T, strike, ivr)

minT1 <- (ivDt %>% mutate(S77 = S7/`S-7`) %>% filter(S77 == min(S77)) %>% select(T))[[1]]
minT2 <- (ivDt %>% mutate(S37 = S3/`S-3`) %>% filter(S37 == min(S37)) %>% select(T))[[1]]
minT3 <- (ivDt %>% mutate(S17 = S1/`S-7`) %>% filter(S17 == min(S17)) %>% select(T))[[1]]

ivData %>% filter(T %in% c(minT1, minT2, minT3)) %>% mutate(T = factor(T)) %>%
	ggplot(aes(x=strike, y=iv, color = T, group = T)) +
	theme_economist() +
	geom_line(linewidth=1) +
	scale_x_continuous(n.breaks=2*numStrikes+1) +
	labs(x="strike dist.", y="iv", color="", title=sprintf("%s Implied Volatility vs. Strikes", symbol), subtitle = sprintf("%s:%s", min(ivData$T), max(ivData$T)), caption='@StockViz') 

ggsave(sprintf("%s/%s.iv-strike.01.png", reportPath, symbol), width=12, height=6, units="in")

toPlot <- melt(ivDiffs %>% select(-T), id='strike')

ivDiffs %>% select(-T) %>% mutate(strike = factor(strike)) %>%
	ggplot(aes(x=ivr, group=strike, color=strike)) + 
		theme_economist() +
		stat_density(geom = "line", linewidth=1) +
		geom_vline(xintercept = 1, linetype="dashed") +
		scale_color_viridis_d() +
		labs(x="p/c iv ratio", y="density", color='strike dist.', title=sprintf("%s Put/Call Implied Volatility Ratio", symbol), subtitle = sprintf("%s:%s", min(ivData$T), max(ivData$T)), caption='@StockViz') 

ggsave(sprintf("%s/%s.iv-strike.density.png", reportPath, symbol), width=12, height=8, units="in")		