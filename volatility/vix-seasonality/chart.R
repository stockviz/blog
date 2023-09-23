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

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

indexDf <- sqlQuery(lcon, "select px_close, time_stamp from VIX_HISTORY")
iXts <- xts(indexDf[,1], indexDf[,2])

indexDf %>% arrange(time_stamp) %>% {
	ggplot(., aes(x=month(time_stamp), y=px_close, group=month(time_stamp))) +
	theme_economist() +
	geom_violin() +
	geom_boxplot(width=0.1) +
	scale_x_continuous(breaks = seq(1, 12, by=1), labels = month(seq(1, 12, by=1), label=T)) +
	labs(x="", y="VIX", title="INDIA VIX by month") +
	annotate("text", x=12, y=max(.$px_close), label = "@StockViz", hjust='right', vjust='top', col="white", cex=6, fontface = "bold", alpha = 0.8) 
}

ggsave(sprintf("%s/INDIA-VIX.monthly.png", reportPath), width=12, height=6, units="in")

allDays <- seq(first(index(iXts)), last(index(iXts)), by=1)
allXts <- xts(rep(NA, length(allDays)), allDays)

allXts2 <- merge(iXts, allXts)
iXts2 <- na.locf(allXts2[,1])

names(iXts2) <- c("VIX")
iTs <- tsbox::ts_tsibble(iXts2)
decomp <- iTs %>% model(STL(value)) %>% components()

p <- decomp %>% autoplot() +
	theme_economist() +
	theme(axis.text.x=element_text(angle=90, hjust=1)) +
	labs(x="") +
	scale_x_date(date_breaks='6 months', date_labels='%Y %b %d') 
	
p + plot_annotation(
  title = "INDIA VIX",
  caption = '@StockViz'
)

ggsave(sprintf("%s/INDIA-VIX.decomposition.png", reportPath), width=18, height=12, units="in")

decomp %>% { 
	ggplot(., aes(x=time, y=season_year)) +
		geom_line() +
		theme_economist() +
		theme(axis.text.x=element_text(angle=90, hjust=1)) +
		labs(x="", title = "INDIA VIX Seasonality") +
		scale_x_date(date_breaks='6 months', date_labels='%Y %b %d') +
		annotate("text", x=min(.$time), y=max(.$season_year), label = "@StockViz", hjust='left', vjust='top', col="white", cex=6, fontface = "bold", alpha = 0.8) 
}	
	
ggsave(sprintf("%s/INDIA-VIX.seasonality.png", reportPath), width=12, height=6, units="in")


decomp %>% mutate(md = as.Date(format(time, "1999-%m-%d")), y = year(time)) %>% filter(time %in% index(iXts)) %>% arrange(md) %>% {
	ggplot(., aes(x=md, y=season_year, color=factor(y, levels=unique(y)))) +
	scale_x_date(date_breaks='1 months', date_labels='%b') +
	scale_color_viridis_d() +
	theme_economist() +
	geom_point() +
	labs(color="", x="", title = "INDIA VIX Seasonality by month") +
	annotate("text", x=min(.$md, na.rm=T), y=max(.$season_year), label = "@StockViz", hjust='left', vjust='top', col="white", cex=6, fontface = "bold", alpha = 0.8) 
}

ggsave(sprintf("%s/INDIA-VIX.seasonality.month.png", reportPath), width=14, height=8, units="in")