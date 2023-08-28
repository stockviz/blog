library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('lubridate')
library('reshape2')
library('viridis')
library('ggpubr')
library('ggthemes')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")

reportPath <- "D:/StockViz/public/blog/rolling returns"

pdf(NULL)

indexName <- "NIFTY 500"
mktCapStartDate <- as.Date("2020-06-01")
mktCapEndDate <- as.Date("2020-08-31")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

mktDt <- sqlQuery(lcon, sprintf("select max(time_stamp) from INDEX_CONST_HISTORY where index_name='%s'", indexName))[[1]]

capsDf <- sqlQuery(lcon, sprintf("select i.symbol, avg(FF_MKT_CAP_CR), avg(MKT_CAP_CR) from INDEX_CONST_HISTORY i, EQUITY_MISC_INFO e
									where i.symbol = e.symbol
									and i.time_stamp = '%s'
									and e.time_stamp >= '%s' and e.time_stamp <= '%s'
									group by i.symbol",
								mktDt, mktCapStartDate, mktCapStartDate))
								
capsDf$free <- 100*capsDf[,2]/sum(capsDf[,2])								
capsDf$full <- 100*capsDf[,3]/sum(capsDf[,3])								

fFloat <- capsDf[order(capsDf$free, decreasing=T), c('symbol', 'free')]
fFloat$symbol <- factor(fFloat$symbol, levels=fFloat$symbol)

fFloat$cum <- cumsum(capsDf[order(capsDf$free, decreasing=T),]$free)
fFloat$colb <- ifelse(fFloat$cum < 50, 0, ifelse(fFloat$cum >= 50 & fFloat$cum < 80, 1, 2))

ggplot(fFloat, aes(x=symbol, y=free, fill=colb)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
	geom_bar(stat="identity", position=position_dodge()) +
	scale_fill_viridis() +
	guides(fill=F, color=F) +
	labs(y='free float (%)', x='', color='', fill='', title=sprintf("%s Constituents", indexName)) +
	annotate("text", x=nrow(fFloat), y=0, label = "@StockViz", hjust='right', vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/%s.free.constituent-weights.png", reportPath, indexName), width=20, height=8, units="in")


fFloat <- capsDf[order(capsDf$full, decreasing=T), c('symbol', 'full')]
fFloat$symbol <- factor(fFloat$symbol, levels=fFloat$symbol)

fFloat$cum <- cumsum(capsDf[order(capsDf$full, decreasing=T),]$full)
fFloat$colb <- ifelse(fFloat$cum < 50, 0, ifelse(fFloat$cum >= 50 & fFloat$cum < 80, 1, 2))

ggplot(fFloat, aes(x=symbol, y=full, fill=colb)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
	geom_bar(stat="identity", position=position_dodge()) +
	scale_fill_viridis() +
	guides(fill=F, color=F) +
	labs(y='full mkt cap (%)', x='', color='', fill='', title=sprintf("%s Constituents", indexName)) +
	annotate("text", x=nrow(fFloat), y=0, label = "@StockViz", hjust='right', vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/%s.full.constituent-weights.png", reportPath, indexName), width=20, height=8, units="in")
