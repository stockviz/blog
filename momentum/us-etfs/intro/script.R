library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('ggrepel')
library('patchwork')
library('viridis')

library("treemapify")
library("grid")
library("gridExtra")
#library("extrafont") #font_import(pattern = "segoe"); loadfonts(device = "win")

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
lconUs2 <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

momEtfs <- sqlQuery(lcon, "select * from ETF_META where FUND like '%momentum%' order by INCEPTION_DATE")

# launches by year
momEtfs %>% mutate(YEAR = year(INCEPTION_DATE)) %>% 
	group_by(YEAR) %>% 
	summarize(LAUNCHES = n()) %>% {
		ggplot(data = ., aes(x=YEAR, y=LAUNCHES, fill=factor(YEAR))) +
			theme_economist() +
			geom_bar(stat="identity") +
			geom_text(aes(label=LAUNCHES), vjust=-0.3, size=3.5) +
			scale_fill_viridis_d() +
			scale_x_continuous(labels = as.character(.$YEAR), breaks = .$YEAR) +
			guides(fill='none') +
			labs(x = "year", y="etf launches", fill="", color="", size="", title="Momentum ETF Launches") +
			annotate("text", x= min(.$YEAR), y=0, label = "@StockViz", vjust='bottom', hjust='left', col="white", cex=6, fontface = "bold", alpha = 0.8)
	}
ggsave(sprintf("%s/launches-year.png", reportPath), width=12, height=6, units="in")
	
# by AUM
aumPlot <- momEtfs %>% mutate(LABEL = ifelse(AUM/sum(AUM) > 0.01, paste0(SYMBOL, "\n", 100*round(AUM/sum(AUM), 2), "%"), SYMBOL)) %>%
	ggplot(aes(area = AUM, fill = ISSUER, label = LABEL)) +
		scale_fill_viridis_d() +
		geom_treemap() +
		geom_treemap_text() 
		
aumGrid <- grid.arrange(aumPlot, top=textGrob("ETF AUM share", gp=gpar(fontfamily = 'Segoe UI', cex=2)), bottom=textGrob("@StockViz", gp=gpar(cex=0.5)), ncol=1)
ggsave(sprintf("%s/aum.png", reportPath), aumGrid, width=12, height=6, units="in")

# relative performance of top 5 by AUM

biggest <- momEtfs %>% slice_max(AUM, n=5) %>% select(SYMBOL, INCEPTION_DATE)
plotStart <- max(biggest$INCEPTION_DATE)
symbols <- c('SPY', biggest$SYMBOL)

dXts <- NULL
annXts <- NULL
for(sym in symbols){
	pDf <- sqlQuery(lconUs2, sprintf("select time_stamp, c from BHAV_EQ_TD where symbol = '%s' and time_stamp >= '%s'", sym, plotStart))
	
	dXts <- merge.xts(dXts, dailyReturn(xts(pDf[,2], pDf[,1])))
	annXts <- merge.xts(annXts, annualReturn(xts(pDf[,2], pDf[,1])))
}
names(dXts) <- symbols
names(annXts) <- symbols

Common.PlotCumReturns(dXts, "Momentum ETF Performance", "Top 5 by AUM", sprintf("%s/aum-perf-cum.png", reportPath))

annDf <- data.frame(100*annXts)
annDf$Y <- year(annXts)
toPlot <- melt(annDf, id='Y')

ggplot(toPlot, aes(x=factor(Y), y=value, fill=variable)) +
	theme_economist() +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text_repel(aes(label=round(value, 2)), size=3.5, position = position_dodge(0.9), color='purple') +
	scale_fill_viridis_d() +
	labs(x = "", y="returns (%)", fill="", color="", size="", title="Momentum ETF Returns", subtitle=sprintf("Top 5 by AUM [%s:%s]", first(index(dXts)), last(index(dXts)))) +
	annotate("text", x=1, y=min(toPlot$value), label = "@StockViz", vjust='bottom', hjust='left', col="white", cex=6, fontface = "bold", alpha = 0.8)

ggsave(sprintf("%s/aum-perf-ann.png", reportPath), width=12, height=6, units="in")

# top 5 by PERF

allEtfs <- momEtfs %>% filter(INCEPTION_DATE <= Sys.Date() - 6*365)
plotStart <- max(allEtfs$INCEPTION_DATE)
symbols <- c('SPY', allEtfs$SYMBOL)

dXts <- NULL
annXts <- NULL
for(sym in symbols){
	pDf <- sqlQuery(lconUs2, sprintf("select time_stamp, c from BHAV_EQ_TD where symbol = '%s' and time_stamp >= '%s'", sym, plotStart))
	
	dXts <- merge.xts(dXts, dailyReturn(xts(pDf[,2], pDf[,1])))
	annXts <- merge.xts(annXts, annualReturn(xts(pDf[,2], pDf[,1])))
}
names(dXts) <- symbols
names(annXts) <- symbols

retDf <- data.frame(t(Return.cumulative(dXts)))
colnames(retDf) <- c("RET")
retDf$SYMBOL <- symbols

bestSyms <- retDf %>% slice_max(RET, n=5) %>% select(SYMBOL)
bestSyms <- bestSyms$SYMBOL
if (!'SPY' %in% bestSyms) bestSyms <- c('SPY', bestSyms)

Common.PlotCumReturns(dXts[, bestSyms], "Momentum ETF Performance", "Top 5 by Performance", sprintf("%s/top-perf-cum.png", reportPath))

annDf <- data.frame(100*annXts[, bestSyms])
annDf$Y <- year(annXts)
toPlot <- melt(annDf, id='Y')

ggplot(toPlot, aes(x=factor(Y), y=value, fill=variable)) +
	theme_economist() +
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text_repel(aes(label=round(value, 2)), size=3.5, position = position_dodge(0.9), color='purple') +
	scale_fill_viridis_d() +
	labs(x = "", y="returns (%)", fill="", color="", size="", title="Momentum ETF Returns", subtitle=sprintf("Top 5 by Returns [%s:%s]", first(index(dXts)), last(index(dXts)))) +
	annotate("text", x=1, y=min(toPlot$value), label = "@StockViz", vjust='bottom', hjust='left', col="white", cex=6, fontface = "bold", alpha = 0.8)

ggsave(sprintf("%s/top-perf-ann.png", reportPath), width=12, height=6, units="in")	