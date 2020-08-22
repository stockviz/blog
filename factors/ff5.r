library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('lubridate')
library('reshape2')
library('viridis')

library('ggthemes')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")

pdf(NULL)
reportPath <- "D:/StockViz/public/blog/factors/plots"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockVizUs2", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

fdf <- sqlQuery(lcon, "select time_stamp, ret/100 from FAMA_FRENCH_5_FACTOR_DAILY where RET_TYPE = 'TTBIA' and KEY_ID='MKT-RF'")
mktXts <- xts(fdf[,2], fdf[,1])

fdf <- sqlQuery(lcon, "select time_stamp, ret/100 from FAMA_FRENCH_5_FACTOR_DAILY where RET_TYPE = 'TTBIA' and KEY_ID='SMB'")
smbXts <- xts(fdf[,2], fdf[,1])

fdf <- sqlQuery(lcon, "select time_stamp, ret/100 from FAMA_FRENCH_5_FACTOR_DAILY where RET_TYPE = 'TTBIA' and KEY_ID='HML'")
hmlXts <- xts(fdf[,2], fdf[,1])

fdf <- sqlQuery(lcon, "select time_stamp, ret/100 from FAMA_FRENCH_5_FACTOR_DAILY where RET_TYPE = 'TTBIA' and KEY_ID='RMW'")
rmwXts <- xts(fdf[,2], fdf[,1])

fdf <- sqlQuery(lcon, "select time_stamp, ret/100 from FAMA_FRENCH_5_FACTOR_DAILY where RET_TYPE = 'TTBIA' and KEY_ID='CMA'")
cmaXts <- xts(fdf[,2], fdf[,1])

toPlot <- merge(smbXts, hmlXts, rmwXts, cmaXts)
names(toPlot) <- c('SMB', 'HML', 'RMW', 'CMA')

Common.PlotCumReturns(toPlot, "Fama-French Factors", "", sprintf("%s/fama-french-5.png", reportPath), NULL)

cmTr <- merge(cumprod(1 +mktXts), cumprod(1 +smbXts), cumprod(1 +hmlXts), cumprod(1 +rmwXts), cumprod(1 +cmaXts))

annRets <- merge(yearlyReturn(cmTr[,1]), yearlyReturn(cmTr[,2]), yearlyReturn(cmTr[,3]), yearlyReturn(cmTr[,4]), yearlyReturn(cmTr[,5]))
names(annRets) <- c('MKT', 'SMB', 'HML', 'RMW', 'CMA')

toPlot <- data.frame(100*annRets)
toPlot$Y <- year(index(annRets))
toPlot$Y <- factor(toPlot$Y, levels=unique(toPlot$Y))
toPlot <- melt(toPlot, id='Y')

ggplot(toPlot, aes(x=Y, y=value, fill=variable)) +
	theme_economist() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	scale_fill_viridis(discrete = TRUE) +
	geom_bar(stat="identity", position=position_dodge()) +
	labs(y='return (%)', x='', color='', fill='', title="Fama-French Factors") +
	annotate("text", x=1, y=min(toPlot$value, na.rm=T), label = "@StockViz", hjust=0, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/fama-french-5.annual.png", reportPath), width=16, height=8, units="in")