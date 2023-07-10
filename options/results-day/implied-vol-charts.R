library('quantmod')
library('PerformanceAnalytics')

library('lubridate')
library('tidyverse')
library('ggpmisc')
library('ggthemes')
library('viridis')


options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

reportPath <- "."

analDf <- read.csv(file=sprintf("%s/results-IV.csv", reportPath)) #

analDf$STRANGLE_RET <- 100*(analDf$STRANGLE_PREMIUM_AFTER/analDf$STRANGLE_PREMIUM_ON - 1)

magWins <- summary(analDf[analDf$STRANGLE_RET > 0,]$STRANGLE_RET)
magLoss <- summary(analDf[analDf$STRANGLE_RET < 0,]$STRANGLE_RET)


ggplot(analDf, aes(x=AVG_IV_ON, y=AVG_IV_AFTER, color=SYMBOL, fill=SYMBOL)) +
	theme_economist() +
	scale_color_viridis_d() +
	geom_point() +
	guides(color='none', fill='none') +
	labs(x = "IV Before", y="IV After", fill="", color="", title="Earnings Day IV Crush") 

ggsave(sprintf("%s/iv-before-after.png", reportPath), width=16, height=8, units="in")


strangleSumm <- summary(analDf$STRANGLE_RET, digits=3)
ssDf <- data.frame(matrix(strangleSumm, nrow=1))
colnames(ssDf) <- names(strangleSumm)

ivSumm <- summary(na.omit(analDf$IV_CRUSH), digits=3)
ivDf <- data.frame(matrix(ivSumm, nrow=1))
colnames(ivDf) <- names(ivSumm)


ggplot(analDf, aes(x=STRANGLE_RET)) +
	theme_economist() +
	geom_histogram(binwidth=10) +
	labs(x = "% Returns", y="", fill="", color="", title="Earnings Day Strangle Returns") +
	annotate(geom='table', x=max(analDf$STRANGLE_RET/2), y=200, label=list(ssDf), hjust = 'right', vjust='top', family='mono')

ggsave(sprintf("%s/strangle-return-distribution.png", reportPath), width=16, height=8, units="in")	
	
ggplot(analDf, aes(x=IV_CRUSH)) +
	theme_economist() +
	geom_histogram(binwidth=10) +
	labs(x = "% Change", y="", fill="", color="", title="Earnings Day IV Crush") +
	annotate(geom='table', x=max(analDf$IV_CRUSH/2, na.rm=T), y=500, label=list(ivDf), hjust = 'right', vjust='top', family='mono')	
	
ggsave(sprintf("%s/iv-change-distribution.png", reportPath), width=16, height=8, units="in")		
	