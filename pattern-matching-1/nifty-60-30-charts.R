source("/mnt/hollandr/config.R")
#source("d:/stockviz/r/config.r")

reportPath<-"."
#reportPath <- "\\\\siberia\\R\\intraday\\pattern-matching-1"

library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('extrafont')
library('reshape2')
library('tidyverse')
library('MetBrewer')
library('ggthemes')

options(stringsAsFactors = FALSE)
options("scipen"=100)

load(file=sprintf("%s/pattern-1.Rda", reportPath)) #allStatsDf

segs <- unique(allStatsDf$SEGMENT)

#plot 'normal' returns
for(seg in segs){
	allStatsDf %>% 
		filter(SEGMENT == seg & ACTUAL_RET < mean(ACTUAL_RET, na.rm=T) + 3* sd(ACTUAL_RET, na.rm=T) & ACTUAL_RET > mean(ACTUAL_RET, na.rm=T) - 3* sd(ACTUAL_RET, na.rm=T)) %>%
		mutate(SD_BUCKET = cut(SD_LB_RET, quantile(SD_LB_RET))) %>%
		ggplot(aes(x = AVG_LB_RET, y = ACTUAL_RET, color = SD_BUCKET)) +
			scale_color_manual(values=met.brewer("Wissing", 5, type="discrete")) +
			geom_point() +
			labs(x='Avg. historical', y='Actual', title=sprintf("Euclidean Pattern Matching Segment %d", seg), subtitle="60min segment across 200 w/ 30min holding") +
			annotate("text", x=0, y=0, label = "@StockViz", col="white", cex=3, fontface = "bold")
			
	ggsave(sprintf("%s/nifty-60.30.200.%d.jpeg", reportPath, seg), width=12, height=6, units="in")
}

#plot outliers
for(seg in segs){
	allStatsDf %>% 
		filter(SEGMENT == seg & (ACTUAL_RET >= mean(ACTUAL_RET, na.rm=T) + 3* sd(ACTUAL_RET, na.rm=T) | ACTUAL_RET <= mean(ACTUAL_RET, na.rm=T) - 3* sd(ACTUAL_RET, na.rm=T))) %>%
		mutate(SD_BUCKET = cut(SD_LB_RET, quantile(SD_LB_RET))) %>%
		ggplot(aes(x = AVG_LB_RET, y = ACTUAL_RET, color = SD_BUCKET)) +
			scale_color_manual(values=met.brewer("Wissing", 5, type="discrete")) +
			geom_point() +
			labs(x='Avg. historical', y='Actual', title=sprintf("Euclidean Pattern Matching Segment %d (outliers)", seg), subtitle="60min segment across 200 w/ 30min holding") +
			annotate("text", x=0, y=0, label = "@StockViz", col="white", cex=3, fontface = "bold")
			
	ggsave(sprintf("%s/nifty-60.30.outliers.200.%d.jpeg", reportPath, seg), width=12, height=6, units="in")
}