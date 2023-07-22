library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')
library('ggthemes')
library('reshape2')
library('viridis')
library('ggrepel')
library('patchwork')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")

reportPath <- "."
pdf(NULL)

indexName <- "NIFTY 50"
corLbs <- c(5, 10, 20, 50, 100)
tileLb <- 500

eqWtCor <- NULL
eqWtCorTile <- NULL
eqWtCorleDf <- data.frame(LB = 0, TILE = 0, LEN = 0)
for(corLb in corLbs){
	load(file=sprintf("%s/%s.corr.%d.RData", reportPath, indexName, corLb)) #eqWtCorXts
	names(eqWtCorXts) <- c(paste0('C', corLb))
	
	eqWtCor <- merge.xts(eqWtCor, eqWtCorXts)
	
	corTile <- rollapply(eqWtCorXts[,1], tileLb, function(X) xts(last(ntile(coredata(X), n=5)), last(index(X))))
	names(corTile) <- c(paste0('T', corLb))
	
	eqWtCorTile <- merge.xts(eqWtCorTile, corTile)
	
	eqWtCorXts <- na.omit(eqWtCorXts)
	
	eqWtCorle <- rle(as.vector(coredata(na.omit(corTile))))
	eqWtCorleDf <- rbind(eqWtCorleDf, data.frame(LB = rep(corLb, length(eqWtCorle[['values']])), TILE = eqWtCorle[['values']], LEN = eqWtCorle[['lengths']]))
}

eqWtCor <- na.omit(eqWtCor)
eqWtCorTile <- na.omit(eqWtCorTile)
eqWtCorleDf <- eqWtCorleDf[-1,]

plotVec <- list()
for(j in 1:ncol(eqWtCorTile)){
	toPlot <- data.frame(eqWtCorTile[,j])
	colnames(toPlot) <- c("TILE")
	toPlot$T <- index(eqWtCorTile[,j])
	
	p <- ggplot(toPlot, aes(x=T, y=.data[["TILE"]])) +
			theme_economist() +
			geom_line() +
			labs(x="", y=paste0(corLbs[j], '-', 'day'))
			
	plotVec[[j]] <- p
}

Reduce(`+`, plotVec) + plot_layout(ncol=1) + 
plot_annotation(
  title = sprintf('%s constituent pair-wise median correlation quintiles', indexName),
  subtitle = sprintf('rolling %d-day tiles', tileLb),
  caption = '@StockViz'
)

ggsave(sprintf("%s/%s.correlation.quintiles.lines.png", reportPath, indexName), width=18, height=6*length(plotVec), units="in")

ggplot(eqWtCorleDf, aes(fill=factor(LB, levels = sort(unique(LB))), x=factor(TILE, levels = sort(unique(TILE))), y=LEN)) +
	theme_economist() +
	geom_violin(position="dodge") +
	stat_summary(fun = median, geom = "point", position = position_dodge(0.9)) +
	labs(x="quintiles", y="runs (days)", fill="correl-lb", title = sprintf("%s runs by quintiles", indexName), subtitle = sprintf('rolling %d-day tiles', tileLb)) +
	annotate("text", x=1, y=0, label = "@StockViz", hjust='right', vjust='top', col="grey", cex=3, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/%s.correlation.quintiles.violin.png", reportPath, indexName), width=12, height=6, units="in")

eqWtCorleDf %>% filter(LB == 20) %>% mutate(TILE = factor(TILE, levels=unique(TILE))) %>%
	ggplot(aes(x=LEN, color=TILE)) +
	theme_economist() +
	stat_density(geom = "line", position = "identity", size=1) +
	scale_color_viridis_d(option='C') +
	labs(x="runs (days)", y="", color="tiles", title = sprintf("%s runs of 20-day pair-wise correlation", indexName), subtitle = sprintf('rolling %d-day tiles', tileLb)) +
	annotate("text", x=20, y=0.1, label = "@StockViz", hjust='left', vjust='top', col="grey", cex=3, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/%s.20-day.correlation.quntile-density.png", reportPath, indexName), width=12, height=6, units="in")	