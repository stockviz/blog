library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('lubridate')
library('reshape2')

library('ggthemes')
library('ggrepel')

options("scipen"=100)
options(stringsAsFactors = FALSE)

source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plot.common.R")
source("D:/StockViz/public/blog/common/misc.R")

pdf(NULL)
reportPath <- "D:/StockViz/public/blog/allocation vs tactical"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

rfRate <- 0.05
rfRateMonthly <- ((rfRate + 1)^(1/12)) - 1

indexName <- "NIFTY 50 TR"
startDate <- as.Date("2003-12-31")
endDate <- as.Date("2020-05-31")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from bhav_index where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", indexName, startDate, endDate))
eqXts <- monthlyReturn(xts(pDf[,2], pDf[,1]))

eqXts <- eqXts[-1,]
names(eqXts) <- c("EQUITY")

pDf <- sqlQuery(lcon, sprintf("select time_stamp, tri from index_ccil_tenor where index_name='0_5' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
bndXts <- monthlyReturn(xts(pDf[,2], pDf[,1]))

bndXts <- bndXts[-1,]
names(bndXts) <- c("BOND")

######################################

#### plot monthly returns

pstart <- first(index(bndXts))
pend <- last(index(bndXts))

eqXts <- Common.NormalizeMonthlyDates(eqXts)
bndXts <- Common.NormalizeMonthlyDates(bndXts)

dDf <- data.frame(100*bndXts)
dDf$T <- index(bndXts)

meanDst <- mean(dDf$BOND)
normDst <- data.frame(BOND=rnorm(nrow(dDf)*1000, mean=meanDst,sd=sd(dDf$BOND)))

ggplot(dDf, aes(x=BOND)) +
	theme_economist() +
	geom_histogram(aes(y=..density..), binwidth=0.25, color="black", fill="lightblue") +
	geom_density(data=normDst, color='maroon', size=2) +
	labs(x='returns (%)', y='density', color='', title=sprintf("%s Monthly Return Density", 'Short-term Bond'), subtitle=sprintf("%s:%s; %d months", pstart, pend, nrow(dDf))) +
	annotate("text", x=0, y=0, label = "@StockViz", hjust=0.5, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)
	
ggsave(sprintf("%s/%s.monthly-return-histogram.png", reportPath, 'bonds'), width=12, height=8, units="in", device="png")

######################################

#### plot cumulative returns

Common.PlotCumReturns(merge(eqXts, bndXts), sprintf("%s and Short-term Bonds", indexName), sprintf("%d months", nrow(dDf)), sprintf("%s/%s-bonds.cumulative-return-actual.png", reportPath, indexName), NULL)

######################################

#### simulate monthly returns

numMonths <- 10*12
numSims <- 1000

#allocation
runSim <- function(bndPct){
	actualRets <- as.numeric((1-bndPct)*eqXts + bndPct*bndXts)
	simDf <- NULL
	for(i in 1:numSims){
		sRets <- sample(actualRets, numMonths)
		sXts <- xts(sRets, index(eqXts[1:numMonths]))
		simDf <- rbind(simDf, 100*as.numeric(Return.annualized(sXts)))
	}
	return(simDf)
}

#equity only
actualRets <- as.numeric(eqXts)
eqDf <- NULL
for(i in 1:numSims){
	sRets <- sample(actualRets, numMonths)
	sXts <- xts(sRets, index(eqXts[1:numMonths]))
	eqDf <- rbind(eqDf, 100*as.numeric(Return.annualized(sXts)))
}

pctSeq <- seq(5, 95, by=5)
bndSim <- NULL
for(bndPct in pctSeq){
	bndSim <- cbind(bndSim, runSim(bndPct/100))
}

colnames(bndSim) <- sapply(pctSeq, function(X) paste0(100-X, "/", X))


######################################

#### plot simulated returns for different allocations

sharpeDf <- data.frame(RET_AVG=0.0, RET_SD=0.0)
for(i in 1:length(pctSeq)){
	sharpeDf <- rbind(sharpeDf, c(mean(bndSim[,i]), sd(bndSim[,i])))
}
sharpeDf <- sharpeDf[-1,]
sharpeDf$EQ_PCT <- 100-pctSeq

sharpeDf$RET_LB <- sharpeDf$RET_AVG-sharpeDf$RET_SD
sharpeDf$RET_UB <- sharpeDf$RET_AVG+sharpeDf$RET_SD

ggplot(sharpeDf, aes(x=EQ_PCT, y=RET_AVG, color=EQ_PCT, label=EQ_PCT)) +
	theme_economist() +
	geom_point() +
	geom_errorbar(aes(ymin=RET_LB, ymax=RET_UB), width=2, position=position_dodge(0.05)) +
	geom_text_repel() +
	guides(color=F) +
	labs(x='equity allocation', y='avg. returns (%)', color='', 
				title=sprintf("%s/Bond Simulated Allocation Strategies", indexName), 
				subtitle=sprintf("data: %s:%s; holding %d months", pstart, pend, numMonths)) +
	annotate("text", x=max(sharpeDf$EQ_PCT), y=min(sharpeDf$RET_AVG), label = "@StockViz", hjust=0.5, vjust=-0.5, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/%s-bond.allocation-sharpe.png", reportPath, indexName), width=12, height=8, units="in", device="png")	

######################################

#### plot cumulative returns for select allocations

allocs <- sharpeDf[sharpeDf$RET_AVG >= 10 & sharpeDf$RET_LB >= 7.5 & sharpeDf$RET_UB >= 12,]
top3 <- head(allocs[order(allocs$RET_SD),], 3)
dXts <- merge(eqXts, bndXts)
for(eqPct in top3$EQ_PCT){
	dXts <- merge(dXts, eqPct/100*eqXts+(1-eqPct/100)*bndXts)
}
names(dXts) <- c(indexName, 'BOND', sapply(top3$EQ_PCT, function(X) paste0(X,'/',100-X)))
Common.PlotCumReturns(dXts, sprintf("%s/Bond Allocation", indexName), "", sprintf("%s/%s-bond.allocation-actual.png", reportPath, indexName), NULL)

######################################

#### plot density of simulated returns for a single allocation portfolio vs. simulated returns for equity only

maxRetEq <- top3[top3$RET_AVG == max(top3$RET_AVG),]$EQ_PCT

allocTxt <- paste0(maxRetEq, "/", 100-maxRetEq)

eqAnn <- as.numeric(Return.annualized(eqXts))*100
actualAnn <- as.numeric(Return.annualized(maxRetEq/100*eqXts+(1-maxRetEq/100)*bndXts))*100
toPlot <- data.frame(ALLOC=bndSim[, allocTxt], EQ=eqDf[,1])

linecolors <- solarized_pal()(2)

ggplot(toPlot) +
	theme_economist() +
	geom_density(aes(x=ALLOC), color=linecolors[1], size=1) +
	geom_density(aes(x=EQ), color=linecolors[2], size=1) +
	geom_vline(xintercept=actualAnn, color=linecolors[1], size=1) +
	geom_text(aes(x=actualAnn, label=sprintf("%s actual annualized returns\n%.2f%%", allocTxt, actualAnn), y=0.05), colour=linecolors[1], angle=90) +
	geom_vline(xintercept=eqAnn, color=linecolors[2], size=1) +
	geom_text(aes(x=eqAnn, label=sprintf("%s actual annualized returns\n%.2f%%", indexName, eqAnn), y=0.05), colour=linecolors[2], angle=90) +
	labs(x='returns (%)', y='density', color='', title=sprintf("%s/Bond %s Simulated Annualized Returns Density", indexName, allocTxt), subtitle=sprintf("%s:%s; %d months", pstart, pend, numMonths)) +
	annotate("text", x=max(toPlot$EQ), y=0, label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.5)

ggsave(sprintf("%s/%s-bond.annualized-return-simulated-density.png", reportPath, indexName), width=16, height=8, units="in", device="png")		