library('RODBC')
library('quantmod')
library('PerformanceAnalytics')

library('tidyverse')
library('lubridate')
library('ggthemes')
library('reshape2')
library('viridis')

library('grid')
library('gridExtra')

options(stringsAsFactors = FALSE)
options("scipen"=100)

source("D:/stockviz/r/config.r")
reportPath <- "D:\\StockViz\\public\\blog\\mutual funds"

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

asofDt <- sqlQuery(lcon, "select max(as_of) from mf_nav_history")[[1]]
startDate <- as.Date("2018-01-01")

schemeCode <- 118989
schemeName <- "HDFC Mid-Cap Opportunities Fund - Growth Option - Direct Plan"
indices <- c('NIFTY MIDCAP 150 TR', 'NIFTY MIDCAP150 QUALITY 50 TR', 'NIFTY500 VALUE 50 TR')

iDf <- sqlQuery(lcon, sprintf("select as_of, nav from mf_nav_history where scheme_code=%d and as_of >= '%s' and as_of <= '%s'", schemeCode, startDate, asofDt))
iXts <- xts(iDf[,2], iDf[,1])
retXts <- monthlyReturn(iXts)
retWeekly <- weeklyReturn(iXts)
names(retXts) <- c("MF")

iRetXts <- NULL
iRetWeekly <- NULL
for(iName in indices){
	iDf <- sqlQuery(lcon, sprintf("select time_stamp, px_close from BHAV_INDEX where index_name='%s' and time_stamp >= '%s' and time_stamp <= '%s'", iName, startDate, asofDt))
	iXts <- xts(iDf[,2], iDf[,1])
	iRetXts <- merge.xts(iRetXts, monthlyReturn(iXts))
	iRetWeekly <- merge.xts(iRetWeekly, weeklyReturn(iXts))
}

names(iRetXts) <- indices

############# full period

fitList <- matrix(nrow=length(indices), ncol=length(indices)+2)
for(benchIndex in 1:length(indices)){
	allXts <- merge(retXts, iRetXts[, 1:benchIndex])
	allXts <- na.omit(allXts)
	
	benchNames <- paste0("B", seq(1:benchIndex))
	names(allXts) <- c('A', benchNames)
	
	linearFit <- lm(formula(paste0("A ~ ", paste(benchNames, collapse='+'))), data.frame(allXts))
	lmCoeffs <- round(as.numeric(linearFit$coeff), 5)
	
	fitList[benchIndex, ] <- c(lmCoeffs, rep(NA, ncol(fitList)-length(lmCoeffs) -1), round(100*summary(linearFit)$adj.r.squared, 0))
}

fitDf <- data.frame(fitList)
names(fitDf) <- c('Ax100', indices, 'ar2')
fitDf$A <- fitDf$Ax100 * 100
fitDf[is.na(fitDf)] <- ""

png(sprintf("%s/%s.lm.table.png", reportPath, schemeName), height = 35*nrow(fitDf), width = 120*ncol(fitDf), bg="white")
grid.table(fitDf, theme=ttheme_default(core=list(fg_params=list(hjust=1, x=0.9))), rows = NULL)
dev.off()

############ rolling

for(benchIndex in 1:length(indices)){
	allXts <- merge(retWeekly, iRetWeekly[, 1:benchIndex])
	allXts <- na.omit(allXts)
	
	benchNames <- paste0("B", seq(1:benchIndex))
	names(allXts) <- c('A', benchNames)
	
	rollingFit <- NULL
	for(i in 50:nrow(allXts)){
		subXts <- allXts[(i-49):i]
		linearFit <- lm(formula(paste0("A ~ ", paste(benchNames, collapse='+'))), data.frame(subXts))
		lmCoeffs <- c(toString(index(allXts[i])), 
						as.numeric(linearFit$coeff), 
						100*summary(linearFit)$adj.r.squared, 
						as.numeric(Return.cumulative(subXts[,1]) - Return.cumulative(subXts[,2])))
	
		rollingFit <- rbind.data.frame(rollingFit, lmCoeffs)
	}
	names(rollingFit) <- c("ASOF", "Ax100", indices[1:benchIndex], "ar2", "XS")
	
	rollingFit[, -1] <- sapply(rollingFit[, -1], as.numeric)
	rollingFit$ASOF <- as.Date(rollingFit$ASOF)
	rollingFit$Ax100 <- rollingFit$Ax100 * 100
		
	toPlot <- within(rollingFit, rm('ar2'))
	toPlot <- melt(toPlot, id='ASOF')
	#toPlot$ASOF <- factor(toPlot$ASOF, levels=unique(toPlot$ASOF))
	ggplot(toPlot, aes(x=ASOF, y=value, color=variable, group=variable, size=variable)) +
		theme_economist() +
		geom_line() +
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
		scale_color_viridis(discrete = TRUE) +
		scale_size_manual(values = seq(from=1, by=0.25, length.out=length(unique(toPlot$variable)))) +
		scale_x_date(breaks = "6 months", date_labels="%b-%d-%Y") +
		labs(x = "", y="", fill="", color="", size="", title=sprintf("%s", schemeName), subtitle=sprintf("Rolling 50 weeks [%s:%s]", first(index(allXts)), last(index(allXts)))) +
		annotate("text", x=last(index(allXts)), y=min(allXts), label = "@StockViz", hjust=1, vjust=0, col="white", cex=6, fontface = "bold", alpha = 0.8)
		
	ggsave(sprintf("%s/%s.lm.%d.png", reportPath, schemeName, benchIndex), width=12, height=7, units="in")
}

