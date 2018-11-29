library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('ggplot2')
library('ggthemes')
library('lubridate')
library('reshape2')
library('ggrepel')
library('dplyr')
library('grid')
library('gridExtra')
library('gtable')

options("scipen"=100)
options(stringsAsFactors = FALSE)
reportPath <- "."
#reportPath <- "D:/StockViz/public/blog/gaps"
source("d:/stockviz/r/config.r")
source("D:/StockViz/public/blog/common/plots.R")

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

startDate<-as.Date("2011-01-01")
endDate<-as.Date("2018-10-31")

niftyDf<-sqlQuery(lcon, sprintf("select TIME_STAMP, PX_OPEN, PX_CLOSE from BHAV_INDEX where index_name='NIFTY 50' and time_stamp >= '%s' and time_stamp <= '%s'", startDate, endDate))
niftyXts<-xts(niftyDf[,-1], as.Date(niftyDf[,1]))

niftyXts<-merge(niftyXts, stats::lag(niftyXts$PX_CLOSE, 1))

names(niftyXts)<-c('O', 'C', 'PC')

niftyXts$C2C<-niftyXts$C-niftyXts$PC
niftyXts$C2O<-niftyXts$O-niftyXts$PC

niftyXts$C2C_PCT<-niftyXts$C2C/niftyXts$PC
niftyXts$C2O_PCT<-niftyXts$C2O/niftyXts$PC

niftyXts$WDAY<-wday(index(niftyXts)) #Sunday is 1, Monday is 2... Saturday is 7
niftyXts$DAY_GAP<-niftyXts$WDAY-stats::lag(niftyXts$WDAY, 1)

niftyXts<-na.omit(niftyXts)

gapRegularMonday<-ifelse(niftyXts$WDAY == 2 & niftyXts$DAY_GAP == -4, niftyXts$C2O_PCT, NA)
gapAfterHoliday<-ifelse(niftyXts$DAY_GAP != 1 & !(niftyXts$WDAY == 2 & niftyXts$DAY_GAP == -4), niftyXts$C2O_PCT, NA)
gapRegularDayNotMonday<-ifelse(niftyXts$DAY_GAP == 1, niftyXts$C2O_PCT, NA)

allGapXts<-100*merge(gapRegularMonday, gapAfterHoliday, gapRegularDayNotMonday)  
allGapDf<-data.frame(allGapXts)
names(allGapDf)<-c('MONDAY', 'HOLIDAY', 'DAY_1')

meltedGapsDf<-melt(allGapDf)

plotStart<-first(index(allGapXts))
plotEnd<-last(index(allGapXts))

pdf(NULL)
ggplot(meltedGapsDf, aes(x=value, color=variable)) +
	theme_economist() +
	stat_density(geom="line", position = "identity", na.rm=T, size=0.8) +
	labs(y='density', x='gap(%)', fill='', color='', title="NIFTY 50 Gaps", subtitle=sprintf("%s:%s", plotStart, plotEnd)) +
	annotate("text", x=min(allGapXts, na.rm=T), y=0, label = "@StockViz", hjust=-.5, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)
ggsave(sprintf("%s/NIFTY.gaps.png", reportPath), width=12, height=6, units="in")

gapSumm<- meltedGapsDf %>% 
	group_by(variable) %>% 
	summarize(N=sum(!is.na(value)), MIN=min(value, na.rm=T), MEDIAN=median(value, na.rm=T), MAX=max(value, na.rm=T), KURT=kurtosis(value, na.rm=T, method='sample'), SKEW=skewness(value, na.rm=T, method='sample')) %>% 
	mutate_if(is.numeric, round, 2)
	
tt1<-arrangeGrob(grobs=list(tableGrob(gapSumm, rows=NULL, theme=tableTheme)), ncol=1, top=textGrob(sprintf("NIFTY Gaps (%s:%s)", plotStart, plotEnd), gp=gpar(fontsize=14, fontface ='bold', fontfamily='Segoe UI')), bottom=textGrob("@StockViz", gp=gpar(fontsize=10, col='grey', fontfamily='Segoe UI')))

ggsave(sprintf("%s/NIFTY.gaps.table.png", reportPath), tt1, width=5, height=nrow(gapSumm)*0.8, units='in')
		