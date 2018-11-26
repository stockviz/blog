library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('extrafont')
library('lubridate')
library('ggplot2')
library('ggthemes')
library('reshape2')
library('grid')
library('gridExtra')
library('gtable')
#library('ggrepel')
#library('dplyr')
library('tvm')

args = commandArgs(TRUE)

pdf(NULL)
options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."

source("d:/stockviz/r/config.r")

projectSip<-function(assetReturns, assetWeights, sipFlow, plotMainTitle=NULL, plotSubTitle=NULL, plotFileName=NULL){
	assetPrices<-data.frame(E=rep(100.0, numMonths), B=100.0)
	investorAsset<-data.frame(E=rep(0.0, numMonths), B=0.0)
	investorVal<-data.frame(E=rep(0.0, numMonths), B=0.0)

	investorAsset[1,]<-sipFlow[1]*assetWeights[1,]/assetPrices[1,]
	investorVal[1,]<-sipFlow[1]*assetWeights[1,]

	for(m in 2:numMonths){
	#for(m in 2:143){
		assetPrices[m,]<-assetPrices[m-1,]*(1+assetReturns[m,])
		iVal<-investorAsset[m-1,]*assetPrices[m,] #before SIP
		iWt<-iVal/sum(iVal)
		
		#rebalance
		if(m%%12 == 0 && (iWt[1] > (1+drift)*assetWeights[m,1] || iWt[2] > (1+drift)*assetWeights[m,2])){
			targetVal <- assetWeights[m,] * sum(iVal)
			excessAmt <- iVal - targetVal
			sttOnExcess <- stt*abs(excessAmt)
			
			profitPct<-iVal/(investorVal[m-11,]+colSums(sipFlow[(m-11):(m-1)]*assetWeights[(m-11):(m-1),]))-1
			profitOnExcess<-profitPct*excessAmt
			profitOnExcess<-unlist(ifelse(profitOnExcess < 0, 0, profitOnExcess))
			
			taxOnProfits <- tax*profitOnExcess
			drag <- sttOnExcess+taxOnProfits
			excessAsset <- as.numeric(excessAmt/assetPrices[m,])
			
			investorAsset[m,]<-investorAsset[m-1,]-excessAsset-drag/assetPrices[m,]
			investorVal[m,]<-iVal-excessAmt-drag
		}
		else {
			investorAsset[m,]<-investorAsset[m-1,]
			investorVal[m,]<-iVal
		}
		
		investorAsset[m,]<-investorAsset[m,] + sipFlow[m]*assetWeights[m,]/assetPrices[m,]
		investorVal[m,]<-investorVal[m,] + sipFlow[m]*assetWeights[m,]
	}

	IRR<-xirr(c(-sipFlow, sum(last(investorVal))), d=seq.Date(from=Sys.Date(), by='month', length.out=(1+length(sipFlow))), interval=c(0, 1))

	if(is.null(plotMainTitle)){
		return(list(investorVal, IRR))
	}
	
	plotSubTitle<-sprintf("%s\nIRR: %.2f%%", plotSubTitle, IRR*100)

	toPlot<-investorVal
	toPlot$ID<-seq(1:nrow(investorVal))
	ggplot(melt(toPlot, id='ID'), aes(x=ID, y=value, color=variable)) +
		theme_economist() +
		geom_line() +
		labs(x = "", y="asset value", fill="", color="", title=plotMainTitle, subtitle=plotSubTitle) +
		annotate("text", x=nrow(investorVal), y=0, label = "@StockViz", hjust=0.8, vjust=-1, col="white", cex=6, fontface = "bold", alpha = 0.6)
	ggsave(plotFileName, width=12, height=6, units="in")	
	#return(list(investorVal, IRR))
}	

lcon <- odbcDriverConnect(sprintf("Driver={SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, ldbname, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

equityName<-args[1]
#equityName<-'NIFTY 50'
bondName<-'0_5'

equityDf<-sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index where index_name='%s'", equityName))
equityXts<-xts(equityDf$px_close, as.Date(equityDf$time_stamp))
equityMonthlyRet<-monthlyReturn(equityXts)

equityMonthlyRet<-equityMonthlyRet[-1]
equityMonthlyRet<-equityMonthlyRet[-nrow(equityMonthlyRet)]

medianEquityReturn<-median(equityMonthlyRet)

bondDf<-sqlQuery(lcon, sprintf("select tri, time_stamp from INDEX_CCIL_TENOR where index_name='%s'", bondName))
bondXts<-xts(bondDf$tri, as.Date(bondDf$time_stamp))
bondMonthlyRet<-monthlyReturn(bondXts)

bondMonthlyRet<-bondMonthlyRet[-1]
bondMonthlyRet<-bondMonthlyRet[-nrow(bondMonthlyRet)]

medianBondReturn<-median(bondMonthlyRet)

############################################

tax<-0.1
stt<-0.001/100
drift<-0.05
numMonths<-12*20
sipFlow<-rep(5000.0, numMonths)
maxIter<-10000

print("#static everything")
#static everything
assetReturns<-data.frame(E=rep(medianEquityReturn, numMonths), B=rep(medianBondReturn, numMonths))
assetWeights<-data.frame(E=rep(0.6, numMonths), B=rep(0.4, numMonths))

mainTitle<-sprintf("Static 60/40 Allocation %s/%s", equityName, bondName)
subTitle<-sprintf("20yr constant equity and bond monthly returns of %.2f%% and %.2f%%", medianEquityReturn*100, medianBondReturn*100)

projectSip(assetReturns, assetWeights, sipFlow, mainTitle, subTitle, sprintf("%s/static.60.40.A.%s.png", reportPath, equityName))

############################################

print("#static allocation, random return sampling")
#static allocation, random return sampling
eqtRnd<-base::sample(equityMonthlyRet, min(nrow(equityMonthlyRet), numMonths))
while(nrow(eqtRnd) < numMonths){
	eqtRnd<-rbind(eqtRnd, base::sample(equityMonthlyRet, min(nrow(equityMonthlyRet), numMonths - nrow(eqtRnd))))
}

bndRnd<-base::sample(bondMonthlyRet, min(nrow(bondMonthlyRet), numMonths))
while(nrow(bndRnd) < numMonths){
	bndRnd<-rbind(bndRnd, base::sample(bondMonthlyRet, min(nrow(bondMonthlyRet), numMonths - nrow(bndRnd))))
}

assetReturns<-data.frame(E=eqtRnd, B=bndRnd)
assetWeights<-data.frame(E=rep(0.6, numMonths), B=rep(0.4, numMonths))

mainTitle<-sprintf("Static 60/40 Allocation %s/%s", equityName, bondName)
subTitle<-"20yr random sampling of monthly returns"

projectSip(assetReturns, assetWeights, sipFlow, mainTitle, subTitle, sprintf("%s/random.60.40.A.%s.png", reportPath, equityName))

print("running projections...")
projections<-c()
terminalAssets<-c()
for(i in 1:maxIter){
	eqtRnd<-base::sample(equityMonthlyRet, min(nrow(equityMonthlyRet), numMonths))
	while(nrow(eqtRnd) < numMonths){
		eqtRnd<-rbind(eqtRnd, base::sample(equityMonthlyRet, min(nrow(equityMonthlyRet), numMonths - nrow(eqtRnd))))
	}

	bndRnd<-base::sample(bondMonthlyRet, min(nrow(bondMonthlyRet), numMonths))
	while(nrow(bndRnd) < numMonths){
		bndRnd<-rbind(bndRnd, base::sample(bondMonthlyRet, min(nrow(bondMonthlyRet), numMonths - nrow(bndRnd))))
	}

	assetReturns<-data.frame(E=eqtRnd, B=bndRnd)
	projectedVals<-projectSip(assetReturns, assetWeights, sipFlow)
	projections<-c(projections, 100*projectedVals[[2]]) #just take the IRR
	
	terminalAssets<-c(terminalAssets, sum(last(projectedVals[[1]])))
}

projDf<-data.frame(projections)
names(projDf)<-c('IRR')
projDf[,1]<-as.numeric(projDf[,1])
medianIrr<-median(projDf[,1])

minIrr<-min(projDf[,1])
maxIrr<-max(projDf[,1])

subTitle<-sprintf("20yr random sampling. Projected IRR: [%.2f%%,%.2f%%]", minIrr, maxIrr)

ggplot(projDf, aes(IRR)) +
	theme_economist() +
	geom_histogram(aes(y=..density..), binwidth=0.25, colour="black", fill="lightblue") +
	geom_density(alpha=.2, fill="#FF6666") +
	geom_vline(aes(xintercept=medianIrr), colour="blue", size=1, linetype='dashed', alpha=0.5) +
	geom_label(aes(x=medianIrr, label=sprintf("%.2f%%", medianIrr), y=0), colour="blue") +
	labs(y='density', x='projected IRR', color='', title=mainTitle, subtitle=subTitle) +
	annotate("text", x=max(projDf$IRR), y=0, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)
	
ggsave(sprintf("%s/random.60.40.IRR.DENSITY.%s.png", reportPath, equityName), dpi=600, width=12, height=6, units="in")

projDf<-data.frame(terminalAssets)
names(projDf)<-c('A')
projDf[,1]<-as.numeric(projDf[,1])
medianIrr<-median(projDf[,1])

minIrr<-min(projDf[,1])
maxIrr<-max(projDf[,1])

subTitle<-sprintf("20yr random sampling. Projected final asset value: [%.2f,%.2f]", minIrr, maxIrr)

ggplot(projDf, aes(A)) +
	theme_economist() +
	geom_histogram(aes(y=..density..), binwidth=(maxIrr-minIrr)/100, colour="black", fill="lightblue") +
	geom_density(alpha=.2, fill="#FF6666") +
	geom_vline(aes(xintercept=medianIrr), colour="blue", size=1, linetype='dashed', alpha=0.5) +
	geom_label(aes(x=medianIrr, label=sprintf("%.2f", medianIrr), y=0), colour="blue") +
	labs(y='density', x='projected final asset value', color='', title=mainTitle, subtitle=subTitle) +
	annotate("text", x=max(projDf$A), y=0, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)
	
ggsave(sprintf("%s/random.60.40.VALUE.DENSITY.%s.png", reportPath, equityName), dpi=600, width=12, height=6, units="in")

	
############################################

print("#glide-path allocation, static returns")
#glide-path allocation, static returns
assetReturns<-data.frame(E=rep(medianEquityReturn, numMonths), B=rep(medianBondReturn, numMonths))
glidePath<-seq(from=0.0, to=0.95, length.out=numMonths)
assetWeights<-data.frame(E=(1-glidePath), B=glidePath)

mainTitle<-sprintf("Glide-path Allocation %s/%s", equityName, bondName)
subTitle<-sprintf("20yr constant equity and bond monthly returns of %.2f%% and %.2f%%", medianEquityReturn*100, medianBondReturn*100)

projectSip(assetReturns, assetWeights, sipFlow, mainTitle, subTitle, sprintf("%s/static.glidePath.A.%s.png", reportPath, equityName))

############################################

print("#glide-path allocation, random return sampling")
#glide-path allocation, random return sampling
assetReturns<-data.frame(E=eqtRnd, B=bndRnd)
mainTitle<-sprintf("Glide-path Allocation %s/%s", equityName, bondName)
subTitle<-"20yr random sampling."
projectSip(assetReturns, assetWeights, sipFlow, mainTitle, subTitle, sprintf("%s/random.glidePath.A.%s.png", reportPath, equityName))

print("running projections...")
projections<-c()
terminalAssets<-c()
for(i in 1:maxIter){
	eqtRnd<-base::sample(equityMonthlyRet, min(nrow(equityMonthlyRet), numMonths))
	while(nrow(eqtRnd) < numMonths){
		eqtRnd<-rbind(eqtRnd, base::sample(equityMonthlyRet, min(nrow(equityMonthlyRet), numMonths - nrow(eqtRnd))))
	}

	bndRnd<-base::sample(bondMonthlyRet, min(nrow(bondMonthlyRet), numMonths))
	while(nrow(bndRnd) < numMonths){
		bndRnd<-rbind(bndRnd, base::sample(bondMonthlyRet, min(nrow(bondMonthlyRet), numMonths - nrow(bndRnd))))
	}

	assetReturns<-data.frame(E=eqtRnd, B=bndRnd)
	projectedVals<-projectSip(assetReturns, assetWeights, sipFlow)
	projections<-c(projections, 100*projectedVals[[2]]) #just take the IRR
	
	terminalAssets<-c(terminalAssets, sum(last(projectedVals[[1]])))
}

projDf<-data.frame(projections)
names(projDf)<-c('IRR')
projDf[,1]<-as.numeric(projDf[,1])
medianIrr<-median(projDf[,1])

minIrr<-min(projDf[,1])
maxIrr<-max(projDf[,1])

subTitle<-sprintf("20yr random sampling. Projected IRR: [%.2f%%, %.2f%%]", minIrr, maxIrr)

ggplot(projDf, aes(IRR)) +
	theme_economist() +
	geom_histogram(aes(y=..density..), binwidth=0.25, colour="black", fill="lightblue") +
	geom_density(alpha=.2, fill="#FF6666") +
	geom_vline(aes(xintercept=medianIrr), colour="blue", size=1, linetype='dashed', alpha=0.5) +
	geom_label(aes(x=medianIrr, label=sprintf("%.2f%%", medianIrr), y=0), colour="blue") +
	labs(y='density', x='projected IRR', color='', title=mainTitle, subtitle=subTitle) +
	annotate("text", x=max(projDf$IRR), y=0, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)
	
ggsave(sprintf("%s/random.glidePath.IRR.DENSITY.%s.png", reportPath, equityName), dpi=600, width=12, height=6, units="in")

projDf<-data.frame(terminalAssets)
names(projDf)<-c('A')
projDf[,1]<-as.numeric(projDf[,1])
medianIrr<-median(projDf[,1])

minIrr<-min(projDf[,1])
maxIrr<-max(projDf[,1])

subTitle<-sprintf("20yr random sampling. Projected final asset value: [%.2f, %.2f]", minIrr, maxIrr)

ggplot(projDf, aes(A)) +
	theme_economist() +
	geom_histogram(aes(y=..density..), binwidth=(maxIrr-minIrr)/100, colour="black", fill="lightblue") +
	geom_density(alpha=.2, fill="#FF6666") +
	geom_vline(aes(xintercept=medianIrr), colour="blue", size=1, linetype='dashed', alpha=0.5) +
	geom_label(aes(x=medianIrr, label=sprintf("%.2f", medianIrr), y=0), colour="blue") +
	labs(y='density', x='projected final asset value', color='', title=mainTitle, subtitle=subTitle) +
	annotate("text", x=max(projDf$A), y=0, label = "@StockViz", hjust=1.1, vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.9)
	
ggsave(sprintf("%s/random.glidePath.VALUE.DENSITY.%s.png", reportPath, equityName), dpi=600, width=12, height=6, units="in")

############################################