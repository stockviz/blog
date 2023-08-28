library('PerformanceAnalytics')
library('extrafont')

Common.PlotCumReturns<-function(toPlot, chartTitle, chartSubTitle, fileName=NULL, dest=NULL){
	if(!is.null(fileName)) {
		#pdf(NULL)
		if(is.null(dest) || is.na(dest)){
			png(fileName, width=1400, height=800, bg="white")
		} else if(dest == "gbus"){
			png(fileName, width=1440, height=1080, bg="white")
		} else if(dest == "twttr"){
			png(fileName, width=1400, height=800, bg="white")
		} else {
			stop("invalid dest argument")
		}
	}
	tryCatch({
		layout(matrix(c(1, 2)), heights = c(2, 1.3), widths = 1)
		par(mar = c(0, 4, 4, 2), family='Segoe UI', bty="n")
		plot_object <-chart.CumReturns(toPlot, cex.legend=1, main=NA, ylab = "Cumulative Return", xaxis = FALSE, legend.loc = "topleft", begin = c("first", "axis"), geometric = TRUE) 
		print(plot_object)
		#mtext("Cumulative Return", side=2, line=1)
		title(main=chartTitle, family='Segoe UI') 
		mtext(chartSubTitle, cex=0.8, line=0.5)
		mtext(sprintf("cumulative: %s; annualized: %s", paste(sprintf("%.2f%%", 100*apply(toPlot, 2, Return.cumulative)), collapse=" / "), paste(sprintf("%.2f%%", 100*apply(toPlot, 2, Return.annualized)), collapse=" / ")), cex=1, line=-1)
		par(mar = c(5, 4, 0, 2))
		plot_object <-chart.Drawdown(toPlot, main = NA, ylab = "Drawdown", event.labels = NULL, ylog = FALSE, geometric = TRUE, bty="n")
		print(plot_object)
		#mtext("Drawdown", side=2, line=1)
		mtext("@StockViz", side=4, col='grey')
	}, error=function(cond){
		print(cond)
	})
	if(!is.null(fileName)) dev.off()
}