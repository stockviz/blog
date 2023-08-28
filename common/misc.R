library('gridExtra')
library('lubridate')
library('extrafont')
library('xts')

tableTheme <- ttheme_minimal(
		core = list(fg_params=list(fontfamily='Segoe UI')),
		colhead = list(fg_params=list(fontfamily='Segoe UI')),
		rowhead = list(fg_params=list(fontfamily='Segoe UI')))

Common.NormalizeMonthlyDates<-function(xtsData){
	xtsDf<-data.frame(xtsData)
	xtsDf$T<-as.Date(sprintf("%d-%d-20", year(index(xtsData)), month(index(xtsData))))

	return(xts(subset(xtsDf, select = -c(T)), xtsDf$T))
}

Common.NormalizeWeeklyDates<-function(xtsData){
	xtsDf<-data.frame(xtsData)
	xtsDf$T<-ceiling_date(index(xtsData), "week")
	return(xts(subset(xtsDf, select = -c(T)), xtsDf$T))
}	