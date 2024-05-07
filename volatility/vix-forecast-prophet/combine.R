library('tidyverse')
library('lubridate')

library('ggthemes')
library('viridis')

pdf(NULL)
options("scipen"=100)
options(stringsAsFactors = FALSE)

reportPath <- "."

load(sprintf("%s/forecastStatsDf.Rdata", reportPath)) #forecastStatsDf

lookback <- 500 #days
forecastDays <- 20 

forecastStatsDf %>% pivot_longer(-TIME_STAMP, names_to='variable') %>% {
	ggplot(., aes(x=value, color=variable)) +
		stat_density(geom = "line", position = "identity", linewidth=1) +
		theme_economist() +
		scale_color_viridis_d() +
		labs(x = "rmse", y="count", fill="", color="", size="", 
				title=sprintf("%d-day INDIA VIX Forecast", forecastDays), 
				subtitle=sprintf("N = %d; lb = %d days; [%s:%s]", nrow(forecastStatsDf), lookback, min(.$TIME_STAMP), max(.$TIME_STAMP)), 
				caption='@StockViz') 
}

ggsave(sprintf("%s/vix-forecast-rmse.png", reportPath), width=12, height=6, units="in")
