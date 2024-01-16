library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('lubridate')
library('reshape2')
library('tidyverse')
library('ggthemes')
library('viridis')
library('patchwork')
library('DT')
library('webshot2')

Sys.setenv(CHROMOTE_CHROME = "C:/Users/shyam/AppData/Local/Google/Chrome/Application/chrome.exe")

options("scipen"=100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "."
source("d:/stockviz/r/config.r")

lcon <- odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;", ldbserver, "StockViz", ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)

maxDiffAsof <- sqlQuery(lcon, "select max(as_of) from MF_TRACKING_DIFF")[[1]]

diffDf <- sqlQuery(lcon, sprintf("select * from MF_TRACKING_DIFF mt, MF_INDEX_FUND mi where mi.ID = mt.MF_IF_ID and YR = 1 and AS_OF = '%s'", maxDiffAsof))

diffMedian <- diffDf %>% summarize(REGULAR = median(REGULAR, na.rm=TRUE), DIRECT = median(DIRECT, na.rm=TRUE)) %>% pivot_longer(cols = everything())
diffTp <- diffDf %>% pivot_longer(c(REGULAR, DIRECT))

p1 <- ggplot(diffTp) +
	theme_economist() +
	stat_density(aes(color=name, x=value), geom = "line", position = "identity", linewidth=1) +
	geom_vline(data = diffMedian, aes(color = name, xintercept = value), linewidth=1) +
	geom_text(data = diffMedian, aes(x = value, y = 0, label = value), angle = 90, vjust = 'bottom', hjust='left', fontface='bold') +
	scale_color_viridis_d() +
	labs(x="tracking difference (%)", y="density", color='', title = "1-year") 

topDiff <- diffDf %>% mutate(DIFF = ifelse(is.na(DIRECT), REGULAR, DIRECT)) %>% slice_min(DIFF, n=10) %>% select(SCHEME_NAME, DIFF)
dtDiff <- datatable(topDiff, rownames = F, class = 'cell-border stripe', filter='none', options = list(dom = 't', pageLength = 30, ordering=F), 
					caption=htmltools::tags$caption(style = 'text-align: center; font-weight: bold; font-family: "Segoe UI"',
													htmltools::em("Worst 1-Year Tracking Difference")))

saveWidget(dtDiff, sprintf("%s/top-tracking-diff.1yr.html", reportPath))
webshot(sprintf("%s/top-tracking-diff.1yr.html", reportPath), sprintf("%s/top-tracking-diff.1yr.png", reportPath))
	
diffDf <- sqlQuery(lcon, sprintf("select * from MF_TRACKING_DIFF mt, MF_INDEX_FUND mi where mi.ID = mt.MF_IF_ID and YR = 5 and AS_OF = '%s'", maxDiffAsof))

diffMedian <- diffDf %>% summarize(REGULAR = median(REGULAR, na.rm=TRUE), DIRECT = median(DIRECT, na.rm=TRUE)) %>% pivot_longer(cols = everything())
diffTp <- diffDf %>% pivot_longer(c(REGULAR, DIRECT))

p2 <- ggplot(diffTp) +
	theme_economist() +
	stat_density(aes(color=name, x=value), geom = "line", position = "identity", linewidth=1) +
	geom_vline(data = diffMedian, aes(color = name, xintercept = value), linewidth=1) +
	geom_text(data = diffMedian, aes(x = value, y = 0, label = value), angle = 90, vjust = 'bottom', hjust='left', fontface='bold') +
	scale_color_viridis_d() +
	labs(x="tracking difference (%)", y="density", color='', title = "5-year") 


p1/p2 + plot_annotation(title = 'Index Fund Tracking Difference', caption = '@StockViz') & theme_economist()

ggsave(sprintf("%s/tracking-difference.png", reportPath), width=12, height=12, units="in")

topDiff <- diffDf %>% mutate(DIFF = ifelse(is.na(DIRECT), REGULAR, DIRECT)) %>% slice_min(DIFF, n=10) %>% select(SCHEME_NAME, DIFF)
dtDiff <- datatable(topDiff, rownames = F, class = 'cell-border stripe', filter='none', options = list(dom = 't', pageLength = 30, ordering=F), 
					caption=htmltools::tags$caption(style = 'text-align: center; font-weight: bold; font-family: "Segoe UI"',
													htmltools::em("Worst 5-Year Tracking Difference")))

saveWidget(dtDiff, sprintf("%s/top-tracking-diff.5yr.html", reportPath))
webshot(sprintf("%s/top-tracking-diff.5yr.html", reportPath), sprintf("%s/top-tracking-diff.5yr.png", reportPath))
