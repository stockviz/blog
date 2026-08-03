#!/usr/bin/env Rscript
# US Sector ETF — RRG Rotation (weekly data, modeled after industry-rotation)
#
# Weekly RRG signals (RS_WMA_N=10, MOM_PERIODS=4), monthly rebalance,
# 0.25% drag.  Holds top N ETFs in the Leading quadrant.
#
# Data:  TIINGO_DATA (StockVizUs2)

suppressPackageStartupMessages({
  library(RODBC)
  library(xts)
  library(PerformanceAnalytics)
  library(gt)
  library(webshot2)
  library(ggplot2)
  library(viridis)
  library(dplyr)
})

source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/hollandC/StockViz/R/plot.common.r")

# ── Parameters (mirroring industry-rotation/common.R) ──
ETFS         <- c("XLY","XLK","XLC","XLP","XLF","XLV","XLI","XLU","XLRE","XLB","XLE")
BENCH        <- "SPY"
TOP_N        <- 5L
RS_WMA_N     <- 10L          # weekly WMA smoothing
MOM_PERIODS  <- 4L            # 4-week RS-Momentum
DRAG         <- 0.25 / 100    # 0.25% per rebalance

SUFFIX <- "-rrg"

reportPath <- "/mnt/data/blog/us-etf-sectors"
dir.create(reportPath, showWarnings=FALSE, recursive=TRUE)

# ── WMA helper ──
wma <- function(x, n) {
  if (length(x) < n) return(rep(NA, length(x)))
  wts <- rev(seq_len(n))
  as.numeric(stats::filter(x, wts / sum(wts), sides = 1))
}

# ── Fetch data ──
cat("Fetching data...\n")
lconUS2 <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockVizUs2", ldbuser, ldbpassword),
  case="nochange", believeNRows=TRUE)

allTickers <- c(ETFS, BENCH)
priceList <- list()
for (tkr in allTickers) {
  df <- sqlQuery(lconUS2, sprintf(
    "SELECT time_stamp, c FROM TIINGO_DATA WHERE ticker='%s' ORDER BY time_stamp", tkr))
  if (nrow(df) == 0) stop(sprintf("No data for %s", tkr))
  priceList[[tkr]] <- xts(df$c, as.Date(df$time_stamp))
}
odbcClose(lconUS2)

dailyRetsList <- lapply(priceList, function(p) na.omit(Return.calculate(p, method="discrete")))
dailyRetsFull <- do.call(merge, dailyRetsList)
rowCounts <- rowSums(!is.na(dailyRetsFull))
dailyRets <- dailyRetsFull[index(dailyRetsFull)[rowCounts >= 6], ]

cat(sprintf("Daily range: %s → %s (%d obs)\n",
            min(index(dailyRets)), max(index(dailyRets)), nrow(dailyRets)))

# ── Build weekly returns ──
weeklyRets <- apply.weekly(dailyRets, Return.cumulative)
weeklyBench <- weeklyRets[, BENCH]

cat(sprintf("Weekly range: %s → %s (%d weeks)\n",
            min(index(weeklyRets)), max(index(weeklyRets)), nrow(weeklyRets)))

# ── RRG computation (industry-rotation convention) ──
computeRrg <- function(etfWk, benchWk, upToIdx) {
  # Use all available data up to upToIdx for RS computation
  if (upToIdx < RS_WMA_N * 2 + MOM_PERIODS + 1) return(c(NA, NA))

  ix <- 1:upToIdx
  etfVals <- as.numeric(etfWk[ix])
  benchVals <- as.numeric(benchWk[ix])

  excess <- (1 + etfVals) / (1 + benchVals) - 1
  rs <- cumprod(1 + excess[!is.na(excess)]) * 100
  rs <- na.omit(rs)

  if (length(rs) < RS_WMA_N * 2 + MOM_PERIODS) return(c(NA, NA))

  rsSmooth <- wma(rs, RS_WMA_N)
  rsBench  <- wma(rsSmooth, RS_WMA_N)

  rsRatio <- na.omit(rsSmooth / rsBench * 100)

  if (length(rsRatio) < MOM_PERIODS + 1) return(c(NA, NA))

  n <- length(rsRatio)
  rsMomentum <- rsRatio[n] / rsRatio[n - MOM_PERIODS] * 100

  c(tail(rsRatio, 1), rsMomentum)
}

# ── Monthly rebalance schedule ──
allWeeks <- index(weeklyRets)
monthEnds <- allWeeks[endpoints(weeklyRets, on="months")]

warmupWeeks <- RS_WMA_N * 2 + MOM_PERIODS + 20
rbStart <- allWeeks[1] + warmupWeeks * 7
rbMonths <- monthEnds[monthEnds >= rbStart]
if (length(rbMonths) == 0) stop("Not enough data for warmup")

cat(sprintf("Rebalance months: %d (first=%s, last=%s)\n",
            length(rbMonths), rbMonths[1], tail(rbMonths, 1)))

# ── Backtest loop ──
portDaily <- xts(rep(NA_real_, nrow(dailyRets)), index(dailyRets))

for (i in seq_along(rbMonths)) {
  rbWk <- rbMonths[i]
  wkIdx <- which(allWeeks == rbWk)

  # Compute RRG at week end
  scores <- data.frame(ETF=character(), rsRatio=numeric(), rsMomentum=numeric(),
                        stringsAsFactors=FALSE)
  for (etf in ETFS) {
    rrg <- computeRrg(weeklyRets[, etf], weeklyBench, wkIdx)
    if (!is.na(rrg[1]) && is.finite(rrg[1])) {
      scores <- rbind(scores, data.frame(ETF=etf, rsRatio=rrg[1], rsMomentum=rrg[2],
                                          stringsAsFactors=FALSE))
    }
  }

  # Top N from Leading quadrant
  leading <- scores[scores$rsRatio > 100 & scores$rsMomentum > 100, ]
  leading <- leading[order(-leading$rsRatio), ]

  if (nrow(leading) > 0) {
    holdings <- head(leading$ETF, TOP_N)
  } else {
    scores <- scores[order(-scores$rsRatio), ]
    holdings <- head(scores$ETF, TOP_N)
  }

  # Invest for the next calendar month
  rbDt <- as.Date(rbWk)
  ym <- as.numeric(format(rbDt, "%Y")) * 12 + as.numeric(format(rbDt, "%m")) - 1
  nxtMo <- as.Date(sprintf("%d-%02d-01", (ym+2) %/% 12, (ym+2) %% 12 + 1))
  period <- paste0(rbDt, "/", nxtMo - 1)

  moRets <- dailyRets[period, holdings, drop=FALSE]
  ew <- rowMeans(moRets, na.rm=TRUE)
  ewXts <- xts(ew, index(moRets))

  firstDay <- which(!is.na(ewXts))[1]
  if (!is.na(firstDay)) ewXts[firstDay] <- ewXts[firstDay] - DRAG

  portDaily[index(na.omit(ewXts))] <- ewXts[index(na.omit(ewXts))]
}

portDaily <- na.omit(portDaily)
spyDaily <- dailyRets[index(portDaily), BENCH]

cat(sprintf("Portfolio date range: %s → %s (%d obs)\n",
            min(index(portDaily)), max(index(portDaily)), nrow(portDaily)))

# ── Annual returns ──
portAnnual <- apply.yearly(portDaily, Return.cumulative)
spyAnnual <- apply.yearly(xts(spyDaily, index(portDaily)), Return.cumulative)
annualTbl <- merge(portAnnual, spyAnnual)
colnames(annualTbl) <- c("RRG Portfolio", "SPY")

annualDf <- data.frame(
  Year = format(index(annualTbl), "%Y"),
  Portfolio = round(as.numeric(annualTbl[,1])*100, 2),
  SPY = round(as.numeric(annualTbl[,2])*100, 2),
  stringsAsFactors = FALSE
)

annualPlot <- reshape2::melt(annualDf, id.vars="Year", variable.name="Series", value.name="Return")
annualPlot$Year <- as.Date(paste0(annualPlot$Year, "-12-31"))

p <- ggplot(annualPlot, aes(x=Year, y=Return, fill=Series)) +
  geom_bar(stat="identity", position="dodge", alpha=0.85) +
  scale_fill_viridis(discrete=TRUE, option="D") +
  labs(title="Annual Returns — RRG US Sector Rotation",
       subtitle=sprintf("Top %d Leading, Weekly RRG (%d/4), Monthly Rebalance, 0.25%% Drag", TOP_N, RS_WMA_N),
       caption="@StockViz", y="Return (%)", x="") +
  theme_minimal(base_size=12) +
  theme(legend.position="bottom", plot.caption=element_text(hjust=1, face="italic", size=8))

ggsave(file.path(reportPath, sprintf("annual-returns%s.png", SUFFIX)), p, width=12, height=6, dpi=120)

# ── Cumulative returns ──
mergedAll <- merge(portDaily, spyDaily)
colnames(mergedAll) <- c("RRG Portfolio", "SPY")
sr_all <- as.numeric(SharpeRatio.annualized(mergedAll))

Common.PlotCumReturns(mergedAll,
  "Cumulative Returns — RRG US Sector Rotation",
  sprintf("RRG Top%d SR=%.2f  SPY SR=%.2f", TOP_N, sr_all[1], sr_all[2]),
  file.path(reportPath, sprintf("cumulative%s.png", SUFFIX)), NULL)

# ── Metrics table ──
metricsDf <- rbind(
  data.frame(
    Portfolio = sprintf("RRG Top%d", TOP_N),
    CAGR   = sprintf("%.2f", as.numeric(Return.annualized(portDaily))*100),
    Sharpe = sprintf("%.2f", as.numeric(SharpeRatio.annualized(portDaily))),
    MaxDD  = sprintf("%.2f", as.numeric(maxDrawdown(portDaily))*100),
    Vol    = sprintf("%.2f", as.numeric(StdDev.annualized(portDaily))*100),
    stringsAsFactors = FALSE
  ),
  data.frame(
    Portfolio = "SPY",
    CAGR   = sprintf("%.2f", as.numeric(Return.annualized(spyDaily))*100),
    Sharpe = sprintf("%.2f", as.numeric(SharpeRatio.annualized(spyDaily))),
    MaxDD  = sprintf("%.2f", as.numeric(maxDrawdown(spyDaily))*100),
    Vol    = sprintf("%.2f", as.numeric(StdDev.annualized(spyDaily))*100),
    stringsAsFactors = FALSE
  )
)

gtMetrics <- metricsDf |>
  gt() |>
  tab_header(title="Performance Metrics", subtitle="RRG US Sector Rotation vs SPY") |>
  tab_source_note(source_note="@StockViz")

gt::gtsave(gtMetrics, file.path(reportPath, sprintf("metrics%s.png", SUFFIX)))

# ── README ──
readme <- paste0(
  "# US Sector ETF — RRG Rotation\n",
  "\n",
  "Blog: https://stockviz.biz/...\n",
  "\n",
  "## Summary\n",
  "\n",
  "Relative Rotation Graph (RRG) signals using weekly returns (RS_WMA_N=",
  RS_WMA_N, ", MOM_PERIODS=", MOM_PERIODS, "), identical to the\n",
  "industry-rotation methodology. Top ", TOP_N,
  " ETFs in the Leading quadrant held monthly with 0.25% drag.\n",
  "\n",
  "Data: ", format(min(index(portDaily)), "%Y-%m-%d"), " → ",
  format(max(index(portDaily)), "%Y-%m-%d"), "\n",
  "\n",
  "**RRG Top", TOP_N, "** vs **SPY**:\n",
  "- RRG CAGR: ", metricsDf$CAGR[1], "%\n",
  "- SPY  CAGR: ", metricsDf$CAGR[2], "%\n",
  "- RRG Sharpe: ", metricsDf$Sharpe[1], "\n",
  "- SPY  Sharpe: ", metricsDf$Sharpe[2], "\n",
  "- RRG MaxDD: ", metricsDf$MaxDD[1], "%\n",
  "- SPY  MaxDD: ", metricsDf$MaxDD[2], "%\n",
  "\n",
  "ETFs: ", paste(ETFS, collapse=", "), "\n"
)

writeLines(readme, file.path(reportPath, sprintf("README%s.md", SUFFIX)))
cat(sprintf("\nDone — output in %s\n", reportPath))
