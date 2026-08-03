#!/usr/bin/env Rscript
# US Sector ETF — Rolling 5-year lowest-drawdown (LD) selection
#
# Data:  TIINGO_DATA (StockVizUs2)
# ETFs:  XLY XLK XLC XLP XLF XLV XLI XLU XLRE XLB XLE
# Bench: SPY
#
# Every 5 years, pick the combination with the lowest MaxDD during that
# window and invest in it for the next year. Outputs suffixed with "-LD".

suppressPackageStartupMessages({
  library(RODBC)
  library(xts)
  library(PerformanceAnalytics)
  library(gt)
  library(webshot2)
  library(ggplot2)
  library(viridis)
  library(zoo)  # for yearmon
})

source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/hollandC/StockViz/R/plot.common.r")

# ── Parameters ──
args <- commandArgs(trailingOnly=TRUE)
METHOD <- if (length(args) > 0) toupper(args[1]) else "LD"
# LD = lowest drawdown, SR = highest Sharpe, HD = highest drawdown
if (!METHOD %in% c("LD", "SR", "HD")) stop("METHOD must be LD, SR, or HD")

ETFS        <- c("XLY","XLK","XLC","XLP","XLF","XLV","XLI","XLU","XLRE","XLB","XLE")
BENCH       <- "SPY"
COMBO_SIZE  <- 4
WINDOW_YRS  <- 5

SUFFIX <- paste0("-", METHOD)

reportPath <- "/mnt/data/blog/us-etf-sectors"
dir.create(reportPath, showWarnings=FALSE, recursive=TRUE)

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

# Build daily returns per ticker; start when >= 6 tickers have data
dailyRetsList <- lapply(priceList, function(p) na.omit(Return.calculate(p, method="discrete")))
dailyRetsFull <- do.call(merge, dailyRetsList)
rowCounts <- rowSums(!is.na(dailyRetsFull))
dailyRets <- dailyRetsFull[index(dailyRetsFull)[rowCounts >= 6], ]

cat(sprintf("Date range: %s → %s (%d obs)\n",
            min(index(dailyRets)), max(index(dailyRets)), nrow(dailyRets)))

# ── Enumerate combinations ──
combos <- combn(ETFS, COMBO_SIZE, simplify=FALSE)
cat(sprintf("Evaluating %d combinations of %d ETFs...\n", length(combos), COMBO_SIZE))

# ── Rolling 5-year lowest-drawdown selection ──
yrEnds <- unique(as.numeric(format(index(dailyRets), "%Y")))
firstYr <- min(yrEnds) + WINDOW_YRS  # first year we can start
investYrs <- yrEnds[yrEnds >= firstYr]

# Build the selected-portfolio daily return series
ldDaily <- xts(rep(NA_real_, nrow(dailyRets)), index(dailyRets))

for (yr in investYrs) {
  # Lookback: 5 years ending at yr-1
  lookbackStart <- as.Date(sprintf("%d-01-01", yr - WINDOW_YRS))
  lookbackEnd   <- as.Date(sprintf("%d-12-31", yr - 1))
  lbRets <- dailyRets[paste0(lookbackStart, "/", lookbackEnd)]

  bestCombo <- NULL
  bestScore <- if (METHOD == "LD") Inf else -Inf  # LD: minimize, SR/HD: maximize

  for (combo in combos) {
    ewRet <- rowMeans(lbRets[, combo], na.rm=TRUE)
    if (all(is.na(ewRet))) next

    if (METHOD == "SR") {
      score <- as.numeric(SharpeRatio.annualized(xts(ewRet, index(lbRets))))
    } else {
      score <- as.numeric(maxDrawdown(xts(ewRet, index(lbRets))))  # LD/HD
    }

    if (!is.na(score) && ((METHOD == "LD" && score < bestScore) ||
                          (METHOD %in% c("SR", "HD") && score > bestScore))) {
      bestScore <- score
      bestCombo <- combo
    }
  }

  if (is.null(bestCombo)) next

  cat(sprintf("  %d: %s (%s=%.2f)\n",
              yr, paste(bestCombo, collapse="+"), METHOD, bestScore))
  investStart <- as.Date(sprintf("%d-01-01", yr))
  investEnd   <- as.Date(sprintf("%d-12-31", yr))
  ivRets <- dailyRets[paste0(investStart, "/", investEnd)]
  ewRet <- rowMeans(ivRets[, bestCombo], na.rm=TRUE)
  ldDaily[index(xts(ewRet, index(ivRets)))] <- ewRet
}

# Drop NAs (years before first window, or missing data)
ldDaily <- na.omit(ldDaily)
spyDaily <- dailyRets[index(ldDaily), BENCH]

cat(sprintf("LD portfolio date range: %s → %s (%d obs)\n",
            min(index(ldDaily)), max(index(ldDaily)), nrow(ldDaily)))

# ── Annual returns ──
ldAnnual <- apply.yearly(ldDaily, Return.cumulative)
spyAnnual <- apply.yearly(xts(spyDaily, index(ldDaily)), Return.cumulative)
annualTbl <- merge(ldAnnual, spyAnnual)
colnames(annualTbl) <- c(sprintf("%s Portfolio", METHOD), "SPY")

annualDf <- data.frame(
  Year = format(index(annualTbl), "%Y"),
  Portfolio = round(as.numeric(annualTbl[,1])*100, 2),
  SPY = round(as.numeric(annualTbl[,2])*100, 2),
  stringsAsFactors = FALSE
)

# Column chart
annualPlot <- reshape2::melt(annualDf, id.vars="Year", variable.name="Series", value.name="Return")
annualPlot$Year <- as.Date(paste0(annualPlot$Year, "-12-31"))

p <- ggplot(annualPlot, aes(x=Year, y=Return, fill=Series)) +
  geom_bar(stat="identity", position="dodge", alpha=0.85) +
  scale_fill_viridis(discrete=TRUE, option="D") +
  labs(title=sprintf("Annual Returns — %d-ETF %s (Rolling %dy)", COMBO_SIZE, METHOD, WINDOW_YRS),
       subtitle=sprintf("%s → %s", format(min(index(ldDaily)), "%Y"), format(max(index(ldDaily)), "%Y")),
       caption="@StockViz", y="Return (%)", x="") +
  theme_minimal(base_size=12) +
  theme(legend.position="bottom", plot.caption=element_text(hjust=1, face="italic", size=8))

ggsave(file.path(reportPath, sprintf("annual-returns%s.png", SUFFIX)), p, width=12, height=6, dpi=120)

# ── Cumulative returns ──
mergedLD <- merge(ldDaily, spyDaily)
colnames(mergedLD) <- c(sprintf("%d-ETF %s", COMBO_SIZE, METHOD), "SPY")
sr_all <- as.numeric(SharpeRatio.annualized(mergedLD))

Common.PlotCumReturns(mergedLD,
  sprintf("Cumulative Returns — Rolling %dy %s", WINDOW_YRS, METHOD),
  sprintf("%d-ETF %s SR=%.2f  SPY SR=%.2f", COMBO_SIZE, METHOD, sr_all[1], sr_all[2]),
  file.path(reportPath, sprintf("cumulative%s.png", SUFFIX)), NULL)

# ── Metrics table ──
calcMetrics <- function(rets, name) {
  data.frame(
    Portfolio = name,
    CAGR   = sprintf("%.2f", as.numeric(Return.annualized(rets))*100),
    Sharpe = sprintf("%.2f", as.numeric(SharpeRatio.annualized(rets))),
    MaxDD  = sprintf("%.2f", as.numeric(maxDrawdown(rets))*100),
    Vol    = sprintf("%.2f", as.numeric(StdDev.annualized(rets))*100),
    Calmar = sprintf("%.2f", as.numeric(Return.annualized(rets)/maxDrawdown(rets))),
    stringsAsFactors = FALSE
  )
}

metricsDf <- rbind(
  calcMetrics(mergedLD[,1], sprintf("%d-ETF %s", COMBO_SIZE, METHOD)),
  calcMetrics(mergedLD[,2], "SPY")
)

gtMetrics <- metricsDf |>
  gt() |>
  tab_header(title="Performance Metrics", subtitle=sprintf("Rolling %dy %s vs SPY", WINDOW_YRS, METHOD)) |>
  tab_source_note(source_note="@StockViz")

gt::gtsave(gtMetrics, file.path(reportPath, sprintf("metrics%s.png", SUFFIX)))

# ── README ──
method_full <- if (METHOD == "SR") "highest Sharpe ratio" else if (METHOD == "HD") "highest max drawdown" else "lowest max drawdown"
method_label <- if (METHOD == "SR") "Highest-Sharpe" else if (METHOD == "HD") "Highest-Drawdown" else "Lowest-Drawdown"

readme <- paste0(
  "# US Sector ETF — Rolling 5-Year ", method_label, " Selection\n",
  "\n",
  "Blog: https://stockviz.biz/...\n",
  "\n",
  "## Summary\n",
  "\n",
  "Among the 11 US sector ETFs (XLY, XLK, XLC, XLP, XLF, XLV, XLI, XLU, XLRE, XLB, XLE),\n",
  "every ", WINDOW_YRS, " years, the **equal-weighted combination of ", COMBO_SIZE, " ETFs with the ",
  method_full, "** during the prior ", WINDOW_YRS, "-year window is selected and held for 1 year.\n",
  "\n",
  "Data period: ", format(min(index(ldDaily)), "%Y-%m-%d"), " → ",
  format(max(index(ldDaily)), "%Y-%m-%d"), "\n",
  "\n",
  "**", COMBO_SIZE, "-ETF ", METHOD, "** vs **SPY**:\n",
  "- CAGR: ", metricsDf$CAGR[1], "%\n",
  "- Sharpe: ", metricsDf$Sharpe[1], "\n",
  "- MaxDD: ", metricsDf$MaxDD[1], "%\n",
  "\n",
  "## Files\n",
  "\n",
  "- `annual-returns", SUFFIX, ".png` — Annual returns column chart\n",
  "- `cumulative", SUFFIX, ".png` — Cumulative returns (", METHOD, " vs SPY)\n",
  "- `metrics", SUFFIX, ".png` — Performance metrics table\n",
  "\n",
  "## Methodology\n",
  "\n",
  "All ", length(ETFS), " choose ", COMBO_SIZE, " = ", length(combos),
  " combinations are evaluated per ", WINDOW_YRS, "-year rolling window.\n",
  "The method (\"", METHOD, "\") selects the combination with the ",
  method_full, " and holds it for 1 year, then re-evaluates.\n"
)

writeLines(readme, file.path(reportPath, sprintf("README%s.md", SUFFIX)))
cat(sprintf("\nDone — output in %s\n", reportPath))
