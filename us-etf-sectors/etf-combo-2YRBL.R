#!/usr/bin/env Rscript
# US Sector ETF — Rolling 5-year, 2-year alternating rebalance (2Y-RBL)
#
# Data:  TIINGO_DATA (StockVizUs2)
# ETFs:  XLY XLK XLC XLP XLF XLV XLI XLU XLRE XLB XLE
# Bench: SPY
#
# Same rolling 5-year lookback selection, but the portfolio is split into 2 halves.
# Each half rebalances every other year, taking turns.  One half rebalances in odd
# years while the other stays invested; they swap in even years.
# Outputs suffixed with "-2Y-RBL".

suppressPackageStartupMessages({
  library(RODBC)
  library(xts)
  library(PerformanceAnalytics)
  library(gt)
  library(webshot2)
  library(ggplot2)
  library(viridis)
})

source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/hollandC/StockViz/R/plot.common.r")

# ── Parameters ──
args <- commandArgs(trailingOnly=TRUE)
METHOD <- if (length(args) > 0) toupper(args[1]) else "LD"
if (!METHOD %in% c("LD", "SR", "HD")) stop("METHOD must be LD, SR, or HD")

ETFS        <- c("XLY","XLK","XLC","XLP","XLF","XLV","XLI","XLU","XLRE","XLB","XLE")
BENCH       <- "SPY"
COMBO_SIZE  <- 4
WINDOW_YRS  <- 5

SUFFIX <- paste0("-2Y-RBL-", METHOD)

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

dailyRetsList <- lapply(priceList, function(p) na.omit(Return.calculate(p, method="discrete")))
dailyRetsFull <- do.call(merge, dailyRetsList)
rowCounts <- rowSums(!is.na(dailyRetsFull))
dailyRets <- dailyRetsFull[index(dailyRetsFull)[rowCounts >= 6], ]

cat(sprintf("Date range: %s → %s (%d obs)\n",
            min(index(dailyRets)), max(index(dailyRets)), nrow(dailyRets)))

# ── Enumerate combinations ──
combos <- combn(ETFS, COMBO_SIZE, simplify=FALSE)
cat(sprintf("Evaluating %d combinations of %d ETFs...\n", length(combos), COMBO_SIZE))

# ── Selection helper ──
pickCombo <- function(lbRets) {
  bestCombo <- NULL
  bestScore <- if (METHOD == "LD") Inf else -Inf
  for (combo in combos) {
    ewRet <- rowMeans(lbRets[, combo], na.rm=TRUE)
    if (all(is.na(ewRet))) next
    if (METHOD == "SR") {
      score <- as.numeric(SharpeRatio.annualized(xts(ewRet, index(lbRets))))
    } else {
      score <- as.numeric(maxDrawdown(xts(ewRet, index(lbRets))))
    }
    if (!is.na(score) && ((METHOD == "LD" && score < bestScore) ||
                          (METHOD %in% c("SR", "HD") && score > bestScore))) {
      bestScore <- score
      bestCombo <- combo
    }
  }
  list(combo=bestCombo, score=bestScore)
}

# ── Rolling 5-year selection with 2Y alternating rebalance ──
yrEnds <- unique(as.numeric(format(index(dailyRets), "%Y")))
firstYr <- min(yrEnds) + WINDOW_YRS
investYrs <- yrEnds[yrEnds >= firstYr]

halfA <- xts(rep(NA_real_, nrow(dailyRets)), index(dailyRets))
halfB <- xts(rep(NA_real_, nrow(dailyRets)), index(dailyRets))

# Track which combo each half is invested in
comboA <- NULL
comboB <- NULL

for (yr in investYrs) {
  lookbackStart <- as.Date(sprintf("%d-01-01", yr - WINDOW_YRS))
  lookbackEnd   <- as.Date(sprintf("%d-12-31", yr - 1))
  lbRets <- dailyRets[paste0(lookbackStart, "/", lookbackEnd)]

  picked <- pickCombo(lbRets)
  investStart <- as.Date(sprintf("%d-01-01", yr))
  investEnd   <- as.Date(sprintf("%d-12-31", yr))
  ivRets <- dailyRets[paste0(investStart, "/", investEnd)]

  if (yr %% 2 == 1) {
    # Odd year: halfA rebalances, halfB stays
    if (!is.null(picked$combo)) comboA <- picked$combo
    if (!is.null(comboA)) {
      ewA <- rowMeans(ivRets[, comboA], na.rm=TRUE)
      halfA[index(xts(ewA, index(ivRets)))] <- ewA
    }
    if (!is.null(comboB)) {
      ewB <- rowMeans(ivRets[, comboB], na.rm=TRUE)
      halfB[index(xts(ewB, index(ivRets)))] <- ewB
    }
  } else {
    # Even year: halfB rebalances, halfA stays
    if (!is.null(picked$combo)) comboB <- picked$combo
    if (!is.null(comboA)) {
      ewA <- rowMeans(ivRets[, comboA], na.rm=TRUE)
      halfA[index(xts(ewA, index(ivRets)))] <- ewA
    }
    if (!is.null(comboB)) {
      ewB <- rowMeans(ivRets[, comboB], na.rm=TRUE)
      halfB[index(xts(ewB, index(ivRets)))] <- ewB
    }
  }

  cat(sprintf("  %d: A=%s  B=%s (%s=%.2f)\n", yr,
              paste(comboA, collapse="+"), paste(comboB, collapse="+"),
              METHOD, picked$score))
}

# Combined portfolio = 50:50 halves
portDaily <- 0.5 * halfA + 0.5 * halfB
portDaily <- na.omit(portDaily)
spyDaily <- dailyRets[index(portDaily), BENCH]

cat(sprintf("Portfolio date range: %s → %s (%d obs)\n",
            min(index(portDaily)), max(index(portDaily)), nrow(portDaily)))

# ── Annual returns ──
portAnnual <- apply.yearly(portDaily, Return.cumulative)
spyAnnual <- apply.yearly(xts(spyDaily, index(portDaily)), Return.cumulative)
annualTbl <- merge(portAnnual, spyAnnual)
colnames(annualTbl) <- c("2Y-RBL Portfolio", "SPY")

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
  labs(title=sprintf("Annual Returns — %d-ETF 2Y-RBL (Rolling %dy, %s)", COMBO_SIZE, WINDOW_YRS, METHOD),
       subtitle=sprintf("%s → %s", format(min(index(portDaily)), "%Y"), format(max(index(portDaily)), "%Y")),
       caption="@StockViz", y="Return (%)", x="") +
  theme_minimal(base_size=12) +
  theme(legend.position="bottom", plot.caption=element_text(hjust=1, face="italic", size=8))

ggsave(file.path(reportPath, sprintf("annual-returns%s.png", SUFFIX)), p, width=12, height=6, dpi=120)

# ── Cumulative returns ──
mergedAll <- merge(portDaily, spyDaily)
colnames(mergedAll) <- c(sprintf("%d-ETF 2Y-RBL", COMBO_SIZE), "SPY")
sr_all <- as.numeric(SharpeRatio.annualized(mergedAll))

Common.PlotCumReturns(mergedAll,
  sprintf("Cumulative Returns — 2Y-RBL Rolling %dy %s", WINDOW_YRS, METHOD),
  sprintf("%d-ETF 2Y-RBL SR=%.2f  SPY SR=%.2f", COMBO_SIZE, sr_all[1], sr_all[2]),
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
  calcMetrics(mergedAll[,1], sprintf("%d-ETF 2Y-RBL", COMBO_SIZE)),
  calcMetrics(mergedAll[,2], "SPY")
)

gtMetrics <- metricsDf |>
  gt() |>
  tab_header(title="Performance Metrics", subtitle=sprintf("2Y-RBL Rolling %dy %s vs SPY", WINDOW_YRS, METHOD)) |>
  tab_source_note(source_note="@StockViz")

gt::gtsave(gtMetrics, file.path(reportPath, sprintf("metrics%s.png", SUFFIX)))

# ── README ──
readme <- paste0(
  "# US Sector ETF — 2-Year Alternating Rebalance (2Y-RBL)\n",
  "\n",
  "Blog: https://stockviz.biz/...\n",
  "\n",
  "## Summary\n",
  "\n",
  "The portfolio is split into two equal halves. Each half rebalances every other\n",
  "year, taking turns: half A rebalances in odd years, half B in even years.\n",
  "Each rebalance selects the best ", COMBO_SIZE, "-ETF combination based on a\n",
  WINDOW_YRS, "-year rolling lookback using the \"", METHOD, "\" criterion.\n",
  "\n",
  "Data period: ", format(min(index(portDaily)), "%Y-%m-%d"), " → ",
  format(max(index(portDaily)), "%Y-%m-%d"), "\n",
  "\n",
  "**", COMBO_SIZE, "-ETF 2Y-RBL** vs **SPY**:\n",
  "- CAGR: ", metricsDf$CAGR[1], "%\n",
  "- Sharpe: ", metricsDf$Sharpe[1], "\n",
  "- MaxDD: ", metricsDf$MaxDD[1], "%\n",
  "\n",
  "## Files\n",
  "\n",
  "- `annual-returns", SUFFIX, ".png` — Annual returns column chart\n",
  "- `cumulative", SUFFIX, ".png` — Cumulative returns (2Y-RBL vs SPY)\n",
  "- `metrics", SUFFIX, ".png` — Performance metrics table\n",
  "\n",
  "## Methodology\n",
  "\n",
  "All ", length(ETFS), " choose ", COMBO_SIZE, " = ", length(combos),
  " combinations are evaluated per ", WINDOW_YRS, "-year rolling window.\n",
  "The method (\"", METHOD, "\") selects the best combination. The portfolio\n",
  "is split 50:50; each half rebalances every 2 years on alternating years.\n"
)

writeLines(readme, file.path(reportPath, sprintf("README%s.md", SUFFIX)))
cat(sprintf("\nDone — output in %s\n", reportPath))
