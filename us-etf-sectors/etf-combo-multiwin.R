#!/usr/bin/env Rscript
# US Sector ETF — Multi-window walk-forward: train best-Sharpe combos on 1-5yr
# lookbacks, then evaluate each on the test set.  Train/test split at 2019-12-31.
#
# Data:  TIINGO_DATA (StockVizUs2)
# ETFs:  XLY XLK XLC XLP XLF XLV XLI XLU XLRE XLB XLE
# Bench: SPY

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
ETFS        <- c("XLY","XLK","XLC","XLP","XLF","XLV","XLI","XLU","XLRE","XLB","XLE")
BENCH       <- "SPY"
COMBO_SIZE  <- 4
TRAIN_END   <- "2019-12-31"
TEST_START  <- "2020-01-01"
WINDOWS     <- 1:5
DRAG        <- 0.25 / 100

SUFFIX <- "-multiwin"

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

# ── Split train / test ──
trainRets <- dailyRets[paste0("/", TRAIN_END)]
testRets  <- dailyRets[paste0(TEST_START, "/")]

cat(sprintf("Train: %s → %s  |  Test: %s → %s\n",
            min(index(trainRets)), max(index(trainRets)),
            min(index(testRets)), max(index(testRets))))

# ── Enumerate combinations ──
combos <- combn(ETFS, COMBO_SIZE, simplify=FALSE)
cat(sprintf("Evaluating %d combinations of %d ETFs...\n", length(combos), COMBO_SIZE))

# ── Sweep all combos × all windows on train, pick the single best SR ──
cat("Sweeping combos × windows on train...\n")
bestWinCombo <- NULL
bestWinSR <- -Inf
bestWindow <- NA

for (w in WINDOWS) {
  trainStart <- as.Date(sprintf("%d-01-01",
    as.numeric(format(max(index(trainRets)), "%Y")) - w))
  wTrain <- trainRets[paste0(trainStart, "/")]
  if (nrow(wTrain) < 252) next

  for (combo in combos) {
    ewRet <- rowMeans(wTrain[, combo], na.rm=TRUE)
    if (all(is.na(ewRet))) next
    sr <- as.numeric(SharpeRatio.annualized(xts(ewRet, index(wTrain))))
    if (!is.na(sr) && sr > bestWinSR) {
      bestWinSR <- sr
      bestWinCombo <- combo
      bestWindow <- w
    }
  }
}

if (is.null(bestWinCombo)) stop("No valid combination found")

cat(sprintf("Best: %dy lookback, %s (train SR=%.2f)\n",
            bestWindow, paste(bestWinCombo, collapse="+"), bestWinSR))

# ── Evaluate best combo on full train and test ──
ewTrain <- rowMeans(trainRets[, bestWinCombo], na.rm=TRUE)
ewTrain[1] <- ewTrain[1] - DRAG
ewTest  <- rowMeans(testRets[, bestWinCombo], na.rm=TRUE)
ewTest[1] <- ewTest[1] - DRAG

portTrain <- ewTrain
portTest  <- ewTest

# Add SPY benchmarks
spyTrainMet <- data.frame(
  Combo = "SPY",
  CAGR   = round(as.numeric(Return.annualized(trainRets[, BENCH]))*100, 2),
  Sharpe = round(as.numeric(SharpeRatio.annualized(trainRets[, BENCH])), 2),
  MaxDD  = round(as.numeric(maxDrawdown(trainRets[, BENCH]))*100, 2),
  Period = "Train", stringsAsFactors = FALSE
)
spyTestMet <- data.frame(
  Combo = "SPY",
  CAGR   = round(as.numeric(Return.annualized(testRets[, BENCH]))*100, 2),
  Sharpe = round(as.numeric(SharpeRatio.annualized(testRets[, BENCH])), 2),
  MaxDD  = round(as.numeric(maxDrawdown(testRets[, BENCH]))*100, 2),
  Period = "Test", stringsAsFactors = FALSE
)
# Build simple metrics table: best combo train/test + SPY train/test
bestMet <- data.frame(
  Combo = paste(bestWinCombo, collapse="+"),
  CAGR   = round(as.numeric(Return.annualized(xts(ewTrain, index(trainRets))))*100, 2),
  Sharpe = round(as.numeric(SharpeRatio.annualized(xts(ewTrain, index(trainRets)))), 2),
  MaxDD  = round(as.numeric(maxDrawdown(xts(ewTrain, index(trainRets))))*100, 2),
  Period = "Train", stringsAsFactors = FALSE
)
bestMetTest <- data.frame(
  Combo = paste(bestWinCombo, collapse="+"),
  CAGR   = round(as.numeric(Return.annualized(xts(ewTest, index(testRets))))*100, 2),
  Sharpe = round(as.numeric(SharpeRatio.annualized(xts(ewTest, index(testRets)))), 2),
  MaxDD  = round(as.numeric(maxDrawdown(xts(ewTest, index(testRets))))*100, 2),
  Period = "Test", stringsAsFactors = FALSE
)
resultDf <- rbind(bestMet, bestMetTest, spyTrainMet, spyTestMet)

# ── Metrics gt table ──
gtTbl <- resultDf |>
  gt(groupname_col="Period") |>
  tab_header(title=sprintf("Walk-Forward: Best %dy SR Combo vs SPY", bestWindow),
             subtitle="Train ≤ 2019-12-31 | Test ≥ 2020-01-01") |>
  tab_style(cell_text(weight="bold", size="larger"), cells_row_groups()) |>
  tab_style(cell_fill("#E3F2FD"), cells_row_groups()) |>
  tab_source_note(source_note="@StockViz")

gt::gtsave(gtTbl, file.path(reportPath, sprintf("metrics%s.png", SUFFIX)))

# ── Cumulative returns: train and test ──

# Test
mergedTest <- merge(xts(ewTest, index(testRets)), testRets[, BENCH])
colnames(mergedTest) <- c(sprintf("%dy Best-SR", bestWindow), "SPY")
sr_test <- as.numeric(SharpeRatio.annualized(mergedTest))

Common.PlotCumReturns(mergedTest,
  sprintf("Cumulative Returns — Test (%s onward)", TEST_START),
  sprintf("%dy Best-SR SR=%.2f  SPY SR=%.2f", bestWindow, sr_test[1], sr_test[2]),
  file.path(reportPath, sprintf("cumulative-test%s.png", SUFFIX)), NULL)

# Train
mergedTrain <- merge(xts(ewTrain, index(trainRets)), trainRets[, BENCH])
colnames(mergedTrain) <- c(sprintf("%dy Best-SR", bestWindow), "SPY")
sr_train <- as.numeric(SharpeRatio.annualized(mergedTrain))

Common.PlotCumReturns(mergedTrain,
  sprintf("Cumulative Returns — Train (-> %s)", TRAIN_END),
  sprintf("%dy Best-SR SR=%.2f  SPY SR=%.2f", bestWindow, sr_train[1], sr_train[2]),
  file.path(reportPath, sprintf("cumulative-train%s.png", SUFFIX)), NULL)

# ── Annual returns chart ──
portAll <- xts(c(ewTrain, ewTest), index(dailyRets))
spyAll <- dailyRets[, BENCH]
portAnnual <- apply.yearly(portAll, Return.cumulative)
spyAnnual  <- apply.yearly(spyAll, Return.cumulative)
annualTbl <- merge(portAnnual, spyAnnual)
colnames(annualTbl) <- c(sprintf("%dy Best-SR", bestWindow), "SPY")

annualDf <- data.frame(
  Year = format(index(annualTbl), "%Y"),
  Portfolio = round(as.numeric(annualTbl[,1])*100, 2),
  SPY = round(as.numeric(annualTbl[,2])*100, 2),
  Period = ifelse(index(annualTbl) <= as.Date(TRAIN_END), "Train", "Test"),
  stringsAsFactors = FALSE
)

annualPlot <- reshape2::melt(annualDf, id.vars=c("Year","Period"), variable.name="Series", value.name="Return")
annualPlot$Year <- as.Date(paste0(annualPlot$Year, "-12-31"))
splitDate <- as.Date(TEST_START)

p <- ggplot(annualPlot, aes(x=Year, y=Return, fill=Series)) +
  geom_bar(stat="identity", position="dodge", alpha=0.85) +
  scale_fill_viridis(discrete=TRUE, option="D") +
  geom_vline(xintercept=splitDate, linetype="dashed", color="grey50", linewidth=0.8) +
  annotate("text", x=splitDate - 600, y=max(annualPlot$Return)*0.95, label="Train", color="grey40", size=3.5) +
  annotate("text", x=splitDate + 600, y=max(annualPlot$Return)*0.95, label="Test",  color="grey40", size=3.5) +
  labs(title=sprintf("Annual Returns — Best %dy SR Combo (%s) vs SPY", bestWindow, paste(bestWinCombo, collapse="+")),
       subtitle=sprintf("Train SR=%.2f | Test SR=%.2f", bestWinSR,
                        as.numeric(SharpeRatio.annualized(xts(ewTest, index(testRets))))),
       caption="@StockViz", y="Return (%)", x="") +
  theme_minimal(base_size=12) +
  theme(legend.position="bottom", plot.caption=element_text(hjust=1, face="italic", size=8))

ggsave(file.path(reportPath, sprintf("annual-returns%s.png", SUFFIX)), p, width=12, height=6, dpi=120)

# ── README ──
readme <- paste0(
  "# US Sector ETF — Multi-Window Walk-Forward\n",
  "\n",
  "Blog: https://stockviz.biz/...\n",
  "\n",
  "## Summary\n",
  "\n",
  "For each lookback window of 1–5 years, the best-Sharpe 4-ETF combination is selected\n",
  "on training data (≤ 2019-12-31) and tested forward.\n",
  "\n",
  "Train period: ", format(min(index(trainRets)), "%Y-%m-%d"), " → ", TRAIN_END, "\n",
  "Test period:  ", TEST_START, " → ", format(max(index(testRets)), "%Y-%m-%d"), "\n",
  "\n",
  "**Best window: ", bestWindow, "yr** (train SR=", round(bestWinSR, 2), ")\n",
  "- Combo: ", paste(bestWinCombo, collapse="+"), "\n",
  "\n",
  "| Metric | Combo (Train) | SPY (Train) | Combo (Test) | SPY (Test) |\n",
  "|---|---|---|---|---|\n",
  "| CAGR | ", resultDf$CAGR[1], "% | ", resultDf$CAGR[3], "% | ", resultDf$CAGR[2], "% | ", resultDf$CAGR[4], "% |\n",
  "| Sharpe | ", resultDf$Sharpe[1], " | ", resultDf$Sharpe[3], " | ", resultDf$Sharpe[2], " | ", resultDf$Sharpe[4], " |\n",
  "| MaxDD | ", resultDf$MaxDD[1], "% | ", resultDf$MaxDD[3], "% | ", resultDf$MaxDD[2], "% | ", resultDf$MaxDD[4], "% |\n",
  "## Files\n",
  "\n",
  "- `annual-returns", SUFFIX, ".png` — Annual returns, best-window combo vs SPY\n",
  "- `cumulative-train", SUFFIX, ".png` — Cumulative returns on train\n",
  "- `cumulative-test", SUFFIX, ".png` — Cumulative returns on test\n",
  "- `metrics", SUFFIX, ".png` — Full metrics table\n",
  "\n",
  "## Methodology\n",
  "\n",
  "All 11 choose 4 = 330 ETF combinations evaluated. For each 1–5yr lookback,\n",
  "the highest-Sharpe combo is selected on the training set and evaluated on the test set.\n"
)

writeLines(readme, file.path(reportPath, sprintf("README%s.md", SUFFIX)))
cat(sprintf("\nDone — output in %s\n", reportPath))
