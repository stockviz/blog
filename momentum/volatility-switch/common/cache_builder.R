# ============================================================================
# Cache builder — daily risk-free return + NIFTY 50 TR + NIFTY500 Momentum 50 TR
# Output: common/cache.rds (xts with columns: RF, NIFTY_50_TR, MOMENTUM50_TR)
# ============================================================================

suppressPackageStartupMessages({
  library('RODBC')
  library('quantmod')
  library('xts')
})

options("scipen" = 100)
options(stringsAsFactors = FALSE)

source("/mnt/hollandC/StockViz/R/config.r")

CACHE_PATH <- "/mnt/data/blog/momentum/volatility-switch/common/cache.rds"

cat(sprintf("Cache path: %s\n", CACHE_PATH))
cat(sprintf("Cache exists: %s\n", file.exists(CACHE_PATH)))

if (file.exists(CACHE_PATH)) {
  cat("Cache already exists. Delete it to rebuild, or call with --force\n")
  cat("Loading existing cache for inspection...\n")
  cache <- readRDS(CACHE_PATH)
  cat(sprintf("  Rows: %d, Cols: %s\n", nrow(cache), paste(colnames(cache), collapse=", ")))
  cat(sprintf("  Date range: %s to %s\n", start(cache), end(cache)))
  print(head(cache))
  print(tail(cache))
  q(save = "no")
}

# Allow --force flag to rebuild even if cache exists
FORCE <- "--force" %in% commandArgs(trailingOnly = TRUE)
if (!FORCE && file.exists(CACHE_PATH)) {
  q(save = "no")
}

# ── Connect to norway (has both ZERO_COUPON_CURVE and bhav_index) ──
cat("\nConnecting to norway...\n")
lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockViz", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

# ── 1. Daily risk-free return from ZERO_COUPON_CURVE ──
# Use maturity=0 (instantaneous/spot rate) for the risk-free proxy.
# Annual yield → daily return: (1 + YIELD/100)^(1/365) - 1
cat("\nQuerying ZERO_COUPON_CURVE (maturity=0)...\n")
rfDf <- sqlQuery(lcon, "
  SELECT time_stamp, yield 
  FROM ZERO_COUPON_CURVE 
  WHERE maturity = 0 
  ORDER BY time_stamp")

cat(sprintf("  Rows: %d, Date range: %s to %s\n", 
            nrow(rfDf), min(rfDf$time_stamp), max(rfDf$time_stamp)))

# Convert annual percentage yield to daily return
rfXts <- xts((1 + rfDf$yield / 100)^(1/365) - 1, as.Date(rfDf$time_stamp))
colnames(rfXts) <- "RF"
cat(sprintf("  RF xts: %d rows, NA count: %d\n", nrow(rfXts), sum(is.na(rfXts))))

# ── 2. Index price series ──
cat("\nQuerying bhav_index (NIFTY 50 TR, NIFTY500 MOMENTUM 50 TR)...\n")
idxDf <- sqlQuery(lcon, "
  SELECT index_name, time_stamp, px_close 
  FROM bhav_index 
  WHERE index_name IN ('NIFTY 50 TR', 'NIFTY500 MOMENTUM 50 TR') 
  ORDER BY time_stamp")

cat(sprintf("  Total rows: %d\n", nrow(idxDf)))

# Pivot to wide xts: columns = index names, rows = dates
idxWide <- reshape(idxDf, idvar = "time_stamp", timevar = "index_name", direction = "wide")
colnames(idxWide) <- gsub("px_close\\.", "", colnames(idxWide))

idxXts <- xts(idxWide[, -1, drop = FALSE], as.Date(idxWide$time_stamp))
colnames(idxXts) <- c("MOMENTUM50_TR", "NIFTY_50_TR")
cat(sprintf("  Index xts: %d rows, cols: %s\n", nrow(idxXts), paste(colnames(idxXts), collapse=", ")))

# ── 3. Merge and align ──
cat("\nMerging risk-free + indices...\n")
cache <- merge(rfXts, idxXts, join = "inner")
cat(sprintf("  Merged: %d rows, %d NA total\n", 
            nrow(cache), sum(is.na(cache))))

# Fill forward any NAs (risk-free series may have gaps when market is closed)
cache <- na.locf(cache, na.rm = FALSE)
cache <- na.omit(cache)
cat(sprintf("  After fill+omit: %d rows\n", nrow(cache)))

cat(sprintf("\nFinal cache:\n"))
cat(sprintf("  Date range: %s to %s\n", start(cache), end(cache)))
cat(sprintf("  Columns: %s\n", paste(colnames(cache), collapse=", ")))
print(head(cache))
print(tail(cache))

# ── 4. Save ──
saveRDS(cache, CACHE_PATH)
cat(sprintf("\nSaved to %s (%d rows)\n", CACHE_PATH, nrow(cache)))

odbcClose(lcon)
