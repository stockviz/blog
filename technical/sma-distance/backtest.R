# ============================================================================
# SMA200 Distance Distribution — NSE stocks
# Min/max distance from SMA200 for stocks with ≥ 500 consecutive days
# Split: pre-2014 vs post-2014, 5% date tolerance
# ============================================================================

suppressPackageStartupMessages({
  library('RODBC')
  library('xts')
  library('quantmod')
  library('TTR')
  library('gt')
  library('webshot2')
})

source("/mnt/hollandC/StockViz/R/config.r")

# ── Config ──
SMA_N          <- 200L
SMA_SHORT      <- 10L
MIN_DAYS       <- 500L
SPLIT_YEAR     <- 2014L
DATE_TOLERANCE <- 0.05  # 5%

reportPath <- "/mnt/data/blog/technical/sma-distance"

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockViz", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

# ── 1. Get all symbols with sufficient history ──
cat("=== Finding eligible stocks ===\n")

# Get distinct symbols and their date counts
symDf <- sqlQuery(lcon, "
  SELECT SYMBOL, MIN(TIME_STAMP) AS first_dt, MAX(TIME_STAMP) AS last_dt, COUNT(*) AS n
  FROM px_history
  GROUP BY SYMBOL
  HAVING COUNT(*) >= 500
  ORDER BY SYMBOL
")

cat(sprintf("  %d stocks with ≥ %d days\n", nrow(symDf), MIN_DAYS))

# ── 2. Process each stock ──
cat("\n=== Computing SMA200 distances ===\n")

cacheFile <- file.path(reportPath, "stock_cache.rds")
if (file.exists(cacheFile)) {
  stockCache <- readRDS(cacheFile)
  cat(sprintf("  Loaded cache: %d stocks\n", length(stockCache)))
} else {
  stockCache <- list()
}

allResults <- list()

for (i in 1:nrow(symDf)) {
  sym <- symDf$SYMBOL[i]
  
  # Check cache
  cacheKey <- paste0(sym, "_SMA", SMA_N, "_S10")
  if (cacheKey %in% names(stockCache)) {
    allResults[[sym]] <- stockCache[[cacheKey]]
    if (i %% 500 == 0) cat(sprintf("  %d/%d (cached)...\n", i, nrow(symDf)))
    next
  }
  
  # Fetch prices
  pDf <- sqlQuery(lcon, sprintf(
    "SELECT TIME_STAMP, PX_CLOSE FROM px_history WHERE SYMBOL = '%s' ORDER BY TIME_STAMP",
    sym
  ))
  
  if (nrow(pDf) < MIN_DAYS) next
  
  pXts <- xts(pDf$PX_CLOSE, as.Date(pDf$TIME_STAMP))
  colnames(pXts) <- sym
  
  # Compute SMA200
  sma <- SMA(pXts, n = SMA_N)
  validIdx <- !is.na(sma) & !is.na(pXts)
  if (sum(validIdx) < MIN_DAYS) next
  
  # Distance = (price - sma) / sma * 100
  distance <- (pXts - sma) / sma * 100
  distXts <- xts(as.numeric(distance), index(pXts))
  colnames(distXts) <- "DIST"
  
  # Split: pre-2014 and post-2014
  PRE_END  <- as.Date("2013-12-31")
  POST_START <- as.Date("2014-01-01")
  
  distPre  <- distXts[index(distXts) <= PRE_END  & !is.na(distXts)]
  distPost <- distXts[index(distXts) >= POST_START & !is.na(distXts)]
  
  # Avoid cross-year boundary: only include dates that are clearly pre or post
  # with 5% tolerance: if a date is within 5% of 2014-01-01 in either direction, skip
  boundary <- as.Date("2014-01-01")
  dateSeq <- as.Date(index(distXts))
  yearFrac <- as.numeric(difftime(dateSeq, as.Date("2014-01-01"), units = "days")) / 365.25
  distPre  <- distXts[yearFrac < -DATE_TOLERANCE & !is.na(as.numeric(distXts))]
  distPost <- distXts[yearFrac >  DATE_TOLERANCE & !is.na(as.numeric(distXts))]
  
  # ── SMA10 → SMA200 crossover transitions ──
  sma10 <- SMA(pXts, n = SMA_SHORT)
  
  # above10: price > SMA10 (binary), above200: price > SMA200 (binary)
  above10  <- lag(ifelse(as.numeric(pXts) > as.numeric(sma10), 1, 0), 1)
  above200 <- lag(ifelse(as.numeric(pXts) > as.numeric(sma), 1, 0), 1)
  above10[is.na(above10)] <- 0; above200[is.na(above200)] <- 0
  
  # Detect transitions: price crossed above SMA10 but still below SMA200 → measure days to cross above SMA200
  # State machine: 0=below both, 1=above10 below200, 2=above both
  state <- ifelse(above10 == 0 & above200 == 0, 0,
           ifelse(above10 == 1 & above200 == 0, 1,
           ifelse(above10 == 1 & above200 == 1, 2, 0)))
  
  state <- as.numeric(state)
  bullishTransitions <- c()
  bearishTransitions <- c()  # days to go from state 2 → 0 (via state 1)
  
  # Bullish: find state=1 periods and time to first state=2
  r <- rle(state)
  pos <- 1
  for (j in 1:length(r$lengths)) {
    if (r$values[j] == 1) {
      # In state 1 (above10, below200). Find if next state is 2
      nextPos <- pos + r$lengths[j]
      if (nextPos <= length(state) && state[nextPos] == 2) {
        # It transitioned to state 2. Count days spent in state 1
        bullishTransitions <- c(bullishTransitions, r$lengths[j])
      }
    }
    pos <- pos + r$lengths[j]
  }
  
  # Bearish: find state 2→0 transitions and their duration through state 1
  # When exiting state 2, the path goes through state 1 (above10, below200) before state 0
  # Count: days from leaving state 2 to entering state 0
  r <- rle(state)
  pos <- 1
  for (j in 1:(length(r$lengths) - 1)) {
    if (r$values[j] == 2 && r$values[j + 1] == 1) {
      # Leaving state 2 into state 1
      st1Len <- r$lengths[j + 1]
      nextPos <- pos + r$lengths[j] + r$lengths[j + 1]
      if (nextPos <= length(state) && state[nextPos] == 0) {
        bearishTransitions <- c(bearishTransitions, st1Len)
      }
    }
    pos <- pos + r$lengths[j]
  }
  
  # Split transitions by period
  preIdx  <- which(yearFrac < -DATE_TOLERANCE)
  postIdx <- which(yearFrac >  DATE_TOLERANCE)
  
  pre_bull_min  <- NA_real_; pre_bull_mean <- NA_real_
  pre_bear_min  <- NA_real_; pre_bear_mean <- NA_real_
  post_bull_min <- NA_real_; post_bull_mean <- NA_real_
  post_bear_min <- NA_real_; post_bear_mean <- NA_real_
  
  if (length(bullishTransitions) > 0) {
    # The transitions are indexed to state runs, not dates. Attribute by run position.
    # Simpler: just compute overall min/mean per stock, not per period
    pre_bull_min  <- min(bullishTransitions)
    pre_bull_mean <- mean(bullishTransitions)
  }
  if (length(bearishTransitions) > 0) {
    pre_bear_min  <- min(bearishTransitions)
    pre_bear_mean <- mean(bearishTransitions)
  }
  
  result <- list(
    symbol      = sym,
    first_date  = as.character(first(index(pXts))),
    last_date   = as.character(last(index(pXts))),
    total_days  = nrow(pXts),
    sma_days    = sum(validIdx),
    
    pre_min      = if (length(distPre)  > 0) min(as.numeric(distPre))  else NA_real_,
    pre_max      = if (length(distPre)  > 0) max(as.numeric(distPre))  else NA_real_,
    pre_mean     = if (length(distPre)  > 0) mean(as.numeric(distPre)) else NA_real_,
    pre_sd       = if (length(distPre)  > 0) sd(as.numeric(distPre))   else NA_real_,
    pre_pct_below = if (length(distPre) > 0) mean(as.numeric(distPre) < 0) * 100 else NA_real_,
    
    post_min      = if (length(distPost) > 0) min(as.numeric(distPost))  else NA_real_,
    post_max      = if (length(distPost) > 0) max(as.numeric(distPost))  else NA_real_,
    post_mean     = if (length(distPost) > 0) mean(as.numeric(distPost)) else NA_real_,
    post_sd       = if (length(distPost) > 0) sd(as.numeric(distPost))   else NA_real_,
    post_pct_below = if (length(distPost)> 0) mean(as.numeric(distPost) < 0) * 100 else NA_real_,
    
    # SMA transition metrics (overall, not split by period — rle-based)
    bull_min_days  = if (length(bullishTransitions) > 0) min(bullishTransitions) else NA_real_,
    bull_mean_days = if (length(bullishTransitions) > 0) mean(bullishTransitions) else NA_real_,
    bull_n         = length(bullishTransitions),
    bear_min_days  = if (length(bearishTransitions) > 0) min(bearishTransitions) else NA_real_,
    bear_mean_days = if (length(bearishTransitions) > 0) mean(bearishTransitions) else NA_real_,
    bear_n         = length(bearishTransitions)
  )
  
  allResults[[sym]] <- result
  stockCache[[cacheKey]] <- result
  
  if (i %% 500 == 0) {
    cat(sprintf("  %d/%d (caching at %d)...\n", i, nrow(symDf), length(stockCache)))
    saveRDS(stockCache, cacheFile)
  }
}

# Final cache save
saveRDS(stockCache, cacheFile)
cat(sprintf("  Done: %d stocks processed, cache saved\n", length(allResults)))

odbcClose(lcon)

# ── 3. Build summary tables ──
cat("\n=== Building summaries ===\n")

validStocks <- allResults[!sapply(allResults, is.null)]
cat(sprintf("  %d valid stocks\n", length(validStocks)))

# Pre-2014 distribution
preMin  <- sapply(validStocks, function(x) x$pre_min)
preMax  <- sapply(validStocks, function(x) x$pre_max)
preMean <- sapply(validStocks, function(x) x$pre_mean)

postMin  <- sapply(validStocks, function(x) x$post_min)
postMax  <- sapply(validStocks, function(x) x$post_max)
postMean <- sapply(validStocks, function(x) x$post_mean)

# Remove NAs
preMin  <- preMin[!is.na(preMin)]
preMax  <- preMax[!is.na(preMax)]
preMean <- preMean[!is.na(preMean)]
postMin <- postMin[!is.na(postMin)]
postMax <- postMax[!is.na(postMax)]
postMean <- postMean[!is.na(postMean)]

rng <- function(x) quantile(x, c(0, 1), na.rm = TRUE)
cat(sprintf("\n  Pre-2014:  stocks=%d  min=%.1f%% to %.1f%%  max=%.1f%% to %.1f%%  mean=%.1f%%\n",
  length(preMin), rng(preMin)[1], rng(preMin)[2], rng(preMax)[1], rng(preMax)[2], mean(preMean)))
cat(sprintf("  Post-2014: stocks=%d  min=%.1f%% to %.1f%%  max=%.1f%% to %.1f%%  mean=%.1f%%\n",
  length(postMin), rng(postMin)[1], rng(postMin)[2], rng(postMax)[1], rng(postMax)[2], mean(postMean)))

# ── 4. Distribution summary GT table ──
cat("\n=== Generating summary tables ===\n")

distSummary <- data.frame(
  Period = c("Pre-2014", "Post-2014"),
  Stocks = c(length(preMin), length(postMin)),
  
  DistMin_P05 = c(round(quantile(preMin, 0.05), 1), round(quantile(postMin, 0.05), 1)),
  DistMin_P25 = c(round(quantile(preMin, 0.25), 1), round(quantile(postMin, 0.25), 1)),
  DistMin_P50 = c(round(quantile(preMin, 0.50), 1), round(quantile(postMin, 0.50), 1)),
  DistMin_P75 = c(round(quantile(preMin, 0.75), 1), round(quantile(postMin, 0.75), 1)),
  DistMin_P95 = c(round(quantile(preMin, 0.95), 1), round(quantile(postMin, 0.95), 1)),
  
  DistMax_P05 = c(round(quantile(preMax, 0.05), 1), round(quantile(postMax, 0.05), 1)),
  DistMax_P25 = c(round(quantile(preMax, 0.25), 1), round(quantile(postMax, 0.25), 1)),
  DistMax_P50 = c(round(quantile(preMax, 0.50), 1), round(quantile(postMax, 0.50), 1)),
  DistMax_P75 = c(round(quantile(preMax, 0.75), 1), round(quantile(postMax, 0.75), 1)),
  DistMax_P95 = c(round(quantile(preMax, 0.95), 1), round(quantile(postMax, 0.95), 1)),
  
  stringsAsFactors = FALSE
)

gtTbl <- distSummary |> gt() |>
  tab_header(
    title = "SMA200 Distance Distribution — NSE Stocks",
    subtitle = sprintf("Stocks with ≥ %d consecutive days | Min/Max distance from SMA%d",
                       MIN_DAYS, SMA_N)
  ) |>
  tab_spanner(label = "Min Distance (%)", columns = starts_with("DistMin")) |>
  tab_spanner(label = "Max Distance (%)", columns = starts_with("DistMax")) |>
  cols_label(
    Period = "Period", Stocks = "# Stocks",
    DistMin_P05 = "p05", DistMin_P25 = "p25", DistMin_P50 = "p50",
    DistMin_P75 = "p75", DistMin_P95 = "p95",
    DistMax_P05 = "p05", DistMax_P25 = "p25", DistMax_P50 = "p50",
    DistMax_P75 = "p75", DistMax_P95 = "p95"
  ) |>
  tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
  tab_style(cell_text(weight = "bold"), cells_column_spanners()) |>
  tab_source_note("@StockViz")

fBase <- file.path(reportPath, "distribution_summary")
gtsave(gtTbl, paste0(fBase, ".html"))
webshot(paste0(fBase, ".html"), paste0(fBase, ".png"),
        selector = "table.gt_table", expand = c(10, 10, 10, 10))
cat(sprintf("  Saved: %s\n", fBase))

# ── 5. Histogram comparison ──
cat("\n=== Saving histogram PNGs ===\n")

pdf(NULL)
png(file.path(reportPath, "hist_min_distance.png"), width = 800, height = 500)
par(mfrow = c(1, 2))
hist(preMin, breaks = 50, main = "Pre-2014: Min Distance from SMA200",
     xlab = "Min Distance (%)", col = "#E3F2FD", border = "white")
abline(v = median(preMin), col = "red", lwd = 2, lty = 2)
hist(postMin, breaks = 50, main = "Post-2014: Min Distance from SMA200",
     xlab = "Min Distance (%)", col = "#E8F5E9", border = "white")
abline(v = median(postMin), col = "red", lwd = 2, lty = 2)
dev.off()
cat("  hist_min_distance.png\n")

png(file.path(reportPath, "hist_max_distance.png"), width = 800, height = 500)
par(mfrow = c(1, 2))
hist(preMax, breaks = 50, main = "Pre-2014: Max Distance from SMA200",
     xlab = "Max Distance (%)", col = "#E3F2FD", border = "white")
abline(v = median(preMax), col = "red", lwd = 2, lty = 2)
hist(postMax, breaks = 50, main = "Post-2014: Max Distance from SMA200",
     xlab = "Max Distance (%)", col = "#E8F5E9", border = "white")
abline(v = median(postMax), col = "red", lwd = 2, lty = 2)
dev.off()
cat("  hist_max_distance.png\n")

# ── 5. SMA crossover transition analysis ──
cat("\n=== SMA crossover transitions ===\n")

bullMin  <- sapply(validStocks, function(x) x$bull_min_days)
bullMean <- sapply(validStocks, function(x) x$bull_mean_days)
bearMin  <- sapply(validStocks, function(x) x$bear_min_days)
bearMean <- sapply(validStocks, function(x) x$bear_mean_days)

bullMin  <- bullMin[!is.na(bullMin) & bullMin >= 5]
bullMean <- bullMean[!is.na(bullMean)]
bearMin  <- bearMin[!is.na(bearMin) & bearMin >= 5]
bearMean <- bearMean[!is.na(bearMean)]

cat(sprintf("  Bullish (SMA10→SMA200): %d stocks, min=%.0fd median=%.0fd mean=%.1fd\n",
  length(bullMin), min(bullMin), median(bullMin), mean(bullMin)))
cat(sprintf("  Bearish (SMA10→SMA200): %d stocks, min=%.0fd median=%.0fd mean=%.1fd\n",
  length(bearMin), min(bearMin), median(bearMin), mean(bearMin)))

# Transition histograms
png(file.path(reportPath, "hist_crossover_transitions.png"), width = 800, height = 500)
par(mfrow = c(1, 2))
hist(bullMin[bullMin <= 200], breaks = 40,
     main = sprintf("Bullish: Days SMA10→SMA200\n(min=%.0fd med=%.0fd n=%d)",
                    min(bullMin), median(bullMin), length(bullMin)),
     xlab = "Days", col = "#C8E6C9", border = "white")
abline(v = median(bullMin), col = "red", lwd = 2, lty = 2)

hist(bearMin[bearMin <= 200], breaks = 40,
     main = sprintf("Bearish: Days SMA10→SMA200\n(min=%.0fd med=%.0fd n=%d)",
                    min(bearMin), median(bearMin), length(bearMin)),
     xlab = "Days", col = "#FFCDD2", border = "white")
abline(v = median(bearMin), col = "red", lwd = 2, lty = 2)
dev.off()
cat("  hist_crossover_transitions.png\n")

# Transition distribution summary GT table
transSummary <- data.frame(
  Direction = c("Bullish", "Bearish"),
  Stocks     = c(length(bullMin), length(bearMin)),
  Min_Days   = c(min(bullMin), min(bearMin)),
  P05 = c(round(quantile(bullMin, 0.05)), round(quantile(bearMin, 0.05))),
  P25 = c(round(quantile(bullMin, 0.25)), round(quantile(bearMin, 0.25))),
  P50 = c(round(quantile(bullMin, 0.50)), round(quantile(bearMin, 0.50))),
  P75 = c(round(quantile(bullMin, 0.75)), round(quantile(bearMin, 0.75))),
  P95 = c(round(quantile(bullMin, 0.95)), round(quantile(bearMin, 0.95))),
  Max_Days = c(max(bullMin), max(bearMin)),
  Mean_Days = c(round(mean(bullMin), 1), round(mean(bearMin), 1)),
  stringsAsFactors = FALSE
)

gtTrans <- transSummary |> gt() |>
  tab_header(
    title = "SMA10 → SMA200 Crossover Transition Distribution",
    subtitle = sprintf("Shortest days to cross SMA200 after SMA10 signal (≥5 days) | %d stocks",
                       length(validStocks))
  ) |>
  tab_spanner(label = "Days Distribution", columns = c(P05, P25, P50, P75, P95)) |>
  cols_label(
    Direction = "Direction", Stocks = "# Stocks",
    Min_Days = "Min", P05 = "p05", P25 = "p25", P50 = "p50",
    P75 = "p75", P95 = "p95", Max_Days = "Max", Mean_Days = "Mean"
  ) |>
  tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
  tab_style(cell_text(weight = "bold"), cells_column_spanners()) |>
  tab_style(cell_fill("#C8E6C9"), cells_body(rows = Direction == "Bullish")) |>
  tab_style(cell_fill("#FFCDD2"), cells_body(rows = Direction == "Bearish")) |>
  tab_source_note("@StockViz")

fTrans <- file.path(reportPath, "transition_summary")
gtsave(gtTrans, paste0(fTrans, ".html"))
webshot(paste0(fTrans, ".html"), paste0(fTrans, ".png"),
        selector = "table.gt_table", expand = c(10, 10, 10, 10))
cat("  transition_summary.png\n")

cat(sprintf("\nOutput: %s/\n", reportPath))
