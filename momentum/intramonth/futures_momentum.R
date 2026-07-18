###############################################################################
# Intramonth Futures Momentum Backtest
# =====================================
# Every month, 6 business days before month-end, SHORT 10 futures with the worst
# momentum (1 contract each). Close on the last trading day of the month.
# Excludes symbols with corporate actions during the holding period.
# Picks next-month expiry to avoid early expiration.
# 0.5% drag per trade. NIFTY 50 benchmark. Start: January 2014.
#
# Usage: Rscript futures_momentum.R [lookback_days]
#          default: 21 (1-month momentum)
###############################################################################

suppressMessages({
  library(RODBC)
  library(xts)
  library(PerformanceAnalytics); library(quantmod)
  library(ggplot2)
  library(viridis)
  library(gt); library(webshot2)
})

source("/mnt/hollandC/StockViz/R/config.r")

# ── Parameters ────────────────────────────────────────────────────────
args          <- commandArgs(trailingOnly = TRUE)
MOM_LOOKBACK  <- if (length(args) > 0) as.integer(args[1]) else 252L  # 1-year momentum
ENTRY_OFFSET  <- 6L
N_POSITIONS   <- 20L
START_YEAR    <- 2014L
DRAG          <- 0.005  # 0.5% per trade

cat(sprintf("Lookback: %d days  |  Drag: %.1f%%  |  Short %d worst\n",
            MOM_LOOKBACK, 100*DRAG, N_POSITIONS))

# ── DB ─────────────────────────────────────────────────────────────────
lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

# ── Load data ─────────────────────────────────────────────────────────
cat("Loading futures...\n")
futDf <- sqlQuery(lcon, "
  select SYMBOL, TIME_STAMP, EXPIRY_DT, PX_CLOSE
  from BHAV_EQ_FUT
  where TIME_STAMP >= '2013-01-01'
  order by SYMBOL, TIME_STAMP, EXPIRY_DT
")
cat(sprintf("  %d rows\n", nrow(futDf)))

cat("Loading corporate actions...\n")
caDf <- sqlQuery(lcon, "
  select SYMBOL, EX_DATE
  from CORP_ACTION
  where SERIES = 'EQ' and EX_DATE >= '2013-01-01'
  order by SYMBOL, EX_DATE
")
cat(sprintf("  %d rows\n", nrow(caDf)))

cat("Loading NIFTY 50...\n")
niftyDf <- sqlQuery(lcon, "
  select px_close, time_stamp
  from bhav_index
  where index_name = 'NIFTY 50' and time_stamp >= '2013-01-01'
  order by time_stamp
")
cat(sprintf("  %d rows\n", nrow(niftyDf)))
odbcClose(lcon)

# ── Prepare ───────────────────────────────────────────────────────────
futDf$TIME_STAMP <- as.Date(futDf$TIME_STAMP)
futDf$EXPIRY_DT  <- as.Date(futDf$EXPIRY_DT)
caDf$EX_DATE     <- as.Date(caDf$EX_DATE)
niftyDf$time_stamp <- as.Date(niftyDf$time_stamp)

allDates    <- sort(unique(futDf$TIME_STAMP))
dateToIdx   <- setNames(seq_along(allDates), as.character(allDates))
niftyXts    <- xts(niftyDf$px_close, order.by = niftyDf$time_stamp)
niftyRets   <- dailyReturn(niftyXts)

# ── Trading calendar ──────────────────────────────────────────────────
monthLast <- tapply(allDates, format(allDates, "%Y-%m"), max, simplify = FALSE)

calendar <- list()
i <- 0L
for (ym in names(monthLast)) {
  exitDate <- as.Date(monthLast[[ym]])
  yr <- as.integer(format(exitDate, "%Y"))
  if (yr < START_YEAR) next
  exitIdx <- dateToIdx[as.character(exitDate)]
  entryIdx <- exitIdx - (ENTRY_OFFSET - 1L)
  if (entryIdx < 1L) next
  i <- i + 1L
  calendar[[i]] <- list(label = ym, entryDate = allDates[entryIdx], exitDate = exitDate)
}
cat(sprintf("Calendar: %d months, %s → %s\n",
            length(calendar), calendar[[1]]$label, calendar[[length(calendar)]]$label))

# ── Corporate action index ────────────────────────────────────────────
caBySymbol <- split(caDf$EX_DATE, caDf$SYMBOL)

hasCorpAction <- function(symbol, entryDate, exitDate) {
  dates <- caBySymbol[[symbol]]
  if (is.null(dates)) return(FALSE)
  any(dates >= entryDate & dates <= exitDate)
}

# ── Momentum ──────────────────────────────────────────────────────────
getMomentum <- function(entryDate, lookback) {
  entryIdx <- dateToIdx[as.character(entryDate)]
  startIdx <- entryIdx - lookback
  if (startIdx < 1L) return(character(0L))

  entryRows <- futDf[futDf$TIME_STAMP == entryDate, ]
  entryPx <- tapply(entryRows$PX_CLOSE, entryRows$SYMBOL, function(x) x[1L])
  startRows <- futDf[futDf$TIME_STAMP == allDates[startIdx], ]
  startPx <- tapply(startRows$PX_CLOSE, startRows$SYMBOL, function(x) x[1L])

  common <- intersect(names(entryPx), names(startPx))
  if (length(common) == 0L) return(character(0L))
  sort((entryPx[common] / startPx[common]) - 1.0)
}

# ── Run backtest ──────────────────────────────────────────────────────
cat("Running...\n")
results <- list()

for (ci in seq_along(calendar)) {
  cal    <- calendar[[ci]]
  entryD <- cal$entryDate
  exitD  <- cal$exitDate

  ranked <- getMomentum(entryD, MOM_LOOKBACK)
  if (length(ranked) == 0L) next

  caFlags <- sapply(names(ranked), hasCorpAction, entryDate = entryD, exitDate = exitD)
  clean   <- ranked[!caFlags]
  if (length(clean) == 0L) next

  topN <- head(clean, N_POSITIONS)

  entryPx <- sapply(names(topN), function(sym) {
    rows <- futDf[futDf$SYMBOL == sym & futDf$TIME_STAMP == entryD & futDf$EXPIRY_DT > exitD, ]
    if (nrow(rows) == 0L) NA_real_ else rows$PX_CLOSE[which.min(rows$EXPIRY_DT)]
  })
  exitPx <- sapply(names(topN), function(sym) {
    rows <- futDf[futDf$SYMBOL == sym & futDf$TIME_STAMP == exitD, ]
    if (nrow(rows) == 0L) NA_real_ else rows$PX_CLOSE[1L]
  })

  valid <- !is.na(entryPx) & !is.na(exitPx) & entryPx > 0 & exitPx > 0
  if (sum(valid) == 0L) next

  posRets <- (entryPx[valid] - exitPx[valid]) / entryPx[valid]
  portRet <- mean(posRets) - DRAG

  results[[length(results) + 1L]] <- data.frame(
    date    = exitD,
    return  = portRet,
    n_pos   = sum(valid),
    gross   = mean(posRets),
    stringsAsFactors = FALSE
  )

  if (ci %% 12 == 0L) {
    cat(sprintf("  %s: %d pos, gross=%.4f net=%.4f\n", cal$label, sum(valid), mean(posRets), portRet))
  }
}

# ── Aggregate ─────────────────────────────────────────────────────────
resDf     <- do.call(rbind, results)
stratXts  <- xts(resDf$return, order.by = as.Date(resDf$date))
stratCum  <- cumprod(1 + stratXts)
colnames(stratCum) <- "Strategy"

# Align NIFTY to strategy dates and compute cumulative
niftySub  <- niftyXts[paste0(start(stratCum), "/")]
niftySubRets <- dailyReturn(niftySub)

# Map monthly strategy returns to daily: fill between rebalance dates
# For cumulative chart, we need daily values. Build by repeating monthly ret
aligned <- merge.xts(stratXts, niftySubRets, join = "inner")
aligned <- na.omit(aligned)
# Only keep strategy dates, fill benchmark daily
stratDates <- index(stratXts)
benchDaily <- niftySubRets; colnames(benchDaily) <- "NIFTY 50"
# For chart: daily benchmark, monthly strategy
allCombined <- merge.xts(stratXts, benchDaily, join = "outer")
allCombined <- na.locf(allCombined, fromLast = FALSE)  # fill NA strategy returns forward
# Actually, simpler: build daily strategy by assigning monthly ret to each day
stratDaily <- xts(rep(NA_real_, nrow(benchDaily)), order.by = index(benchDaily))
colnames(stratDaily) <- "Strategy"
for (j in seq_len(nrow(resDf))) {
  if (j == 1L) {
    stratDaily[as.character(resDf$date[j])] <- resDf$return[j]
  } else {
    stratDaily[as.character(resDf$date[j])] <- resDf$return[j]
  }
}
# Fill strategy returns forward across the month (flat between rebalance dates)
stratDaily <- na.locf(stratDaily)
# Set pre-first-rebalance to 0
stratDaily[is.na(stratDaily)] <- 0

# Build cumulative from daily
stratDailyCum <- cumprod(1 + stratDaily)
benchDailyCum <- cumprod(1 + benchDaily)
combined <- na.omit(merge.xts(stratDailyCum, benchDailyCum))
colnames(combined) <- c("Strategy", "NIFTY 50")

# ── Cumulative return chart (ggplot) ──────────────────────────────────
cumDf <- fortify(combined, melt = TRUE)
names(cumDf) <- c("Date", "Series", "Value")

p <- ggplot(cumDf, aes(x = Date, y = Value, color = Series)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Strategy" = "#2196F3", "NIFTY 50" = "#757575")) +
  scale_y_log10(labels = scales::number_format(accuracy = 0.1)) +
  labs(
    title    = "Intramonth Futures Momentum",
    subtitle = sprintf("Short %d worst | %d-day lookback | %.1f%% drag | %s → %s",
                       N_POSITIONS, MOM_LOOKBACK, 100*DRAG,
                       format(min(cumDf$Date), "%b %Y"), format(max(cumDf$Date), "%b %Y")),
    x        = "", y = "Cumulative Return (log scale)", color = "",
    caption  = "@StockViz"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    plot.title        = element_text(face = "bold"),
    plot.caption      = element_text(hjust = 0, size = 8, color = "grey50"),
    panel.grid.minor  = element_blank()
  )

ggsave("cumulative_returns.png", p, width = 10, height = 6, dpi = 120)
cat("Saved: cumulative_returns.png\n")

# ── Annual returns bar chart ──────────────────────────────────────────
annCombined <- apply.yearly(merge.xts(stratDaily, benchDaily), Return.cumulative)
annDf <- fortify(annCombined, melt = TRUE)
names(annDf) <- c("Year", "Series", "Return")
annDf$Year <- as.numeric(format(annDf$Year, "%Y"))

p2 <- ggplot(annDf, aes(x = factor(Year), y = Return * 100, fill = Series)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#2196F3", "#BDBDBD")) +
  labs(
    title    = "Annual Returns",
    subtitle = sprintf("Short %d worst | %d-day lookback", N_POSITIONS, MOM_LOOKBACK),
    x = "", y = "Return (%)", fill = "", caption = "@StockViz"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1),
    legend.position   = "bottom",
    plot.caption      = element_text(hjust = 0, size = 8, color = "grey50")
  )

ggsave("annual_returns.png", p2, width = 14, height = 6, dpi = 120)
cat("Saved: annual_returns.png\n")

# ── Metrics table ─────────────────────────────────────────────────────
computeMetrics <- function(rets, label) {
  annRet  <- Return.annualized(rets)[1, 1]
  annVol  <- sd(coredata(rets), na.rm = TRUE) * sqrt(12)
  sharpe  <- SharpeRatio.annualized(rets)[1, 1]
  maxDD   <- maxDrawdown(rets)
  winRate <- sum(coredata(rets) > 0, na.rm = TRUE) / sum(!is.na(coredata(rets)))
  data.frame(
    Strategy       = label,
    `Ann. Return`  = annRet,
    `Ann. Vol`     = annVol,
    Sharpe         = sharpe,
    `Max DD`       = maxDD,
    `Win Rate`     = winRate,
    Months         = sum(!is.na(coredata(rets))),
    check.names    = FALSE,
    stringsAsFactors = FALSE
  )
}

# Use monthly returns for strategy metrics (excludes zero-fill days)
stratMets <- computeMetrics(stratXts, "Strategy")
# Benchmark: daily returns on strategy dates
benchOnStrat <- benchDaily[index(stratXts)]
benchMets <- computeMetrics(benchOnStrat, "NIFTY 50")

metsTbl <- rbind(stratMets, benchMets)

gtTbl <- metsTbl |>
  gt() |>
  tab_header(
    title    = "Performance Metrics",
    subtitle = sprintf("%d-day lookback | %.1f%% drag | %s → %s",
                       MOM_LOOKBACK, 100*DRAG,
                       format(min(index(stratXts)), "%b %Y"),
                       format(max(index(stratXts)), "%b %Y"))
  ) |>
  fmt_percent(columns = c(`Ann. Return`, `Ann. Vol`, `Max DD`, `Win Rate`), decimals = 1) |>
  fmt_number(columns = Sharpe, decimals = 2) |>
  fmt_number(columns = Months, decimals = 0) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes()) |>
  tab_style(
    style = cell_fill(color = "#8B0000"),
    locations = cells_body(columns = `Ann. Return`, rows = `Ann. Return` < 0)
  ) |>
  tab_style(
    style = cell_fill(color = "#8B0000"),
    locations = cells_body(columns = `Max DD`, rows = `Max DD` > 0.20)
  )

gtsave(gtTbl, "metrics.png")
webshot2::webshot(paste0("file://", getwd(), "/metrics.html"), "metrics.png", selector = "table", expand = c(10, 10, 10, 10))
cat("Saved: metrics.png\n")

# ── Console summary ───────────────────────────────────────────────────
cat(sprintf("\n%s\n", paste(rep("=", 60), collapse = "")))
cat(sprintf("INTRAMONTH FUTURES MOMENTUM  |  %d-day lookback  |  %.1f%% drag\n",
            MOM_LOOKBACK, 100*DRAG))
cat(sprintf("%s\n", paste(rep("=", 60), collapse = "")))
cat(sprintf("Period:        %s → %s\n",
            format(min(index(stratXts)), "%Y-%m-%d"), format(max(index(stratXts)), "%Y-%m-%d")))
cat(sprintf("Total months:  %d\n", nrow(resDf)))
cat(sprintf("Avg positions: %.1f\n", mean(resDf$n_pos)))
cat(sprintf("Win rate:      %d/%d (%.1f%%)\n",
            sum(resDf$return > 0), nrow(resDf), 100*sum(resDf$return>0)/nrow(resDf)))
cat(sprintf("Ann. Return:   %+.2f%%\n", 100*stratMets$`Ann. Return`))
cat(sprintf("Ann. Vol:      %.2f%%\n", 100*stratMets$`Ann. Vol`))
cat(sprintf("Sharpe:        %+.2f\n", stratMets$Sharpe))
cat(sprintf("Max Drawdown:  %.2f%%\n", 100*stratMets$`Max DD`))
cat(sprintf("\nOutput: equity_curve.csv  cumulative_returns.png  annual_returns.png  metrics.png\n"))
write.csv(resDf, "equity_curve.csv", row.names = FALSE)
