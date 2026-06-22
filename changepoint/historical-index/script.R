source("../common/regime_classify.R")

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('ggrepel')
library('gtExtras')
library('webshot2')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")
source("/mnt/data/blog/common/theme.returns.common.r")

drag <- 0.2/100

print("connecting to norway...")
lcon <- odbcDriverConnect(
  sprintf(
    "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
    ldbserver,
    "StockViz",
    ldbuser,
    ldbpassword
  ),
  case = "nochange",
  believeNRows = TRUE
)

startDate <- as.Date("2005-04-01")
trainEndDt <- as.Date("2015-12-01")
testRange <- "2016-01-01/2024-12-31"

#define the universe
indices <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR")

#fetch the prices
fileName <- "../common/prices_index.Rdata"
pXts <- NULL
syms <- NULL
if(file.exists(fileName)){
  print("loading prices from cache...")
  load(fileName)
  syms <- names(pXts)
} else {
  print("loading prices from database...")
  syms <- c()
  for(iName in indices){
    pDf <- sqlQuery(lcon, sprintf("select px_close, time_stamp from bhav_index
                      where index_name = '%s'
                      and time_stamp >= '%s'",
                      iName, startDate))
    
    if(nrow(pDf) == 0) next
    pXts <- merge.xts(pXts, xts(pDf$px_close, pDf$time_stamp))
  }
  names(pXts) <- indices
  
  save(pXts, file = fileName)
}

#calcuate daily returns
dSymXts <- do.call(merge.xts, lapply(indices, \(x) dailyReturn(pXts[,x])))
names(dSymXts) <- indices

statsFile <- "../common/cp-stats_index.Rdata"
classXts <- NULL
if(file.exists(statsFile)){
  print("loading stats from cache...")
  load(statsFile)
  
  for(i in 1:length(indices)){
    iName <- indices[i]
    classRet <- classRets[[iName]]
    classXts <- merge.xts(classXts, classRet$regime_tbl |> 
                        mutate(Regime = if_else(Regime == 'STABLE', 1, 0)) |>
                        dplyr::select(Date, Regime) |> 
                        as.xts())
  }
  names(classXts) <- indices
} else {
  print("calculating stats...")
  classRets <- list()
  for(i in 1:length(indices)){
    iName <- indices[i]
    classRet <- classify_regime(dSymXts[, i])
    classXts <- merge.xts(classXts, classRet$regime_tbl |> 
                        mutate(Regime = if_else(Regime == 'STABLE', 1, 0)) |>
                        dplyr::select(Date, Regime) |> 
                        as.xts())
    classRets[[iName]] <- classRet
  }
  
  names(classXts) <- indices
  
  save(classRets, file = statsFile)
}

###############

# ---- Helper: compute four strategy returns for a given date range ----
# price_xts: single-column xts of prices
# regime_xts: single-column xts of regime (1=STABLE, 0=UNSTABLE)
# date_range: character e.g. "2016-01-01/2024-12-31"
# ret_xts: optional pre-computed daily returns xts (avoids re-computing)
compute_strategies <- function(price_xts, regime_xts, date_range,
                               sma_lb = 50, drag = 0.2/100,
                               ret_xts = NULL) {
  if (is.null(ret_xts)) {
    retL1 <- stats::lag(dailyReturn(price_xts[date_range]), -1)
  } else {
    retL1 <- stats::lag(ret_xts[date_range], -1)
  }
  pxSubset <- price_xts[date_range]
  classSubset <- regime_xts[date_range]
  smaPx <- SMA(pxSubset, sma_lb)

  smaGross <- ifelse(pxSubset > smaPx, retL1, 0)
  trd <- ifelse(pxSubset > smaPx, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  smaNet <- ifelse(trd != 0, smaGross - drag, smaGross)

  cpGross <- ifelse(classSubset == 1, retL1, 0)
  trd <- ifelse(classSubset == 1, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  cpNet <- ifelse(trd != 0, cpGross - drag, cpGross)

  smaCpGross <- ifelse(pxSubset > smaPx & classSubset == 1, retL1, 0)
  trd <- ifelse(pxSubset > smaPx & classSubset == 1, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  smaCpNet <- ifelse(trd != 0, smaCpGross - drag, smaCpGross)

  toPlot <- na.omit(merge(smaNet, cpNet, smaCpNet, retL1))
  names(toPlot) <- c("SMA", "CP", "SMA_CP", "B&H")
  toPlot
}

smaLb <- 50
for(i in 1:length(indices)){
  iName <- indices[i]
  retL1 <- stats::lag(dSymXts[paste0("/", trainEndDt), i], -1)
  pxSubset <- pXts[paste0("/", trainEndDt), i]
  smaPx <- SMA(pxSubset, smaLb)
  
  smaGross <- ifelse(pxSubset > smaPx, retL1, 0)
  trd <- ifelse(pxSubset > smaPx, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  smaNet <- ifelse(trd != 0, smaGross - drag, smaGross)
  
  cpGross <- ifelse(classXts[, i] == 1, retL1, 0)
  trd <- ifelse(classXts[, i] == 1, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  cpNet <- ifelse(trd != 0, cpGross - drag, cpGross)
  
  smaCpGross <- ifelse(pxSubset > smaPx & classXts[, i] == 1, retL1, 0)
  trd <- ifelse(pxSubset > smaPx & classXts[, i] == 1, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  smaCpNet <- ifelse(trd != 0, smaCpGross - drag, smaCpGross)
  
  toPlot <- na.omit(merge(smaNet, cpNet, smaCpNet, retL1))
  names(toPlot) <- c("SMA", "CP", "SMA_CP", "B&H")
  
  sharpe <- SharpeRatio.annualized(toPlot)
  print(sharpe)
  
  Common.PlotCumReturns(toPlot, iName, 
                        sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), #NULL)
                        sprintf("%s/%s.sma-cp.train.png", reportPath, iName), NULL)
  
  ddTb <- tibble()
  for(j in 1:ncol(toPlot)){
    tdd <- table.Drawdowns(toPlot[,j])
    tdd[,4]<-format(round(100*tdd[,4], 2), nsmall = 2)
    tdd$INDEX <- sprintf("%s (%s)", iName, names(toPlot)[j])
    ddTb <- rbind(ddTb, tdd)
  }
  
  ddFileNameHtml <- sprintf("%s/%s.sma-cp.train.drawdowns.html", reportPath, iName)
  ddFileNameImg <- sprintf("%s/%s.sma-cp.train.drawdowns.png", reportPath, iName)
  
  ddTb |>
    gt(groupname_col = 'INDEX') |>
    tab_header(title = "Drawdowns", subtitle = sprintf('%s:%s', index(xts::first(toPlot)), index(xts::last(toPlot)))) |>
    sub_missing(missing_text = '') |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    opt_stylize(style=5) |>
    gtsave(ddFileNameHtml)
  
  webshot2::webshot(
    ddFileNameHtml,
    ddFileNameImg, 
    selector = "table.gt_table", 
    expand = c(10, 10, 10, 10)
  )
}




for(i in 1:length(indices)){
  iName <- indices[i]
  retL1 <- stats::lag(dSymXts[testRange, i], -1)
  pxSubset <- pXts[testRange, i]
  smaPx <- SMA(pxSubset, smaLb)
  
  smaGross <- ifelse(pxSubset > smaPx, retL1, 0)
  trd <- ifelse(pxSubset > smaPx, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  smaNet <- ifelse(trd != 0, smaGross - drag, smaGross)
  
  cpGross <- ifelse(classXts[, i] == 1, retL1, 0)
  trd <- ifelse(classXts[, i] == 1, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  cpNet <- ifelse(trd != 0, cpGross - drag, cpGross)
  
  smaCpGross <- ifelse(pxSubset > smaPx & classXts[, i] == 1, retL1, 0)
  trd <- ifelse(pxSubset > smaPx & classXts[, i] == 1, 1, 0)
  trd <- trd - stats::lag(trd, 1)
  smaCpNet <- ifelse(trd != 0, smaCpGross - drag, smaCpGross)
  
  toPlot <- na.omit(merge(smaNet, cpNet, smaCpNet, retL1))
  names(toPlot) <- c("SMA", "CP", "SMA_CP", "B&H")
  
  sharpe <- SharpeRatio.annualized(toPlot)
  print(sharpe)
  
  Common.PlotCumReturns(toPlot, iName, 
                        sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), #NULL)
                        sprintf("%s/%s.sma-cp.test.png", reportPath, iName), NULL)
  
  ddTb <- tibble()
  for(j in 1:ncol(toPlot)){
    tdd <- table.Drawdowns(toPlot[,j])
    tdd[,4]<-format(round(100*tdd[,4], 2), nsmall = 2)
    tdd$INDEX <- sprintf("%s (%s)", iName, names(toPlot)[j])
    ddTb <- rbind(ddTb, tdd)
  }
  
  ddFileNameHtml <- sprintf("%s/%s.sma-cp.test.drawdowns.html", reportPath, iName)
  ddFileNameImg <- sprintf("%s/%s.sma-cp.test.drawdowns.png", reportPath, iName)

  ddTb |>
    gt(groupname_col = 'INDEX') |>
    tab_header(title = "Drawdowns", subtitle = sprintf('%s:%s', index(xts::first(toPlot)), index(xts::last(toPlot)))) |>
    sub_missing(missing_text = '') |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    opt_stylize(style=5) |>
    gtsave(ddFileNameHtml)

  webshot2::webshot(
    ddFileNameHtml,
    ddFileNameImg,
    selector = "table.gt_table",
    expand = c(10, 10, 10, 10)
  )
}

# =========================================================================
# Sliding-window analysis
# =========================================================================
# Window: 5 years ending on each date  |  Slide: 1 trading day
# Phase 1: compute all regime classifications (cached)
# Phase 2: compute strategies from cached regimes

print("=== SLIDING WINDOW ===")
window_days <- 365 * 5

cache_file <- sprintf("%s/window-class-cache.Rdata", reportPath)
window_cache <- list()
if (file.exists(cache_file)) {
  load(cache_file)
}

all_dates <- index(pXts)
n_total <- length(all_dates)

# ---- Phase 1: classify regimes (windows ending on each date) ----
print("  Phase 1: classifying regimes...")
for (iName in indices) {
  cat(sprintf("    %s\n", iName))
  for (i in seq_along(all_dates)) {
    window_start <- all_dates[i] - window_days + 1
    window_end   <- all_dates[i]
    if (window_start < first(all_dates)) next

    cache_key <- sprintf("sliding_%s_%s_%s", iName, window_start, window_end)
    if (cache_key %in% names(window_cache)) next  # already cached

    window_range <- paste0(window_start, "/", window_end)
    window_ret <- dSymXts[window_range, iName]

    if (nrow(window_ret) < 100) next

    window_class <- tryCatch({
      classify_regime(window_ret)
    }, error = function(e) NULL)

    if (!is.null(window_class)) {
      window_cache[[cache_key]] <- window_class$regime_tbl
    }

    if (i %% 500 == 0) {
      print(paste(iName, window_range))
      save(window_cache, file = cache_file)
    }
  }
}
save(window_cache, file = cache_file)

# ---- Phase 2: build consolidated regime xts + compute strategies ----
# Sliding: for each date D, use regime from the 5-year window ENDING on D.
# This is look-ahead-free: the classification for window [D-5y, D] only
# sees data up to D.
print("  Phase 2: building consolidated regimes + computing strategies...")

full_range <- paste0(first(all_dates), "/", last(all_dates))

sliding_results <- tibble()
sliding_strats <- list()

for (iName in indices) {
  cat(sprintf("    %s\n", iName))
  regime_vec <- rep(NA_integer_, length(all_dates))

  for (i in seq_along(all_dates)) {
    window_start <- all_dates[i] - window_days + 1
    window_end   <- all_dates[i]
    if (window_start < first(all_dates)) next

    cache_key <- sprintf("sliding_%s_%s_%s", iName, window_start, window_end)
    if (!cache_key %in% names(window_cache)) next

    tbl <- window_cache[[cache_key]]
    row <- tbl |> filter(Date == window_end)
    if (nrow(row) == 0) row <- tail(tbl, 1)
    if (nrow(row) > 0) {
      regime_vec[i] <- ifelse(row$Regime[1] == 'STABLE', 1L, 0L)
    }
    if (i %% 500 == 0) cat(sprintf("      %d/%d\n", i, n_total))
  }

  regime_xts <- xts(regime_vec, order.by = all_dates)
  regime_xts <- na.omit(regime_xts)

  if (nrow(regime_xts) < 50) next

  strat <- tryCatch({
    compute_strategies(pXts[, iName], regime_xts, full_range,
                       sma_lb = smaLb, drag = drag,
                       ret_xts = dSymXts[, iName])
  }, error = function(e) NULL)

  if (is.null(strat) || nrow(strat) < 50) next

  sliding_strats[[iName]] <- strat

  sharpe <- SharpeRatio.annualized(strat)
  ann_ret <- Return.annualized(strat)

  sliding_results <- rbind(sliding_results, tibble(
    Index         = iName,
    SMA_Ret       = round(ann_ret[1, "SMA"], 4),
    CP_Ret        = round(ann_ret[1, "CP"], 4),
    SMA_CP_Ret    = round(ann_ret[1, "SMA_CP"], 4),
    BH_Ret        = round(ann_ret[1, "B&H"], 4),
    SMA_Sharpe    = round(sharpe[1, "SMA"], 3),
    CP_Sharpe     = round(sharpe[1, "CP"], 3),
    SMA_CP_Sharpe = round(sharpe[1, "SMA_CP"], 3),
    BH_Sharpe     = round(sharpe[1, "B&H"], 3)
  ))
}

print("Sliding-window Sharpe ratios:")
print(sliding_results)

# gt table
sliding_results |>
  gt() |>
  tab_header(
    title = "Sliding Window",
    subtitle = "5-year look-back; consolidated regime; annualized returns + Sharpe"
  ) |>
  tab_spanner(label = "Annualized Return", columns = ends_with("_Ret")) |>
  tab_spanner(label = "Sharpe Ratio", columns = ends_with("_Sharpe")) |>
  fmt_percent(columns = ends_with("_Ret"), decimals = 2) |>
  fmt_number(columns = ends_with("_Sharpe"), decimals = 2) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Index)
  ) |>
  cols_label(
    SMA_Ret = "SMA", CP_Ret = "CP", SMA_CP_Ret = "SMA+CP", BH_Ret = "B&H",
    SMA_Sharpe = "SMA", CP_Sharpe = "CP", SMA_CP_Sharpe = "SMA+CP", BH_Sharpe = "B&H"
  ) |>
  gtsave(sprintf("%s/sliding-window-sharpe.html", reportPath))

webshot2::webshot(
  sprintf("%s/sliding-window-sharpe.html", reportPath),
  sprintf("%s/sliding-window-sharpe.png", reportPath),
  selector = "table.gt_table",
  expand = c(10, 10, 10, 10)
)

# sliding-window drawdowns
print("  Sliding-window drawdowns...")
for (iName in names(sliding_strats)) {
  strat <- sliding_strats[[iName]]
  ddTb <- tibble()
  for (j in 1:ncol(strat)) {
    tdd <- table.Drawdowns(strat[, j])
    tdd[, 4] <- format(round(100 * tdd[, 4], 2), nsmall = 2)
    tdd$INDEX <- sprintf("%s (%s)", iName, names(strat)[j])
    ddTb <- rbind(ddTb, tdd)
  }

  ddHtml <- sprintf("%s/%s.sliding.drawdowns.html", reportPath, iName)
  ddImg  <- sprintf("%s/%s.sliding.drawdowns.png", reportPath, iName)

  ddTb |>
    gt(groupname_col = "INDEX") |>
    tab_header(
      title = "Drawdowns — Sliding Window",
      subtitle = sprintf("%s: %s", iName,
                         paste(format(range(index(strat))), collapse = " → "))
    ) |>
    sub_missing(missing_text = "") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    gtsave(ddHtml)

  webshot2::webshot(ddHtml, ddImg,
                    selector = "table.gt_table",
                    expand = c(10, 10, 10, 10))
}

# sliding-window cumulative returns
for (iName in names(sliding_strats)) {
  strat <- sliding_strats[[iName]]
  Common.PlotCumReturns(strat, iName,
    sprintf("Sliding Window: %s → %s",
            format(start(strat), "%Y-%m-%d"), format(end(strat), "%Y-%m-%d")),
    sprintf("%s/%s.sliding.cumret.png", reportPath, iName), NULL)
}
# =========================================================================
# Expanding-window analysis
# =========================================================================
# Window: 2005 → date  |  Expands 1 day each step
# Phase 1: compute all regime classifications (cached)
# Phase 2: compute strategies from cached regimes

print("=== EXPANDING WINDOW ===")
min_window_days <- 365 * 5

# ---- Phase 1: classify regimes ----
print("  Phase 1: classifying regimes...")
for (iName in indices) {
  cat(sprintf("    %s\n", iName))
  for (i in seq_along(all_dates)) {
    window_start <- all_dates[1]
    window_end   <- all_dates[i]
    if (as.numeric(window_end - window_start) < min_window_days) next

    cache_key <- sprintf("expanding_%s_%s_%s", iName, window_start, window_end)
    if (cache_key %in% names(window_cache)) next

    window_range <- paste0(window_start, "/", window_end)
    window_ret <- dSymXts[window_range, iName]

    if (nrow(window_ret) < 100) next

    window_class <- tryCatch({
      classify_regime(window_ret)
    }, error = function(e) NULL)

    if (!is.null(window_class)) {
      window_cache[[cache_key]] <- window_class$regime_tbl
    }

    if (i %% 500 == 0) {
      print(paste(iName, window_range))
      save(window_cache, file = cache_file)
    }
  }
}
save(window_cache, file = cache_file)

# ---- Phase 2: build consolidated regime xts + compute strategies ----
# For each date, pull regime from the expanding window ending on that date.
print("  Phase 2: building consolidated regimes + computing strategies...")

expanding_results <- tibble()
expanding_strats <- list()

for (iName in indices) {
  cat(sprintf("    %s\n", iName))
  regime_vec <- rep(NA_integer_, length(all_dates))

  for (i in seq_along(all_dates)) {
    window_start <- all_dates[1]
    window_end   <- all_dates[i]
    if (as.numeric(window_end - window_start) < min_window_days) next

    cache_key <- sprintf("expanding_%s_%s_%s", iName, window_start, window_end)
    if (!cache_key %in% names(window_cache)) next

    tbl <- window_cache[[cache_key]]
    row <- tbl |> filter(Date == window_end)
    if (nrow(row) == 0) row <- tail(tbl, 1)
    if (nrow(row) > 0) {
      regime_vec[i] <- ifelse(row$Regime[1] == 'STABLE', 1L, 0L)
    }
    if (i %% 500 == 0) cat(sprintf("      %d/%d\n", i, n_total))
  }

  regime_xts <- xts(regime_vec, order.by = all_dates)
  regime_xts <- na.omit(regime_xts)

  if (nrow(regime_xts) < 50) next

  strat <- tryCatch({
    compute_strategies(pXts[, iName], regime_xts, full_range,
                       sma_lb = smaLb, drag = drag,
                       ret_xts = dSymXts[, iName])
  }, error = function(e) NULL)

  if (is.null(strat) || nrow(strat) < 50) next

  expanding_strats[[iName]] <- strat

  sharpe <- SharpeRatio.annualized(strat)

  expanding_results <- rbind(expanding_results, tibble(
    Index         = iName,
    SMA_Sharpe    = round(sharpe[1, "SMA"], 3),
    CP_Sharpe     = round(sharpe[1, "CP"], 3),
    SMA_CP_Sharpe = round(sharpe[1, "SMA_CP"], 3),
    BH_Sharpe     = round(sharpe[1, "B&H"], 3)
  ))
}

print("Expanding-window Sharpe ratios:")
print(expanding_results)

# gt table
expanding_results |>
  gt() |>
  tab_header(
    title = "Expanding Window",
    subtitle = "2005 → date; consolidated regime; annualized returns + Sharpe"
  ) |>
  tab_spanner(label = "Annualized Return", columns = ends_with("_Ret")) |>
  tab_spanner(label = "Sharpe Ratio", columns = ends_with("_Sharpe")) |>
  fmt_percent(columns = ends_with("_Ret"), decimals = 2) |>
  fmt_number(columns = ends_with("_Sharpe"), decimals = 2) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Index)
  ) |>
  cols_label(
    SMA_Ret = "SMA", CP_Ret = "CP", SMA_CP_Ret = "SMA+CP", BH_Ret = "B&H",
    SMA_Sharpe = "SMA", CP_Sharpe = "CP", SMA_CP_Sharpe = "SMA+CP", BH_Sharpe = "B&H"
  ) |>
  gtsave(sprintf("%s/expanding-window-sharpe.html", reportPath))

webshot2::webshot(
  sprintf("%s/expanding-window-sharpe.html", reportPath),
  sprintf("%s/expanding-window-sharpe.png", reportPath),
  selector = "table.gt_table",
  expand = c(10, 10, 10, 10)
)

# expanding-window drawdowns
print("  Expanding-window drawdowns...")
for (iName in names(expanding_strats)) {
  strat <- expanding_strats[[iName]]
  ddTb <- tibble()
  for (j in 1:ncol(strat)) {
    tdd <- table.Drawdowns(strat[, j])
    tdd[, 4] <- format(round(100 * tdd[, 4], 2), nsmall = 2)
    tdd$INDEX <- sprintf("%s (%s)", iName, names(strat)[j])
    ddTb <- rbind(ddTb, tdd)
  }

  ddHtml <- sprintf("%s/%s.expanding.drawdowns.html", reportPath, iName)
  ddImg  <- sprintf("%s/%s.expanding.drawdowns.png", reportPath, iName)

  ddTb |>
    gt(groupname_col = "INDEX") |>
    tab_header(
      title = "Drawdowns — Expanding Window",
      subtitle = sprintf("%s: %s", iName,
                         paste(format(range(index(strat))), collapse = " → "))
    ) |>
    sub_missing(missing_text = "") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    gtsave(ddHtml)

  webshot2::webshot(ddHtml, ddImg,
                    selector = "table.gt_table",
                    expand = c(10, 10, 10, 10))
}

# expanding-window cumulative returns
for (iName in names(expanding_strats)) {
  strat <- expanding_strats[[iName]]
  Common.PlotCumReturns(strat, iName,
    sprintf("Expanding Window: %s → %s",
            format(start(strat), "%Y-%m-%d"), format(end(strat), "%Y-%m-%d")),
    sprintf("%s/%s.expanding.cumret.png", reportPath, iName), NULL)
}
