source("../common/regime_classify.R")

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')
library('gtExtras')
library('webshot2')
library('parallel')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")
source("/mnt/data/blog/common/theme.returns.common.r")

drag  <- 0.2/100
smaLb <- 50

print("connecting to norway...")
lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockViz", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

startDate <- as.Date("2005-04-01")
indices <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR")

# ---- Fetch prices ----
fileName <- "../common/prices_index.Rdata"
pXts <- NULL
if (file.exists(fileName)) {
  print("loading prices from cache...")
  load(fileName)
} else {
  print("loading prices from database...")
  for (iName in indices) {
    pDf <- sqlQuery(lcon, sprintf(
      "select px_close, time_stamp from bhav_index
       where index_name = '%s' and time_stamp >= '%s'", iName, startDate))
    if (nrow(pDf) == 0) next
    pXts <- merge.xts(pXts, xts(pDf$px_close, pDf$time_stamp))
  }
  names(pXts) <- indices
  save(pXts, file = fileName)
}

dSymXts <- do.call(merge.xts, lapply(indices, \(x) dailyReturn(pXts[, x])))
names(dSymXts) <- indices

# ---- Phase 1: Load regime cache ----
print("=== LOADING REGIME CACHE ===")
cache_file <- sprintf("%s/window-class-cache.Rdata", reportPath)
if (!file.exists(cache_file)) {
  parent_cache <- "../historical-index/window-class-cache.Rdata"
  if (file.exists(parent_cache)) {
    cat(sprintf("  linking from %s\n", parent_cache))
    file.symlink(parent_cache, cache_file)
  }
}
window_cache <- list()
if (file.exists(cache_file)) {
  load(cache_file)
  cat(sprintf("  loaded %d cached classifications\n", length(window_cache)))
}
all_dates <- index(pXts)

# ---- Phase 2: Expanding window ----
print("=== EXPANDING WINDOW ===")
window_days <- 365 * 5
full_range <- paste0(first(all_dates), "/", last(all_dates))

results <- tibble()
strats  <- list()

for (iName in indices) {
  cat(sprintf("  %s\n", iName))
  label <- gsub(" TR$", "", iName)

  vs_vec <- rep(NA_real_, length(all_dates))
  for (i in seq_along(all_dates)) {
    d <- all_dates[i]
    ws <- d - window_days + 1
    if (ws < first(all_dates)) next
    ck <- sprintf("sliding_%s_%s_%s", iName, ws, d)
    if (!ck %in% names(window_cache)) next
    tbl <- window_cache[[ck]]
    row <- tbl |> filter(Date == d)
    if (nrow(row) == 0) row <- tail(tbl, 1)
    if (nrow(row) > 0) {
      vs_vec[i] <- row$N_Unstable[1] / pmax(row$N_Total[1], 1)
    }
  }

  vs_xts <- xts(vs_vec, order.by = all_dates)
  vs_xts <- na.locf(vs_xts, fromLast = FALSE)
  vs_xts <- na.omit(vs_xts)
  first_valid <- which(!is.na(coredata(vs_xts)))[1]
  vs_xts <- vs_xts[first_valid:nrow(vs_xts)]
  if (nrow(vs_xts) < 50) next

  retL1  <- stats::lag(dSymXts[full_range, iName], -1)
  pxSub  <- pXts[full_range, iName]
  smaPx  <- SMA(pxSub, smaLb)

  vsAligned <- merge(retL1, vs_xts, join = "left")[, 2]
  vs        <- coredata(vsAligned)
  in_dt     <- ifelse(pxSub < smaPx, 1, 0)

  # FusLin: vote-share sizing during downtrends, fully invested otherwise
  fusPos   <- ifelse(in_dt, 1 - vs, 1)
  fusGross <- fusPos * retL1
  fusNet   <- ifelse(abs(fusPos - stats::lag(fusPos, 1)) > 1e-10,
                     fusGross - abs(fusPos - stats::lag(fusPos, 1)) * drag,
                     fusGross)

  bhPos <- xts(rep(1, nrow(retL1)), order.by = index(retL1))

  R <- na.omit(merge(fusNet, retL1))
  names(R) <- c("FusLin", "B_H")
  P <- na.omit(merge(fusPos, bhPos))
  P <- P[index(R)]
  names(P) <- names(R)

  if (nrow(R) < 50) next
  strats[[label]] <- list(rets = R, positions = P)

  ann_ret  <- Return.annualized(R)
  sharpe   <- SharpeRatio.annualized(R)
  dd       <- maxDrawdown(R)
  calmar   <- ann_ret / abs(dd)
  time_in  <- colMeans(coredata(P))
  turnover <- colMeans(abs(diff(P)), na.rm = TRUE)

  row <- tibble(Index = iName)
  for (cn in names(R)) {
    row[[paste0(cn, "_Ret")]]    <- round(ann_ret[1, cn], 4)
    row[[paste0(cn, "_Sharpe")]] <- round(sharpe[1, cn], 3)
    row[[paste0(cn, "_DD")]]     <- round(-as.numeric(dd[1, cn]), 4)
    row[[paste0(cn, "_Calmar")]] <- round(as.numeric(calmar[1, cn]), 3)
    row[[paste0(cn, "_TimeIn")]] <- round(as.numeric(time_in[cn]), 3)
    row[[paste0(cn, "_Tvr")]]    <- round(as.numeric(turnover[cn]), 4)
  }
  results <- rbind(results, row)
}

print("Results:")
print(results)

# ---- Metrics table ----
show_labels <- c(
  "Ret" = "Annualized Return", "Sharpe" = "Sharpe Ratio",
  "DD" = "Max Drawdown", "Calmar" = "Calmar Ratio",
  "TimeIn" = "Time in Market"
)
show_suffixes <- names(show_labels)
all_cols <- names(results)

display_cols <- "Index"
spanner_map <- list()
for (suf in show_suffixes) {
  cols <- grep(paste0("_", suf, "$"), all_cols, value = TRUE)
  if (length(cols) > 0) {
    display_cols <- c(display_cols, cols)
    spanner_map[[show_labels[suf]]] <- cols
  }
}

tbl <- results |> select(all_of(display_cols)) |> gt() |>
  tab_header(
    title = "FusLin vs Buy & Hold — Expanding Window",
    subtitle = "Vote-share sizing during downtrends, fully invested in uptrends"
  )

for (nm in names(spanner_map)) {
  tbl <- tbl |> tab_spanner(label = nm, columns = all_of(spanner_map[[nm]]))
}

for (suf in show_suffixes) {
  cols <- grep(paste0("_", suf, "$"), all_cols, value = TRUE)
  if (length(cols) == 0) next
  if (suf %in% c("Ret", "DD")) {
    tbl <- tbl |> fmt_percent(columns = all_of(cols), decimals = 1)
  } else if (suf == "TimeIn") {
    tbl <- tbl |> fmt_percent(columns = all_of(cols), decimals = 0)
  } else {
    tbl <- tbl |> fmt_number(columns = all_of(cols), decimals = 2)
  }
}

tbl <- tbl |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = Index))

# B&H beat highlighting
d <- results
bh_ret <- "B_H_Ret"; bh_sr <- "B_H_Sharpe"; bh_dd <- "B_H_DD"
for (col in grep("_Ret$", names(d), value = TRUE)) {
  if (col == bh_ret || !bh_ret %in% names(d)) next
  rows <- which(d[[col]] > d[[bh_ret]])
  if (length(rows) > 0) tbl <- tbl |> tab_style(
    style = cell_text(weight = "bold", color = "#1a6b1a"),
    locations = cells_body(columns = all_of(col), rows = rows))
}
for (col in grep("_Sharpe$", names(d), value = TRUE)) {
  if (col == bh_sr || !bh_sr %in% names(d)) next
  rows <- which(d[[col]] > d[[bh_sr]])
  if (length(rows) > 0) tbl <- tbl |> tab_style(
    style = cell_text(weight = "bold", color = "#1a6b1a"),
    locations = cells_body(columns = all_of(col), rows = rows))
}
for (col in grep("_DD$", names(d), value = TRUE)) {
  if (col == bh_dd || !bh_dd %in% names(d)) next
  rows <- which(d[[col]] > d[[bh_dd]])
  if (length(rows) > 0) tbl <- tbl |> tab_style(
    style = cell_text(weight = "bold", color = "#1a6b1a"),
    locations = cells_body(columns = all_of(col), rows = rows))
}

gtsave(tbl, sprintf("%s/metrics.html", reportPath))
webshot2::webshot(
  sprintf("%s/metrics.html", reportPath),
  sprintf("%s/metrics.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10), vwidth = 1200)

# ---- Cumulative return charts (with Sharpe in subtitle) ----
print("  Cumulative return charts...")
for (iName in indices) {
  label <- gsub(" TR$", "", iName)
  if (!label %in% names(strats)) next
  R <- strats[[label]]$rets
  sr <- SharpeRatio.annualized(R)

  fileName <- sprintf("%s/%s.cumret.png", reportPath, label)
  png(fileName, width = 1400, height = 800, bg = "white")

  tryCatch({
    layout(matrix(c(1, 2)), heights = c(2, 1.3), widths = 1)
    par(mar = c(0, 4, 4, 2), bty = "n")
    plot_object <- chart.CumReturns(R, cex.legend = 1, main = NA,
      ylab = "Cumulative Return", xaxis = FALSE, legend.loc = "topleft",
      begin = c("first", "axis"), geometric = TRUE)
    print(plot_object)
    title(main = iName)
    mtext(sprintf("%s → %s  |  FusLin SR=%.2f  B&H SR=%.2f  |  cum: %s  ann: %s",
          format(start(R), "%Y-%m-%d"), format(end(R), "%Y-%m-%d"),
          round(sr[1, "FusLin"], 2), round(sr[1, "B_H"], 2),
          paste(sprintf("%.2f%%", 100*apply(R, 2, Return.cumulative)),
                collapse = " / "),
          paste(sprintf("%.2f%%", 100*apply(R, 2, Return.annualized)),
                collapse = " / ")),
          cex = 0.9, line = 0)

    par(mar = c(5, 4, 0, 2))
    plot_object <- chart.Drawdown(R, main = NA, ylab = "Drawdown",
      event.labels = NULL, ylog = FALSE, geometric = TRUE, bty = "n")
    print(plot_object)
    mtext("@StockViz", side = 4, col = "grey")
  }, error = function(e) {
    cat(sprintf("  ERROR in chart for %s: %s\n", iName, e$message))
  })

  dev.off()
}

print("=== DONE ===")
