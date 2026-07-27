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

# ---- Vote-share sizing rules ----
vs_linear    <- function(vs) 1 - vs
vs_threshlin <- function(vs, tl = 0.3, th = 0.7) ifelse(vs < tl, 1, ifelse(vs > th, 0, (th - vs) / (th - tl)))
vs_sigmoid   <- function(vs, k = 10) 1 / (1 + exp(k * (vs - 0.5)))

# ---- Strategy display order ----
# B&H: always long
# SMA: long when close > 50-day MA
# CP:  binary exit on UNSTABLE
# SMA+CP: AND gate
# DG: exit only when UNSTABLE AND downtrend
# Lin/ThrLin/Sig: vote-share continuous sizing

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

# ---- Helper: compute all strategy returns + positions ----
# vote_share_xts: optional xts of vote share [0,1] for continuous sizing
compute_strategies <- function(price_xts, regime_xts, date_range,
                               sma_lb = 50, drag = 0.2/100,
                               ret_xts = NULL, vote_share_xts = NULL) {
  if (is.null(ret_xts)) {
    retL1 <- stats::lag(dailyReturn(price_xts[date_range]), -1)
  } else {
    retL1 <- stats::lag(ret_xts[date_range], -1)
  }
  pxSubset <- price_xts[date_range]

  classSubset <- merge(retL1, regime_xts, join = "left")[, 2]
  smaPx <- SMA(pxSubset, sma_lb)

  # --- SMA ---
  smaPos   <- ifelse(pxSubset > smaPx, 1, 0)
  smaGross <- smaPos * retL1
  trd      <- smaPos - stats::lag(smaPos, 1)
  smaNet   <- ifelse(trd != 0, smaGross - drag, smaGross)

  # --- CP (binary) ---
  cpPos   <- ifelse(classSubset == 1, 1, 0)
  cpGross <- cpPos * retL1
  trd     <- cpPos - stats::lag(cpPos, 1)
  cpNet   <- ifelse(trd != 0, cpGross - drag, cpGross)

  # --- SMA+CP ---
  smaCpPos   <- ifelse(pxSubset > smaPx & classSubset == 1, 1, 0)
  smaCpGross <- smaCpPos * retL1
  trd        <- smaCpPos - stats::lag(smaCpPos, 1)
  smaCpNet   <- ifelse(trd != 0, smaCpGross - drag, smaCpGross)

  # --- DG: exit only when UNSTABLE AND downtrend ---
  dgPos   <- ifelse(classSubset == 0 & pxSubset < smaPx, 0, 1)
  dgGross <- dgPos * retL1
  trd     <- dgPos - stats::lag(dgPos, 1)
  dgNet   <- ifelse(trd != 0, dgGross - drag, dgGross)

  # --- Vote-share sizing (if vote_share_xts provided) ---
  linPos <- threshPos <- sigPos <- NULL
  has_vs <- !is.null(vote_share_xts) && nrow(vote_share_xts) > 0
  if (has_vs) {
    vsAligned <- merge(retL1, vote_share_xts, join = "left")[, 2]
    vsVec <- coredata(vsAligned)

    linPos    <- vs_linear(vsVec)
    threshPos <- vs_threshlin(vsVec)
    sigPos    <- vs_sigmoid(vsVec)

    linGross    <- linPos * retL1
    trd         <- linPos - stats::lag(linPos, 1)
    linNet      <- ifelse(abs(trd) > 1e-10, linGross - abs(trd) * drag, linGross)

    thrGross    <- threshPos * retL1
    trd         <- threshPos - stats::lag(threshPos, 1)
    thrNet      <- ifelse(abs(trd) > 1e-10, thrGross - abs(trd) * drag, thrGross)

    sigGross    <- sigPos * retL1
    trd         <- sigPos - stats::lag(sigPos, 1)
    sigNet      <- ifelse(abs(trd) > 1e-10, sigGross - abs(trd) * drag, sigGross)

    # --- Fused: vote-share sizing only during downtrends, else fully invested ---
    # downtrend = price < SMA.  During downtrends: reduce exposure via vote share.
    # Otherwise: stay 100% long regardless of regime signal.
    in_downtrend <- ifelse(pxSubset < smaPx, 1, 0)

    fusLinPos   <- ifelse(in_downtrend, linPos, 1)
    fusLinGross <- fusLinPos * retL1
    trd         <- fusLinPos - stats::lag(fusLinPos, 1)
    fusLinNet   <- ifelse(abs(trd) > 1e-10, fusLinGross - abs(trd) * drag, fusLinGross)

    fusThrPos   <- ifelse(in_downtrend, threshPos, 1)
    fusThrGross <- fusThrPos * retL1
    trd         <- fusThrPos - stats::lag(fusThrPos, 1)
    fusThrNet   <- ifelse(abs(trd) > 1e-10, fusThrGross - abs(trd) * drag, fusThrGross)

    fusSigPos   <- ifelse(in_downtrend, sigPos, 1)
    fusSigGross <- fusSigPos * retL1
    trd         <- fusSigPos - stats::lag(fusSigPos, 1)
    fusSigNet   <- ifelse(abs(trd) > 1e-10, fusSigGross - abs(trd) * drag, fusSigGross)
  }

  bhPos <- xts(rep(1, nrow(retL1)), order.by = index(retL1))

  # Assemble returns
  ret_cols <- list(SMA = smaNet, CP = cpNet, SMA_CP = smaCpNet, DG = dgNet)
  pos_cols <- list(SMA = smaPos, CP = cpPos, SMA_CP = smaCpPos, DG = dgPos)
  if (has_vs) {
    ret_cols$Lin     <- linNet
    ret_cols$ThrLin  <- thrNet
    ret_cols$Sig     <- sigNet
    ret_cols$FusLin  <- fusLinNet
    ret_cols$FusThr  <- fusThrNet
    ret_cols$FusSig  <- fusSigNet
    pos_cols$Lin     <- linPos
    pos_cols$ThrLin  <- threshPos
    pos_cols$Sig     <- sigPos
    pos_cols$FusLin  <- fusLinPos
    pos_cols$FusThr  <- fusThrPos
    pos_cols$FusSig  <- fusSigPos
  }
  ret_cols$B_H <- retL1
  pos_cols$B_H <- bhPos

  all_rets <- na.omit(do.call(merge, unname(ret_cols)))
  names(all_rets) <- names(ret_cols)
  all_pos <- na.omit(do.call(merge, unname(pos_cols)))
  all_pos <- all_pos[index(all_rets)]
  names(all_pos) <- names(all_rets)

  list(rets = all_rets, positions = all_pos)
}

# ---- Helper: compute metrics ----
compute_metrics <- function(strat_list) {
  R <- strat_list$rets
  P <- strat_list$positions
  ann_ret  <- Return.annualized(R)
  sharpe   <- SharpeRatio.annualized(R)
  dd       <- maxDrawdown(R)
  calmar   <- ann_ret / abs(dd)
  time_in  <- colMeans(coredata(P))   # avg position = effective exposure (not binary >0)
  turnover <- colMeans(abs(diff(P)), na.rm = TRUE)
  list(ann_ret = ann_ret, sharpe = sharpe, max_dd = dd,
       calmar = calmar, time_in = time_in, turnover = turnover)
}

build_result_row <- function(strat_obj, iName, test_start, test_end) {
  strat_test <- strat_obj$rets
  m <- compute_metrics(strat_obj)
  row <- tibble(Index = iName,
                Window_Start = as.character(test_start),
                Window_End   = as.character(test_end))
  for (cn in names(strat_test)) {
    row[[paste0(cn, "_Ret")]]    <- round(m$ann_ret[1, cn], 4)
    row[[paste0(cn, "_Sharpe")]] <- round(m$sharpe[1, cn], 3)
    row[[paste0(cn, "_TimeIn")]] <- round(as.numeric(m$time_in[cn]), 3)
    row[[paste0(cn, "_Tvr")]]    <- round(as.numeric(m$turnover[cn]), 4)
  }
  row
}

summarise_results <- function(df) {
  df |> group_by(Index) |>
    summarise(
      Windows = n(),
      across(matches("_Ret$"),     ~ round(mean(.x, na.rm = TRUE), 4)),
      across(matches("_Sharpe$"),  ~ round(mean(.x, na.rm = TRUE), 3)),
      across(matches("_TimeIn$"),  ~ round(mean(.x, na.rm = TRUE), 3)),
      across(matches("_Tvr$"),     ~ round(mean(.x, na.rm = TRUE), 4)),
      .groups = "drop")
}

# ---- Helper: merged DD/Calmar from concatenated window series ----
add_merged_dd_calmar <- function(summary_df, strats_list, indices_vec, pattern) {
  dd_rows <- tibble()
  for (iName in indices_vec) {
    label <- gsub(" TR$", "", iName)
    idx <- strats_list[grepl(paste0("^", label, pattern), names(strats_list))]
    if (length(idx) == 0) next
    merged <- do.call(rbind.xts, lapply(idx, `[[`, "rets"))
    merged <- na.omit(merged)
    if (nrow(merged) < 20) next
    dd <- maxDrawdown(merged)
    calmar <- Return.annualized(merged) / abs(dd)
    row <- tibble(Index = iName)
    for (cn in names(merged)) {
      row[[paste0(cn, "_DD")]]     <- round(-as.numeric(dd[1, cn]), 4)
      row[[paste0(cn, "_Calmar")]] <- round(as.numeric(calmar[1, cn]), 3)
    }
    dd_rows <- rbind(dd_rows, row)
  }
  if (nrow(dd_rows) > 0) summary_df %>% left_join(dd_rows, by = "Index") else summary_df
}

# =========================================================================
# Phase 1: Regime classification
# =========================================================================
print("=== PHASE 1: REGIME CLASSIFICATION ===")
window_days <- 365 * 5; ncores <- 4

cache_file <- sprintf("%s/window-class-cache.Rdata", reportPath)
cache_is_shared <- FALSE
if (!file.exists(cache_file)) {
  parent_cache <- "../historical-index/window-class-cache.Rdata"
  if (file.exists(parent_cache)) {
    cat(sprintf("  linking cache from %s\n", parent_cache))
    file.symlink(parent_cache, cache_file)
    cache_is_shared <- TRUE
  }
}

window_cache <- list()
if (file.exists(cache_file)) {
  load(cache_file)
  cat(sprintf("  loaded %d cached classifications%s\n",
              length(window_cache),
              if (cache_is_shared) " (shared, read-only)" else ""))
}

all_dates <- index(pXts)
needed <- FALSE

for (iName in indices) {
  cat(sprintf("  %s\n", iName))
  tasks <- list()
  for (i in seq_along(all_dates)) {
    window_start <- all_dates[i] - window_days + 1
    window_end   <- all_dates[i]
    if (window_start < first(all_dates)) next
    cache_key <- sprintf("sliding_%s_%s_%s", iName, window_start, window_end)
    if (cache_key %in% names(window_cache)) next
    tasks[[length(tasks) + 1]] <- list(
      key = cache_key, start = window_start, end = window_end, name = iName)
  }
  if (length(tasks) == 0) { cat("      all cached, skipping\n"); next }
  needed <- TRUE
  batch_size <- 500; n_batches <- ceiling(length(tasks) / batch_size)
  for (b in seq_len(n_batches)) {
    bs <- (b - 1) * batch_size + 1; be <- min(b * batch_size, length(tasks))
    batch <- tasks[bs:be]
    results <- mclapply(batch, function(task) {
      window_range <- paste0(task$start, "/", task$end)
      window_ret   <- dSymXts[window_range, task$name]
      if (nrow(window_ret) < 100) return(NULL)
      window_class <- tryCatch({ classify_regime(window_ret) },
                               error = function(e) NULL)
      if (!is.null(window_class)) {
        stats::setNames(list(window_class$regime_tbl), task$key)
      } else NULL
    }, mc.cores = ncores)
    for (res in results) {
      if (!is.null(res) && !is.null(names(res)))
        window_cache[[names(res)]] <- res[[1]]
    }
    print(paste(iName, batch[[1]]$start, "to", batch[[length(batch)]]$end,
                sprintf("[batch %d/%d]", b, n_batches)))
    if (!cache_is_shared) save(window_cache, file = cache_file)
  }
}
if (!needed) cat("  all classifications cached, Phase 1 skipped\n")

# =========================================================================
# Phase 2a: Sliding window
# =========================================================================
print("=== PHASE 2a: SLIDING WINDOW ===")
test_step_days <- 252L

process_sliding_index <- function(iName) {
  cache_keys <- grep(sprintf("^sliding_%s_", iName), names(window_cache), value = TRUE)
  if (length(cache_keys) == 0) return(NULL)

  all_windows <- tibble()
  for (ck in cache_keys) {
    tbl <- window_cache[[ck]]
    if (is.null(tbl) || nrow(tbl) == 0) next
    all_windows <- rbind(all_windows, tibble(
      train_start = min(tbl$Date), train_end = max(tbl$Date)))
  }
  if (nrow(all_windows) == 0) return(NULL)

  all_windows <- all_windows |> arrange(train_end) |>
    distinct(train_end, .keep_all = TRUE) |>
    filter(row_number() %% test_step_days == 0L)

  results_rows <- list(); strats <- list()
  label <- gsub(" TR$", "", iName)

  for (wi in seq_len(nrow(all_windows))) {
    train_end <- all_windows$train_end[wi]
    test_start_idx <- which(all_dates == train_end) + 1L
    if (is.na(test_start_idx) || test_start_idx > length(all_dates)) next
    test_end_idx <- min(test_start_idx + test_step_days - 1L, length(all_dates))
    test_start <- all_dates[test_start_idx]; test_end <- all_dates[test_end_idx]
    test_range <- paste0(test_start, "/", test_end)
    px_range   <- paste0(first(all_dates), "/", test_end)

    # Build regime + vote-share vectors
    regime_vec <- c(); vs_vec <- c()
    for (j in test_start_idx:test_end_idx) {
      d <- all_dates[j]; ws <- d - window_days + 1
      ck <- sprintf("sliding_%s_%s_%s", iName, ws, d)
      tbl <- window_cache[[ck]]
      if (is.null(tbl)) {
        regime_vec <- c(regime_vec, NA_integer_); vs_vec <- c(vs_vec, NA_real_)
        next
      }
      row <- tbl |> filter(Date == d)
      if (nrow(row) == 0) row <- tail(tbl, 1)
      if (nrow(row) > 0) {
        regime_vec <- c(regime_vec, ifelse(row$Regime[1] == "STABLE", 1L, 0L))
        vs_vec     <- c(vs_vec, row$N_Unstable[1] / pmax(row$N_Total[1], 1))
      } else {
        regime_vec <- c(regime_vec, NA_integer_); vs_vec <- c(vs_vec, NA_real_)
      }
    }
    regime_xts <- xts(regime_vec, order.by = all_dates[test_start_idx:test_end_idx])
    regime_xts <- na.locf(regime_xts, fromLast = FALSE)
    vs_xts     <- xts(vs_vec, order.by = all_dates[test_start_idx:test_end_idx])
    vs_xts     <- na.locf(vs_xts, fromLast = FALSE)
    if (nrow(regime_xts) < 20) next

    strat <- tryCatch({
      compute_strategies(pXts[, iName], regime_xts, px_range,
        sma_lb = smaLb, drag = drag, ret_xts = dSymXts[, iName],
        vote_share_xts = vs_xts)
    }, error = function(e) NULL)
    if (is.null(strat) || nrow(strat$rets) < 20) next

    strat_test <- list(
      rets = strat$rets[test_range],
      positions = strat$positions[test_range])
    if (nrow(strat_test$rets) < 20) next

    strats[[sprintf("%s_%s_%s", label, test_start, test_end)]] <- strat_test
    results_rows[[length(results_rows) + 1]] <-
      build_result_row(strat_test, iName, test_start, test_end)
  }
  list(results_rows = results_rows, strats = strats)
}

index_results <- mclapply(indices, process_sliding_index, mc.cores = ncores)

sliding_results <- tibble(); sliding_strats <- list()
for (res in index_results) {
  if (is.null(res)) next
  for (row in res$results_rows) sliding_results <- rbind(sliding_results, row)
  for (nm in names(res$strats)) sliding_strats[[nm]] <- res$strats[[nm]]
}

sliding_summary <- summarise_results(sliding_results)
sliding_summary <- add_merged_dd_calmar(sliding_summary, sliding_strats,
                                        indices, "_[0-9]{4}-")
print("Sliding-window summary:"); print(sliding_summary)

# =========================================================================
# Phase 2b: Expanding window
# =========================================================================
print("=== PHASE 2b: EXPANDING WINDOW ===")
full_range <- paste0(first(all_dates), "/", last(all_dates))

process_expanding_index <- function(iName) {
  regime_vec <- rep(NA_integer_, length(all_dates))
  vs_vec     <- rep(NA_real_, length(all_dates))
  for (i in seq_along(all_dates)) {
    d <- all_dates[i]; ws <- d - window_days + 1
    if (ws < first(all_dates)) next
    cache_key <- sprintf("sliding_%s_%s_%s", iName, ws, d)
    if (!cache_key %in% names(window_cache)) next
    tbl <- window_cache[[cache_key]]
    row <- tbl |> filter(Date == d)
    if (nrow(row) == 0) row <- tail(tbl, 1)
    if (nrow(row) > 0) {
      regime_vec[i] <- ifelse(row$Regime[1] == "STABLE", 1L, 0L)
      vs_vec[i]     <- row$N_Unstable[1] / pmax(row$N_Total[1], 1)
    }
  }

  regime_xts <- xts(regime_vec, order.by = all_dates)
  regime_xts <- na.locf(regime_xts, fromLast = FALSE)
  regime_xts <- na.omit(regime_xts)
  vs_xts <- xts(vs_vec, order.by = all_dates)
  vs_xts <- na.locf(vs_xts, fromLast = FALSE)
  vs_xts <- na.omit(vs_xts)
  first_valid <- which(!is.na(coredata(regime_xts)))[1]
  regime_xts <- regime_xts[first_valid:nrow(regime_xts)]
  vs_xts     <- vs_xts[first_valid:nrow(vs_xts)]
  if (nrow(regime_xts) < 50) return(NULL)

  strat <- tryCatch({
    compute_strategies(pXts[, iName], regime_xts, full_range,
      sma_lb = smaLb, drag = drag, ret_xts = dSymXts[, iName],
      vote_share_xts = vs_xts)
  }, error = function(e) NULL)
  if (is.null(strat) || nrow(strat$rets) < 50) return(NULL)

  row <- build_result_row(strat, iName, start(strat$rets), end(strat$rets))
  # Add DD/Calmar directly
  R <- strat$rets; dd <- maxDrawdown(R); calmar <- Return.annualized(R) / abs(dd)
  for (cn in names(R)) {
    row[[paste0(cn, "_DD")]]     <- round(-as.numeric(dd[1, cn]), 4)
    row[[paste0(cn, "_Calmar")]] <- round(as.numeric(calmar[1, cn]), 3)
  }
  list(name = iName, strat = strat, row = row)
}

expanding_index_results <- mclapply(indices, process_expanding_index, mc.cores = ncores)

expanding_results <- tibble(); expanding_strats <- list()
for (res in expanding_index_results) {
  if (is.null(res)) next
  expanding_results <- rbind(expanding_results, res$row)
  expanding_strats[[res$name]] <- res$strat
}
expanding_summary <- expanding_results
print("Expanding-window results:"); print(expanding_summary)

# =========================================================================
# Phase 2c: Frozen annual model
# =========================================================================
print("=== PHASE 2c: FROZEN ANNUAL MODEL ===")
frozen_results <- tibble(); frozen_strats <- list()

for (iName in indices) {
  cat(sprintf("  %s\n", iName))
  label <- gsub(" TR$", "", iName)
  first_train_end <- first(all_dates) + window_days - 1
  if (first_train_end > last(all_dates)) next
  anchor_dates <- all_dates[all_dates >= first_train_end]
  anchor_dates <- anchor_dates[seq(1, length(anchor_dates), by = test_step_days)]

  for (i in seq_along(anchor_dates)) {
    ad <- anchor_dates[i]
    train_start <- ad - window_days + 1; train_end <- ad
    ck <- sprintf("sliding_%s_%s_%s", iName, train_start, train_end)
    if (!ck %in% names(window_cache)) next
    tbl <- window_cache[[ck]]
    row <- tbl |> filter(Date == train_end)
    if (nrow(row) == 0) row <- tail(tbl, 1)
    if (nrow(row) == 0) next
    regime_at_freeze <- ifelse(row$Regime[1] == "STABLE", 1L, 0L)
    vs_at_freeze     <- row$N_Unstable[1] / pmax(row$N_Total[1], 1)
    # vs_at_freeze in [0,1]: 0 = all methods agree calm, 1 = all agree turbulent

    px_at_freeze <- as.numeric(pXts[train_end, iName])
    sma_hist <- SMA(pXts[paste0(train_start, "/", train_end), iName], smaLb)
    sma_at_freeze <- as.numeric(tail(sma_hist, 1))
    downtrend_at_freeze <- !is.na(sma_at_freeze) && px_at_freeze < sma_at_freeze

    test_start_idx <- which(all_dates == train_end) + 1L
    if (is.na(test_start_idx) || test_start_idx > length(all_dates)) next
    test_end_idx <- min(test_start_idx + test_step_days - 1L, length(all_dates))
    test_start <- all_dates[test_start_idx]; test_end <- all_dates[test_end_idx]
    test_range <- paste0(test_start, "/", test_end)
    px_range   <- paste0(first(all_dates), "/", test_end)

    retL1 <- stats::lag(dSymXts[px_range, iName], -1)
    pxSub <- pXts[px_range, iName]; smaPx <- SMA(pxSub, smaLb)

    # Frozen regime + vote share
    regime_frozen <- xts(rep(regime_at_freeze, nrow(retL1)), order.by = index(retL1))
    classSubset <- merge(retL1, regime_frozen, join = "left")[, 2]

    # Vote share frozen at train_end — actual continuous fraction, not binary
    vs_frozen <- xts(rep(vs_at_freeze, nrow(retL1)),
                     order.by = index(retL1))
    vsAligned <- merge(retL1, vs_frozen, join = "left")[, 2]

    # --- SMA ---
    smaPos   <- ifelse(pxSub > smaPx, 1, 0)
    smaNet   <- ifelse((smaPos - stats::lag(smaPos, 1)) != 0,
                       smaPos * retL1 - drag, smaPos * retL1)

    # --- CP (frozen) ---
    cpPos   <- ifelse(classSubset == 1, 1, 0)
    cpNet   <- cpPos * retL1  # no friction — constant position

    # --- SMA+CP (frozen regime, dynamic SMA) ---
    smaCpPos <- ifelse(pxSub > smaPx & classSubset == 1, 1, 0)
    smaCpNet <- ifelse((smaCpPos - stats::lag(smaCpPos, 1)) != 0,
                       smaCpPos * retL1 - drag, smaCpPos * retL1)

    # --- DG (frozen regime, dynamic SMA direction check) ---
    dgPos <- ifelse(classSubset == 0 & pxSub < smaPx, 0, 1)
    dgNet <- ifelse((dgPos - stats::lag(dgPos, 1)) != 0,
                    dgPos * retL1 - drag, dgPos * retL1)

    # --- Vote-share sizing (frozen vote share) ---
    vsVec <- coredata(vsAligned)
    linPos <- vs_linear(vsVec); thrPos <- vs_threshlin(vsVec); sigPos <- vs_sigmoid(vsVec)
    linNet <- ifelse(abs(linPos - stats::lag(linPos, 1)) > 1e-10,
                     linPos * retL1 - abs(linPos - stats::lag(linPos, 1)) * drag,
                     linPos * retL1)
    thrNet <- ifelse(abs(thrPos - stats::lag(thrPos, 1)) > 1e-10,
                     thrPos * retL1 - abs(thrPos - stats::lag(thrPos, 1)) * drag,
                     thrPos * retL1)
    sigNet <- ifelse(abs(sigPos - stats::lag(sigPos, 1)) > 1e-10,
                     sigPos * retL1 - abs(sigPos - stats::lag(sigPos, 1)) * drag,
                     sigPos * retL1)

    # Fused: vote-share sizing only during downtrends, else fully invested
    fusLinPos <- ifelse(pxSub < smaPx, linPos, 1)
    fusLinNet <- ifelse(abs(fusLinPos - stats::lag(fusLinPos, 1)) > 1e-10,
                        fusLinPos * retL1 - abs(fusLinPos - stats::lag(fusLinPos, 1)) * drag,
                        fusLinPos * retL1)
    fusThrPos <- ifelse(pxSub < smaPx, thrPos, 1)
    fusThrNet <- ifelse(abs(fusThrPos - stats::lag(fusThrPos, 1)) > 1e-10,
                        fusThrPos * retL1 - abs(fusThrPos - stats::lag(fusThrPos, 1)) * drag,
                        fusThrPos * retL1)
    fusSigPos <- ifelse(pxSub < smaPx, sigPos, 1)
    fusSigNet <- ifelse(abs(fusSigPos - stats::lag(fusSigPos, 1)) > 1e-10,
                        fusSigPos * retL1 - abs(fusSigPos - stats::lag(fusSigPos, 1)) * drag,
                        fusSigPos * retL1)

    bhPos <- xts(rep(1, nrow(retL1)), order.by = index(retL1))

    all_rets <- na.omit(merge(smaNet, cpNet, smaCpNet, dgNet,
                              linNet, thrNet, sigNet,
                              fusLinNet, fusThrNet, fusSigNet, retL1))
    cn <- c("SMA","CP","SMA_CP","DG","Lin","ThrLin","Sig",
            "FusLin","FusThr","FusSig","B_H")
    names(all_rets) <- cn
    all_pos <- na.omit(merge(smaPos, cpPos, smaCpPos, dgPos,
                             linPos, thrPos, sigPos,
                             fusLinPos, fusThrPos, fusSigPos, bhPos))
    all_pos <- all_pos[index(all_rets)]; names(all_pos) <- cn

    all_rets_test <- all_rets[test_range]
    all_pos_test  <- all_pos[test_range]
    if (nrow(all_rets_test) < 20) next

    # Entry friction: CP and DG have constant position 1.0 when frozen STABLE.
    # Lin/ThrLin/Sig have partial positions — charge drag * actual position.
    if (regime_at_freeze == 1L) {
      for (s in c("CP", "DG")) {
        if (!is.na(all_rets_test[1, s])) all_rets_test[1, s] <- all_rets_test[1, s] - drag
      }
    }
    for (s in c("Lin", "ThrLin", "Sig", "FusLin", "FusThr", "FusSig")) {
      pos <- as.numeric(all_pos_test[1, s])
      if (!is.na(pos) && pos > 0) {
        all_rets_test[1, s] <- all_rets_test[1, s] - drag * pos
      }
    }

    strat_obj <- list(rets = all_rets_test, positions = all_pos_test)
    frozen_strats[[sprintf("%s_frozen_%s_%s", label, test_start, test_end)]] <- strat_obj
    frozen_results <- rbind(frozen_results,
      build_result_row(strat_obj, iName, test_start, test_end))
  }
}

frozen_summary <- if (nrow(frozen_results) > 0) summarise_results(frozen_results) else tibble()
frozen_summary <- add_merged_dd_calmar(frozen_summary, frozen_strats, indices, "_frozen_")
if (nrow(frozen_summary) > 0) {
  print("Frozen annual summary:"); print(frozen_summary)
}

# =========================================================================
# gt tables
# =========================================================================
show_labels <- c("Ret"="Annualized Return", "Sharpe"="Sharpe Ratio",
  "DD"="Max Drawdown", "Calmar"="Calmar Ratio",
  "TimeIn"="Time in Market", "Tvr"="Turnover")
show_suffixes <- names(show_labels)

build_gt_table <- function(df, title, subtitle, df_ref = NULL) {
  if (is.null(df_ref)) df_ref <- df; d <- df_ref
  all_cols <- names(df)
  display_cols <- "Index"
  if ("Windows" %in% all_cols) display_cols <- c(display_cols, "Windows")
  spanner_map <- list()
  for (suf in show_suffixes) {
    cols <- grep(paste0("_", suf, "$"), all_cols, value = TRUE)
    if (length(cols) > 0) {
      display_cols <- c(display_cols, cols)
      spanner_map[[show_labels[suf]]] <- cols
    }
  }
  tbl <- df |> select(all_of(display_cols)) |> gt() |>
    tab_header(title = title, subtitle = subtitle)
  for (nm in names(spanner_map))
    tbl <- tbl |> tab_spanner(label = nm, columns = all_of(spanner_map[[nm]]))
  for (suf in show_suffixes) {
    cols <- grep(paste0("_", suf, "$"), all_cols, value = TRUE)
    if (length(cols) == 0) next
    if (suf %in% c("Ret","DD")) tbl <- tbl |> fmt_percent(columns = all_of(cols), decimals = 1)
    else if (suf == "TimeIn") tbl <- tbl |> fmt_percent(columns = all_of(cols), decimals = 0)
    else tbl <- tbl |> fmt_number(columns = all_of(cols), decimals = 2)
  }
  if ("Windows" %in% all_cols) tbl <- tbl |> fmt_number(columns = Windows, decimals = 0)
  tbl <- tbl |> tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = Index))
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
  tbl
}

webshot_save <- function(html_path, png_path) {
  webshot2::webshot(html_path, png_path,
    selector = "table.gt_table", expand = c(10,10,10,10), vwidth = 4000, vheight = 2000)
}

if (nrow(sliding_summary) > 0) {
  s <- build_gt_table(sliding_summary, "Combined Strategies — Sliding Window",
    "Train 5yr / test 1yr. Mean across ~15 windows.", sliding_summary)
  gtsave(s, sprintf("%s/sliding-metrics.html", reportPath))
  webshot_save(sprintf("%s/sliding-metrics.html", reportPath),
               sprintf("%s/sliding-metrics.png", reportPath))
}
if (nrow(expanding_summary) > 0) {
  e <- build_gt_table(expanding_summary, "Combined Strategies — Expanding Window",
    "2005 → date; consolidated regime.", expanding_summary)
  gtsave(e, sprintf("%s/expanding-metrics.html", reportPath))
  webshot_save(sprintf("%s/expanding-metrics.html", reportPath),
               sprintf("%s/expanding-metrics.png", reportPath))
}
if (nrow(frozen_summary) > 0) {
  f <- build_gt_table(frozen_summary, "Combined Strategies — Frozen Annual",
    "Regime frozen at train_end. Held for entire test year.", frozen_summary)
  gtsave(f, sprintf("%s/frozen-metrics.html", reportPath))
  webshot_save(sprintf("%s/frozen-metrics.html", reportPath),
               sprintf("%s/frozen-metrics.png", reportPath))
}

# Combined table
combined_all <- NULL
for (src in list(
  list(df = sliding_summary,  label = "Sliding (train/test, mean across windows)"),
  list(df = expanding_summary, label = "Expanding (2005 → date)"),
  list(df = frozen_summary,    label = "Frozen Annual (train_end freeze)"))) {
  df <- src$df
  if (is.data.frame(df) && nrow(df) > 0) {
    df <- df %>% mutate(Window = src$label)
    combined_all <- if (is.null(combined_all)) df else bind_rows(combined_all, df)
  }
}
if (!is.null(combined_all) && nrow(combined_all) > 0) {
  all_cols <- names(combined_all)
  display_cols <- c("Window","Index")
  if ("Windows" %in% all_cols) display_cols <- c(display_cols, "Windows")
  spanner_map <- list()
  for (suf in show_suffixes) {
    cols <- grep(paste0("_", suf, "$"), all_cols, value = TRUE)
    if (length(cols) > 0) {
      display_cols <- c(display_cols, cols)
      spanner_map[[show_labels[suf]]] <- cols
    }
  }
  ctbl <- combined_all |> select(all_of(display_cols)) |>
    gt(groupname_col = "Window") |>
    tab_header(title = "Combined Strategies — All Methodologies",
      subtitle = "Binary (SMA/CP/SMA+CP/DG) + vote-share (Lin/ThrLin/Sig) vs B&H")
  for (nm in names(spanner_map))
    ctbl <- ctbl |> tab_spanner(label = nm, columns = all_of(spanner_map[[nm]]))
  for (suf in show_suffixes) {
    cols <- grep(paste0("_", suf, "$"), all_cols, value = TRUE)
    if (length(cols) == 0) next
    if (suf %in% c("Ret","DD")) ctbl <- ctbl |> fmt_percent(columns = all_of(cols), decimals = 1)
    else if (suf == "TimeIn") ctbl <- ctbl |> fmt_percent(columns = all_of(cols), decimals = 0)
    else ctbl <- ctbl |> fmt_number(columns = all_of(cols), decimals = 2)
  }
  if ("Windows" %in% all_cols) ctbl <- ctbl |> fmt_number(columns = Windows, decimals = 0)
  ctbl <- ctbl |> tab_style(cell_text(weight="bold"), locations = cells_column_labels()) |>
    tab_style(cell_text(weight="bold"), locations = cells_row_groups()) |>
    tab_style(cell_text(weight="bold"), locations = cells_body(columns = Index))
  d <- combined_all
  bh_ret <- "B_H_Ret"; bh_sr <- "B_H_Sharpe"; bh_dd <- "B_H_DD"
  for (col in grep("_Ret$", names(d), value = TRUE)) {
    if (col == bh_ret || !bh_ret %in% names(d)) next
    rows <- which(d[[col]] > d[[bh_ret]])
    if (length(rows) > 0) ctbl <- ctbl |> tab_style(
      style = cell_text(weight="bold", color="#1a6b1a"),
      locations = cells_body(columns = all_of(col), rows = rows))
  }
  for (col in grep("_Sharpe$", names(d), value = TRUE)) {
    if (col == bh_sr || !bh_sr %in% names(d)) next
    rows <- which(d[[col]] > d[[bh_sr]])
    if (length(rows) > 0) ctbl <- ctbl |> tab_style(
      style = cell_text(weight="bold", color="#1a6b1a"),
      locations = cells_body(columns = all_of(col), rows = rows))
  }
  for (col in grep("_DD$", names(d), value = TRUE)) {
    if (col == bh_dd || !bh_dd %in% names(d)) next
    rows <- which(d[[col]] > d[[bh_dd]])
    if (length(rows) > 0) ctbl <- ctbl |> tab_style(
      style = cell_text(weight="bold", color="#1a6b1a"),
      locations = cells_body(columns = all_of(col), rows = rows))
  }
  gtsave(ctbl, sprintf("%s/combined-metrics.html", reportPath))
  webshot_save(sprintf("%s/combined-metrics.html", reportPath),
               sprintf("%s/combined-metrics.png", reportPath))
}

# =========================================================================
# Drawdowns + Cumulative charts
# =========================================================================
print("  Drawdown tables + charts...")
for (iName in indices) {
  label <- gsub(" TR$", "", iName)

  # Sliding drawdowns
  idx <- sliding_strats[grepl(paste0("^", label, "_"), names(sliding_strats))]
  if (length(idx) > 0) {
    merged <- do.call(rbind.xts, lapply(idx, `[[`, "rets")); merged <- na.omit(merged)
    if (nrow(merged) >= 20) {
      ddown <- table.Drawdowns(merged)
      if (!is.null(ddown) && nrow(ddown) > 0) {
        as_tibble(ddown, rownames="S") |> gt() |>
          tab_header(title=paste("Drawdowns —",iName,"(sliding)")) |>
          fmt_percent(columns = "Depth", decimals = 1) |>
          fmt_number(columns = c("Length", "To Trough", "Recovery"), decimals = 0) |>
          gtsave(sprintf("%s/%s.sliding.drawdowns.html",reportPath,iName))
        webshot_save(sprintf("%s/%s.sliding.drawdowns.html",reportPath,iName),
                     sprintf("%s/%s.sliding.drawdowns.png",reportPath,iName))
      }
    }
  }
  # Expanding drawdowns
  if (iName %in% names(expanding_strats)) {
    R <- expanding_strats[[iName]]$rets; ddTb <- tibble()
    for (j in 1:ncol(R)) {
      tdd <- table.Drawdowns(R[,j]); tdd_df <- as_tibble(tdd)
      tdd_df$INDEX <- sprintf("%s (%s)", iName, names(R)[j])
      ddTb <- rbind(ddTb, tdd_df)
    }
    ddTb |> gt(groupname_col="INDEX") |>
      tab_header(title="Drawdowns — Expanding Window",
        subtitle=sprintf("%s: %s → %s",iName,format(start(R),"%Y-%m-%d"),format(end(R),"%Y-%m-%d"))) |>
      fmt_percent(columns = "Depth", decimals = 1) |>
      fmt_number(columns = c("Length", "To Trough", "Recovery"), decimals = 0) |>
      sub_missing(columns = everything(), missing_text = "") |> tab_style(cell_text(weight="bold"),cells_row_groups()) |>
      tab_style(cell_text(weight="bold"),cells_column_labels()) |>
      gtsave(sprintf("%s/%s.expanding.drawdowns.html",reportPath,iName))
    webshot_save(sprintf("%s/%s.expanding.drawdowns.html",reportPath,iName),
                 sprintf("%s/%s.expanding.drawdowns.png",reportPath,iName))
  }
  # Frozen drawdowns
  f_idx <- frozen_strats[grepl(paste0("^",label,"_frozen_"), names(frozen_strats))]
  if (length(f_idx) > 0) {
    merged <- do.call(rbind.xts, lapply(f_idx, `[[`, "rets")); merged <- na.omit(merged)
    if (nrow(merged) >= 20) {
      ddown <- table.Drawdowns(merged)
      if (!is.null(ddown) && nrow(ddown) > 0) {
        as_tibble(ddown, rownames="S") |> gt() |>
          tab_header(title=paste("Drawdowns —",iName,"(frozen)")) |>
          fmt_percent(columns = "Depth", decimals = 1) |>
          fmt_number(columns = c("Length", "To Trough", "Recovery"), decimals = 0) |>
          gtsave(sprintf("%s/%s.frozen.drawdowns.html",reportPath,iName))
        webshot_save(sprintf("%s/%s.frozen.drawdowns.html",reportPath,iName),
                     sprintf("%s/%s.frozen.drawdowns.png",reportPath,iName))
      }
    }
  }

  # Cumulative charts
  if (length(idx) > 0) {
    merged <- do.call(rbind.xts, lapply(idx, `[[`, "rets")); merged <- na.omit(merged)
    if (nrow(merged) >= 20) Common.PlotCumReturns(merged, iName,
      sprintf("Sliding: %s → %s", format(start(merged),"%Y-%m-%d"), format(end(merged),"%Y-%m-%d")),
      sprintf("%s/%s.sliding.cumret.png",reportPath,iName), NULL)
  }
  if (iName %in% names(expanding_strats)) {
    R <- expanding_strats[[iName]]$rets
    Common.PlotCumReturns(R, iName,
      sprintf("Expanding: %s → %s", format(start(R),"%Y-%m-%d"), format(end(R),"%Y-%m-%d")),
      sprintf("%s/%s.expanding.cumret.png",reportPath,iName), NULL)
  }
  if (length(f_idx) > 0) {
    merged <- do.call(rbind.xts, lapply(f_idx, `[[`, "rets")); merged <- na.omit(merged)
    if (nrow(merged) >= 20) Common.PlotCumReturns(merged, iName,
      sprintf("Frozen: %s → %s", format(start(merged),"%Y-%m-%d"), format(end(merged),"%Y-%m-%d")),
      sprintf("%s/%s.frozen.cumret.png",reportPath,iName), NULL)
  }
}

print("=== DONE ===")
