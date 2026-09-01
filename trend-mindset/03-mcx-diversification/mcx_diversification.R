# ============================================================================
# 03-mcx-diversification — MCX liquidity screen + commodity diversification
# ============================================================================
# Part of the trend-mindset plan (plan.md item 16 / Stage 3):
#   1. Screen candidate MCX futures (GOLD, SILVER, CRUDEOIL, NATURALGAS, COPPER)
#      for usable history, stale/frozen prices, plausible prices, and a
#      reproducible front/continuous construction. (bhav_com_mcx has NO
#      volume/turnover column, so "liquidity" is proxied by price freshness,
#      observation completeness, and roll continuity — documented in findings.)
#   2. Build a reproducible continuous front-month series per contract using
#      the house "held-contract" roll convention (see bookB-midselect-fut.R):
#      the return earned on day k is the close-to-close return of the front
#      (near-month) contract held on day k, which removes the roll gap by
#      construction. The EMA signal is computed on the back-adjusted price
#      (cumprod of those returns) so expiry jumps do not distort the signal.
#   3. Test the exact book rule: fast EMA smoothing constant 0.30, slow 0.05,
#      long when fast > slow, otherwise flat/cash. Causal: signal known at
#      close t is applied from day t+1. Drag = 25 bps per unit position change.
#   4. Compare equal-weight vs inverse-volatility-balanced commodity sleeves.
#   5. Combine an equity TR sleeve (NIFTY 50 TR + NIFTY MIDCAP SELECT TR under
#      the same EMA rule) with the commodity sleeve at 5/10/20% (10% primary).
#
# All imports at top. No credentials printed. Only data-supported contracts
# are retained; exclusions are recorded in findings.md and screening.csv.
# ============================================================================

suppressPackageStartupMessages({
  library(RODBC)
  library(xts)
  library(zoo)
  library(PerformanceAnalytics)
  library(tidyverse)
})

options("scipen" = 100)
options(stringsAsFactors = FALSE)

source("/mnt/hollandC/StockViz/R/config.r")   # connection vars only; never printed

OUT_DIR <- "/mnt/data/blog/trend-mindset/03-mcx-diversification"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
setwd(OUT_DIR)

DRAG <- 0.0025           # 25 bps per unit position/weight change
FAST_A <- 0.30           # book's fast EMA smoothing constant
SLOW_A <- 0.05           # book's slow EMA smoothing constant
PRE_END   <- as.Date("2019-12-31")
POST_START <- as.Date("2020-05-01")
CANDIDATES <- c("GOLD", "SILVER", "CRUDEOIL", "NATURALGAS", "COPPER")
EQ_INDICES <- c("NIFTY 50 TR", "NIFTY MIDCAP SELECT TR")
COMMOD_ALLO <- c(0.05, 0.10, 0.20)   # primary = 0.10 (plan item 16)

cat(sprintf("=== 03-mcx-diversification build: %s ===\n", format(Sys.time())))

## ── 0. DB connection ────────────────────────────────────────────────────────
lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

## ── 1. Helpers ──────────────────────────────────────────────────────────────

# EMA with an explicit smoothing constant (alpha), not a lookback.
# EMA_t = alpha * P_t + (1-alpha) * EMA_{t-1}, seeded with the first price.
ema_alpha <- function(x, alpha) {
  x <- as.numeric(x)
  y <- rep(NA_real_, length(x))
  first <- which(is.finite(x))[1]
  if (is.na(first)) return(y)
  y[first] <- x[first]
  if (first < length(x)) {
    for (i in (first + 1):length(x)) {
      y[i] <- alpha * x[i] + (1 - alpha) * y[i - 1]
    }
  }
  y
}

# Annualized-realized-vol-based inverse weight for a return vector (lagged 1d).
inverse_vol_weights <- function(ret_vec, window = 20L) {
  v <- as.numeric(ret_vec)
  sd_run <- runSD(v, n = window)          # causal: uses data up to day k
  sd_lag <- c(NA_real_, head(sd_run, -1)) # weight known before day k
  w <- 1 / sd_lag
  w[!is.finite(w) | w < 0] <- NA_real_
  w
}

# Metrics for a numeric daily-return vector.
compute_metrics <- function(rets, system, window) {
  r <- as.numeric(rets); r <- r[is.finite(r)]
  if (length(r) < 30) {
    return(data.frame(system = system, window = window, n = length(r),
                      cagr = NA, vol = NA, sharpe = NA, sortino = NA,
                      maxdd = NA, longest_dd_days = NA))
  }
  eq <- cumprod(1 + r)
  peak <- cummax(eq)
  dd <- eq / peak - 1
  under <- dd < 0
  rl <- rle(under)
  longest_dd <- if (any(rl$values)) max(rl$lengths[rl$values]) else 0
  data.frame(
    system = system, window = window, n = length(r),
    cagr   = as.numeric(Return.annualized(r)),
    vol    = sd(r) * sqrt(252),
    sharpe = as.numeric(SharpeRatio.annualized(r)),
    sortino = as.numeric(SortinoRatio(r, MAR = 0)),
    maxdd  = as.numeric(maxDrawdown(r)),
    longest_dd_days = longest_dd
  )
}

slice_window <- function(dates, window) {
  if (window == "full") return(rep(TRUE, length(dates)))
  if (window == "pre")  return(dates <= PRE_END)
  if (window == "post") return(dates >= POST_START)
  stop("bad window")
}

## ── 2. Load MCX futures and build continuous front series ──────────────────
mcx <- list()          # per-contract: dates, front price, held return, roll info
screen_rows <- list()  # screening output
roll_rows <- list()    # roll stats output

for (cm in CANDIDATES) {
  df <- sqlQuery(lcon, sprintf(
    "select expiry, expiry_series, time_stamp, px_close from BHAV_COM_MCX
     where contract='%s' and OTYPE in ('FUTCOM','XX') order by time_stamp", cm))
  df$time_stamp <- as.Date(df$time_stamp)
  df$expiry     <- as.Date(df$expiry)
  df <- df[is.finite(df$px_close) & df$px_close > 0, ]

  dates <- sort(unique(df$time_stamp))

  # front = min-expiry contract per day (verified == expiry_series 0)
  front_exp <- sapply(dates, function(d) {
    sub <- df[df$time_stamp == d, ]
    sub$expiry[which.min(sub$expiry)]
  })
  names(front_exp) <- as.character(dates)

  # held-contract (roll-adjusted) front return: return of the front contract
  # held on day k, close-to-close. Removes roll gaps by construction.
  px_lookup <- split(df$px_close, paste(df$expiry, df$time_stamp, sep = "|"))
  px_lookup <- setNames(
    df$px_close, paste(df$expiry, df$time_stamp, sep = "|"))

  held_ret <- rep(NA_real_, length(dates))
  for (k in 2:length(dates)) {
    held <- as.character(front_exp[k])
    k1   <- as.character(front_exp[k])          # same contract on day k-1
    pk   <- px_lookup[paste(held, dates[k], sep = "|")]
    pkm1 <- px_lookup[paste(held, dates[k - 1], sep = "|")]
    if (length(pk) && length(pkm1) && is.finite(pk) && is.finite(pkm1) && pkm1 > 0) {
      held_ret[k] <- pk / pkm1 - 1
    }
  }
  held_ret[!is.finite(held_ret)] <- 0

  # back-adjusted front price (cumprod of roll-adjusted returns) for the signal
  back_adj <- cumprod(1 + held_ret)

  # roll dates = days the front contract's expiry changed
  roll_flag <- c(FALSE, front_exp[-1] != front_exp[-length(front_exp)])

  # roll spread (contango/backwardation): new-front vs old-front on the last
  # day both trade (day k-1 of the roll).
  roll_spread <- rep(NA_real_, length(dates))
  for (k in which(roll_flag)) {
    old <- as.character(front_exp[k - 1]); new <- as.character(front_exp[k])
    po <- px_lookup[paste(old, dates[k - 1], sep = "|")]
    pn <- px_lookup[paste(new, dates[k - 1], sep = "|")]
    if (length(po) && length(pn) && is.finite(po) && is.finite(pn) && po > 0) {
      roll_spread[k] <- pn / po - 1
    }
  }

  mcx[[cm]] <- list(dates = dates, front_exp = front_exp,
                    held_ret = held_ret, back_adj = back_adj,
                    roll_flag = roll_flag, roll_spread = roll_spread)

  # ---- screening metrics ----
  dr <- held_ret
  dr[!is.finite(dr)] <- NA_real_
  zero_run <- rle(dr == 0 & !is.na(dr))
  max_stale <- max(c(0, zero_run$lengths[zero_run$values]), na.rm = TRUE)
  n_zero <- sum(dr == 0, na.rm = TRUE)
  big_move <- sum(abs(dr) > 0.15, na.rm = TRUE)
  max_abs_ret <- max(abs(dr), na.rm = TRUE)

  # duplicate / missing front-contract dates
  per_day <- df %>% group_by(time_stamp) %>%
    summarise(n_front = sum(expiry_series == 0), .groups = "drop")
  dup_front_days <- sum(per_day$n_front > 1)
  miss_front_days <- sum(per_day$n_front == 0)

  n_pre  <- sum(dates <= PRE_END)
  n_post <- sum(dates >= POST_START)

  screen_rows[[length(screen_rows) + 1]] <- data.frame(
    contract = cm,
    first_date = min(dates), last_date = max(dates),
    n_days_total = length(dates),
    n_days_pre = n_pre, n_days_post = n_post,
    n_zero_ret_days = n_zero, max_stale_run_days = max_stale,
    n_bigmove_gt15pct = big_move, max_abs_ret = round(max_abs_ret, 5),
    dup_front_days = dup_front_days, miss_front_days = miss_front_days,
    n_rolls = sum(roll_flag),
    avg_roll_spread = round(mean(roll_spread[is.finite(roll_spread)]), 5),
    pass = (n_pre >= 500 & n_post >= 500 & max_stale <= 5 &
              max_abs_ret < 200 & miss_front_days == 0 & dup_front_days == 0),
    note = ifelse(cm == "CRUDEOIL",
      "2020-04-20 px_close=1 (WTI-negative shock, suspected circuit/placeholder print); held-contract roll removes spurious 1323x jump",
      "")
  )

  # ---- roll stats (per roll date) ----
  rd <- dates[roll_flag]
  if (length(rd)) {
    roll_rows[[length(roll_rows) + 1]] <- data.frame(
      contract = cm, roll_date = rd,
      old_expiry = front_exp[match(rd, dates) - 1],
      new_expiry = front_exp[match(rd, dates)],
      roll_spread = roll_spread[match(rd, dates)])
  }
}

screen_df <- do.call(rbind, screen_rows); row.names(screen_df) <- NULL
roll_df   <- do.call(rbind, roll_rows);   row.names(roll_df) <- NULL
cat("\n=== SCREENING ===\n"); print(screen_df, row.names = FALSE)

## ── 3. Exact EMA trend rule per commodity ───────────────────────────────────
trend <- list()   # per commodity: dates, gross, net25, position, trades, tim
trend_metrics <- list()
for (cm in names(mcx)) {
  M <- mcx[[cm]]
  f <- ema_alpha(M$back_adj, FAST_A)
  s <- ema_alpha(M$back_adj, SLOW_A)
  signal <- as.integer(f > s); signal[is.na(signal)] <- 0
  pos <- c(0, head(signal, -1))            # position during day k = signal_{k-1}
  r <- M$held_ret
  gross <- pos * r
  flip  <- abs(c(0, diff(pos)))
  net   <- gross - DRAG * flip

  trend[[cm]] <- data.frame(date = M$dates, front_ret = r,
                            signal = signal, position = pos,
                            gross = gross, net25 = net, flip = flip)

  tim <- mean(pos > 0)
  n_trades <- sum(flip > 0)
  turnover <- sum(flip)

  for (w in c("full", "pre", "post")) {
    sel <- slice_window(M$dates, w)
    gm <- compute_metrics(gross[sel], cm, w);  gm$system <- paste0(cm, "_gross")
    nm <- compute_metrics(net[sel],   cm, w);  nm$system <- paste0(cm, "_net25")
    trend_metrics[[length(trend_metrics) + 1]] <- gm
    trend_metrics[[length(trend_metrics) + 1]] <- nm
  }
  # time-in-market / turnover (full sample)
  trend_metrics[[length(trend_metrics) + 1]] <- data.frame(
    system = paste0(cm, "_net25"), window = "full",
    n = length(M$dates), cagr = NA, vol = NA, sharpe = NA, sortino = NA,
    maxdd = NA, longest_dd_days = NA, time_in_mkt = tim,
    n_trades = n_trades, turnover = turnover)
}

trend_metrics_df <- do.call(rbind, trend_metrics); row.names(trend_metrics_df) <- NULL

# drag sensitivity (0 / 10 / 25 / 50 bps) for each commodity — full + post
drag_sens <- list()
for (cm in names(mcx)) {
  M <- mcx[[cm]]
  t <- trend[[cm]]
  for (d in c(0, 0.0010, 0.0025, 0.0050)) {
    net_d <- t$gross - d * t$flip
    for (w in c("full", "post")) {
      sel <- slice_window(M$dates, w)
      mm <- compute_metrics(net_d[sel], cm, w)
      drag_sens[[length(drag_sens) + 1]] <- data.frame(
        contract = cm, drag_bps = d * 1e4, window = w,
        cagr = mm$cagr, sharpe = mm$sharpe, maxdd = mm$maxdd)
    }
  }
}
drag_sens_df <- do.call(rbind, drag_sens); row.names(drag_sens_df) <- NULL

## ── 4. Commodity sleeves (equal-weight and inverse-vol-balanced) ────────────
# Align commodities on a common date grid (intersection).
common_dates <- Reduce(intersect, lapply(mcx, function(M) M$dates))
idx <- function(M) match(common_dates, M$dates)

N <- length(mcx)
# matrix of gross trend returns and positions (rows = common dates)
gross_mat <- sapply(names(mcx), function(cm) trend[[cm]]$gross[idx(mcx[[cm]])])
pos_mat   <- sapply(names(mcx), function(cm) trend[[cm]]$position[idx(mcx[[cm]])])
held_mat  <- sapply(names(mcx), function(cm) mcx[[cm]]$held_ret[idx(mcx[[cm]])])

# equal weight
ew_w <- matrix(1 / N, nrow = nrow(pos_mat), ncol = N)
# inverse-vol weights, monthly rebalanced
iv_raw <- sapply(names(mcx), function(cm) inverse_vol_weights(held_mat[, cm], 20))
month_id <- format(common_dates, "%Y-%m")
iv_w <- matrix(NA_real_, nrow = nrow(pos_mat), ncol = N)
for (mi in unique(month_id)) {
  r <- which(month_id == mi)
  w <- iv_raw[r[1], ]
  w[!is.finite(w)] <- 0
  if (sum(w) > 0) w <- w / sum(w) else w <- rep(1 / N, N)
  iv_w[r, ] <- rep(w, each = length(r))
}

sleeve_ret <- function(W) {
  pos <- W * pos_mat
  gross <- rowSums(pos * held_mat)
  turn  <- rowSums(abs(pos - rbind(0, pos[-nrow(pos), , drop = FALSE])))
  net   <- gross - DRAG * turn
  list(gross = gross, net = net, turn = turn)
}
ew <- sleeve_ret(ew_w)
vb <- sleeve_ret(iv_w)

sleeve_metrics <- list()
for (w in c("full", "pre", "post")) {
  sel <- slice_window(common_dates, w)
  sleeve_metrics[[length(sleeve_metrics) + 1]] <-
    compute_metrics(ew$net[sel], "COMMOD_EW_net25", w)
  sleeve_metrics[[length(sleeve_metrics) + 1]] <-
    compute_metrics(vb$net[sel], "COMMOD_VB_net25", w)
  sleeve_metrics[[length(sleeve_metrics) + 1]] <-
    compute_metrics(ew$gross[sel], "COMMOD_EW_gross", w)
  sleeve_metrics[[length(sleeve_metrics) + 1]] <-
    compute_metrics(vb$gross[sel], "COMMOD_VB_gross", w)
}
sleeve_metrics_df <- do.call(rbind, sleeve_metrics); row.names(sleeve_metrics_df) <- NULL

## ── 5. Equity TR sleeve (same EMA rule on NIFTY 50 TR + MIDCAP SELECT TR) ───
eq_trend <- list()
for (inm in EQ_INDICES) {
  idf <- sqlQuery(lcon, sprintf(
    "select time_stamp, px_close from bhav_index where index_name='%s' order by time_stamp", inm))
  idf$time_stamp <- as.Date(idf$time_stamp)
  px <- idf$px_close[idf$px_close > 0]
  d  <- idf$time_stamp[idf$px_close > 0]
  r  <- c(0, diff(px) / head(px, -1))
  f  <- ema_alpha(px, FAST_A); s <- ema_alpha(px, SLOW_A)
  signal <- as.integer(f > s); signal[is.na(signal)] <- 0
  pos <- c(0, head(signal, -1))
  gross <- pos * r
  flip <- abs(c(0, diff(pos)))
  net  <- gross - DRAG * flip
  eq_trend[[inm]] <- data.frame(date = d, ret = r, gross = gross, net = net,
                                pos = pos, flip = flip)
}

# align equity on common_dates (carry via left-join on date)
eq_aligned <- lapply(eq_trend, function(t) {
  m <- match(common_dates, t$date)
  list(gross = ifelse(is.na(m), 0, t$gross[m]),
       net   = ifelse(is.na(m), 0, t$net[m]),
       pos   = ifelse(is.na(m), 0, t$pos[m]),
       flip  = ifelse(is.na(m), 0, t$flip[m]))
})
# equity sleeve = equal-weight average of the two equity trend legs (net)
eq_sleeve_net <- (eq_aligned[[1]]$net + eq_aligned[[2]]$net) / 2
eq_sleeve_gross <- (eq_aligned[[1]]$gross + eq_aligned[[2]]$gross) / 2

## ── 6. Combined equity/commodity portfolios ─────────────────────────────────
# commodity sleeve net (primary = equal-weight; also vol-balanced)
combine <- function(comm_net, allo, rebalance = FALSE) {
  wc <- allo
  target_eq <- 1 - wc
  if (!rebalance) {
    # no overlay: weights drift with returns
    net <- target_eq * eq_sleeve_net + wc * comm_net
    return(net)
  }
  # monthly rebalanced: reset to target weights at month start, charge 25bps
  # on the drift correction.
  eq_eq  <- cumprod(1 + eq_sleeve_net)
  cm_eq  <- cumprod(1 + comm_net)
  tot    <- eq_eq + cm_eq          # both started at 1 unit
  w_eq   <- eq_eq / tot
  w_cm   <- cm_eq / tot
  drift  <- (abs(w_eq - target_eq) + abs(w_cm - wc)) / 2   # one-way weight change
  rebal  <- c(0, head(as.integer(month_id[-1] != month_id[-length(month_id)]), -1))
  # rebalance cost charged at the first day of each new month (drift so far)
  cost <- rep(0, length(common_dates))
  cost[rebal == 1] <- DRAG * drift[rebal == 1]
  net <- target_eq * eq_sleeve_net + wc * comm_net - cost
  net
}

combined_metrics <- list()
for (comm_name in c("EW", "VB")) {
  comm_net <- if (comm_name == "EW") ew$net else vb$net
  for (a in COMMOD_ALLO) {
    for (rb in c(FALSE, TRUE)) {
      net <- combine(comm_net, a, rebalance = rb)
      lbl <- sprintf("EQ+%s_%d%c%s", comm_name, round(a * 100),
                     "%", if (rb) "_rebal" else "")
      for (w in c("full", "pre", "post")) {
        sel <- slice_window(common_dates, w)
        combined_metrics[[length(combined_metrics) + 1]] <-
          compute_metrics(net[sel], lbl, w)
      }
      if (rb) {
        assign(sprintf("comb_%s_%d_rebal", comm_name, round(a * 100)), net)
      } else {
        assign(sprintf("comb_%s_%d", comm_name, round(a * 100)), net)
      }
    }
  }
}
# equity-only benchmark
for (w in c("full", "pre", "post")) {
  sel <- slice_window(common_dates, w)
  combined_metrics[[length(combined_metrics) + 1]] <-
    compute_metrics(eq_sleeve_net[sel], "EQ_ONLY_net25", w)
}
combined_metrics_df <- do.call(rbind, combined_metrics); row.names(combined_metrics_df) <- NULL

odbcClose(lcon)

## ── 7. Write outputs ────────────────────────────────────────────────────────
write.csv(screen_df, file.path(OUT_DIR, "screening.csv"), row.names = FALSE)
write.csv(roll_df,   file.path(OUT_DIR, "roll_stats.csv"), row.names = FALSE)

# per-commodity daily outputs
daily_comm <- lapply(names(trend), function(cm) {
  t <- trend[[cm]]; t$contract <- cm; t[, c("contract", setdiff(names(t), "contract"))]
})
daily_comm_df <- do.call(rbind, daily_comm)
write.csv(daily_comm_df, file.path(OUT_DIR, "daily_commodity_returns.csv"),
          row.names = FALSE)

# continuous front series (prices + held returns) for reproducibility
front_df <- do.call(rbind, lapply(names(mcx), function(cm) {
  M <- mcx[[cm]]
  data.frame(contract = cm, date = M$dates, front_expiry = M$front_exp,
             back_adj_price = M$back_adj, front_ret = M$held_ret,
             roll_flag = M$roll_flag, roll_spread = M$roll_spread)
}))
write.csv(front_df, file.path(OUT_DIR, "daily_continuous_front.csv"),
          row.names = FALSE)

# sleeves
sleeve_df <- data.frame(date = common_dates,
                        COMMOD_EW_net25 = ew$net, COMMOD_EW_gross = ew$gross,
                        COMMOD_VB_net25 = vb$net, COMMOD_VB_gross = vb$gross,
                        EQ_ONLY_net25 = eq_sleeve_net)
write.csv(sleeve_df, file.path(OUT_DIR, "daily_sleeves.csv"), row.names = FALSE)

# combined portfolios
comb_df <- data.frame(date = common_dates, EQ_ONLY_net25 = eq_sleeve_net)
for (cn in c("EW", "VB")) {
  for (a in COMMOD_ALLO) {
    pct <- round(a * 100)
    v1 <- get(sprintf("comb_%s_%d", cn, pct))
    v2 <- get(sprintf("comb_%s_%d_rebal", cn, pct))
    comb_df[[sprintf("EQ+%s_%d", cn, pct)]] <- v1
    comb_df[[sprintf("EQ+%s_%d_rebal", cn, pct)]] <- v2
  }
}
write.csv(comb_df, file.path(OUT_DIR, "daily_combined.csv"), row.names = FALSE)

# consolidated metrics
all_metrics <- rbind(trend_metrics_df, sleeve_metrics_df, combined_metrics_df)
write.csv(all_metrics, file.path(OUT_DIR, "metrics.csv"), row.names = FALSE)
write.csv(drag_sens_df, file.path(OUT_DIR, "drag_sensitivity.csv"), row.names = FALSE)

cat("\n=== SLEEVE & COMBINED METRICS (net, 25bps) ===\n")
print(all_metrics[all_metrics$window == "full" &
                  grepl("COMMOD_|EQ_ONLY|EQ\\+", all_metrics$system), ], row.names = FALSE)

cat("\n=== DRAG SENSITIVITY (post window) ===\n")
print(drag_sens_df[drag_sens_df$window == "post", ], row.names = FALSE)

cat("\nWrote outputs to", OUT_DIR, "\n")
cat("Build complete:", format(Sys.time()), "\n")
