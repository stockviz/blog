# VAA (Vigilant Asset Allocation) Backtest
# Keller & Keuning (2017) — Breadth Momentum and Vigilant Asset Allocation
# Universe: VAA-G12 (12 risky assets + 3-asset cash universe)
# Data: bhav_eq_td on StockVizUs2

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('gt')
library('xts')
library('zoo')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")
pdf(NULL)

# ═══════════════════════════════════════════════════════════════════════════════
# parameters
# ═══════════════════════════════════════════════════════════════════════════════

RISKY_TICKERS <- c("SPY", "IWM", "QQQ", "VGK", "EWJ", "VWO",
                   "VNQ", "GSG", "GLD", "TLT", "LQD", "HYG")

CASH_TICKERS  <- c("SHY", "IEF", "LQD")  # LQD in both

ALL_TICKERS   <- unique(c(RISKY_TICKERS, CASH_TICKERS))
N_RISKY       <- length(RISKY_TICKERS)

DRAG           <- 0.001  # 0.10% one-way transaction cost

# ═══════════════════════════════════════════════════════════════════════════════
# connect to StockVizUs2
# ═══════════════════════════════════════════════════════════════════════════════

cat("connecting to StockVizUs2...\n")
lconUS2 <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockVizUs2", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

# ═══════════════════════════════════════════════════════════════════════════════
# load daily prices for all tickers
# ═══════════════════════════════════════════════════════════════════════════════

cat("loading daily prices...\n")
daily_prices <- list()
daily_rets   <- list()

for (sym in ALL_TICKERS) {
  pDf <- sqlQuery(lconUS2, sprintf("
    select C, TIME_STAMP
    from bhav_eq_td
    where SYMBOL = '%s'
    order by TIME_STAMP", sym))

  if (nrow(pDf) < 260) {
    cat(sprintf("  %s: too few rows (%d), skipping\n", sym, nrow(pDf)))
    next
  }
  daily_prices[[sym]] <- xts(pDf$C, pDf$TIME_STAMP)
  daily_rets[[sym]]   <- dailyReturn(daily_prices[[sym]])
  cat(sprintf("  %s: %d rows, %s → %s\n", sym, nrow(pDf),
              as.character(as.Date(first(index(daily_prices[[sym]])))),
              as.character(as.Date(last(index(daily_prices[[sym]]))))))
}

odbcClose(lconUS2)

# ═══════════════════════════════════════════════════════════════════════════════
# determine common date range and month-end dates
# ═══════════════════════════════════════════════════════════════════════════════

cat("\ndetermining common date range...\n")

first_dates <- sapply(ALL_TICKERS, function(sym) {
  if (sym %in% names(daily_prices)) first(index(daily_prices[[sym]])) else NA
})
last_dates <- sapply(ALL_TICKERS, function(sym) {
  if (sym %in% names(daily_prices)) last(index(daily_prices[[sym]])) else NA
})

common_first <- max(first_dates, na.rm = TRUE)
common_last  <- min(last_dates, na.rm = TRUE)

cat(sprintf("  common first: %s\n", as.character(as.Date(common_first))))
cat(sprintf("  common last:  %s\n", as.character(as.Date(common_last))))

# Binding ticker
binding_idx <- which.max(first_dates)
cat(sprintf("  binding ticker: %s (first date: %s)\n",
            names(first_dates)[binding_idx],
            as.character(as.Date(first_dates[binding_idx]))))

# ── build monthly end-of-month price xts ─────────────────────────────────────

cat("building monthly end-of-month prices...\n")
monthly_prices <- list()

for (sym in ALL_TICKERS) {
  pxts  <- daily_prices[[sym]]
  mon   <- to.monthly(pxts, OHLC = FALSE)
  index(mon) <- as.Date(index(mon))
  monthly_prices[[sym]] <- mon
}

# Merge into single xts
mon_merged <- do.call(merge.xts, monthly_prices[ALL_TICKERS])
names(mon_merged) <- ALL_TICKERS
mon_merged <- na.omit(mon_merged)

# ── add 12-month lookback for momentum ───────────────────────────────────────

# We need 12 trailing months for the 13612W filter
# Find the first month where all tickers have 12 prior monthly closes
first_momentum_date <- index(mon_merged)[13]  # row 13 = 12 months of history
cat(sprintf("  first usable momentum date: %s\n",
            as.character(as.Date(first_momentum_date))))

# Backtest start: first date we can form portfolio (month after signal)
# The signal at month-end t forms the portfolio for month t+1
signal_dates <- index(mon_merged)[13:(nrow(mon_merged) - 1)]
trade_dates  <- index(mon_merged)[14:nrow(mon_merged)]

cat(sprintf("  first trade date: %s\n", as.character(as.Date(trade_dates[1]))))
cat(sprintf("  last trade date:  %s\n",
            as.character(as.Date(last(trade_dates)))))

# ═══════════════════════════════════════════════════════════════════════════════
# 13612W momentum filter
# score = 12*(p0/p1) + 4*(p0/p3) + 2*(p0/p6) + 1*(p0/p12) - 19
# ═══════════════════════════════════════════════════════════════════════════════

cat("\ncomputing 13612W momentum...\n")

compute_13612w <- function(prices_xts, sym) {
  # prices_xts: monthly end-of-month prices
  px <- coredata(prices_xts)
  n  <- nrow(prices_xts)

  mom <- rep(NA_real_, n)
  for (i in 13:n) {
    p0  <- px[i]
    p1  <- px[i - 1]
    p3  <- px[i - 3]
    p6  <- px[i - 6]
    p12 <- px[i - 12]

    if (is.na(p1) || is.na(p3) || is.na(p6) || is.na(p12) || p1 == 0 || p3 == 0 || p6 == 0 || p12 == 0) next

    mom[i] <- 12 * (p0 / p1 - 1) + 4 * (p0 / p3 - 1) + 2 * (p0 / p6 - 1) + 1 * (p0 / p12 - 1)
  }
  xts(mom, order.by = index(prices_xts))
}

momentum_xts <- do.call(merge.xts, lapply(ALL_TICKERS, function(sym) {
  compute_13612w(monthly_prices[[sym]], sym)
}))
names(momentum_xts) <- ALL_TICKERS
momentum_xts <- na.omit(momentum_xts)

cat(sprintf("  13612W computed: %d rows, %d cols\n", nrow(momentum_xts), ncol(momentum_xts)))

# ═══════════════════════════════════════════════════════════════════════════════
# backtest function
# ═══════════════════════════════════════════════════════════════════════════════

run_vaa_backtest <- function(mom_xts, price_xts, T_val, B_val, txn_cost = DRAG) {
  # mom_xts: 13612W scores for ALL tickers (risky + cash)
  # price_xts: monthly EoM prices for ALL tickers
  # T_val: Top T risky assets to hold
  # B_val: breadth threshold for crash protection

  sig_dates <- index(mom_xts)
  n_months  <- length(sig_dates)

  # ── compute monthly returns from prices ──
  mon_rets <- price_xts / stats::lag(price_xts, 1) - 1
  mon_rets <- mon_rets[index(mon_rets) %in% sig_dates, ]
  mon_rets <- mon_rets[-1, ]  # first row has no prior price
  trd_dates <- index(mon_rets)

  n_trd <- length(trd_dates)

  # ── initialize ──
  vaa_rets      <- rep(NA_real_, n_trd)
  weights_list  <- vector("list", n_trd)
  cash_fraction <- rep(NA_real_, n_trd)
  turnover_vec  <- rep(NA_real_, n_trd)
  n_bad_vec     <- rep(NA_integer_, n_trd)

  prev_weights <- rep(0, N_RISKY + 1)  # risky + cash asset index
  names(prev_weights) <- c(RISKY_TICKERS, "CASH")

  for (t_idx in seq_len(n_trd)) {
    trd_date <- trd_dates[t_idx]
    sig_date <- sig_dates[t_idx]  # signal from prior month-end

    # ── get momentum scores at signal date ──
    mom_row <- mom_xts[as.character(sig_date), ]

    # ── relative momentum: rank risky assets ──
    risky_mom <- as.numeric(mom_row[, RISKY_TICKERS])
    names(risky_mom) <- RISKY_TICKERS
    risky_mom <- risky_mom[!is.na(risky_mom)]

    if (length(risky_mom) < T_val) next

    ranked <- sort(risky_mom, decreasing = TRUE)

    # ── breadth: count "bad" assets (13612W <= 0) ──
    n_bad <- sum(risky_mom <= 0, na.rm = TRUE)
    n_bad_vec[t_idx] <- n_bad

    # ── cash fraction with Easy Trading rounding ──
    if (n_bad >= B_val) {
      cf_raw <- 1
    } else {
      cf_raw <- n_bad / B_val
    }
    # ET rounding: floor(n_bad * T / B) / T, capped at 1
    cf_et <- floor(n_bad * T_val / B_val) / T_val
    cf_et <- min(cf_et, 1)
    cf_et <- max(cf_et, 0)

    cash_fraction[t_idx] <- cf_et

    # ── Top T selection ──
    top_t <- names(ranked)[1:T_val]

    # ── replace worst assets with cash per ET rounding ──
    n_cash_slots <- round(cf_et * T_val)

    risky_holdings <- top_t
    if (n_cash_slots > 0) {
      # Remove the worst-ranked assets (from end of top_t)
      risky_holdings <- top_t[1:(T_val - n_cash_slots)]
      if (length(risky_holdings) == 0) risky_holdings <- character(0)
    }

    # ── cash universe: pick best-ranked cash asset ──
    cash_mom <- as.numeric(mom_row[, CASH_TICKERS])
    names(cash_mom) <- CASH_TICKERS
    cash_mom <- cash_mom[!is.na(cash_mom)]

    best_cash <- names(which.max(cash_mom))[1]  # no absolute filter on cash

    # ── build weights ──
    weights <- rep(0, N_RISKY + 1)
    names(weights) <- c(RISKY_TICKERS, "CASH")

    if (length(risky_holdings) > 0) {
      w_per <- 1 / T_val
      weights[risky_holdings] <- w_per
    }

    if (n_cash_slots > 0) {
      weights["CASH"] <- n_cash_slots / T_val
    }

    weights_list[[t_idx]] <- c(weights, cash_asset = best_cash)

    # ── portfolio return for this month ──
    # risky asset returns
    rets_row <- as.numeric(mon_rets[as.character(trd_date), RISKY_TICKERS])
    names(rets_row) <- RISKY_TICKERS

    # cash asset return
    cash_ret <- as.numeric(mon_rets[as.character(trd_date), best_cash])

    port_ret <- 0
    if (length(risky_holdings) > 0) {
      port_ret <- port_ret + sum(weights[risky_holdings] *
                                 rets_row[risky_holdings], na.rm = TRUE)
    }
    port_ret <- port_ret + weights["CASH"] * cash_ret

    # ── transaction cost ──
    curr_weights <- c(
      sapply(RISKY_TICKERS, function(s) if (s %in% risky_holdings) 1/T_val else 0),
      CASH = n_cash_slots / T_val
    )

    # turnover = sum(|new - old|) / 2
    to <- sum(abs(curr_weights - prev_weights)) / 2
    turnover_vec[t_idx] <- to

    port_ret <- port_ret - to * txn_cost

    vaa_rets[t_idx] <- port_ret
    prev_weights <- curr_weights
  }

  vaa_xts <- xts(vaa_rets, order.by = trd_dates)
  vaa_xts <- vaa_xts[!is.na(coredata(vaa_xts)), ]

  list(
    returns       = vaa_xts,
    cash_fraction = cash_fraction[!is.na(vaa_rets)],
    turnover      = turnover_vec[!is.na(vaa_rets)],
    n_bad         = n_bad_vec[!is.na(vaa_rets)],
    weights       = weights_list[!is.na(vaa_rets)]
  )
}

# ═══════════════════════════════════════════════════════════════════════════════
# performance metrics
# ═══════════════════════════════════════════════════════════════════════════════

compute_metrics <- function(ret_xts, ewc_rets = NULL) {
  if (NROW(ret_xts) < 3) return(NULL)

  r <- Return.annualized(ret_xts)[1, 1]
  v <- StdDev.annualized(ret_xts)[1, 1]
  d <- maxDrawdown(ret_xts)

  # Sharpe: excess over EWC return (paper uses EWC as risk-free proxy)
  if (!is.null(ewc_rets)) {
    ewc_aligned <- ewc_rets[index(ret_xts)]
    excess <- ret_xts - ewc_aligned
    sharpe <- SharpeRatio.annualized(excess)[1, 1]
  } else {
    sharpe <- SharpeRatio.annualized(ret_xts)[1, 1]
  }

  mar <- r / d

  # RAD
  if (r >= 0 && d <= 0.50) {
    rad <- r * (1 - d / (1 - d))
  } else {
    rad <- 0
  }

  list(
    R      = r,
    V      = v,
    D      = d,
    Sharpe = sharpe,
    MAR    = mar,
    RAD    = rad
  )
}

# ═══════════════════════════════════════════════════════════════════════════════
# compute benchmark returns
# ═══════════════════════════════════════════════════════════════════════════════

cat("\ncomputing benchmark returns...\n")

# Monthly returns from the merged monthly price xts
all_dates <- index(mon_merged)
mon_rets_all <- mon_merged / stats::lag(mon_merged, 1) - 1
mon_rets_all <- mon_rets_all[-1, ]  # drop first row (NA)

# ── EW: equal-weight risky, monthly rebalanced ──
ew_weights <- rep(1 / N_RISKY, N_RISKY)
names(ew_weights) <- RISKY_TICKERS
ew_rets <- xts(rowSums(coredata(mon_rets_all[, RISKY_TICKERS]) *
                       matrix(ew_weights, nrow = nrow(mon_rets_all),
                              ncol = N_RISKY, byrow = TRUE), na.rm = TRUE),
               order.by = index(mon_rets_all))
names(ew_rets) <- "EW"

# ── EWC: equal-weight cash, monthly rebalanced ──
ewc_weights <- rep(1 / length(CASH_TICKERS), length(CASH_TICKERS))
names(ewc_weights) <- CASH_TICKERS
ewc_rets <- xts(rowSums(coredata(mon_rets_all[, CASH_TICKERS]) *
                         matrix(ewc_weights, nrow = nrow(mon_rets_all),
                                ncol = length(CASH_TICKERS), byrow = TRUE), na.rm = TRUE),
                order.by = index(mon_rets_all))
names(ewc_rets) <- "EWC"

# ── 60/40: 60% SPY / 40% IEF, monthly rebalanced ──
sixty40_rets <- xts(0.6 * coredata(mon_rets_all[, "SPY"]) +
                     0.4 * coredata(mon_rets_all[, "IEF"]),
                    order.by = index(mon_rets_all))
names(sixty40_rets) <- "60/40"

# ── SPY: buy & hold ──
spy_rets <- mon_rets_all[, "SPY"]
names(spy_rets) <- "SPY"

# ── Dual Momentum: replace bad assets within Top T by cash ──
cat("computing Dual momentum...\n")

run_dual_backtest <- function(mom_xts, mon_rets_all, T_val) {
  sig_dates <- index(mom_xts)
  trd_dates_all <- index(mon_rets_all)

  dual_rets     <- rep(NA_real_, length(trd_dates_all))
  turnover_vec  <- rep(NA_real_, length(trd_dates_all))
  prev_weights  <- rep(0, N_RISKY + 1)  # +1 for cash

  for (t_idx in seq_along(trd_dates_all)) {
    trd_date <- trd_dates_all[t_idx]

    # find the most recent signal date
    sig_idx <- max(which(sig_dates < trd_date))
    if (sig_idx < 1 || is.infinite(sig_idx)) next
    sig_date <- sig_dates[sig_idx]

    mom_row <- mom_xts[as.character(sig_date), ]
    risky_mom <- as.numeric(mom_row[, RISKY_TICKERS])
    names(risky_mom) <- RISKY_TICKERS
    risky_mom <- risky_mom[!is.na(risky_mom)]

    if (length(risky_mom) < T_val) next

    ranked <- sort(risky_mom, decreasing = TRUE)

    # Top T based on relative momentum
    top_t <- names(ranked)[1:T_val]

    # Remove bad assets (13612W <= 0) from top_t, replace with cash
    good_assets <- top_t[risky_mom[top_t] > 0]
    n_bad_slots <- T_val - length(good_assets)

    cash_mom <- as.numeric(mom_row[, CASH_TICKERS])
    names(cash_mom) <- CASH_TICKERS
    best_cash <- names(which.max(cash_mom))[1]

    # weights: 1/T for each good asset, n_bad_slots/T for cash
    curr_weights <- rep(0, N_RISKY + 1)
    names(curr_weights) <- c(RISKY_TICKERS, "CASH")
    if (length(good_assets) > 0) {
      curr_weights[good_assets] <- 1 / T_val
    }
    curr_weights["CASH"] <- n_bad_slots / T_val

    # portfolio return
    rets_row <- as.numeric(mon_rets_all[as.character(trd_date), RISKY_TICKERS])
    names(rets_row) <- RISKY_TICKERS
    cash_ret <- as.numeric(mon_rets_all[as.character(trd_date), best_cash])

    port_ret <- 0
    if (length(good_assets) > 0) {
      port_ret <- port_ret + sum(curr_weights[good_assets] * rets_row[good_assets], na.rm = TRUE)
    }
    port_ret <- port_ret + curr_weights["CASH"] * cash_ret

    to <- sum(abs(curr_weights - prev_weights)) / 2
    turnover_vec[t_idx] <- to
    port_ret <- port_ret - to * DRAG

    dual_rets[t_idx] <- port_ret
    prev_weights <- curr_weights
  }

  xts(dual_rets, order.by = trd_dates_all)
}

# ═══════════════════════════════════════════════════════════════════════════════
# in-sample / out-of-sample split
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nestablishing IS/OS split...\n")

# Determine backtest date range
bt_start <- trade_dates[1]
bt_end   <- last(trade_dates)
midpoint <- bt_start + (bt_end - bt_start) / 2

# Find the actual date closest to the midpoint
mid_diff <- abs(trade_dates - midpoint)
mid_idx  <- which.min(mid_diff)
is_end_date <- trade_dates[mid_idx]

cat(sprintf("  backtest:  %s → %s\n",
            as.character(as.Date(bt_start)),
            as.character(as.Date(bt_end))))
cat(sprintf("  IS / OS split at: %s\n", as.character(as.Date(is_end_date))))

# ═══════════════════════════════════════════════════════════════════════════════
# grid search: optimize (T, B) over IS period
# ═══════════════════════════════════════════════════════════════════════════════

cat("\ngrid search: T=1..6, B=1..6 on IS...\n")

T_range <- 1:6
B_range <- 1:6

# Filter data to IS period
is_end_sig <- max(which(signal_dates < is_end_date))
is_signal_dates <- signal_dates[1:is_end_sig]
is_mom_xts <- momentum_xts[as.character(is_signal_dates), ]

# Need prices through IS end for return computation
is_price_end <- max(which(index(mon_merged) <= is_end_date))
is_mon_merged <- mon_merged[1:is_price_end, ]
is_ewc <- ewc_rets[paste0("/", as.character(is_end_date))]
is_ewc <- is_ewc[!is.na(coredata(is_ewc)), ]

grid_results <- data.frame(
  T_val = integer(),
  B_val = integer(),
  R     = numeric(),
  V     = numeric(),
  D     = numeric(),
  Sharpe = numeric(),
  MAR   = numeric(),
  RAD   = numeric(),
  CF    = numeric(),
  TTC   = numeric(),
  stringsAsFactors = FALSE
)

for (T_val in T_range) {
  for (B_val in B_range) {
    cat(sprintf("  T=%d, B=%d...\n", T_val, B_val))
    res <- run_vaa_backtest(is_mom_xts, is_mon_merged, T_val, B_val)

    if (NROW(res$returns) < 12) {
      cat("    insufficient returns\n")
      next
    }

    # Limit to IS period
    ret_is <- res$returns[paste0("/", as.character(is_end_date))]
    ret_is <- ret_is[!is.na(coredata(ret_is)), ]

    if (NROW(ret_is) < 12) next

    metrics <- compute_metrics(ret_is, is_ewc)

    cf_avg  <- mean(res$cash_fraction, na.rm = TRUE)
    ttc_avg <- mean(res$turnover, na.rm = TRUE) * 2 * DRAG * 12  # annualized

    grid_results <- rbind(grid_results, data.frame(
      T_val  = T_val,
      B_val  = B_val,
      R      = metrics$R,
      V      = metrics$V,
      D      = metrics$D,
      Sharpe = metrics$Sharpe,
      MAR    = metrics$MAR,
      RAD    = metrics$RAD,
      CF     = cf_avg,
      TTC    = ttc_avg,
      stringsAsFactors = FALSE
    ))
  }
}

cat("\ngrid search results:\n")
grid_results <- grid_results[order(-grid_results$RAD), ]
print(grid_results, row.names = FALSE)

opt_T <- grid_results$T_val[1]
opt_B <- grid_results$B_val[1]
cat(sprintf("\noptimal: T=%d, B=%d (RAD=%.4f)\n", opt_T, opt_B, grid_results$RAD[1]))

# ═══════════════════════════════════════════════════════════════════════════════
# full sample run with optimal (T, B)
# ═══════════════════════════════════════════════════════════════════════════════

cat(sprintf("\nrunning VAA full sample with optimal T=%d, B=%d...\n", opt_T, opt_B))

vaa_full <- run_vaa_backtest(momentum_xts, mon_merged, opt_T, opt_B, DRAG)
vaa_rets_full <- vaa_full$returns
names(vaa_rets_full) <- "VAA"

cat(sprintf("  VAA returns: %d months\n", NROW(vaa_rets_full)))

# ── Dual momentum with same T ──
cat("running Dual momentum full sample...\n")
dual_rets <- run_dual_backtest(momentum_xts, mon_rets_all, opt_T)
dual_rets <- dual_rets[!is.na(coredata(dual_rets)), ]
names(dual_rets) <- "Dual"

# ═══════════════════════════════════════════════════════════════════════════════
# align all strategies for comparison
# ═══════════════════════════════════════════════════════════════════════════════

cat("\naligning all strategies...\n")

# Find common date range
common_start <- max(
  first(index(vaa_rets_full)),
  first(index(ew_rets)),
  first(index(ewc_rets)),
  first(index(sixty40_rets)),
  first(index(spy_rets)),
  first(index(dual_rets))
)

common_end <- min(
  last(index(vaa_rets_full)),
  last(index(ew_rets)),
  last(index(ewc_rets)),
  last(index(sixty40_rets)),
  last(index(spy_rets)),
  last(index(dual_rets))
)

cat(sprintf("  common range: %s → %s\n",
            as.character(as.Date(common_start)),
            as.character(as.Date(common_end))))

all_strats <- na.omit(merge(
  vaa_rets_full[paste0(common_start, "/", common_end)],
  dual_rets[paste0(common_start, "/", common_end)],
  ew_rets[paste0(common_start, "/", common_end)],
  ewc_rets[paste0(common_start, "/", common_end)],
  sixty40_rets[paste0(common_start, "/", common_end)],
  spy_rets[paste0(common_start, "/", common_end)]
))

cat(sprintf("  combined: %d rows, %d cols\n", nrow(all_strats), ncol(all_strats)))

# ═══════════════════════════════════════════════════════════════════════════════
# compute metrics for IS, OS, FS
# ═══════════════════════════════════════════════════════════════════════════════

cat("\ncomputing performance metrics...\n")

compute_all_metrics <- function(rets, ewc_xts, period_name) {
  ewc_period <- ewc_xts[index(rets)]
  metrics <- compute_metrics(rets, ewc_period)
  if (is.null(metrics)) return(NULL)

  data.frame(
    Period = period_name,
    R      = metrics$R * 100,
    V      = metrics$V * 100,
    D      = abs(metrics$D) * 100,
    Sharpe = metrics$Sharpe,
    MAR    = metrics$MAR,
    RAD    = metrics$RAD,
    stringsAsFactors = FALSE
  )
}

all_period_metrics <- data.frame()

for (strat_name in colnames(all_strats)) {
  ret_x <- all_strats[, strat_name]

  # IS
  is_ret <- ret_x[paste0("/", as.character(is_end_date))]
  if (NROW(is_ret) > 12) {
    m <- compute_all_metrics(is_ret, ewc_rets, paste(strat_name, "IS"))
    if (!is.null(m)) all_period_metrics <- rbind(all_period_metrics, m)
  }

  # OS
  os_ret <- ret_x[paste0(as.character(is_end_date), "/")]
  if (NROW(os_ret) > 12) {
    m <- compute_all_metrics(os_ret, ewc_rets, paste(strat_name, "OS"))
    if (!is.null(m)) all_period_metrics <- rbind(all_period_metrics, m)
  }

  # FS
  if (NROW(ret_x) > 12) {
    m <- compute_all_metrics(ret_x, ewc_rets, paste(strat_name, "FS"))
    if (!is.null(m)) all_period_metrics <- rbind(all_period_metrics, m)
  }
}

# ═══════════════════════════════════════════════════════════════════════════════
# additional VAA-specific metrics
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nVAA-specific metrics:\n")

vaa_cf  <- mean(vaa_full$cash_fraction, na.rm = TRUE) * 100
vaa_ttc <- mean(vaa_full$turnover, na.rm = TRUE) * 2 * DRAG * 12 * 100  # % per year
vaa_bad <- mean(vaa_full$n_bad, na.rm = TRUE)

cat(sprintf("  avg cash fraction: %.1f%%\n", vaa_cf))
cat(sprintf("  avg TTC (annual):  %.2f%%\n", vaa_ttc))
cat(sprintf("  avg bad assets:    %.1f / %d\n", vaa_bad, N_RISKY))

# ═══════════════════════════════════════════════════════════════════════════════
# report: metrics table
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding metrics table...\n")

# ── reshape for gt ──
metrics_tbl <- all_period_metrics |>
  mutate(
    Strategy = sub(" (IS|OS|FS)$", "", Period),
    Window   = sub(".* (IS|OS|FS)$", "\\1", Period)
  ) |>
  mutate(Window = recode(Window, IS = "In-Sample", OS = "Out-of-Sample", FS = "Full Sample")) |>
  select(Strategy, Window, R, V, D, Sharpe, MAR, RAD) |>
  # metrics are already in percentage form (×100); scale to decimals for fmt_percent
  mutate(R = R/100, V = V/100, D = D/100) |>
  arrange(factor(Window, levels = c("In-Sample", "Out-of-Sample", "Full Sample")),
          factor(Strategy, levels = c("VAA", "Dual", "EW", "EWC", "60/40", "SPY")))

STRAT_COLORS <- c(
  VAA   = "#f0f4ff",  # light blue
  Dual  = "#f0fff0",  # light green
  EW    = "#f5f5f5",  # light grey (benchmark)
  EWC   = "#f5f5f5",
  `60/40` = "#f5f5f5",
  `X60.40` = "#f5f5f5",
  SPY   = "#f5f5f5"
)

tbl <- metrics_tbl |>
  gt(groupname_col = "Window") |>
  tab_header(
    title = sprintf("VAA-G12 Backtest Metrics (T=%d, B=%d)", opt_T, opt_B),
    subtitle = sprintf("13612W Breadth Momentum | %s → %s | 0.10%% one-way cost",
                       as.character(as.Date(common_start)),
                       as.character(as.Date(common_end)))
  ) |>
  cols_label(
    Strategy = "",
    R        = "CAGR %",
    V        = "Vol %",
    D        = "MaxDD %",
    Sharpe   = "Sharpe",
    MAR      = "MAR",
    RAD      = "RAD"
  ) |>
  fmt_percent(columns = c(R, V, D), decimals = 1) |>
  fmt_number(columns = c(Sharpe, MAR), decimals = 2) |>
  fmt_number(columns = RAD, decimals = 4) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels()) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups()) |>
  tab_source_note(source_note = "@StockViz") |>
  tab_style(style = cell_text(align = "right"),
            locations = cells_source_notes())

# row background colors
for (s in names(STRAT_COLORS)) {
  rows <- which(metrics_tbl$Strategy == s)
  if (length(rows) > 0) {
    tbl <- tbl |>
      tab_style(style = cell_fill(color = STRAT_COLORS[s]),
                locations = cells_body(rows = rows))
  }
}

# red for negative returns / drawdowns > 20%
for (col_name in c("R")) {
  neg_rows <- which(metrics_tbl[[col_name]] < 0)
  if (length(neg_rows) > 0)
    tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000"),
              locations = cells_body(columns = all_of(col_name), rows = neg_rows))
}
for (col_name in c("D")) {
  hi_rows <- which(metrics_tbl[[col_name]] > 0.20)
  if (length(hi_rows) > 0)
    tbl <- tbl |> tab_style(style = cell_text(color = "#8B0000", weight = "bold"),
              locations = cells_body(columns = all_of(col_name), rows = hi_rows))
}

# green for returns > 2pp
for (col_name in c("R")) {
  hi_rows <- which(metrics_tbl[[col_name]] > 0.02)
  if (length(hi_rows) > 0)
    tbl <- tbl |> tab_style(style = cell_text(color = "#006400"),
              locations = cells_body(columns = all_of(col_name), rows = hi_rows))
}

print(tbl)

gtsave(tbl, sprintf("%s/vaa-metrics.html", reportPath))
webshot2::webshot(
  sprintf("%s/vaa-metrics.html", reportPath),
  sprintf("%s/vaa-metrics.png", reportPath),
  selector = "table.gt_table", expand = c(10, 10, 10, 10)
)

# ═══════════════════════════════════════════════════════════════════════════════
# report: grid search table (top 10 by RAD)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\ntop 10 grid results (by RAD):\n")
print(head(grid_results, 10), row.names = FALSE)

# ═══════════════════════════════════════════════════════════════════════════════
# charts
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding charts...\n")

# ── cumulative returns (full sample) ──
main_strats <- c("VAA", "Dual", "60/40", "EW", "SPY")
to_plot <- all_strats[, main_strats[main_strats %in% colnames(all_strats)]]

sr <- sapply(colnames(to_plot), function(nm) {
  round(SharpeRatio.annualized(to_plot[, nm])[1, 1], 2)
})
sr_text <- paste0(colnames(to_plot), "=", sr, collapse = ", ")

Common.PlotCumReturns(to_plot,
  sprintf("VAA-G12 Backtest (T=%d, B=%d)", opt_T, opt_B),
  sprintf("%s | %s → %s | SR: %s",
          "13612W Breadth Momentum",
          as.character(as.Date(first(index(to_plot)))),
          as.character(as.Date(last(index(to_plot)))),
          sr_text),
  sprintf("%s/vaa-cumulative.png", reportPath), NULL)

# ── cumulative returns (out-of-sample only) ──
os_plot <- to_plot[paste0(as.character(is_end_date), "/")]
if (NROW(os_plot) > 12) {
  sr_os <- sapply(colnames(os_plot), function(nm) {
    round(SharpeRatio.annualized(os_plot[, nm])[1, 1], 2)
  })
  sr_os_text <- paste0(colnames(os_plot), "=", sr_os, collapse = ", ")

  Common.PlotCumReturns(os_plot,
    sprintf("VAA-G12 Out-of-Sample (T=%d, B=%d)", opt_T, opt_B),
    sprintf("13612W Breadth Momentum | %s → %s | SR: %s",
            as.character(as.Date(first(index(os_plot)))),
            as.character(as.Date(last(index(os_plot)))),
            sr_os_text),
    sprintf("%s/vaa-cumulative-os.png", reportPath), NULL)
}

# ── drawdowns ──
png(sprintf("%s/vaa-drawdowns.png", reportPath), width = 1400, height = 800, bg = "white")
chart.Drawdown(to_plot, main = sprintf("VAA-G12 Drawdowns (T=%d, B=%d)", opt_T, opt_B),
                legend.loc = "bottomleft", ylab = "Drawdown")
mtext("@StockViz", side = 4, col = 'grey')
dev.off()

# ═══════════════════════════════════════════════════════════════════════════════
# save results
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nsaving results...\n")

save(
  all_strats, grid_results, all_period_metrics,
  opt_T, opt_B, vaa_full,
  vaa_cf, vaa_ttc, vaa_bad,
  is_end_date, common_first, common_last, common_start, common_end,
  momentum_xts, mon_merged,
  file = sprintf("%s/vaa-results.Rdata", reportPath)
)

cat("\nDone.\n")
cat(sprintf("  optimal: T=%d, B=%d\n", opt_T, opt_B))
cat(sprintf("  VAA Sharpe: %.2f\n",
            SharpeRatio.annualized(vaa_rets_full)[1, 1]))
cat(sprintf("  VAA MaxDD:  %.1f%%\n",
            -maxDrawdown(vaa_rets_full) * 100))
cat(sprintf("  VAA CAGR:   %.1f%%\n",
            Return.annualized(vaa_rets_full)[1, 1] * 100))
