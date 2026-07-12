# DAA-R (Defensive Asset Allocation — Robust)
# Keller & Keuning (2018) — Canary Universe + Breadth Momentum
# Walk-forward canary selection, tiered costs, small pre-registered grid
# Two canary-pool variants: EEM-based and SPHB-based

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
# universe definition
# ═══════════════════════════════════════════════════════════════════════════════

RISKY_TICKERS      <- c("SPY", "IWM", "EFA", "EEM", "VNQ", "DBC", "GLD", "TLT", "HYG", "LQD")
CASH_TICKERS       <- c("BIL", "IEF", "SHY")
CANARY_POOL_EEM    <- c("HYG", "EEM", "TLT", "IEF", "UUP")
CANARY_POOL_SPHB   <- c("HYG", "SPHB", "TLT", "IEF", "UUP")

ALL_TICKERS <- unique(c(RISKY_TICKERS, CASH_TICKERS, CANARY_POOL_EEM, CANARY_POOL_SPHB))
N_RISKY     <- length(RISKY_TICKERS)

# Tiered transaction costs
COST_TIER1 <- 0.0005
COST_TIER2 <- 0.0015
TIER1 <- c("SPY", "IWM", "EFA", "EEM", "GLD", "TLT", "IEF", "SHY", "BIL")
TIER2 <- c("HYG", "LQD", "VNQ", "DBC", "UUP", "SPHB")

COST_MAP <- setNames(
  c(rep(COST_TIER1, length(TIER1)), rep(COST_TIER2, length(TIER2))),
  c(TIER1, TIER2)
)

T_GRID <- c(max(3, floor(N_RISKY/3)), ceiling(N_RISKY/2))  # [3, 5]
B_GRID <- c(1, 2)
MIN_TRAIN_MONTHS <- 60

# ═══════════════════════════════════════════════════════════════════════════════
# connect + load daily prices
# ═══════════════════════════════════════════════════════════════════════════════

cat("connecting to StockVizUs2...\n")
lconUS2 <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockVizUs2", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

cat("loading daily prices...\n")
daily_prices <- list()
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
  cat(sprintf("  %s: %d rows, %s → %s\n", sym, nrow(pDf),
              as.character(as.Date(first(index(daily_prices[[sym]])))),
              as.character(as.Date(last(index(daily_prices[[sym]]))))))
}
odbcClose(lconUS2)

# ═══════════════════════════════════════════════════════════════════════════════
# build monthly prices + 13612W momentum
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding monthly prices...\n")
monthly_prices <- list()
for (sym in names(daily_prices)) {
  mon <- to.monthly(daily_prices[[sym]], OHLC = FALSE)
  index(mon) <- as.Date(index(mon))
  monthly_prices[[sym]] <- mon
}

mon_merged <- do.call(merge.xts, monthly_prices[names(daily_prices)])
names(mon_merged) <- names(daily_prices)
mon_merged <- na.omit(mon_merged)
mon_rets <- mon_merged / stats::lag(mon_merged, 1) - 1
mon_rets <- mon_rets[-1, ]

cat(sprintf("  merged: %d months, %d tickers\n", nrow(mon_merged), ncol(mon_merged)))
cat(sprintf("  range: %s → %s\n",
            as.character(as.Date(first(index(mon_merged)))),
            as.character(as.Date(last(index(mon_merged))))))

cat("\ncomputing 13612W momentum...\n")
compute_13612w <- function(px) {
  n <- nrow(px)
  mom <- rep(NA_real_, n)
  for (i in 13:n) {
    p0 <- coredata(px)[i]; p1 <- coredata(px)[i-1]; p3 <- coredata(px)[i-3]
    p6 <- coredata(px)[i-6]; p12 <- coredata(px)[i-12]
    if (any(is.na(c(p1,p3,p6,p12))) || any(c(p1,p3,p6,p12)==0)) next
    mom[i] <- (12*(p0/p1-1)+4*(p0/p3-1)+2*(p0/p6-1)+1*(p0/p12-1))/4
  }
  xts(mom, order.by=index(px))
}

momentum_xts <- do.call(merge.xts, lapply(names(monthly_prices), function(sym)
  compute_13612w(monthly_prices[[sym]])))
names(momentum_xts) <- names(monthly_prices)
momentum_xts <- na.omit(momentum_xts)
cat(sprintf("  13612W: %d rows, %d cols\n", nrow(momentum_xts), ncol(momentum_xts)))

# ═══════════════════════════════════════════════════════════════════════════════
# monthly portfolio runner
# ═══════════════════════════════════════════════════════════════════════════════

run_daa_portfolio <- function(mon_ret_xts, mom_xts, canary_set, T_val, B_val) {
  trd_dates <- index(mon_ret_xts)
  n_trd <- length(trd_dates)
  rets <- rep(NA_real_, n_trd)
  prev_wts <- setNames(rep(0, length(ALL_TICKERS)), ALL_TICKERS)

  for (t_idx in seq_len(n_trd)) {
    trd_date <- trd_dates[t_idx]
    mom_dates <- index(mom_xts)
    sig_idx <- max(which(mom_dates < trd_date))
    if (sig_idx < 1 || is.infinite(sig_idx)) next
    sig_date <- mom_dates[sig_idx]

    mom_row <- mom_xts[as.character(sig_date), ]
    if (all(is.na(coredata(mom_row)))) next

    canary_mom <- as.numeric(mom_row[, canary_set[canary_set %in% colnames(mom_row)]])
    n_bad <- sum(canary_mom <= 0, na.rm=TRUE)
    cf <- min(n_bad / B_val, 1)

    risky_mom <- as.numeric(mom_row[, RISKY_TICKERS])
    names(risky_mom) <- RISKY_TICKERS
    risky_mom <- risky_mom[!is.na(risky_mom)]
    if (length(risky_mom) < T_val) next
    top_t <- names(sort(risky_mom, decreasing=TRUE))[1:T_val]

    cash_mom <- as.numeric(mom_row[, CASH_TICKERS])
    names(cash_mom) <- CASH_TICKERS
    best_cash <- names(which.max(cash_mom))[1]

    new_wts <- setNames(rep(0, length(ALL_TICKERS)), ALL_TICKERS)
    new_wts[top_t] <- (1 - cf) / T_val
    new_wts[best_cash] <- new_wts[best_cash] + cf

    rets_row <- as.numeric(mon_ret_xts[as.character(trd_date), ])
    port_ret <- sum(new_wts * rets_row, na.rm=TRUE)
    cost <- sum(abs(new_wts - prev_wts) * COST_MAP[names(new_wts)], na.rm=TRUE)
    port_ret <- port_ret - cost

    rets[t_idx] <- port_ret
    prev_wts <- new_wts
  }
  xts(rets, order.by=trd_dates)
}

# ═══════════════════════════════════════════════════════════════════════════════
# walk-forward runner
# ═══════════════════════════════════════════════════════════════════════════════

run_walkforward <- function(canary_pool, label) {
  cat(sprintf("\n══════ %s ══════\n", label))

  avail_canary <- canary_pool[canary_pool %in% colnames(mon_rets)]
  canary_combos <- list()
  for (c1 in avail_canary) canary_combos[[c1]] <- c1
  if (length(avail_canary) >= 2)
    for (i in 1:(length(avail_canary)-1))
      for (j in (i+1):length(avail_canary))
        canary_combos[[paste(avail_canary[i], avail_canary[j], sep="+")]] <-
          c(avail_canary[i], avail_canary[j])

  cat(sprintf("  canary candidates: %d combinations\n", length(canary_combos)))

  all_dates <- index(mon_rets)
  all_years <- sort(unique(as.numeric(format(all_dates, "%Y"))))
  warmup_end_year <- as.numeric(format(all_dates[1], "%Y")) + (MIN_TRAIN_MONTHS %/% 12)
  test_years <- all_years[all_years >= warmup_end_year]
  cat(sprintf("  test years: %d → %d (%d years)\n", test_years[1], last(test_years), length(test_years)))

  wf_returns <- list()
  wf_canary  <- list()
  wf_params  <- list()

  for (y in test_years) {
    cat(sprintf("\n  ── year %d ──\n", y))
    train_end <- as.Date(sprintf("%d-12-31", y-1))
    test_dates <- all_dates[format(all_dates, "%Y") == as.character(y)]
    if (length(test_dates) == 0) { cat("    no test dates\n"); next }

    train_mom <- momentum_xts[paste0("/", as.character(train_end))]
    train_ret <- mon_rets[paste0("/", as.character(train_end))]
    train_dates <- index(train_mom)
    train_dates <- train_dates[train_dates <= train_end]
    if (length(train_dates) < MIN_TRAIN_MONTHS) {
      cat(sprintf("    only %d training months\n", length(train_dates))); next
    }

    # step 1: select canary
    best_canary_score <- -Inf; best_canary_set <- NULL
    ref_T <- ceiling(N_RISKY/2); ref_B <- 1
    for (cn in names(canary_combos)) {
      cset <- canary_combos[[cn]]
      port <- run_daa_portfolio(train_ret, train_mom, cset, ref_T, ref_B)
      port <- port[!is.na(coredata(port)), ]
      if (NROW(port) < 36) next
      r <- Return.annualized(port)[1,1]; d <- maxDrawdown(port)
      score <- if (r >= 0 && d <= 0.50) r*(1-d/(1-d)) else 0
      if (score > best_canary_score) { best_canary_score <- score; best_canary_set <- cset }
    }
    if (is.null(best_canary_set)) { cat("    no valid canary\n"); next }
    cat(sprintf("    canary: %s (RAD=%.4f)\n", paste(best_canary_set,collapse="+"), best_canary_score))

    # step 2: select T, B
    best_param_score <- -Inf; best_T <- T_GRID[1]; best_B <- B_GRID[1]
    for (Tv in T_GRID) for (Bv in B_GRID) {
      port <- run_daa_portfolio(train_ret, train_mom, best_canary_set, Tv, Bv)
      port <- port[!is.na(coredata(port)), ]
      if (NROW(port) < 36) next
      r <- Return.annualized(port)[1,1]; d <- maxDrawdown(port)
      score <- if (r >= 0 && d <= 0.50) r*(1-d/(1-d)) else 0
      if (score > best_param_score) { best_param_score <- score; best_T <- Tv; best_B <- Bv }
    }
    cat(sprintf("    params: T=%d, B=%d (RAD=%.4f)\n", best_T, best_B, best_param_score))

    # step 3: test year
    test_ret <- mon_rets[as.character(test_dates), ]
    test_mom <- momentum_xts[as.character(test_dates), ]
    port <- run_daa_portfolio(test_ret, test_mom, best_canary_set, best_T, best_B)
    if (NROW(port) > 0) {
      wf_returns[[as.character(y)]] <- port
      wf_canary[[as.character(y)]]  <- best_canary_set
      wf_params[[as.character(y)]]  <- c(T=best_T, B=best_B)
    }
  }

  rets <- do.call(rbind, wf_returns)
  rets <- rets[!is.na(coredata(rets)), ]
  names(rets) <- label

  # canary history
  cat(sprintf("\n  canary history (%s):\n", label))
  for (y in names(wf_canary))
    cat(sprintf("    %s: %s (T=%d, B=%d)\n", y,
                paste(wf_canary[[y]],collapse="+"), wf_params[[y]]["T"], wf_params[[y]]["B"]))

  list(returns=rets, canary=wf_canary, params=wf_params)
}

# ═══════════════════════════════════════════════════════════════════════════════
# run both variants
# ═══════════════════════════════════════════════════════════════════════════════

res_eem  <- run_walkforward(CANARY_POOL_EEM,  "DAA-EEM")
res_sphb <- run_walkforward(CANARY_POOL_SPHB, "DAA-SPHB")

# ═══════════════════════════════════════════════════════════════════════════════
# align on common dates
# ═══════════════════════════════════════════════════════════════════════════════

cat("\naligning on common date range...\n")

common_start <- max(first(index(res_eem$returns)), first(index(res_sphb$returns)))
common_end   <- min(last(index(res_eem$returns)),  last(index(res_sphb$returns)))

cat(sprintf("  common range: %s → %s\n",
            as.character(as.Date(common_start)), as.character(as.Date(common_end))))

daa_eem  <- res_eem$returns[paste0(common_start, "/", common_end)]
daa_sphb <- res_sphb$returns[paste0(common_start, "/", common_end)]

# benchmarks on same range
common_ret <- mon_rets[paste0(common_start, "/", common_end), ]

ew_wts <- rep(1/N_RISKY, N_RISKY); names(ew_wts) <- RISKY_TICKERS
ew_rets <- xts(rowSums(coredata(common_ret[,RISKY_TICKERS]) *
               matrix(ew_wts, nrow=NROW(common_ret), ncol=N_RISKY, byrow=TRUE), na.rm=TRUE),
               order.by=index(common_ret)); names(ew_rets) <- "EW"

N_CASH <- length(CASH_TICKERS)
ewc_wts <- rep(1/N_CASH, N_CASH); names(ewc_wts) <- CASH_TICKERS
ewc_rets <- xts(rowSums(coredata(common_ret[,CASH_TICKERS]) *
                matrix(ewc_wts, nrow=NROW(common_ret), ncol=N_CASH, byrow=TRUE), na.rm=TRUE),
                order.by=index(common_ret)); names(ewc_rets) <- "EWC"

sixty40_rets <- xts(0.6*coredata(common_ret[,"SPY"]) + 0.4*coredata(common_ret[,"IEF"]),
                    order.by=index(common_ret)); names(sixty40_rets) <- "60/40"

spy_rets <- common_ret[,"SPY"]; names(spy_rets) <- "SPY"

all_strats <- na.omit(merge(daa_eem, daa_sphb, ew_rets, ewc_rets, sixty40_rets, spy_rets))
cat(sprintf("  combined: %d rows, %d cols\n", NROW(all_strats), ncol(all_strats)))

# ═══════════════════════════════════════════════════════════════════════════════
# metrics
# ═══════════════════════════════════════════════════════════════════════════════

compute_metrics <- function(ret_xts, ewc_xts=NULL) {
  if (NROW(ret_xts) < 3) return(NULL)
  r <- Return.annualized(ret_xts)[1,1]
  v <- StdDev.annualized(ret_xts)[1,1]
  d <- maxDrawdown(ret_xts)
  if (!is.null(ewc_xts)) {
    sharpe <- SharpeRatio.annualized(ret_xts - ewc_xts[index(ret_xts)])[1,1]
  } else {
    sharpe <- SharpeRatio.annualized(ret_xts)[1,1]
  }
  mar <- r/d
  rad <- if (r >= 0 && d <= 0.50) r*(1-d/(1-d)) else 0
  list(R=r, V=v, D=d, Sharpe=sharpe, MAR=mar, RAD=rad)
}

# ── IS/OS/FS split at midpoint of common range ──
midpoint <- common_start + (common_end - common_start) / 2
mid_diff <- abs(index(all_strats) - midpoint)
is_end_date <- index(all_strats)[which.min(mid_diff)]
cat(sprintf("  IS/OS split: %s\n", as.character(as.Date(is_end_date))))

all_period_metrics <- data.frame()
for (nm in colnames(all_strats)) {
  ret_x <- all_strats[, nm]

  for (period in c("IS", "OS", "FS")) {
    if (period == "IS") {
      sub <- ret_x[paste0("/", as.character(is_end_date))]
    } else if (period == "OS") {
      sub <- ret_x[paste0(as.character(is_end_date), "/")]
    } else {
      sub <- ret_x
    }
    if (NROW(sub) < 12) next
    m <- compute_metrics(sub, ewc_rets)
    if (!is.null(m))
      all_period_metrics <- rbind(all_period_metrics, data.frame(
        Period = paste(nm, period),
        R=m$R*100, V=m$V*100, D=abs(m$D)*100,
        Sharpe=m$Sharpe, MAR=m$MAR, RAD=m$RAD, stringsAsFactors=FALSE))
  }
}

# reshape for gt
metrics_tbl <- all_period_metrics |>
  mutate(
    Strategy = sub(" (IS|OS|FS)$", "", Period),
    Window   = sub(".* (IS|OS|FS)$", "\\1", Period)
  ) |>
  mutate(Window = recode(Window, IS="In-Sample", OS="Out-of-Sample", FS="Full Sample")) |>
  select(Strategy, Window, R, V, D, Sharpe, MAR, RAD) |>
  mutate(R=R/100, V=V/100, D=D/100) |>
  arrange(factor(Window, levels=c("In-Sample","Out-of-Sample","Full Sample")),
          factor(Strategy, levels=c("DAA.EEM","DAA.SPHB","EW","EWC","60/40","SPY")))

# ═══════════════════════════════════════════════════════════════════════════════
# canary stability summary
# ═══════════════════════════════════════════════════════════════════════════════

for (variant in c("EEM", "SPHB")) {
  res <- if (variant == "EEM") res_eem else res_sphb
  cat(sprintf("\ncanary stability — %s pool:\n", variant))
  freq <- table(sapply(res$canary, paste, collapse="+"))
  print(sort(freq, decreasing=TRUE))
}

# ═══════════════════════════════════════════════════════════════════════════════
# gt metrics table
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding metrics table...\n")

STRAT_COLORS <- c(
  `DAA.EEM`  = "#f0f4ff",  # light blue
  `DAA.SPHB` = "#f0fff0",  # light green
  EW         = "#f5f5f5",
  EWC        = "#f5f5f5",
  `60/40`    = "#f5f5f5",
  SPY        = "#f5f5f5"
)

tbl <- metrics_tbl |>
  gt(groupname_col="Window") |>
  tab_header(
    title = "DAA-R Walk-Forward — EEM vs SPHB Canary",
    subtitle = sprintf("%s → %s | Tiered costs",
                       as.character(as.Date(first(index(all_strats)))),
                       as.character(as.Date(last(index(all_strats)))))
  ) |>
  cols_label(Strategy="", R="CAGR %", V="Vol %", D="MaxDD %",
             Sharpe="Sharpe", MAR="MAR", RAD="RAD") |>
  fmt_percent(columns=c(R,V,D), decimals=1) |>
  fmt_number(columns=c(Sharpe,MAR), decimals=2) |>
  fmt_number(columns=RAD, decimals=4) |>
  tab_style(style=cell_text(weight="bold"), locations=cells_column_labels()) |>
  tab_style(style=cell_text(weight="bold"), locations=cells_row_groups()) |>
  tab_source_note(source_note="@StockViz") |>
  tab_style(style=cell_text(align="right"), locations=cells_source_notes())

for (s in names(STRAT_COLORS)) {
  rows <- which(metrics_tbl$Strategy == s)
  if (length(rows) > 0)
    tbl <- tbl |> tab_style(style=cell_fill(color=STRAT_COLORS[s]),
                            locations=cells_body(rows=rows))
}

for (col_name in c("R")) {
  neg <- which(metrics_tbl[[col_name]] < 0)
  if (length(neg) > 0)
    tbl <- tbl |> tab_style(style=cell_text(color="#8B0000"),
              locations=cells_body(columns=all_of(col_name), rows=neg))
  hi <- which(metrics_tbl[[col_name]] > 0.02)
  if (length(hi) > 0)
    tbl <- tbl |> tab_style(style=cell_text(color="#006400"),
              locations=cells_body(columns=all_of(col_name), rows=hi))
}
hi_dd <- which(metrics_tbl$D > 0.20)
if (length(hi_dd) > 0)
  tbl <- tbl |> tab_style(style=cell_text(color="#8B0000", weight="bold"),
            locations=cells_body(columns="D", rows=hi_dd))

print(tbl)
gtsave(tbl, sprintf("%s/daa-metrics.html", reportPath))
webshot2::webshot(
  sprintf("%s/daa-metrics.html", reportPath),
  sprintf("%s/daa-metrics.png", reportPath),
  selector="table.gt_table", expand=c(10,10,10,10))

# ═══════════════════════════════════════════════════════════════════════════════
# charts
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding charts...\n")

main_strats <- c("DAA.EEM", "DAA.SPHB", "60/40", "EW", "SPY")
to_plot <- all_strats[, main_strats[main_strats %in% colnames(all_strats)]]

sr <- sapply(colnames(to_plot), function(nm)
  round(SharpeRatio.annualized(to_plot[,nm])[1,1], 2))
sr_text <- paste0(colnames(to_plot), "=", sr, collapse=", ")

Common.PlotCumReturns(to_plot,
  "DAA-R — EEM vs SPHB Canary",
  sprintf("Walk-Forward Canary + Breadth | %s → %s | SR: %s",
          as.character(as.Date(first(index(to_plot)))),
          as.character(as.Date(last(index(to_plot)))),
          sr_text),
  sprintf("%s/daa-cumulative.png", reportPath), NULL)

# OS: 2020-07 onward
os_split <- "2020-07-01"
os_plot <- to_plot[paste0(os_split, "/")]
if (NROW(os_plot) > 12) {
  sr_os <- sapply(colnames(os_plot), function(nm)
    round(SharpeRatio.annualized(os_plot[,nm])[1,1], 2))
  sr_os_text <- paste0(colnames(os_plot), "=", sr_os, collapse=", ")
  Common.PlotCumReturns(os_plot,
    "DAA-R — Recent (2020-07 →)",
    sprintf("Walk-Forward Canary + Breadth | %s → %s | SR: %s",
            as.character(as.Date(first(index(os_plot)))),
            as.character(as.Date(last(index(os_plot)))),
            sr_os_text),
    sprintf("%s/daa-cumulative-os.png", reportPath), NULL)
}

# Drawdowns
png(sprintf("%s/daa-drawdowns.png", reportPath), width=1400, height=800, bg="white")
chart.Drawdown(to_plot, main="DAA-R Drawdowns — EEM vs SPHB",
               legend.loc="bottomleft", ylab="Drawdown")
mtext("@StockViz", side=4, col='grey')
dev.off()

# ═══════════════════════════════════════════════════════════════════════════════
# save
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nsaving results...\n")
save(all_strats, res_eem, res_sphb, all_period_metrics, metrics_tbl,
     is_end_date, common_ret, momentum_xts, mon_merged,
     file=sprintf("%s/daa-results.Rdata", reportPath))

cat("\nDone.\n")
for (nm in c("DAA.EEM", "DAA.SPHB")) {
  r <- Return.annualized(all_strats[,nm])[1,1]*100
  d <- -maxDrawdown(all_strats[,nm])*100
  s <- SharpeRatio.annualized(all_strats[,nm])[1,1]
  cat(sprintf("  %s: CAGR=%.1f%%  MaxDD=%.1f%%  Sharpe=%.2f\n", nm, r, d, s))
}
