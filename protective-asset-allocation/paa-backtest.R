# PAA-R (Protective Asset Allocation — Robust)
# Keller & Keuning (2016) — SMA Momentum + Bond Fraction Blending
# Walk-forward canary/parameter selection, smoothed bond fraction, tiered costs
# Two canary-pool variants: EEM-based and SPHB-based

library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('gt')
library('xts')
library('zoo')
library('TTR')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")
pdf(NULL)

# ═══════════════════════════════════════════════════════════════════════════════
# universe definition
# ═══════════════════════════════════════════════════════════════════════════════

RISKY_TICKERS       <- c("SPY", "IWM", "EFA", "EEM", "VNQ", "DBC", "GLD", "TLT", "HYG", "LQD")
BOND_CANDIDATES     <- c("IEF", "SHY", "BIL", "AGG")
CANARY_POOL_EEM     <- c("EEM", "AGG")
CANARY_POOL_SPHB    <- c("SPHB", "AGG")

ALL_TICKERS <- unique(c(RISKY_TICKERS, BOND_CANDIDATES, CANARY_POOL_EEM, CANARY_POOL_SPHB))
N_RISKY     <- length(RISKY_TICKERS)

# Tiered transaction costs
COST_TIER1 <- 0.0005; COST_TIER2 <- 0.0015
TIER1 <- c("SPY","IWM","EFA","EEM","GLD","TLT","IEF","SHY","BIL","AGG")
TIER2 <- c("HYG","LQD","VNQ","DBC","SPHB")
COST_MAP <- setNames(c(rep(COST_TIER1,length(TIER1)), rep(COST_TIER2,length(TIER2))), c(TIER1,TIER2))

# Pre-registered parameter grid (small, per plan)
L_GRID <- c(6, 12)          # lookback months
TOP_GRID <- c(3, 5)         # top N risky assets
A_GRID <- c(0, 1)           # protection factor

MIN_TRAIN_MONTHS <- 60

# ═══════════════════════════════════════════════════════════════════════════════
# connect + load daily prices
# ═══════════════════════════════════════════════════════════════════════════════

cat("connecting to StockVizUs2...\n")
lconUS2 <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockVizUs2", ldbuser, ldbpassword),
  case="nochange", believeNRows=TRUE)

cat("loading daily prices...\n")
daily_prices <- list()
for (sym in ALL_TICKERS) {
  pDf <- sqlQuery(lconUS2, sprintf("select C, TIME_STAMP from bhav_eq_td where SYMBOL='%s' order by TIME_STAMP", sym))
  if (nrow(pDf) < 260) { cat(sprintf("  %s: too few rows (%d)\n", sym, nrow(pDf))); next }
  daily_prices[[sym]] <- xts(pDf$C, pDf$TIME_STAMP)
  cat(sprintf("  %s: %d rows, %s → %s\n", sym, nrow(pDf),
              as.character(as.Date(first(index(daily_prices[[sym]])))),
              as.character(as.Date(last(index(daily_prices[[sym]]))))))
}
odbcClose(lconUS2)

# ═══════════════════════════════════════════════════════════════════════════════
# build monthly prices
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding monthly prices...\n")
monthly_prices <- list()
for (sym in names(daily_prices)) {
  mon <- to.monthly(daily_prices[[sym]], OHLC=FALSE)
  index(mon) <- as.Date(index(mon))
  monthly_prices[[sym]] <- mon
}
mon_merged <- do.call(merge.xts, monthly_prices[names(daily_prices)])
names(mon_merged) <- names(daily_prices)
mon_merged <- na.omit(mon_merged)
mon_rets <- mon_merged / stats::lag(mon_merged, 1) - 1
mon_rets <- mon_rets[-1, ]
cat(sprintf("  merged: %d months, %d tickers\n  range: %s → %s\n",
            nrow(mon_merged), ncol(mon_merged),
            as.character(as.Date(first(index(mon_merged)))),
            as.character(as.Date(last(index(mon_merged))))))

# ═══════════════════════════════════════════════════════════════════════════════
# SMA momentum filter
# ═══════════════════════════════════════════════════════════════════════════════

cat("\ncomputing SMA momentum...\n")
# Pre-compute SMA for all lookbacks
SMA_LOOKBACKS <- c(3, 6, 9, 12)  # used by grid search
sma_cache <- list()
for (L in SMA_LOOKBACKS) {
  sma_xts <- do.call(merge.xts, lapply(names(monthly_prices), function(sym)
    SMA(monthly_prices[[sym]], L)))
  names(sma_xts) <- names(monthly_prices)
  sma_cache[[as.character(L)]] <- na.omit(sma_xts)
  cat(sprintf("  SMA(%d): %d rows\n", L, nrow(sma_cache[[as.character(L)]])))
}

# SMA momentum score = log(price / SMA) — positive = above SMA
compute_sma_mom <- function(price_xts, sma_xts, L) {
  cd <- intersect(index(price_xts), index(sma_cache[[as.character(L)]]))
  log(coredata(price_xts[cd]) / coredata(sma_cache[[as.character(L)]][cd]))
}

# ═══════════════════════════════════════════════════════════════════════════════
# monthly portfolio runner (PAA-style with smoothed bond fraction)
# ═══════════════════════════════════════════════════════════════════════════════

run_paa_portfolio <- function(mon_ret_xts, price_xts, canary_set, L, TopN, A_val) {
  trd_dates <- index(mon_ret_xts)
  n_trd <- length(trd_dates)
  rets <- rep(NA_real_, n_trd)
  prev_wts <- setNames(rep(0, length(ALL_TICKERS)), ALL_TICKERS)

  # Compute SMA for each ticker column, aligned to price_xts dates
  # Cap L to available data length
  effL <- min(L, nrow(price_xts) - 1)
  sma_cols <- lapply(colnames(price_xts), function(sym) {
    sma_raw <- SMA(price_xts[, sym], effL)
    sma_raw[is.na(coredata(sma_raw)), ] <- coredata(price_xts[, sym])[is.na(coredata(sma_raw))]
    sma_raw
  })
  sma_xts <- do.call(merge.xts, sma_cols)
  names(sma_xts) <- colnames(price_xts)

  # SMA momentum = log(px / sma)
  cd <- index(price_xts)
  sma_mom <- xts(log(coredata(price_xts[cd]) / coredata(sma_xts[cd])), order.by = cd)

  for (t_idx in seq_len(n_trd)) {
    trd_date <- trd_dates[t_idx]

    # signal from prior date
    mom_dates <- index(sma_mom)
    sig_idx <- max(which(mom_dates < trd_date))
    if (sig_idx < 1 || is.infinite(sig_idx)) next
    sig_date <- mom_dates[sig_idx]

    mom_row <- sma_mom[as.character(sig_date), ]
    if (all(is.na(coredata(mom_row)))) next

    # canary breadth (sigmoid-smoothed)
    canary <- canary_set[canary_set %in% colnames(mom_row)]
    canary_mom <- as.numeric(mom_row[, canary])
    breadth <- mean(plogis(canary_mom, scale = 0.05), na.rm = TRUE)

    # smoothed bond fraction
    bf <- 1 - breadth^(1 + A_val)

    # top N risky
    risky_mom <- as.numeric(mom_row[, RISKY_TICKERS])
    names(risky_mom) <- RISKY_TICKERS
    risky_mom <- risky_mom[!is.na(risky_mom)]
    if (length(risky_mom) < TopN) next
    top_n <- names(sort(risky_mom, decreasing = TRUE))[1:TopN]

    # best bond
    bond_mom <- as.numeric(mom_row[, BOND_CANDIDATES])
    names(bond_mom) <- BOND_CANDIDATES
    best_bond <- names(which.max(bond_mom))[1]

    # weights
    new_wts <- setNames(rep(0, length(ALL_TICKERS)), ALL_TICKERS)
    new_wts[top_n] <- (1 - bf) / TopN
    new_wts[best_bond] <- new_wts[best_bond] + bf

    # portfolio return
    rets_row <- as.numeric(mon_ret_xts[as.character(trd_date), ])
    port_ret <- sum(new_wts * rets_row, na.rm = TRUE)
    cost <- sum(abs(new_wts - prev_wts) * COST_MAP[names(new_wts)], na.rm = TRUE)
    port_ret <- port_ret - cost

    rets[t_idx] <- port_ret
    prev_wts <- new_wts
  }
  xts(rets, order.by = trd_dates)
}

# ═══════════════════════════════════════════════════════════════════════════════
# walk-forward runner
# ═══════════════════════════════════════════════════════════════════════════════

run_walkforward_paa <- function(canary_pool, label) {
  cat(sprintf("\n══════ %s ══════\n", label))

  avail_canary <- canary_pool[canary_pool %in% colnames(mon_rets)]
  canary_combos <- list()
  for (c1 in avail_canary) canary_combos[[c1]] <- c1
  if (length(avail_canary) >= 2)
    for (i in 1:(length(avail_canary)-1))
      for (j in (i+1):length(avail_canary))
        canary_combos[[paste(avail_canary[i], avail_canary[j], sep="+")]] <- c(avail_canary[i], avail_canary[j])

  cat(sprintf("  canary candidates: %d\n", length(canary_combos)))

  all_dates <- index(mon_rets)
  all_years <- sort(unique(as.numeric(format(all_dates, "%Y"))))
  warmup_end_year <- as.numeric(format(all_dates[1], "%Y")) + (MIN_TRAIN_MONTHS %/% 12)
  test_years <- all_years[all_years >= warmup_end_year]
  cat(sprintf("  test years: %d → %d (%d years)\n", test_years[1], last(test_years), length(test_years)))

  wf_returns <- list(); wf_canary <- list(); wf_params <- list()

  for (y in test_years) {
    cat(sprintf("\n  ── year %d ──\n", y))
    train_end <- as.Date(sprintf("%d-12-31", y-1))
    test_dates <- all_dates[format(all_dates, "%Y") == as.character(y)]
    if (length(test_dates) == 0) { cat("    no test dates\n"); next }

    train_ret <- mon_rets[paste0("/", as.character(train_end))]
    train_px  <- mon_merged[paste0("/", as.character(train_end))]
    train_dates <- index(train_ret)
    if (length(train_dates) < MIN_TRAIN_MONTHS) {
      cat(sprintf("    only %d training months\n", length(train_dates))); next
    }

    # step 1: select canary (using reference L=12, TopN=5, A=0)
    best_canary_score <- -Inf; best_canary_set <- NULL
    ref_L <- 12; ref_Top <- 5; ref_A <- 0
    for (cn in names(canary_combos)) {
      cset <- canary_combos[[cn]]
      port <- run_paa_portfolio(train_ret, train_px, cset, ref_L, ref_Top, ref_A)
      port <- port[!is.na(coredata(port)), ]
      if (NROW(port) < 36) next
      r <- Return.annualized(port)[1,1]; d <- maxDrawdown(port)
      score <- if (r >= 0 && d <= 0.50) r*(1-d/(1-d)) else 0
      if (score > best_canary_score) { best_canary_score <- score; best_canary_set <- cset }
    }
    if (is.null(best_canary_set)) { cat("    no valid canary\n"); next }
    cat(sprintf("    canary: %s (RAD=%.4f)\n", paste(best_canary_set,collapse="+"), best_canary_score))

    # step 2: select L, TopN, A
    best_param_score <- -Inf; best_L <- L_GRID[1]; best_Top <- TOP_GRID[1]; best_A <- A_GRID[1]
    for (Lv in L_GRID) for (Tv in TOP_GRID) for (Av in A_GRID) {
      port <- run_paa_portfolio(train_ret, train_px, best_canary_set, Lv, Tv, Av)
      port <- port[!is.na(coredata(port)), ]
      if (NROW(port) < 36) next
      r <- Return.annualized(port)[1,1]; d <- maxDrawdown(port)
      score <- if (r >= 0 && d <= 0.50) r*(1-d/(1-d)) else 0
      if (score > best_param_score) { best_param_score <- score; best_L <- Lv; best_Top <- Tv; best_A <- Av }
    }
    cat(sprintf("    params: L=%d, Top=%d, a=%d (RAD=%.4f)\n", best_L, best_Top, best_A, best_param_score))

    # step 3: test year
    test_ret <- mon_rets[as.character(test_dates), ]
    test_px  <- mon_merged[as.character(test_dates), ]
    port <- run_paa_portfolio(test_ret, test_px, best_canary_set, best_L, best_Top, best_A)
    if (NROW(port) > 0) {
      wf_returns[[as.character(y)]] <- port
      wf_canary[[as.character(y)]]  <- best_canary_set
      wf_params[[as.character(y)]]  <- c(L=best_L, Top=best_Top, A=best_A)
    }
  }

  rets <- do.call(rbind, wf_returns)
  rets <- rets[!is.na(coredata(rets)), ]
  names(rets) <- label

  cat(sprintf("\n  canary history (%s):\n", label))
  for (y in names(wf_canary))
    cat(sprintf("    %s: %s (L=%d, Top=%d, a=%d)\n", y,
                paste(wf_canary[[y]],collapse="+"),
                wf_params[[y]]["L"], wf_params[[y]]["Top"], wf_params[[y]]["A"]))

  list(returns=rets, canary=wf_canary, params=wf_params)
}

# ═══════════════════════════════════════════════════════════════════════════════
# run both variants
# ═══════════════════════════════════════════════════════════════════════════════

res_eem  <- run_walkforward_paa(CANARY_POOL_EEM,  "PAA-EEM")
res_sphb <- run_walkforward_paa(CANARY_POOL_SPHB, "PAA-SPHB")

# ═══════════════════════════════════════════════════════════════════════════════
# align on common dates
# ═══════════════════════════════════════════════════════════════════════════════

cat("\naligning on common date range...\n")
common_start <- max(first(index(res_eem$returns)), first(index(res_sphb$returns)))
common_end   <- min(last(index(res_eem$returns)),  last(index(res_sphb$returns)))
cat(sprintf("  common range: %s → %s\n",
            as.character(as.Date(common_start)), as.character(as.Date(common_end))))

paa_eem  <- res_eem$returns[paste0(common_start, "/", common_end)]
paa_sphb <- res_sphb$returns[paste0(common_start, "/", common_end)]
common_ret <- mon_rets[paste0(common_start, "/", common_end), ]

# benchmarks
ew_wts <- rep(1/N_RISKY, N_RISKY); names(ew_wts) <- RISKY_TICKERS
ew_rets <- xts(rowSums(coredata(common_ret[,RISKY_TICKERS]) *
               matrix(ew_wts, nrow=NROW(common_ret), ncol=N_RISKY, byrow=TRUE), na.rm=TRUE),
               order.by=index(common_ret)); names(ew_rets) <- "EW"

NC <- length(BOND_CANDIDATES); ewc_wts <- rep(1/NC, NC); names(ewc_wts) <- BOND_CANDIDATES
ewc_rets <- xts(rowSums(coredata(common_ret[,BOND_CANDIDATES]) *
                matrix(ewc_wts, nrow=NROW(common_ret), ncol=NC, byrow=TRUE), na.rm=TRUE),
                order.by=index(common_ret)); names(ewc_rets) <- "EWC"

sixty40_rets <- xts(0.6*coredata(common_ret[,"SPY"]) + 0.4*coredata(common_ret[,"IEF"]),
                    order.by=index(common_ret)); names(sixty40_rets) <- "60/40"
spy_rets <- common_ret[,"SPY"]; names(spy_rets) <- "SPY"

all_strats <- na.omit(merge(paa_eem, paa_sphb, ew_rets, ewc_rets, sixty40_rets, spy_rets))
cat(sprintf("  combined: %d rows, %d cols\n", NROW(all_strats), ncol(all_strats)))

# ═══════════════════════════════════════════════════════════════════════════════
# IS/OS/FS metrics
# ═══════════════════════════════════════════════════════════════════════════════

compute_metrics <- function(ret_xts, ewc_xts=NULL) {
  if (NROW(ret_xts) < 3) return(NULL)
  r <- Return.annualized(ret_xts)[1,1]; v <- StdDev.annualized(ret_xts)[1,1]; d <- maxDrawdown(ret_xts)
  sharpe <- if (!is.null(ewc_xts)) SharpeRatio.annualized(ret_xts-ewc_xts[index(ret_xts)])[1,1]
            else SharpeRatio.annualized(ret_xts)[1,1]
  mar <- r/d; rad <- if (r >= 0 && d <= 0.50) r*(1-d/(1-d)) else 0
  list(R=r, V=v, D=d, Sharpe=sharpe, MAR=mar, RAD=rad)
}

midpoint <- common_start + (common_end - common_start)/2
mid_diff <- abs(index(all_strats) - midpoint)
is_end_date <- index(all_strats)[which.min(mid_diff)]
cat(sprintf("  IS/OS split: %s\n", as.character(as.Date(is_end_date))))

all_period_metrics <- data.frame()
for (nm in colnames(all_strats)) {
  ret_x <- all_strats[, nm]
  for (period in c("IS","OS","FS")) {
    sub <- switch(period,
      IS = ret_x[paste0("/", as.character(is_end_date))],
      OS = ret_x[paste0(as.character(is_end_date), "/")],
      FS = ret_x)
    if (NROW(sub) < 12) next
    m <- compute_metrics(sub, ewc_rets)
    if (!is.null(m))
      all_period_metrics <- rbind(all_period_metrics, data.frame(
        Period=paste(nm, period), R=m$R*100, V=m$V*100, D=abs(m$D)*100,
        Sharpe=m$Sharpe, MAR=m$MAR, RAD=m$RAD, stringsAsFactors=FALSE))
  }
}

metrics_tbl <- all_period_metrics |>
  mutate(Strategy=sub(" (IS|OS|FS)$","",Period),
         Window=recode(sub(".* (IS|OS|FS)$","\\1",Period), IS="In-Sample", OS="Out-of-Sample", FS="Full Sample")) |>
  select(Strategy, Window, R, V, D, Sharpe, MAR, RAD) |>
  mutate(R=R/100, V=V/100, D=D/100) |>
  arrange(factor(Window, levels=c("In-Sample","Out-of-Sample","Full Sample")),
          factor(Strategy, levels=c("PAA.EEM","PAA.SPHB","EW","EWC","60/40","SPY")))

# ═══════════════════════════════════════════════════════════════════════════════
# canary stability
# ═══════════════════════════════════════════════════════════════════════════════

for (variant in c("EEM","SPHB")) {
  res <- if (variant=="EEM") res_eem else res_sphb
  cat(sprintf("\ncanary stability — %s pool:\n", variant))
  freq <- table(sapply(res$canary, paste, collapse="+"))
  print(sort(freq, decreasing=TRUE))
}

# ═══════════════════════════════════════════════════════════════════════════════
# gt metrics table
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding metrics table...\n")

STRAT_COLORS <- c(
  `PAA.EEM`  = "#f0f4ff",
  `PAA.SPHB` = "#f0fff0",
  EW="", EWC="", `60/40`="", SPY=""  # inherit default
)
STRAT_COLORS[STRAT_COLORS==""] <- "#f5f5f5"

tbl <- metrics_tbl |> gt(groupname_col="Window") |>
  tab_header(title="PAA-R Walk-Forward — EEM vs SPHB Canary",
    subtitle=sprintf("SMA Momentum + Smoothed Bond Fraction | %s → %s | Tiered costs",
                     as.character(as.Date(first(index(all_strats)))),
                     as.character(as.Date(last(index(all_strats)))))) |>
  cols_label(Strategy="", R="CAGR %", V="Vol %", D="MaxDD %", Sharpe="Sharpe", MAR="MAR", RAD="RAD") |>
  fmt_percent(columns=c(R,V,D), decimals=1) |> fmt_number(columns=c(Sharpe,MAR), decimals=2) |>
  fmt_number(columns=RAD, decimals=4) |>
  tab_style(style=cell_text(weight="bold"), locations=cells_column_labels()) |>
  tab_style(style=cell_text(weight="bold"), locations=cells_row_groups()) |>
  tab_source_note(source_note="@StockViz") |>
  tab_style(style=cell_text(align="right"), locations=cells_source_notes())

for (s in names(STRAT_COLORS)) {
  rows <- which(metrics_tbl$Strategy == s)
  if (length(rows) > 0) tbl <- tbl |> tab_style(style=cell_fill(color=STRAT_COLORS[s]), locations=cells_body(rows=rows))
}
for (cn in "R") {
  neg <- which(metrics_tbl[[cn]] < 0)
  if (length(neg) > 0) tbl <- tbl |> tab_style(style=cell_text(color="#8B0000"), locations=cells_body(columns=all_of(cn), rows=neg))
  hi <- which(metrics_tbl[[cn]] > 0.02)
  if (length(hi) > 0) tbl <- tbl |> tab_style(style=cell_text(color="#006400"), locations=cells_body(columns=all_of(cn), rows=hi))
}
hi_dd <- which(metrics_tbl$D > 0.20)
if (length(hi_dd) > 0) tbl <- tbl |> tab_style(style=cell_text(color="#8B0000", weight="bold"), locations=cells_body(columns="D", rows=hi_dd))

print(tbl)
gtsave(tbl, sprintf("%s/paa-metrics.html", reportPath))
webshot2::webshot(sprintf("%s/paa-metrics.html", reportPath), sprintf("%s/paa-metrics.png", reportPath),
                  selector="table.gt_table", expand=c(10,10,10,10))

# ═══════════════════════════════════════════════════════════════════════════════
# charts
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding charts...\n")
main_strats <- c("PAA.EEM","PAA.SPHB","60/40","EW","SPY")
to_plot <- all_strats[, main_strats[main_strats %in% colnames(all_strats)]]

sr <- sapply(colnames(to_plot), function(nm) round(SharpeRatio.annualized(to_plot[,nm])[1,1],2))
sr_text <- paste0(colnames(to_plot), "=", sr, collapse=", ")

Common.PlotCumReturns(to_plot, "PAA-R — EEM vs SPHB Canary",
  sprintf("SMA Momentum + Smoothed Bond Fraction | %s → %s | SR: %s",
          as.character(as.Date(first(index(to_plot)))),
          as.character(as.Date(last(index(to_plot)))), sr_text),
  sprintf("%s/paa-cumulative.png", reportPath), NULL)

# OS
os_split <- as.character(is_end_date)
os_plot <- to_plot[paste0(os_split, "/")]
if (NROW(os_plot) > 12) {
  sr_os <- sapply(colnames(os_plot), function(nm) round(SharpeRatio.annualized(os_plot[,nm])[1,1],2))
  sr_os_text <- paste0(colnames(os_plot), "=", sr_os, collapse=", ")
  Common.PlotCumReturns(os_plot, "PAA-R — Recent (2020-07 →)",
    sprintf("SMA Momentum + Smoothed BF | %s → %s | SR: %s",
            as.character(as.Date(first(index(os_plot)))),
            as.character(as.Date(last(index(os_plot)))), sr_os_text),
    sprintf("%s/paa-cumulative-os.png", reportPath), NULL)
}

png(sprintf("%s/paa-drawdowns.png", reportPath), width=1400, height=800, bg="white")
chart.Drawdown(to_plot, main="PAA-R Drawdowns — EEM vs SPHB", legend.loc="bottomleft", ylab="Drawdown")
mtext("@StockViz", side=4, col='grey')
dev.off()

# ═══════════════════════════════════════════════════════════════════════════════
# save
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nsaving results...\n")
save(all_strats, res_eem, res_sphb, all_period_metrics, metrics_tbl, is_end_date,
     common_ret, mon_merged, file=sprintf("%s/paa-results.Rdata", reportPath))

cat("\nDone.\n")
for (nm in c("PAA.EEM","PAA.SPHB")) {
  r <- Return.annualized(all_strats[,nm])[1,1]*100
  d <- -maxDrawdown(all_strats[,nm])*100
  s <- SharpeRatio.annualized(all_strats[,nm])[1,1]
  cat(sprintf("  %s: CAGR=%.1f%%  MaxDD=%.1f%%  Sharpe=%.2f\n", nm, r, d, s))
}
