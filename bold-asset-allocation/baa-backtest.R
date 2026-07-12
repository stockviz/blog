# BAA-R (Bold Asset Allocation — Robust)
# Keller (2022) — SMA Offensive + 13612W Canary + Multi-Asset Defensive
# Walk-forward canary/parameter selection, smoothed breadth, tiered costs
# Two offensive variants (SPY/QQQ) × two canary pools (EEM/SPHB)

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
# universe
# ═══════════════════════════════════════════════════════════════════════════════

OFFENSIVE_BASE  <- c("SPY", "IWM", "EFA", "EEM", "VNQ", "DBC", "GLD", "TLT", "HYG", "LQD")
OFFENSIVE_QQQ   <- c("QQQ", "IWM", "EFA", "EEM", "VNQ", "DBC", "GLD", "TLT", "HYG", "LQD")
DEFENSIVE_TICKERS <- c("BIL", "IEF", "SHY", "TLT", "LQD", "AGG")
CANARY_POOL_EEM  <- c("EEM", "AGG")
CANARY_POOL_SPHB <- c("SPHB", "AGG")

ALL_TICKERS <- unique(c(OFFENSIVE_BASE, OFFENSIVE_QQQ, DEFENSIVE_TICKERS,
                        CANARY_POOL_EEM, CANARY_POOL_SPHB))
N_OFF <- length(OFFENSIVE_BASE)

# Tiered costs
COST_TIER1 <- 0.0005; COST_TIER2 <- 0.0015
TIER1 <- c("SPY","QQQ","IWM","EFA","EEM","GLD","TLT","IEF","SHY","BIL","AGG")
TIER2 <- c("HYG","LQD","VNQ","DBC","SPHB")
COST_MAP <- setNames(c(rep(COST_TIER1,length(TIER1)), rep(COST_TIER2,length(TIER2))), c(TIER1,TIER2))

# Pre-registered grid
B_GRID <- c(1, 2)
TOP_GRID <- c(3, 5, 7)
MIN_TRAIN_MONTHS <- 60
DEF_TOP <- 3  # Top3 defensive selection (fixed per paper)

# ═══════════════════════════════════════════════════════════════════════════════
# connect + load
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
  cat(sprintf("  %s: %d rows\n", sym, nrow(pDf)))
}
odbcClose(lconUS2)

# ═══════════════════════════════════════════════════════════════════════════════
# monthly prices + momentum (both SMA and 13612W)
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

cat(sprintf("  merged: %d months, %d tickers, %s → %s\n",
            nrow(mon_merged), ncol(mon_merged),
            as.character(as.Date(first(index(mon_merged)))),
            as.character(as.Date(last(index(mon_merged))))))

# 13612W momentum
cat("computing 13612W momentum...\n")
compute_13612w <- function(px) {
  n <- nrow(px); mom <- rep(NA_real_, n)
  for (i in 13:n) {
    p0 <- coredata(px)[i]; p1 <- coredata(px)[i-1]; p3 <- coredata(px)[i-3]
    p6 <- coredata(px)[i-6]; p12 <- coredata(px)[i-12]
    if (any(is.na(c(p1,p3,p6,p12))) || any(c(p1,p3,p6,p12)==0)) next
    mom[i] <- (12*(p0/p1-1)+4*(p0/p3-1)+2*(p0/p6-1)+1*(p0/p12-1))/4
  }
  xts(mom, order.by=index(px))
}
mom_13612w <- na.omit(do.call(merge.xts, lapply(names(monthly_prices),
  function(sym) compute_13612w(monthly_prices[[sym]]))))
names(mom_13612w) <- names(monthly_prices)
cat(sprintf("  13612W: %d rows\n", nrow(mom_13612w)))

# ═══════════════════════════════════════════════════════════════════════════════
# BAA portfolio runner
# ═══════════════════════════════════════════════════════════════════════════════

run_baa_portfolio <- function(mon_ret_xts, price_xts, mom_xts, offensive_set,
                               canary_set, B_val, TopN) {
  trd_dates <- index(mon_ret_xts)
  n_trd <- length(trd_dates)
  rets <- rep(NA_real_, n_trd)
  prev_wts <- setNames(rep(0, length(ALL_TICKERS)), ALL_TICKERS)

  # SMA(12) for offensive momentum (BAA uses fixed SMA12 for offense)
  SMA_L <- 12
  effL <- min(SMA_L, nrow(price_xts)-1)
  sma_cols <- lapply(colnames(price_xts), function(sym) {
    s <- SMA(price_xts[,sym], effL)
    s[is.na(coredata(s)),] <- coredata(price_xts[,sym])[is.na(coredata(s)),]
    s
  })
  sma_xts <- do.call(merge.xts, sma_cols); names(sma_xts) <- colnames(price_xts)
  cd <- index(price_xts)
  off_mom <- xts(log(coredata(price_xts[cd])/coredata(sma_xts[cd])), order.by=cd)

  for (t_idx in seq_len(n_trd)) {
    trd_date <- trd_dates[t_idx]

    # Signal from prior date (separate for SMA and 13612W)
    off_dates <- index(off_mom)
    off_sig <- max(which(off_dates < trd_date))
    if (off_sig < 1 || is.infinite(off_sig)) next
    off_sig_date <- off_dates[off_sig]

    can_dates <- index(mom_xts)
    can_sig <- max(which(can_dates < trd_date))
    if (can_sig < 1 || is.infinite(can_sig)) next
    can_sig_date <- can_dates[can_sig]

    # Offensive selection: SMA(12) momentum, top TopN
    off_row <- off_mom[as.character(off_sig_date), offensive_set]
    off_mom_vals <- as.numeric(off_row); names(off_mom_vals) <- offensive_set
    off_mom_vals <- off_mom_vals[!is.na(off_mom_vals)]
    if (length(off_mom_vals) < TopN) next
    top_off <- names(sort(off_mom_vals, decreasing=TRUE))[1:TopN]

    # Canary breadth: 13612W, smoothed sigmoid
    can_row <- mom_xts[as.character(can_sig_date), canary_set[canary_set %in% colnames(mom_xts)]]
    can_vals <- as.numeric(can_row)
    scores <- plogis(can_vals, scale=0.05)
    breadth <- mean(scores, na.rm=TRUE)

    # Defensive fraction: smooth, 1 - breadth^B
    df <- 1 - breadth^B_val

    # Defensive selection: 13612W momentum, absolute (only > 0) + relative, top DEF_TOP
    def_row <- mom_xts[as.character(can_sig_date), DEFENSIVE_TICKERS]
    def_mom_vals <- as.numeric(def_row); names(def_mom_vals) <- DEFENSIVE_TICKERS
    # Absolute momentum filter: only assets with 13612W > 0
    good_def <- def_mom_vals[def_mom_vals > 0 & !is.na(def_mom_vals)]
    if (length(good_def) == 0) {
      # Fallback: best cash by momentum regardless of sign
      best_def <- names(which.max(def_mom_vals))[1]
      top_def <- best_def
    } else {
      top_def <- names(sort(good_def, decreasing=TRUE))[1:min(DEF_TOP, length(good_def))]
    }

    # Weights
    new_wts <- setNames(rep(0, length(ALL_TICKERS)), ALL_TICKERS)
    new_wts[top_off] <- (1 - df) / TopN
    new_wts[top_def] <- new_wts[top_def] + df / length(top_def)

    # Return
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

run_walkforward_baa <- function(offensive_set, canary_pool, label) {
  cat(sprintf("\n══════ %s ══════\n", label))

  avail_canary <- canary_pool[canary_pool %in% colnames(mon_rets)]
  canary_combos <- list()
  for (c1 in avail_canary) canary_combos[[c1]] <- c1
  if (length(avail_canary) >= 2)
    for (i in 1:(length(avail_canary)-1))
      for (j in (i+1):length(avail_canary))
        canary_combos[[paste(avail_canary[i],avail_canary[j],sep="+")]] <- c(avail_canary[i],avail_canary[j])

  cat(sprintf("  canary candidates: %d, offensive: %s\n", length(canary_combos),
              paste(offensive_set[1:2],collapse="/")))

  all_dates <- index(mon_rets)
  all_years <- sort(unique(as.numeric(format(all_dates,"%Y"))))
  warmup_end_year <- as.numeric(format(all_dates[1],"%Y")) + (MIN_TRAIN_MONTHS %/% 12)
  test_years <- all_years[all_years >= warmup_end_year]
  cat(sprintf("  test years: %d → %d (%d)\n", test_years[1], last(test_years), length(test_years)))

  wf_returns <- list(); wf_canary <- list(); wf_params <- list()

  for (y in test_years) {
    cat(sprintf("\n  ── %d ──\n", y))
    train_end <- as.Date(sprintf("%d-12-31", y-1))
    test_dates <- all_dates[format(all_dates,"%Y")==as.character(y)]
    if (length(test_dates)==0) { cat("    no test dates\n"); next }

    train_ret <- mon_rets[paste0("/",as.character(train_end))]
    train_px  <- mon_merged[paste0("/",as.character(train_end))]
    train_mom <- mom_13612w[paste0("/",as.character(train_end))]
    train_idx <- index(train_ret)
    if (length(train_idx) < MIN_TRAIN_MONTHS) { cat(sprintf("    only %d months\n", length(train_idx))); next }

    # step 1: select canary (ref: B=1, TopN=5)
    best_score <- -Inf; best_cset <- NULL
    for (cn in names(canary_combos)) {
      cset <- canary_combos[[cn]]
      port <- run_baa_portfolio(train_ret, train_px, train_mom, offensive_set, cset, 1, 5)
      port <- port[!is.na(coredata(port)),]
      if (NROW(port) < 36) next
      r <- Return.annualized(port)[1,1]; d <- maxDrawdown(port)
      sc <- if (r>=0 && d<=0.50) r*(1-d/(1-d)) else 0
      if (sc > best_score) { best_score <- sc; best_cset <- cset }
    }
    if (is.null(best_cset)) { cat("    no valid canary\n"); next }
    cat(sprintf("    canary: %s (RAD=%.4f)\n", paste(best_cset,collapse="+"), best_score))

    # step 2: select B, TopN
    best_ps <- -Inf; best_B <- B_GRID[1]; best_Top <- TOP_GRID[1]
    for (Bv in B_GRID) for (Tv in TOP_GRID) {
      port <- run_baa_portfolio(train_ret, train_px, train_mom, offensive_set, best_cset, Bv, Tv)
      port <- port[!is.na(coredata(port)),]
      if (NROW(port) < 36) next
      r <- Return.annualized(port)[1,1]; d <- maxDrawdown(port)
      sc <- if (r>=0 && d<=0.50) r*(1-d/(1-d)) else 0
      if (sc > best_ps) { best_ps <- sc; best_B <- Bv; best_Top <- Tv }
    }
    cat(sprintf("    params: B=%d, Top=%d (RAD=%.4f)\n", best_B, best_Top, best_ps))

    # step 3: test year
    test_ret <- mon_rets[as.character(test_dates),]
    test_px  <- mon_merged[as.character(test_dates),]
    test_mom <- mom_13612w[as.character(test_dates),]
    port <- run_baa_portfolio(test_ret, test_px, test_mom, offensive_set, best_cset, best_B, best_Top)
    if (NROW(port)>0) {
      wf_returns[[as.character(y)]] <- port
      wf_canary[[as.character(y)]] <- best_cset
      wf_params[[as.character(y)]] <- c(B=best_B, Top=best_Top)
    }
  }

  rets <- do.call(rbind, wf_returns)
  rets <- rets[!is.na(coredata(rets)),]; names(rets) <- label

  cat(sprintf("\n  history (%s):\n", label))
  for (y in names(wf_canary))
    cat(sprintf("    %s: %s (B=%d, Top=%d)\n", y,
                paste(wf_canary[[y]],collapse="+"), wf_params[[y]]["B"], wf_params[[y]]["Top"]))

  list(returns=rets, canary=wf_canary, params=wf_params)
}

# ═══════════════════════════════════════════════════════════════════════════════
# run: 2 offensive × 2 canary = 4 variants
# ═══════════════════════════════════════════════════════════════════════════════

res_spy_eem   <- run_walkforward_baa(OFFENSIVE_BASE, CANARY_POOL_EEM,  "BAA-SPY-EEM")
res_spy_sphb  <- run_walkforward_baa(OFFENSIVE_BASE, CANARY_POOL_SPHB, "BAA-SPY-SPHB")
res_qqq_eem   <- run_walkforward_baa(OFFENSIVE_QQQ,  CANARY_POOL_EEM,  "BAA-QQQ-EEM")
res_qqq_sphb  <- run_walkforward_baa(OFFENSIVE_QQQ,  CANARY_POOL_SPHB, "BAA-QQQ-SPHB")

# ═══════════════════════════════════════════════════════════════════════════════
# align on common dates
# ═══════════════════════════════════════════════════════════════════════════════

all_res <- list(res_spy_eem, res_spy_sphb, res_qqq_eem, res_qqq_sphb)
common_start <- max(sapply(all_res, function(r) first(index(r$returns))))
common_end   <- min(sapply(all_res, function(r) last(index(r$returns))))
cat(sprintf("\ncommon range: %s → %s\n",
            as.character(as.Date(common_start)), as.character(as.Date(common_end))))

baa_rets <- do.call(merge.xts, lapply(all_res, function(r)
  r$returns[paste0(common_start,"/",common_end)]))
common_ret <- mon_rets[paste0(common_start,"/",common_end),]

# Benchmarks
ew_wts <- rep(1/N_OFF, N_OFF); names(ew_wts) <- OFFENSIVE_BASE
ew_rets <- xts(rowSums(coredata(common_ret[,OFFENSIVE_BASE]) *
               matrix(ew_wts,nrow=NROW(common_ret),ncol=N_OFF,byrow=TRUE),na.rm=TRUE),
               order.by=index(common_ret)); names(ew_rets) <- "EW"

NC <- length(DEFENSIVE_TICKERS); ewc_wts <- rep(1/NC,NC)
ewc_rets <- xts(rowSums(coredata(common_ret[,DEFENSIVE_TICKERS]) *
                matrix(ewc_wts,nrow=NROW(common_ret),ncol=NC,byrow=TRUE),na.rm=TRUE),
                order.by=index(common_ret)); names(ewc_rets) <- "EWC"

sixty40_rets <- xts(0.6*coredata(common_ret[,"SPY"])+0.4*coredata(common_ret[,"IEF"]),
                    order.by=index(common_ret)); names(sixty40_rets) <- "60/40"
spy_rets <- common_ret[,"SPY"]; names(spy_rets) <- "SPY"

# QQQ equal-weight (QQQ-based offensive universe)
N_QQQ <- length(OFFENSIVE_QQQ)
qqq_ew_wts <- rep(1/N_QQQ, N_QQQ); names(qqq_ew_wts) <- OFFENSIVE_QQQ
qqq_ew_rets <- xts(rowSums(coredata(common_ret[,OFFENSIVE_QQQ]) *
                   matrix(qqq_ew_wts,nrow=NROW(common_ret),ncol=N_QQQ,byrow=TRUE),na.rm=TRUE),
                   order.by=index(common_ret)); names(qqq_ew_rets) <- "EW-Q"

all_strats <- na.omit(merge(baa_rets, ew_rets, qqq_ew_rets, ewc_rets, sixty40_rets, spy_rets))
cat(sprintf("  combined: %d rows, %d cols\n", NROW(all_strats), ncol(all_strats)))

# ═══════════════════════════════════════════════════════════════════════════════
# IS/OS/FS metrics
# ═══════════════════════════════════════════════════════════════════════════════

compute_metrics <- function(ret_xts, ewc_xts=NULL) {
  if (NROW(ret_xts) < 3) return(NULL)
  r <- Return.annualized(ret_xts)[1,1]; v <- StdDev.annualized(ret_xts)[1,1]; d <- maxDrawdown(ret_xts)
  sharpe <- if (!is.null(ewc_xts)) SharpeRatio.annualized(ret_xts-ewc_xts[index(ret_xts)])[1,1]
            else SharpeRatio.annualized(ret_xts)[1,1]
  mar <- r/d; rad <- if (r>=0 && d<=0.50) r*(1-d/(1-d)) else 0
  list(R=r,V=v,D=d,Sharpe=sharpe,MAR=mar,RAD=rad)
}

midpoint <- common_start+(common_end-common_start)/2
mid_diff <- abs(as.numeric(index(all_strats)) - as.numeric(midpoint))
is_end_date <- index(all_strats)[which.min(mid_diff)]
cat(sprintf("  IS/OS split: %s\n", as.character(as.Date(is_end_date))))

all_period_metrics <- data.frame()
for (nm in colnames(all_strats)) {
  ret_x <- all_strats[,nm]
  for (period in c("IS","OS","FS")) {
    sub <- switch(period, IS=ret_x[paste0("/",as.character(is_end_date))],
                  OS=ret_x[paste0(as.character(is_end_date),"/")], FS=ret_x)
    if (NROW(sub) < 12) next
    m <- compute_metrics(sub, ewc_rets)
    if (!is.null(m))
      all_period_metrics <- rbind(all_period_metrics, data.frame(
        Period=paste(nm,period), R=m$R*100, V=m$V*100, D=abs(m$D)*100,
        Sharpe=m$Sharpe, MAR=m$MAR, RAD=m$RAD, stringsAsFactors=FALSE))
  }
}

metrics_tbl <- all_period_metrics |>
  mutate(Strategy=sub(" (IS|OS|FS)$","",Period),
         Window=recode(sub(".* (IS|OS|FS)$","\\1",Period), IS="In-Sample",OS="Out-of-Sample",FS="Full Sample")) |>
  select(Strategy,Window,R,V,D,Sharpe,MAR,RAD) |>
  mutate(R=R/100,V=V/100,D=D/100) |>
  arrange(factor(Window,levels=c("In-Sample","Out-of-Sample","Full Sample")),
          factor(Strategy,levels=c("BAA.SPY.EEM","BAA.SPY.SPHB","BAA.QQQ.EEM","BAA.QQQ.SPHB","EW","EW.Q","EWC","60/40","SPY")))

# ═══════════════════════════════════════════════════════════════════════════════
# gt table
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding metrics table...\n")

STRAT_COLORS <- c(
  `BAA.SPY.EEM`="#f0f4ff", `BAA.SPY.SPHB`="#f0f4ff",
  `BAA.QQQ.EEM`="#f0fff0", `BAA.QQQ.SPHB`="#f0fff0",
  EW="#f5f5f5", `EW.Q`="#f5f5f5", EWC="#f5f5f5", `60/40`="#f5f5f5", SPY="#f5f5f5")

tbl <- metrics_tbl |> gt(groupname_col="Window") |>
  tab_header(title="BAA-R Walk-Forward — SPY vs QQQ, EEM vs SPHB Canary",
    subtitle=sprintf("SMA Offensive + 13612W Canary + Multi-Asset Defensive | %s → %s",
                     as.character(as.Date(first(index(all_strats)))),
                     as.character(as.Date(last(index(all_strats)))))) |>
  cols_label(Strategy="",R="CAGR %",V="Vol %",D="MaxDD %",Sharpe="Sharpe",MAR="MAR",RAD="RAD") |>
  fmt_percent(columns=c(R,V,D),decimals=1) |> fmt_number(columns=c(Sharpe,MAR),decimals=2) |>
  fmt_number(columns=RAD,decimals=4) |>
  tab_style(style=cell_text(weight="bold"),locations=cells_column_labels()) |>
  tab_style(style=cell_text(weight="bold"),locations=cells_row_groups()) |>
  tab_source_note(source_note="@StockViz") |>
  tab_style(style=cell_text(align="right"),locations=cells_source_notes())

for (s in names(STRAT_COLORS)) {
  rows <- which(metrics_tbl$Strategy==s)
  if (length(rows)>0) tbl <- tbl |> tab_style(style=cell_fill(color=STRAT_COLORS[s]),locations=cells_body(rows=rows))
}
for (cn in "R") {
  neg <- which(metrics_tbl[[cn]]<0)
  if (length(neg)>0) tbl <- tbl |> tab_style(style=cell_text(color="#8B0000"),locations=cells_body(columns=all_of(cn),rows=neg))
  hi <- which(metrics_tbl[[cn]]>0.02)
  if (length(hi)>0) tbl <- tbl |> tab_style(style=cell_text(color="#006400"),locations=cells_body(columns=all_of(cn),rows=hi))
}
hi_dd <- which(metrics_tbl$D>0.20)
if (length(hi_dd)>0) tbl <- tbl |> tab_style(style=cell_text(color="#8B0000",weight="bold"),locations=cells_body(columns="D",rows=hi_dd))

print(tbl)
gtsave(tbl,sprintf("%s/baa-metrics.html",reportPath))
webshot2::webshot(sprintf("%s/baa-metrics.html",reportPath),sprintf("%s/baa-metrics.png",reportPath),
                  selector="table.gt_table",expand=c(10,10,10,10))

# ═══════════════════════════════════════════════════════════════════════════════
# charts
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding charts...\n")
main_strats <- c("BAA.SPY.EEM","BAA.SPY.SPHB","BAA.QQQ.EEM","BAA.QQQ.SPHB","60/40","EW","EW.Q","SPY")
to_plot <- all_strats[, main_strats[main_strats %in% colnames(all_strats)]]

sr <- sapply(colnames(to_plot),function(nm) round(SharpeRatio.annualized(to_plot[,nm])[1,1],2))
sr_text <- paste0(colnames(to_plot),"=",sr,collapse=", ")

Common.PlotCumReturns(to_plot,"BAA-R — SPY vs QQQ, EEM vs SPHB",
  sprintf("SMA Offensive + 13612W Canary | %s → %s | SR: %s",
          as.character(as.Date(first(index(to_plot)))),
          as.character(as.Date(last(index(to_plot)))),sr_text),
  sprintf("%s/baa-cumulative.png",reportPath),NULL)

os_split <- as.character(is_end_date)
os_plot <- to_plot[paste0(os_split,"/")]
if (NROW(os_plot)>12) {
  sr_os <- sapply(colnames(os_plot),function(nm) round(SharpeRatio.annualized(os_plot[,nm])[1,1],2))
  Common.PlotCumReturns(os_plot,"BAA-R — Recent (2020-07 →)",
    sprintf("SMA + 13612W | %s → %s | SR: %s",
            as.character(as.Date(first(index(os_plot)))),
            as.character(as.Date(last(index(os_plot)))),paste0(colnames(os_plot),"=",sr_os,collapse=", ")),
    sprintf("%s/baa-cumulative-os.png",reportPath),NULL)
}

png(sprintf("%s/baa-drawdowns.png",reportPath),width=1400,height=800,bg="white")
chart.Drawdown(to_plot,main="BAA-R Drawdowns",legend.loc="bottomleft",ylab="Drawdown")
mtext("@StockViz",side=4,col='grey')
dev.off()

# ═══════════════════════════════════════════════════════════════════════════════
# save
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nsaving results...\n")
save(all_strats,all_res,all_period_metrics,metrics_tbl,is_end_date,common_ret,mon_merged,
     file=sprintf("%s/baa-results.Rdata",reportPath))

cat("\nDone.\n")
for (nm in colnames(baa_rets)) {
  r <- Return.annualized(all_strats[,nm])[1,1]*100
  d <- -maxDrawdown(all_strats[,nm])*100
  s <- SharpeRatio.annualized(all_strats[,nm])[1,1]
  cat(sprintf("  %s: CAGR=%.1f%%  MaxDD=%.1f%%  Sharpe=%.2f\n", nm, r, d, s))
}
