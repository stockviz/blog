# ABA — Adaptive Breadth Allocation
# SMA offensive + 13612W defensive (self-breadth, no canary)
# Smoothed sigmoid breadth + hysteresis + simple cash sleeve
# Two offensive variants: SPY-based and MTUM-based

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

OFFENSIVE_SPY  <- c("SPY", "IWM", "EFA", "EEM", "VNQ", "DBC", "GLD", "TLT", "HYG", "LQD")
OFFENSIVE_MTUM <- c("MTUM", "IWM", "EFA", "EEM", "VNQ", "DBC", "GLD", "TLT", "HYG", "LQD")
DEFENSIVE_TICKERS <- c("BIL", "IEF", "SHY")

ALL_TICKERS <- unique(c(OFFENSIVE_SPY, OFFENSIVE_MTUM, DEFENSIVE_TICKERS))
N_OFF <- length(OFFENSIVE_SPY)

# Tiered costs
COST_TIER1 <- 0.0005; COST_TIER2 <- 0.0015
TIER1 <- c("SPY","MTUM","IWM","EFA","EEM","GLD","TLT","IEF","SHY","BIL")
TIER2 <- c("HYG","LQD","VNQ","DBC")
COST_MAP <- setNames(c(rep(COST_TIER1,length(TIER1)),rep(COST_TIER2,length(TIER2))),c(TIER1,TIER2))

# Pre-registered grid
L_GRID    <- c(6, 12)       # SMA lookback
TOP_GRID  <- c(3, 5)        # offensive TopN
B_GRID    <- c(1, 2, 3)     # breadth exponent
# 12 combinations

MIN_TRAIN_MONTHS <- 60
SIGMA <- 0.05                # sigmoid steepness
MAX_DF_CHANGE <- 0.25        # hysteresis cap

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
# monthly prices + 13612W momentum
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

# 13612W momentum (for defensive/breadth)
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
# ABA portfolio runner
# ═══════════════════════════════════════════════════════════════════════════════

run_aba_portfolio <- function(mon_ret_xts, price_xts, mom_xts, offensive_set, L, TopN, B_val) {
  trd_dates <- index(mon_ret_xts)
  n_trd <- length(trd_dates)
  rets <- rep(NA_real_, n_trd)
  prev_wts <- setNames(rep(0, length(ALL_TICKERS)), ALL_TICKERS)
  prev_df  <- 0

  # SMA(L) momentum for offensive selection
  effL <- min(L, nrow(price_xts)-1)
  sma_cols <- lapply(colnames(price_xts), function(sym) {
    s <- SMA(price_xts[,sym], effL)
    s[is.na(coredata(s)),] <- coredata(price_xts[,sym])[is.na(coredata(s)),]
    s
  })
  sma_xts <- do.call(merge.xts, sma_cols); names(sma_xts) <- colnames(price_xts)
  cd <- index(price_xts)
  offensive_mom <- xts(log(coredata(price_xts[cd])/coredata(sma_xts[cd])), order.by=cd)

  for (t_idx in seq_len(n_trd)) {
    trd_date <- trd_dates[t_idx]

    # signal from prior date for both filters
    off_dates <- index(offensive_mom)
    off_sig <- max(which(off_dates < trd_date))
    if (off_sig < 1 || is.infinite(off_sig)) next
    off_sig_date <- off_dates[off_sig]

    def_dates <- index(mom_xts)
    def_sig <- max(which(def_dates < trd_date))
    if (def_sig < 1 || is.infinite(def_sig)) next
    def_sig_date <- def_dates[def_sig]

    # ── offensive selection: SMA(L), TopN ──
    off_row <- offensive_mom[as.character(off_sig_date), offensive_set]
    off_vals <- as.numeric(off_row); names(off_vals) <- offensive_set
    off_vals <- off_vals[!is.na(off_vals)]
    if (length(off_vals) < TopN) next
    top_off <- names(sort(off_vals, decreasing=TRUE))[1:TopN]

    # ── defensive fraction: 13612W breadth (self-breadth from offensive universe) ──
    def_row <- mom_xts[as.character(def_sig_date), offensive_set]
    def_vals <- as.numeric(def_row)
    scores <- plogis(def_vals / SIGMA)    # sigmoid smoothing
    breadth <- mean(scores, na.rm=TRUE)
    df_new <- 1 - breadth^B_val

    # hysteresis
    df_change <- df_new - prev_df
    df_change <- max(min(df_change, MAX_DF_CHANGE), -MAX_DF_CHANGE)
    df <- prev_df + df_change

    # ── defensive asset selection ──
    cash_row <- mom_xts[as.character(def_sig_date), DEFENSIVE_TICKERS]
    cash_vals <- as.numeric(cash_row); names(cash_vals) <- DEFENSIVE_TICKERS
    best_cash <- names(which.max(cash_vals))[1]

    # ── weights ──
    new_wts <- setNames(rep(0, length(ALL_TICKERS)), ALL_TICKERS)
    new_wts[top_off] <- (1 - df) / TopN
    new_wts[best_cash] <- new_wts[best_cash] + df

    # ── return ──
    rets_row <- as.numeric(mon_ret_xts[as.character(trd_date), ])
    port_ret <- sum(new_wts * rets_row, na.rm=TRUE)
    cost <- sum(abs(new_wts - prev_wts) * COST_MAP[names(new_wts)], na.rm=TRUE)
    port_ret <- port_ret - cost

    rets[t_idx] <- port_ret
    prev_wts <- new_wts
    prev_df  <- df
  }
  xts(rets, order.by=trd_dates)
}

# ═══════════════════════════════════════════════════════════════════════════════
# walk-forward runner
# ═══════════════════════════════════════════════════════════════════════════════

run_walkforward_aba <- function(offensive_set, label) {
  cat(sprintf("\n══════ %s ══════\n", label))
  cat(sprintf("  offensive: %s\n", offensive_set[1]))

  all_dates <- index(mon_rets)
  all_years <- sort(unique(as.numeric(format(all_dates,"%Y"))))
  warmup_end_year <- as.numeric(format(all_dates[1],"%Y")) + (MIN_TRAIN_MONTHS %/% 12)
  test_years <- all_years[all_years >= warmup_end_year]
  cat(sprintf("  test years: %d → %d (%d)\n", test_years[1], last(test_years), length(test_years)))

  wf_returns <- list(); wf_params <- list()

  for (y in test_years) {
    cat(sprintf("\n  ── %d ──\n", y))
    train_end <- as.Date(sprintf("%d-12-31", y-1))
    test_dates <- all_dates[format(all_dates,"%Y")==as.character(y)]
    if (length(test_dates)==0) { cat("    no test dates\n"); next }

    train_ret <- mon_rets[paste0("/",as.character(train_end))]
    train_px  <- mon_merged[paste0("/",as.character(train_end))]
    train_mom <- mom_13612w[paste0("/",as.character(train_end))]
    if (length(index(train_ret)) < MIN_TRAIN_MONTHS) { cat(sprintf("    only %d months\n",length(index(train_ret)))); next }

    # select L, TopN, B
    best_score <- -Inf; best_L <- L_GRID[1]; best_Top <- TOP_GRID[1]; best_B <- B_GRID[1]
    for (Lv in L_GRID) for (Tv in TOP_GRID) for (Bv in B_GRID) {
      port <- run_aba_portfolio(train_ret, train_px, train_mom, offensive_set, Lv, Tv, Bv)
      port <- port[!is.na(coredata(port)),]
      if (NROW(port) < 36) next
      r <- Return.annualized(port)[1,1]; d <- maxDrawdown(port)
      sc <- if (r>=0 && d<=0.50) r*(1-d/(1-d)) else 0
      if (sc > best_score) { best_score <- sc; best_L <- Lv; best_Top <- Tv; best_B <- Bv }
    }
    cat(sprintf("    params: L=%d, Top=%d, B=%d (RAD=%.4f)\n", best_L, best_Top, best_B, best_score))

    # test year
    test_ret <- mon_rets[as.character(test_dates),]
    test_px  <- mon_merged[as.character(test_dates),]
    test_mom <- mom_13612w[as.character(test_dates),]
    port <- run_aba_portfolio(test_ret, test_px, test_mom, offensive_set, best_L, best_Top, best_B)
    if (NROW(port)>0) {
      wf_returns[[as.character(y)]] <- port
      wf_params[[as.character(y)]] <- c(L=best_L, Top=best_Top, B=best_B)
    }
  }

  rets <- do.call(rbind, wf_returns)
  rets <- rets[!is.na(coredata(rets)),]; names(rets) <- label

  cat(sprintf("\n  param history (%s):\n", label))
  for (y in names(wf_params))
    cat(sprintf("    %s: L=%d, Top=%d, B=%d\n", y, wf_params[[y]]["L"], wf_params[[y]]["Top"], wf_params[[y]]["B"]))

  list(returns=rets, params=wf_params)
}

# ═══════════════════════════════════════════════════════════════════════════════
# run both variants
# ═══════════════════════════════════════════════════════════════════════════════

res_spy  <- run_walkforward_aba(OFFENSIVE_SPY,  "ABA-SPY")
res_mtum <- run_walkforward_aba(OFFENSIVE_MTUM, "ABA-MTUM")

# ═══════════════════════════════════════════════════════════════════════════════
# align on common dates
# ═══════════════════════════════════════════════════════════════════════════════

common_start <- max(first(index(res_spy$returns)), first(index(res_mtum$returns)))
common_end   <- min(last(index(res_spy$returns)),  last(index(res_mtum$returns)))
cat(sprintf("\ncommon range: %s → %s\n",
            as.character(as.Date(common_start)), as.character(as.Date(common_end))))

aba_spy  <- res_spy$returns[paste0(common_start,"/",common_end)]
aba_mtum <- res_mtum$returns[paste0(common_start,"/",common_end)]
common_ret <- mon_rets[paste0(common_start,"/",common_end),]

# benchmarks
ew_wts <- rep(1/N_OFF, N_OFF); names(ew_wts) <- OFFENSIVE_SPY
ew_rets <- xts(rowSums(coredata(common_ret[,OFFENSIVE_SPY]) *
               matrix(ew_wts,nrow=NROW(common_ret),ncol=N_OFF,byrow=TRUE),na.rm=TRUE),
               order.by=index(common_ret)); names(ew_rets) <- "EW"

NC <- length(DEFENSIVE_TICKERS); ewc_wts <- rep(1/NC,NC)
ewc_rets <- xts(rowSums(coredata(common_ret[,DEFENSIVE_TICKERS]) *
                matrix(ewc_wts,nrow=NROW(common_ret),ncol=NC,byrow=TRUE),na.rm=TRUE),
                order.by=index(common_ret)); names(ewc_rets) <- "EWC"

sixty40_rets <- xts(0.6*coredata(common_ret[,"SPY"])+0.4*coredata(common_ret[,"IEF"]),
                    order.by=index(common_ret)); names(sixty40_rets) <- "60/40"
spy_rets <- common_ret[,"SPY"]; names(spy_rets) <- "SPY"
mtum_rets <- common_ret[,"MTUM"]; names(mtum_rets) <- "MTUM"

all_strats <- na.omit(merge(aba_spy, aba_mtum, ew_rets, ewc_rets, sixty40_rets, spy_rets, mtum_rets))
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
          factor(Strategy,levels=c("ABA.SPY","ABA.MTUM","EW","EWC","60/40","SPY","MTUM")))

# ═══════════════════════════════════════════════════════════════════════════════
# gt table
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding metrics table...\n")

STRAT_COLORS <- c(
  `ABA.SPY`="#f0f4ff", `ABA.MTUM`="#f0fff0",
  EW="#f5f5f5", EWC="#f5f5f5", `60/40`="#f5f5f5", SPY="#f5f5f5", MTUM="#f5f5f5")

tbl <- metrics_tbl |> gt(groupname_col="Window") |>
  tab_header(title="ABA — Adaptive Breadth Allocation",
    subtitle=sprintf("SMA Offense + 13612W Self-Breadth + Hysteresis | %s → %s",
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
gtsave(tbl,sprintf("%s/aba-metrics.html",reportPath))
webshot2::webshot(sprintf("%s/aba-metrics.html",reportPath),sprintf("%s/aba-metrics.png",reportPath),
                  selector="table.gt_table",expand=c(10,10,10,10))

# ═══════════════════════════════════════════════════════════════════════════════
# charts
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding charts...\n")
main_strats <- c("ABA.SPY","ABA.MTUM","X60.40","EW","SPY","MTUM")
to_plot <- all_strats[, main_strats[main_strats %in% colnames(all_strats)]]

sr <- sapply(colnames(to_plot),function(nm) round(SharpeRatio.annualized(to_plot[,nm])[1,1],2))
sr_text <- paste0(colnames(to_plot),"=",sr,collapse=", ")

Common.PlotCumReturns(to_plot,"ABA — Adaptive Breadth Allocation",
  sprintf("SMA + 13612W Self-Breadth + Hysteresis | %s → %s | SR: %s",
          as.character(as.Date(first(index(to_plot)))),
          as.character(as.Date(last(index(to_plot)))),sr_text),
  sprintf("%s/aba-cumulative.png",reportPath),NULL)

# OS chart (aligned to IS/OS split)
os_split <- as.character(is_end_date)
os_plot <- to_plot[paste0(os_split,"/")]
if (NROW(os_plot)>12) {
  sr_os <- sapply(colnames(os_plot),function(nm) round(SharpeRatio.annualized(os_plot[,nm])[1,1],2))
  Common.PlotCumReturns(os_plot,"ABA — Out-of-Sample",
    sprintf("SMA + 13612W | %s → %s | SR: %s",
            as.character(as.Date(first(index(os_plot)))),
            as.character(as.Date(last(index(os_plot)))),
            paste0(colnames(os_plot),"=",sr_os,collapse=", ")),
    sprintf("%s/aba-cumulative-os.png",reportPath),NULL)
}

png(sprintf("%s/aba-drawdowns.png",reportPath),width=1400,height=800,bg="white")
chart.Drawdown(to_plot,main="ABA Drawdowns",legend.loc="bottomleft",ylab="Drawdown")
mtext("@StockViz",side=4,col='grey')
dev.off()

# ═══════════════════════════════════════════════════════════════════════════════
# save
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nsaving results...\n")
save(all_strats,res_spy,res_mtum,all_period_metrics,metrics_tbl,is_end_date,common_ret,mon_merged,
     file=sprintf("%s/aba-results.Rdata",reportPath))

cat("\nDone.\n")
for (nm in c("ABA.SPY","ABA.MTUM")) {
  r <- Return.annualized(all_strats[,nm])[1,1]*100
  d <- -maxDrawdown(all_strats[,nm])*100
  s <- SharpeRatio.annualized(all_strats[,nm])[1,1]
  cat(sprintf("  %s: CAGR=%.1f%%  MaxDD=%.1f%%  Sharpe=%.2f\n", nm, r, d, s))
}
