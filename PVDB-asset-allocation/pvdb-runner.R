# PVDB — PAA · VAA · DAA · BAA · ABA : Consolidated Backtest Runner
# Run all five breadth-momentum strategies from a single script.
# Dates are dynamic — safe to run years from now on updated data.

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
# 1. UNIVERSES
# ═══════════════════════════════════════════════════════════════════════════════

# VAA
VAA_RISKY   <- c("SPY","IWM","QQQ","VGK","EWJ","VWO","VNQ","GSG","GLD","TLT","LQD","HYG")
VAA_CASH    <- c("SHY","IEF","LQD")

# DAA
DAA_RISKY   <- c("SPY","IWM","EFA","EEM","VNQ","DBC","GLD","TLT","HYG","LQD")
DAA_CASH    <- c("BIL","IEF","SHY")
DAA_CANARY  <- c("HYG","SPHB","TLT","IEF","UUP")   # SPHB pool (best performer)

# PAA
PAA_RISKY   <- c("SPY","IWM","EFA","EEM","VNQ","DBC","GLD","TLT","HYG","LQD")
PAA_CASH    <- c("IEF","SHY","BIL","AGG")
PAA_CANARY  <- c("EEM","AGG")                        # EEM pool (best performer)

# BAA
BAA_OFF_SPY <- c("SPY","IWM","EFA","EEM","VNQ","DBC","GLD","TLT","HYG","LQD")
BAA_OFF_QQQ <- c("QQQ","IWM","EFA","EEM","VNQ","DBC","GLD","TLT","HYG","LQD")
BAA_DEF     <- c("BIL","IEF","SHY","TLT","LQD","AGG")
BAA_CANARY  <- c("SPHB","AGG")

# ABA (SPY-based, self-breadth)
ABA_OFF  <- c("SPY","IWM","EFA","EEM","VNQ","DBC","GLD","TLT","HYG","LQD")
ABA_DEF  <- c("BIL","IEF","SHY")

ALL_TICKERS <- unique(c(
  VAA_RISKY, VAA_CASH,
  DAA_RISKY, DAA_CASH, DAA_CANARY,
  PAA_RISKY, PAA_CASH, PAA_CANARY,
  BAA_OFF_SPY, BAA_OFF_QQQ, BAA_DEF, BAA_CANARY,
  ABA_OFF, ABA_DEF
))

# ═══════════════════════════════════════════════════════════════════════════════
# 2. TIERED COSTS
# ═══════════════════════════════════════════════════════════════════════════════

COST_TIER1 <- 0.0005; COST_TIER2 <- 0.0015
TIER1 <- c("SPY","QQQ","IWM","EFA","EEM","VGK","EWJ","VWO","GLD","GSG","TLT","IEF","SHY","BIL","AGG")
TIER2 <- c("HYG","LQD","VNQ","DBC","SPHB","UUP")
COST_MAP <- setNames(c(rep(COST_TIER1,length(TIER1)),rep(COST_TIER2,length(TIER2))),c(TIER1,TIER2))

# ═══════════════════════════════════════════════════════════════════════════════
# 3. CONNECT + LOAD ALL DATA
# ═══════════════════════════════════════════════════════════════════════════════

cat("connecting to StockVizUs2...\n")
lconUS2 <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockVizUs2", ldbuser, ldbpassword),
  case="nochange", believeNRows=TRUE)

cat("loading daily prices for", length(ALL_TICKERS), "tickers...\n")
daily_prices <- list()
for (sym in ALL_TICKERS) {
  pDf <- sqlQuery(lconUS2, sprintf("select C, TIME_STAMP from bhav_eq_td where SYMBOL='%s' order by TIME_STAMP", sym))
  if (nrow(pDf) < 260) { cat(sprintf("  %s: too few rows (%d), skipping\n", sym, nrow(pDf))); next }
  daily_prices[[sym]] <- xts(pDf$C, pDf$TIME_STAMP)
  cat(sprintf("  %s: %d rows, %s → %s\n", sym, nrow(pDf),
              as.character(as.Date(first(index(daily_prices[[sym]])))),
              as.character(as.Date(last(index(daily_prices[[sym]]))))))
}
odbcClose(lconUS2)

# ═══════════════════════════════════════════════════════════════════════════════
# 4. MONTHLY PRICES + 13612W MOMENTUM
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
cat(sprintf("  %d months, %d tickers, %s → %s\n",
            nrow(mon_merged), ncol(mon_merged),
            as.character(as.Date(first(index(mon_merged)))),
            as.character(as.Date(last(index(mon_merged))))))

# 13612W momentum
cat("computing 13612W momentum...\n")
comp136 <- function(px) {
  n <- nrow(px); mom <- rep(NA_real_, n)
  for (i in 13:n) {
    p0 <- coredata(px)[i]; p1 <- coredata(px)[i-1]; p3 <- coredata(px)[i-3]
    p6 <- coredata(px)[i-6]; p12 <- coredata(px)[i-12]
    if (any(is.na(c(p1,p3,p6,p12))) || any(c(p1,p3,p6,p12)==0)) next
    mom[i] <- (12*(p0/p1-1)+4*(p0/p3-1)+2*(p0/p6-1)+1*(p0/p12-1))/4
  }
  xts(mom, order.by=index(px))
}
mom136 <- na.omit(do.call(merge.xts, lapply(names(monthly_prices), function(s) comp136(monthly_prices[[s]]))))
names(mom136) <- names(monthly_prices)
cat(sprintf("  13612W: %d rows\n", nrow(mom136)))

# ═══════════════════════════════════════════════════════════════════════════════
# 5. SHARED HELPERS
# ═══════════════════════════════════════════════════════════════════════════════

MIN_TRAIN <- 60   # 5-year warm-up

# SMA(L) momentum for a price xts (single or multi-column)
sma_momentum <- function(px, L) {
  effL <- min(L, nrow(px)-1)
  sma <- do.call(merge.xts, lapply(colnames(px), function(sym) {
    s <- SMA(px[,sym], effL)
    s[is.na(coredata(s)),] <- coredata(px[,sym])[is.na(coredata(s)),]
    s
  }))
  names(sma) <- colnames(px)
  cd <- index(px)
  xts(log(coredata(px[cd])/coredata(sma[cd])), order.by=cd)
}

# RAD selection score
rad_score <- function(port) {
  port <- port[!is.na(coredata(port)),]
  if (NROW(port) < 36) return(-Inf)
  r <- Return.annualized(port)[1,1]; d <- maxDrawdown(port)
  if (r >= 0 && d <= 0.50) r*(1-d/(1-d)) else 0
}

# Walk-forward generic runner
run_wf <- function(name, wf_fn, test_years) {
  cat(sprintf("\n── %s ──\n", name))
  rets_list <- list()
  for (y in test_years) {
    train_end <- as.Date(sprintf("%d-12-31", y-1))
    test_dates <- all_dates[format(all_dates,"%Y")==as.character(y)]
    if (length(test_dates) < 2) next

    train_ret <- mon_rets[paste0("/",as.character(train_end))]
    train_px  <- mon_merged[paste0("/",as.character(train_end))]
    train_mom <- mom136[paste0("/",as.character(train_end))]
    if (length(index(train_ret)) < MIN_TRAIN) next

    # Run strategy-specific optimization + test
    port <- wf_fn(train_ret, train_px, train_mom, test_dates, y)
    if (!is.null(port) && NROW(port) > 0)
      rets_list[[as.character(y)]] <- port
  }
  rets <- do.call(rbind, rets_list)
  rets <- rets[!is.na(coredata(rets)),]
  names(rets) <- name
  rets
}

# Portfolio cost application
apply_costs <- function(new_wts, prev_wts, port_ret) {
  cost <- sum(abs(new_wts - prev_wts) * COST_MAP[names(new_wts)], na.rm=TRUE)
  port_ret - cost
}

# ═══════════════════════════════════════════════════════════════════════════════
# 6. STRATEGY-SPECIFIC WALK-FORWARD FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════════

# ── VAA ──
vaa_run <- function(train_ret, train_px, train_mom, test_dates, y) {
  # Grid: T=[1..6], B=[1..6]
  best_sc <- -Inf; best_T <- 2; best_B <- 2
  for (Tv in 1:6) for (Bv in 1:6) {
    port <- vaa_portfolio(train_ret, train_mom, VAA_RISKY, VAA_CASH, Tv, Bv)
    sc <- rad_score(port); if (sc > best_sc) { best_sc <- sc; best_T <- Tv; best_B <- Bv }
  }
  test_ret <- mon_rets[as.character(test_dates),]
  test_mom <- mom136[as.character(test_dates),]
  vaa_portfolio(test_ret, test_mom, VAA_RISKY, VAA_CASH, best_T, best_B)
}

vaa_portfolio <- function(mon_ret_xts, mom_xts, risky, cash, T_val, B_val) {
  trd_dates <- index(mon_ret_xts); n_trd <- length(trd_dates)
  rets <- rep(NA_real_, n_trd); prev_wts <- setNames(rep(0,length(ALL_TICKERS)),ALL_TICKERS)
  for (t_idx in seq_len(n_trd)) {
    trd_date <- trd_dates[t_idx]
    md <- index(mom_xts); si <- max(which(md < trd_date))
    if (si < 1 || is.infinite(si)) next; sd <- md[si]
    mr <- mom_xts[as.character(sd),]; if (all(is.na(coredata(mr)))) next

    rm <- as.numeric(mr[,risky]); names(rm) <- risky; rm <- rm[!is.na(rm)]
    n_bad <- sum(rm <= 0, na.rm=TRUE)
    cf <- min(floor(n_bad * T_val / B_val) / T_val, 1)
    ranked <- sort(rm, decreasing=TRUE); top_t <- names(ranked)[1:min(T_val,length(ranked))]
    n_cash <- round(cf * T_val)
    risky_hold <- if (n_cash > 0) top_t[1:(T_val-n_cash)] else top_t
    cm <- as.numeric(mr[,cash]); names(cm) <- cash; best_c <- names(which.max(cm))[1]

    nw <- setNames(rep(0,length(ALL_TICKERS)),ALL_TICKERS)
    if (length(risky_hold)>0) nw[risky_hold] <- 1/T_val
    nw[best_c] <- nw[best_c] + n_cash/T_val

    rr <- as.numeric(mon_ret_xts[as.character(trd_date),])
    rets[t_idx] <- apply_costs(nw, prev_wts, sum(nw*rr, na.rm=TRUE))
    prev_wts <- nw
  }
  xts(rets, order.by=trd_dates)
}

# ── DAA ──
daa_run <- function(train_ret, train_px, train_mom, test_dates, y) {
  ac <- DAA_CANARY[DAA_CANARY %in% colnames(mon_rets)]
  combos <- as.list(ac); names(combos) <- ac
  if (length(ac)>=2) for (i in 1:(length(ac)-1)) for (j in (i+1):length(ac))
    combos[[paste(ac[i],ac[j],sep="+")]] <- c(ac[i],ac[j])

  best_csc <- -Inf; best_cset <- NULL
  for (cn in names(combos)) {
    cs <- combos[[cn]]
    port <- daa_portfolio(train_ret, train_mom, cs, DAA_RISKY, DAA_CASH, 5, 1)
    sc <- rad_score(port); if (sc > best_csc) { best_csc <- sc; best_cset <- cs }
  }
  if (is.null(best_cset)) return(NULL)

  best_psc <- -Inf; best_B <- 1; best_T <- 3
  for (Bv in 1:2) for (Tv in c(3,5)) {
    port <- daa_portfolio(train_ret, train_mom, best_cset, DAA_RISKY, DAA_CASH, Tv, Bv)
    sc <- rad_score(port); if (sc > best_psc) { best_psc <- sc; best_B <- Bv; best_T <- Tv }
  }
  test_ret <- mon_rets[as.character(test_dates),]
  test_mom <- mom136[as.character(test_dates),]
  daa_portfolio(test_ret, test_mom, best_cset, DAA_RISKY, DAA_CASH, best_T, best_B)
}

daa_portfolio <- function(mon_ret_xts, mom_xts, canary, risky, cash, T_val, B_val) {
  trd_dates <- index(mon_ret_xts); n_trd <- length(trd_dates)
  rets <- rep(NA_real_, n_trd); prev_wts <- setNames(rep(0,length(ALL_TICKERS)),ALL_TICKERS)
  for (t_idx in seq_len(n_trd)) {
    trd_date <- trd_dates[t_idx]
    md <- index(mom_xts); si <- max(which(md < trd_date))
    if (si < 1 || is.infinite(si)) next; sd <- md[si]
    mr <- mom_xts[as.character(sd),]; if (all(is.na(coredata(mr)))) next

    cm <- as.numeric(mr[,canary[canary %in% colnames(mr)]]); n_bad <- sum(cm <= 0, na.rm=TRUE)
    cf <- min(n_bad / B_val, 1)

    rm <- as.numeric(mr[,risky]); names(rm) <- risky; rm <- rm[!is.na(rm)]
    if (length(rm) < T_val) next
    top_t <- names(sort(rm, decreasing=TRUE))[1:T_val]

    cashm <- as.numeric(mr[,cash]); best_c <- names(which.max(cashm))[1]

    nw <- setNames(rep(0,length(ALL_TICKERS)),ALL_TICKERS)
    nw[top_t] <- (1-cf)/T_val; nw[best_c] <- nw[best_c] + cf

    rr <- as.numeric(mon_ret_xts[as.character(trd_date),])
    rets[t_idx] <- apply_costs(nw, prev_wts, sum(nw*rr, na.rm=TRUE))
    prev_wts <- nw
  }
  xts(rets, order.by=trd_dates)
}

# ── PAA ──
paa_run <- function(train_ret, train_px, train_mom, test_dates, y) {
  ac <- PAA_CANARY[PAA_CANARY %in% colnames(mon_rets)]
  combos <- as.list(ac); names(combos) <- ac
  if (length(ac)>=2) for (i in 1:(length(ac)-1)) for (j in (i+1):length(ac))
    combos[[paste(ac[i],ac[j],sep="+")]] <- c(ac[i],ac[j])

  best_csc <- -Inf; best_cset <- NULL
  for (cn in names(combos)) {
    cs <- combos[[cn]]
    port <- paa_portfolio(train_ret, train_px, cs, PAA_RISKY, PAA_CASH, 12, 3, 0)
    sc <- rad_score(port); if (sc > best_csc) { best_csc <- sc; best_cset <- cs }
  }
  if (is.null(best_cset)) return(NULL)

  best_psc <- -Inf; best_L <- 12; best_T <- 3; best_A <- 0
  for (Lv in c(6,12)) for (Tv in c(3,5)) for (Av in c(0,1)) {
    port <- paa_portfolio(train_ret, train_px, best_cset, PAA_RISKY, PAA_CASH, Lv, Tv, Av)
    sc <- rad_score(port); if (sc > best_psc) { best_psc <- sc; best_L <- Lv; best_T <- Tv; best_A <- Av }
  }
  test_ret <- mon_rets[as.character(test_dates),]
  test_px  <- mon_merged[as.character(test_dates),]
  paa_portfolio(test_ret, test_px, best_cset, PAA_RISKY, PAA_CASH, best_L, best_T, best_A)
}

paa_portfolio <- function(mon_ret_xts, price_xts, canary, risky, cash, L, TopN, A_val) {
  trd_dates <- index(mon_ret_xts); n_trd <- length(trd_dates)
  rets <- rep(NA_real_, n_trd); prev_wts <- setNames(rep(0,length(ALL_TICKERS)),ALL_TICKERS)
  prev_df <- 0
  sma_mom <- sma_momentum(price_xts, L)

  for (t_idx in seq_len(n_trd)) {
    trd_date <- trd_dates[t_idx]
    md <- index(sma_mom); si <- max(which(md < trd_date))
    if (si < 1 || is.infinite(si)) next; sd <- md[si]
    mr <- sma_mom[as.character(sd),]; if (all(is.na(coredata(mr)))) next

    cm <- as.numeric(mr[,canary[canary %in% colnames(mr)]])
    breadth <- mean(plogis(cm, scale=0.05), na.rm=TRUE)
    df <- 1 - breadth^(1 + A_val)

    rm <- as.numeric(mr[,risky]); names(rm) <- risky; rm <- rm[!is.na(rm)]
    if (length(rm) < TopN) next
    top_n <- names(sort(rm, decreasing=TRUE))[1:TopN]

    cm2 <- as.numeric(mr[,cash]); best_c <- names(which.max(cm2))[1]

    nw <- setNames(rep(0,length(ALL_TICKERS)),ALL_TICKERS)
    nw[top_n] <- (1-df)/TopN; nw[best_c] <- nw[best_c] + df

    rr <- as.numeric(mon_ret_xts[as.character(trd_date),])
    rets[t_idx] <- apply_costs(nw, prev_wts, sum(nw*rr, na.rm=TRUE))
    prev_wts <- nw; prev_df <- df
  }
  xts(rets, order.by=trd_dates)
}

# ── BAA (SPY + SPHB pool) ──
baa_run <- function(train_ret, train_px, train_mom, test_dates, y) {
  offensive <- BAA_OFF_SPY
  ac <- BAA_CANARY[BAA_CANARY %in% colnames(mon_rets)]
  combos <- as.list(ac); names(combos) <- ac
  if (length(ac)>=2) for (i in 1:(length(ac)-1)) for (j in (i+1):length(ac))
    combos[[paste(ac[i],ac[j],sep="+")]] <- c(ac[i],ac[j])

  best_csc <- -Inf; best_cset <- NULL
  for (cn in names(combos)) {
    cs <- combos[[cn]]
    port <- baa_portfolio(train_ret, train_px, train_mom, offensive, BAA_DEF, cs, 1, 5)
    sc <- rad_score(port); if (sc > best_csc) { best_csc <- sc; best_cset <- cs }
  }
  if (is.null(best_cset)) return(NULL)

  best_psc <- -Inf; best_B <- 1; best_T <- 3
  for (Bv in 1:2) for (Tv in c(3,5,7)) {
    port <- baa_portfolio(train_ret, train_px, train_mom, offensive, BAA_DEF, best_cset, Bv, Tv)
    sc <- rad_score(port); if (sc > best_psc) { best_psc <- sc; best_B <- Bv; best_T <- Tv }
  }
  test_ret <- mon_rets[as.character(test_dates),]
  test_px  <- mon_merged[as.character(test_dates),]
  test_mom <- mom136[as.character(test_dates),]
  baa_portfolio(test_ret, test_px, test_mom, offensive, BAA_DEF, best_cset, best_B, best_T)
}

baa_portfolio <- function(mon_ret_xts, price_xts, mom_xts, offensive, defensive, canary, B_val, TopN) {
  trd_dates <- index(mon_ret_xts); n_trd <- length(trd_dates)
  rets <- rep(NA_real_, n_trd); prev_wts <- setNames(rep(0,length(ALL_TICKERS)),ALL_TICKERS)
  off_mom <- sma_momentum(price_xts, 12)

  for (t_idx in seq_len(n_trd)) {
    trd_date <- trd_dates[t_idx]
    od <- index(off_mom); osi <- max(which(od < trd_date))
    if (osi < 1 || is.infinite(osi)) next; osd <- od[osi]
    cd <- index(mom_xts); csi <- max(which(cd < trd_date))
    if (csi < 1 || is.infinite(csi)) next; csd <- cd[csi]

    or <- off_mom[as.character(osd),offensive]
    ov <- as.numeric(or); names(ov) <- offensive; ov <- ov[!is.na(ov)]
    if (length(ov) < TopN) next
    top_off <- names(sort(ov,decreasing=TRUE))[1:TopN]

    cr <- mom_xts[as.character(csd),canary[canary %in% colnames(mom_xts)]]
    cv <- as.numeric(cr); breadth <- mean(plogis(cv,scale=0.05),na.rm=TRUE)
    df <- 1 - breadth^B_val

    dr <- mom_xts[as.character(csd),defensive]
    dv <- as.numeric(dr); names(dv) <- defensive
    gd <- dv[dv > 0 & !is.na(dv)]
    top_def <- if (length(gd)==0) names(which.max(dv))[1]
               else names(sort(gd,decreasing=TRUE))[1:min(3,length(gd))]

    nw <- setNames(rep(0,length(ALL_TICKERS)),ALL_TICKERS)
    nw[top_off] <- (1-df)/TopN; nw[top_def] <- nw[top_def] + df/length(top_def)

    rr <- as.numeric(mon_ret_xts[as.character(trd_date),])
    rets[t_idx] <- apply_costs(nw, prev_wts, sum(nw*rr, na.rm=TRUE))
    prev_wts <- nw
  }
  xts(rets, order.by=trd_dates)
}

# ── ABA (SPY + self-breadth) ──
aba_run <- function(train_ret, train_px, train_mom, test_dates, y) {
  offensive <- ABA_OFF
  best_sc <- -Inf; best_L <- 12; best_T <- 3; best_B <- 1
  for (Lv in c(6,12)) for (Tv in c(3,5)) for (Bv in 1:3) {
    port <- aba_portfolio(train_ret, train_px, train_mom, offensive, ABA_DEF, Lv, Tv, Bv)
    sc <- rad_score(port); if (sc > best_sc) { best_sc <- sc; best_L <- Lv; best_T <- Tv; best_B <- Bv }
  }
  test_ret <- mon_rets[as.character(test_dates),]
  test_px  <- mon_merged[as.character(test_dates),]
  test_mom <- mom136[as.character(test_dates),]
  aba_portfolio(test_ret, test_px, test_mom, offensive, ABA_DEF, best_L, best_T, best_B)
}

aba_portfolio <- function(mon_ret_xts, price_xts, mom_xts, offensive, cash, L, TopN, B_val) {
  SIGMA <- 0.05; MAX_DF <- 0.25
  trd_dates <- index(mon_ret_xts); n_trd <- length(trd_dates)
  rets <- rep(NA_real_, n_trd); prev_wts <- setNames(rep(0,length(ALL_TICKERS)),ALL_TICKERS)
  prev_df <- 0
  off_mom <- sma_momentum(price_xts, L)

  for (t_idx in seq_len(n_trd)) {
    trd_date <- trd_dates[t_idx]
    od <- index(off_mom); osi <- max(which(od < trd_date))
    if (osi < 1 || is.infinite(osi)) next; osd <- od[osi]
    cd <- index(mom_xts); csi <- max(which(cd < trd_date))
    if (csi < 1 || is.infinite(csi)) next; csd <- cd[csi]

    or <- off_mom[as.character(osd),offensive]
    ov <- as.numeric(or); names(ov) <- offensive; ov <- ov[!is.na(ov)]
    if (length(ov) < TopN) next
    top_off <- names(sort(ov,decreasing=TRUE))[1:TopN]

    dr <- mom_xts[as.character(csd),offensive]
    dv <- as.numeric(dr); scores <- plogis(dv/SIGMA)
    breadth <- mean(scores, na.rm=TRUE); df_new <- 1 - breadth^B_val
    dc <- df_new - prev_df; dc <- max(min(dc, MAX_DF), -MAX_DF); df <- prev_df + dc

    cr <- mom_xts[as.character(csd),cash]
    cv <- as.numeric(cr); names(cv) <- cash; best_c <- names(which.max(cv))[1]

    nw <- setNames(rep(0,length(ALL_TICKERS)),ALL_TICKERS)
    nw[top_off] <- (1-df)/TopN; nw[best_c] <- nw[best_c] + df

    rr <- as.numeric(mon_ret_xts[as.character(trd_date),])
    rets[t_idx] <- apply_costs(nw, prev_wts, sum(nw*rr, na.rm=TRUE))
    prev_wts <- nw; prev_df <- df
  }
  xts(rets, order.by=trd_dates)
}

# ═══════════════════════════════════════════════════════════════════════════════
# 7. RUN ALL STRATEGIES
# ═══════════════════════════════════════════════════════════════════════════════

all_dates <- index(mon_rets)
all_years <- sort(unique(as.numeric(format(all_dates, "%Y"))))
warmup_end <- as.numeric(format(all_dates[1], "%Y")) + (MIN_TRAIN %/% 12)
test_years <- all_years[all_years >= warmup_end]
cat(sprintf("\ntest years: %d → %d (%d years)\n", test_years[1], last(test_years), length(test_years)))

strategy_rets <- list()

strategy_rets[["VAA"]]  <- run_wf("VAA",  vaa_run, test_years)
strategy_rets[["DAA"]]  <- run_wf("DAA",  daa_run, test_years)
strategy_rets[["PAA"]]  <- run_wf("PAA",  paa_run, test_years)
strategy_rets[["BAA"]]  <- run_wf("BAA",  baa_run, test_years)
strategy_rets[["ABA"]]  <- run_wf("ABA",  aba_run, test_years)

# ═══════════════════════════════════════════════════════════════════════════════
# 8. ALIGN ON COMMON DATES + BENCHMARKS
# ═══════════════════════════════════════════════════════════════════════════════

non_null <- strategy_rets[sapply(strategy_rets, function(x) !is.null(x) && NROW(x) > 0)]
common_start <- max(sapply(non_null, function(r) first(index(r))))
common_end   <- min(sapply(non_null, function(r) last(index(r))))
cat(sprintf("\ncommon range: %s → %s\n",
            as.character(as.Date(common_start)), as.character(as.Date(common_end))))

aligned <- do.call(merge.xts, lapply(non_null, function(r) r[paste0(common_start,"/",common_end)]))
common_ret <- mon_rets[paste0(common_start,"/",common_end),]

# Benchmarks
N_RISKY <- length(VAA_RISKY); ew_wts <- rep(1/N_RISKY, N_RISKY); names(ew_wts) <- VAA_RISKY
ew <- xts(rowSums(coredata(common_ret[,VAA_RISKY]) *
            matrix(ew_wts,nrow=NROW(common_ret),ncol=N_RISKY,byrow=TRUE),na.rm=TRUE),
            order.by=index(common_ret)); names(ew) <- "EW"

N_CASH <- 3; ewc_wts <- rep(1/N_CASH,N_CASH); names(ewc_wts) <- c("BIL","IEF","SHY")
ewc <- xts(rowSums(coredata(common_ret[,c("BIL","IEF","SHY")]) *
             matrix(ewc_wts,nrow=NROW(common_ret),ncol=N_CASH,byrow=TRUE),na.rm=TRUE),
             order.by=index(common_ret)); names(ewc) <- "EWC"

sixty40 <- xts(0.6*coredata(common_ret[,"SPY"])+0.4*coredata(common_ret[,"IEF"]),
               order.by=index(common_ret)); names(sixty40) <- "60/40"
spy <- common_ret[,"SPY"]; names(spy) <- "SPY"

all_strats <- na.omit(merge(aligned, ew, ewc, sixty40, spy))
cat(sprintf("  combined: %d rows, %d cols\n", NROW(all_strats), ncol(all_strats)))

# ═══════════════════════════════════════════════════════════════════════════════
# 9. IS/OS/FS METRICS
# ═══════════════════════════════════════════════════════════════════════════════

is_end <- as.Date("2026-07-12")   # OS split — last date of consolidated data
cat(sprintf("  OS split: %s\n", as.character(is_end)))

comp_metrics <- function(ret_xts, ewc_xts=NULL) {
  if (NROW(ret_xts) < 3) return(NULL)
  r <- Return.annualized(ret_xts)[1,1]; v <- StdDev.annualized(ret_xts)[1,1]; d <- maxDrawdown(ret_xts)
  sharpe <- if (!is.null(ewc_xts)) SharpeRatio.annualized(ret_xts-ewc_xts[index(ret_xts)])[1,1]
            else SharpeRatio.annualized(ret_xts)[1,1]
  list(R=r,V=v,D=d,Sharpe=sharpe,MAR=r/d,RAD=if(r>=0&&d<=0.50) r*(1-d/(1-d)) else 0)
}

all_metrics <- data.frame()
for (nm in colnames(all_strats)) {
  ret_x <- all_strats[,nm]
  for (p in c("IS","OS","FS")) {
    sub <- switch(p, IS=ret_x[paste0("/",as.character(is_end))],
                  OS=ret_x[paste0(as.character(is_end),"/")], FS=ret_x)
    if (NROW(sub) < 12) next
    m <- comp_metrics(sub, ewc)
    if (!is.null(m))
      all_metrics <- rbind(all_metrics, data.frame(
        Period=paste(nm,p), R=m$R*100, V=m$V*100, D=abs(m$D)*100,
        Sharpe=m$Sharpe, MAR=m$MAR, RAD=m$RAD, stringsAsFactors=FALSE))
  }
}

metrics_tbl <- all_metrics |>
  mutate(Strategy=sub(" (IS|OS|FS)$","",Period),
         Window=recode(sub(".* (IS|OS|FS)$","\\1",Period), IS="In-Sample", OS="Out-of-Sample", FS="Full Sample")) |>
  select(Strategy,Window,R,V,D,Sharpe,MAR,RAD) |>
  mutate(R=R/100,V=V/100,D=D/100) |>
  arrange(factor(Window, levels=c("In-Sample","Out-of-Sample","Full Sample")),
          factor(Strategy, levels=c("ABA","BAA","VAA","DAA","PAA","EW","EWC","60/40","SPY")))

# ═══════════════════════════════════════════════════════════════════════════════
# 10. GT TABLE
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding metrics table...\n")

STRAT_COLORS <- c(
  ABA="#f0f4ff", BAA="#f0fff0", VAA="#fff3e0", DAA="#fce4ec", PAA="#f3e5f5",
  EW="#f5f5f5", EWC="#f5f5f5", `60/40`="#f5f5f5", SPY="#f5f5f5")

tbl <- metrics_tbl |> gt(groupname_col="Window") |>
  tab_header(title="PVDB — Consolidated Backtest",
    subtitle=sprintf("%s → %s | Walk-forward, real ETFs, tiered costs",
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
gtsave(tbl, sprintf("%s/pvdb-metrics.html", reportPath))
webshot2::webshot(sprintf("%s/pvdb-metrics.html", reportPath), sprintf("%s/pvdb-metrics.png", reportPath),
                  selector="table.gt_table", expand=c(10,10,10,10))

# ═══════════════════════════════════════════════════════════════════════════════
# 11. CHARTS
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nbuilding charts...\n")
chart_strats <- c("ABA","BAA","VAA","DAA","PAA","60/40","SPY")
to_plot <- all_strats[, chart_strats[chart_strats %in% colnames(all_strats)]]

sr <- sapply(colnames(to_plot), function(nm) round(SharpeRatio.annualized(to_plot[,nm])[1,1],2))
sr_text <- paste0(colnames(to_plot),"=",sr,collapse=", ")

Common.PlotCumReturns(to_plot, "PVDB — Consolidated (All Strategies)",
  sprintf("%s → %s | SR: %s",
          as.character(as.Date(first(index(to_plot)))),
          as.character(as.Date(last(index(to_plot)))), sr_text),
  sprintf("%s/pvdb-cumulative.png", reportPath), NULL)

os_plot <- to_plot[paste0(as.character(is_end),"/")]
if (NROW(os_plot) > 12) {
  sr_os <- sapply(colnames(os_plot), function(nm) round(SharpeRatio.annualized(os_plot[,nm])[1,1],2))
  Common.PlotCumReturns(os_plot, "PVDB — Out-of-Sample",
    sprintf("%s → %s | SR: %s",
            as.character(as.Date(first(index(os_plot)))),
            as.character(as.Date(last(index(os_plot)))),
            paste0(colnames(os_plot),"=",sr_os,collapse=", ")),
    sprintf("%s/pvdb-cumulative-os.png", reportPath), NULL)
}

png(sprintf("%s/pvdb-drawdowns.png", reportPath), width=1400, height=800, bg="white")
chart.Drawdown(to_plot, main="PVDB Drawdowns", legend.loc="bottomleft", ylab="Drawdown")
mtext("@StockViz", side=4, col='grey')
dev.off()

# ═══════════════════════════════════════════════════════════════════════════════
# 12. SAVE
# ═══════════════════════════════════════════════════════════════════════════════

cat("\nsaving results...\n")
save(all_strats, strategy_rets, all_metrics, metrics_tbl, is_end, common_ret, mon_merged,
     file=sprintf("%s/pvdb-results.Rdata", reportPath))

cat("\nDone — all five strategies.\n")
for (nm in c("ABA","BAA","VAA","DAA","PAA")) {
  if (!nm %in% colnames(all_strats)) next
  r <- Return.annualized(all_strats[,nm])[1,1]*100
  d <- -maxDrawdown(all_strats[,nm])*100
  s <- SharpeRatio.annualized(all_strats[,nm])[1,1]
  cat(sprintf("  %s: CAGR=%.1f%%  MaxDD=%.1f%%  Sharpe=%.2f\n", nm, r, d, s))
}
