# ============================================================================
# VIX Regime Timing — LONG-SHORT + FAST OVERLAY Backtest
# Continuous regime score [-1,+1]; weight = target_risk/VIX, can go net short
# Fast overlay: VIX-spike trigger overriding slow signal during crash windows
# Assets: NIFTY 50 TR, NIFTY MIDCAP SELECT TR
# ============================================================================

suppressPackageStartupMessages({
  library('RODBC')
  library('quantmod')
  library('PerformanceAnalytics')
  library('xts')
  library('tidyverse')
  library('gt')
  library('webshot2')
})

options("scipen" = 100)
options(stringsAsFactors = FALSE)
pdf(NULL)

reportPath <- "/mnt/data/blog/volatility/targeting2"
source("/mnt/hollandC/StockViz/R/config.r")

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, "StockViz", ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE
)

# ============================================================================
# PHASE 0 — PRE-REGISTERED PARAMETERS
# ============================================================================

# Slow regime signal
VIX_LEVEL_WINDOW  <- 126L
VIX_TREND_WINDOW  <- 20L
TARGET_RISK       <- 15.0   # 15% annual (VIX stored in % units: 20 = 20%)
TILT_MAGNITUDE    <- 1.50   # >1.0 required to go net short
MIN_WEIGHT        <- -1.50
MAX_WEIGHT        <- 1.50
TRANSACTION_COST  <- 0.005  # 50 bps
VOL_LOOKBACK      <- 60L

# Fast overlay (pre-registered — Phase 0 🚀)
OVERLAY_SPIKE_SIGMA   <- 3.0    # VIX > N std dev above 20-day avg
OVERLAY_SPIKE_WINDOW  <- 20L    # base window for VIX avg/std
OVERLAY_COOLDOWN      <- 10L    # linear decay days back to slow weight
OVERLAY_REENTRY       <- 5L     # quiet days before handing back to slow signal
OVERLAY_REDUCED_WT   <- 0.0     # weight when overlay is fully active (flat)

# Fast-shock test windows (pre-registered — Phase 0 🚀)
FAST_SHOCK_WINDOWS <- list(
  "COVID (Feb-Apr 2020)" = c("2020-02-19", "2020-04-30")
)

# Train/test split
TRAIN_END_DATE    <- "2018-12-31"

# Thresholds
MIN_SHORT_PCT       <- 10.0
BORROW_COST_ANNUAL  <- 0.02

# Grinding-bull windows
GRINDING_BULL_WINDOWS <- list(
  "2017" = c("2017-01-01", "2017-12-31"),
  "2021" = c("2021-01-01", "2021-12-31"),
  "2024" = c("2024-01-01", "2024-12-31")
)

# ============================================================================
# PHASE 1 — DATA PREPARATION
# ============================================================================

cat("\n=== PHASE 1: DATA PREPARATION ===\n")

vixDf <- sqlQuery(lcon, "select TIME_STAMP, PX_CLOSE from vix_history order by TIME_STAMP")
vixXts <- xts(vixDf$PX_CLOSE, as.Date(vixDf$TIME_STAMP))
cat(sprintf("VIX: %s → %s, %d rows\n", first(index(vixXts)), last(index(vixXts)), nrow(vixXts)))

indices <- c("NIFTY 50 TR", "NIFTY MIDCAP SELECT TR")
pxList <- list()
for (idx in indices) {
  pxDf <- sqlQuery(lcon, sprintf(
    "select TIME_STAMP, PX_CLOSE from bhav_index where INDEX_NAME='%s' order by TIME_STAMP", idx))
  pxList[[idx]] <- xts(pxDf$PX_CLOSE, as.Date(pxDf$TIME_STAMP))
}
odbcClose(lcon)

retList <- list()
for (idx in indices) retList[[idx]] <- dailyReturn(pxList[[idx]], type = "log")

common_dates <- Reduce(intersect, c(list(index(vixXts)), lapply(retList, index)))
cat(sprintf("Common trading days: %d\n", length(common_dates)))

vixXts  <- vixXts[common_dates]
n50Ret  <- retList[["NIFTY 50 TR"]][common_dates]
midRet  <- retList[["NIFTY MIDCAP SELECT TR"]][common_dates]
names(n50Ret) <- "N50"; names(midRet) <- "MID"

n        <- nrow(vixXts)
vix_vals <- as.numeric(coredata(vixXts))
dates    <- index(vixXts)

# ============================================================================
# PHASE 2 — SLOW REGIME SIGNAL CONSTRUCTION
# ============================================================================

cat("\n=== PHASE 2: SLOW REGIME SIGNAL ===\n")

# Pre-compute rolling averages/slopes
roll_avg <- as.numeric(rollapply(vixXts, VIX_LEVEL_WINDOW, mean, fill = NA, align = "right"))
roll_slp <- as.numeric(rollapply(vixXts, VIX_TREND_WINDOW,
  function(x) tryCatch(lm(x ~ seq_along(x))$coefficients[2], error = function(e) NA_real_),
  fill = NA, align = "right"))

# Level score: expanding percentile -> [-1,+1]
level_score <- rep(NA_real_, n)
for (i in seq_len(n)) {
  if (is.na(roll_avg[i])) next
  prior <- roll_avg[1:i]; prior <- prior[!is.na(prior)]
  if (length(prior) < 20) next
  level_score[i] <- 2 * (ecdf(prior)(roll_avg[i]) - 0.5)
}

# Trend score: rolling z-score of slope -> tanh -> [-1,+1]
trend_score <- rep(NA_real_, n)
for (i in seq_len(n)) {
  if (is.na(roll_slp[i])) next
  ws <- max(1L, i - VIX_TREND_WINDOW + 1L)
  wv <- roll_slp[ws:i]; wv <- wv[!is.na(wv)]
  if (length(wv) < 20) next
  mu <- mean(wv); sig <- sd(wv)
  trend_score[i] <- if (sig == 0) 0 else tanh((roll_slp[i] - mu) / sig)
}

# Regime score: bullish - bearish, clip [-1,+1]
rs <- rep(NA_real_, n)
for (i in seq_len(n)) {
  if (is.na(level_score[i]) || is.na(trend_score[i])) next
  bull <- max( level_score[i], 0) * max(-trend_score[i], 0)
  bear <- max(-level_score[i], 0) * max( trend_score[i], 0)
  rs[i] <- pmax(pmin(bull - bear, 1), -1)
}

sig_xts <- na.omit(xts(cbind(regime_score = rs), order.by = dates))
cat(sprintf("Slow signal: %s → %s, %d rows\n",
            first(index(sig_xts)), last(index(sig_xts)), nrow(sig_xts)))
cat(sprintf("Regime score: mean=%.3f sd=%.3f min=%.3f max=%.3f\n",
            mean(rs, na.rm = TRUE), sd(rs, na.rm = TRUE),
            min(rs, na.rm = TRUE), max(rs, na.rm = TRUE)))
cat(sprintf("  >0: %.1f%%, <0: %.1f%%, ~0(|.|<.1): %.1f%%\n",
            mean(rs > 0, na.rm = TRUE)*100, mean(rs < 0, na.rm = TRUE)*100,
            mean(abs(rs) < 0.1, na.rm = TRUE)*100))

# ============================================================================
# PHASE 2b — FAST-ACTING OVERLAY SIGNAL 🚀
# ============================================================================

cat("\n=== PHASE 2b: FAST-ACTING OVERLAY ===\n")

# VIX spike trigger: VIX > N std dev above 20-day average
vix_roll_avg <- as.numeric(rollapply(vixXts, OVERLAY_SPIKE_WINDOW, mean, fill = NA, align = "right"))
vix_roll_sd  <- as.numeric(rollapply(vixXts, OVERLAY_SPIKE_WINDOW, sd,   fill = NA, align = "right"))
vix_zscore   <- (vix_vals - vix_roll_avg) / vix_roll_sd
overlay_raw  <- vix_zscore > OVERLAY_SPIKE_SIGMA

# Overlay state machine: trigger → active with cooldown decay → re-entry check
overlay_active   <- logical(n)   # overlay is suppressing weight
overlay_decay_wt <- numeric(n)   # multiplier: 0 = fully active, 1 = fully released

days_since_trigger <- 0L
cooldown_remaining  <- 0L
quiet_days          <- 0L
in_overlay          <- FALSE

for (i in seq_len(n)) {
  if (overlay_raw[i] && !is.na(overlay_raw[i])) {
    # new trigger
    days_since_trigger <- 0L
    cooldown_remaining <- OVERLAY_COOLDOWN
    in_overlay         <- TRUE
    quiet_days         <- 0L
  }

  if (in_overlay) {
    days_since_trigger <- days_since_trigger + 1L
    if (cooldown_remaining > 0) {
      # linear decay: weight returns from reduced to full over cooldown days
      progress <- (OVERLAY_COOLDOWN - cooldown_remaining) / OVERLAY_COOLDOWN
      overlay_active[i]   <- TRUE
      overlay_decay_wt[i] <- OVERLAY_REDUCED_WT + progress * (1 - OVERLAY_REDUCED_WT)
      cooldown_remaining  <- cooldown_remaining - 1L
    } else {
      # cooldown done — check re-entry
      if (!overlay_raw[i] || is.na(overlay_raw[i])) {
        quiet_days <- quiet_days + 1L
      } else {
        quiet_days <- 0L
      }
      if (quiet_days >= OVERLAY_REENTRY) {
        in_overlay <- FALSE
        overlay_active[i]   <- FALSE
        overlay_decay_wt[i] <- 1.0
      } else {
        overlay_active[i]   <- TRUE
        overlay_decay_wt[i] <- 1.0  # past cooldown, back to full but still "active" for tracking
      }
    }
  } else {
    overlay_active[i]   <- FALSE
    overlay_decay_wt[i] <- 1.0
  }
}

overlay_xts <- na.omit(xts(cbind(active = overlay_active, decay = overlay_decay_wt), order.by = dates))
cat(sprintf("Overlay triggers: %d, active days: %d (%.1f%%), avg episode: %.1f days\n",
            sum(overlay_raw, na.rm = TRUE),
            sum(overlay_active, na.rm = TRUE),
            mean(overlay_active, na.rm = TRUE)*100,
            if (sum(overlay_raw, na.rm = TRUE) > 0)
              sum(overlay_active, na.rm = TRUE)/sum(overlay_raw, na.rm = TRUE) else 0))

# False positive rate: overlay triggers outside known fast-shock windows
shock_dates <- logical(n)
for (sw in FAST_SHOCK_WINDOWS) {
  shock_dates <- shock_dates | (dates >= as.Date(sw[1]) & dates <= as.Date(sw[2]))
}
fp_triggers <- sum(overlay_raw & !shock_dates, na.rm = TRUE)
tp_triggers <- sum(overlay_raw & shock_dates, na.rm = TRUE)
cat(sprintf("Trigger precision: %d true pos / %d total (%.1f%%)\n",
            tp_triggers, tp_triggers + fp_triggers,
            if (tp_triggers+fp_triggers > 0) tp_triggers/(tp_triggers+fp_triggers)*100 else 0))

# ============================================================================
# PHASE 3 — PORTFOLIO CONSTRUCTION
# ============================================================================

cat("\n=== PHASE 3: PORTFOLIO CONSTRUCTION ===\n")

sig_dates <- index(sig_xts)
ret_dates <- intersect(sig_dates, common_dates)
ret_dates <- ret_dates[-1]

# Align
n50_a    <- as.numeric(coredata(n50Ret[ret_dates]))
mid_a    <- as.numeric(coredata(midRet[ret_dates]))
sig_a    <- as.numeric(coredata(sig_xts[ret_dates]$regime_score))
ov_active_a <- as.numeric(coredata(overlay_xts[ret_dates]$active))
ov_decay_a  <- as.numeric(coredata(overlay_xts[ret_dates]$decay))

T_len <- length(ret_dates)
month_starts <- unique(as.Date(format(ret_dates, "%Y-%m-01")))
month_starts <- month_starts[month_starts >= ret_dates[1]]

# Trailing VIX for target_risk / VIX
trailing_vix_60 <- as.numeric(rollapply(vixXts, VOL_LOOKBACK, mean, fill = NA, align = "right"))
tvix_a <- trailing_vix_60[match(ret_dates, dates)]

# Inverse-vol split weights between two assets
invvol_split <- function(i) {
  d <- ret_dates[i]; ms <- max(month_starts[month_starts <= d])
  we <- ms - 1; ws <- we - VOL_LOOKBACK
  is <- which(ret_dates >= ws)[1]; ie <- max(which(ret_dates <= we))
  if (is.na(is) || is.na(ie) || ie - is < 20) return(c(0.5, 0.5))
  v1 <- sd(n50_a[is:ie], na.rm = TRUE); v2 <- sd(mid_a[is:ie], na.rm = TRUE)
  if (is.na(v1) || v1 == 0) v1 <- 1
  if (is.na(v2) || v2 == 0) v2 <- 1
  w1 <- (1/v1)/(1/v1 + 1/v2); c(w1, 1 - w1)
}

# --- Portfolio 1: B&H (100% NIFTY 50 TR) ---
port_bh <- n50_a

# --- Portfolio 2: Long-Only Inverse-Vol ---
port_lo <- numeric(T_len)
for (i in seq_len(T_len)) {
  sw <- invvol_split(i)
  et <- min(TARGET_RISK / max(tvix_a[i], 2.0, na.rm = TRUE), MAX_WEIGHT)  # avoid div/0
  if (is.na(et)) et <- 0.5
  w1 <- et * sw[1]; w2 <- et * sw[2]
  port_lo[i] <- w1 * n50_a[i] + w2 * mid_a[i]
}

# --- Portfolio 3: Long-Short (slow signal only) ---
rs_lag <- c(0, sig_a[-length(sig_a)])  # 1-day lag
port_ls  <- numeric(T_len)
ls_w1 <- ls_w2 <- numeric(T_len)
ls_is_short <- logical(T_len)

for (i in seq_len(T_len)) {
  sw <- invvol_split(i)
  base_et <- min(TARGET_RISK / max(tvix_a[i], 2.0, na.rm = TRUE), MAX_WEIGHT)
  if (is.na(base_et)) base_et <- 0.5
  tilted_et <- base_et * (1 + TILT_MAGNITUDE * rs_lag[i])
  tilted_et <- pmax(pmin(tilted_et, MAX_WEIGHT), MIN_WEIGHT)
  w1 <- tilted_et * sw[1]; w2 <- tilted_et * sw[2]
  to <- if (i > 1) abs(w1 - ls_w1[i-1]) + abs(w2 - ls_w2[i-1]) else 0
  port_ls[i] <- w1 * n50_a[i] + w2 * mid_a[i] - to * TRANSACTION_COST
  ls_w1[i] <- w1; ls_w2[i] <- w2
  ls_is_short[i] <- (w1 + w2) < 0
}

# --- Portfolio 4: Long-Short + Fast Overlay 🚀 ---
port_ov  <- numeric(T_len)
ov_w1 <- ov_w2 <- numeric(T_len)
ov_is_short <- logical(T_len)
ov_is_overlay <- logical(T_len)

for (i in seq_len(T_len)) {
  sw <- invvol_split(i)
  base_et <- min(TARGET_RISK / max(tvix_a[i], 2.0, na.rm = TRUE), MAX_WEIGHT)
  if (is.na(base_et)) base_et <- 0.5
  tilted_et <- base_et * (1 + TILT_MAGNITUDE * rs_lag[i])

  # Overlay override: multiply by decay weight when overlay active
  if (!is.na(ov_active_a[i]) && ov_active_a[i] > 0) {
    tilted_et <- tilted_et * ov_decay_a[i]
  }

  tilted_et <- pmax(pmin(tilted_et, MAX_WEIGHT), MIN_WEIGHT)
  w1 <- tilted_et * sw[1]; w2 <- tilted_et * sw[2]
  to <- if (i > 1) abs(w1 - ov_w1[i-1]) + abs(w2 - ov_w2[i-1]) else 0
  port_ov[i] <- w1 * n50_a[i] + w2 * mid_a[i] - to * TRANSACTION_COST
  ov_w1[i] <- w1; ov_w2[i] <- w2
  ov_is_short[i] <- (w1 + w2) < 0
  ov_is_overlay[i] <- (!is.na(ov_active_a[i]) && ov_active_a[i] > 0)
}

# Combine
all_ports <- na.omit(xts(cbind(port_bh, port_lo, port_ls, port_ov), order.by = ret_dates))
colnames(all_ports) <- c("B&H", "InvVol-LO", "InvVol-LS", "InvVol-LS+OV")
cat(sprintf("Portfolio returns: %s → %s, %d rows\n",
            first(index(all_ports)), last(index(all_ports)), nrow(all_ports)))

# ⚠ Short diagnostics
short_pct_ls <- mean(ls_is_short) * 100
short_pct_ov <- mean(ov_is_short) * 100
overlay_pct  <- mean(ov_is_overlay) * 100
cat(sprintf("LS:  net short %.1f%%, overlay active %.1f%%\n", short_pct_ls, overlay_pct))
cat(sprintf("LS+OV: net short %.1f%%\n", short_pct_ov))

if (short_pct_ls < MIN_SHORT_PCT && short_pct_ov < MIN_SHORT_PCT) {
  cat(sprintf("WARNING: short frequency below %d%% — short claim UNTESTED\n", MIN_SHORT_PCT))
}

# ============================================================================
# PHASE 4 — IN-SAMPLE METRICS
# ============================================================================

cat("\n=== PHASE 4: IN-SAMPLE METRICS ===\n")

compute_metrics <- function(rets_xts) {
  m <- list()
  for (col in colnames(rets_xts)) {
    r <- rets_xts[, col]; rv <- as.numeric(coredata(r))
    m[[col]] <- c(CAGR = Return.annualized(r)[1,1],
      Vol = sd(rv, na.rm = TRUE) * sqrt(252),
      Sharpe = SharpeRatio.annualized(r)[1,1],
      Sortino = SortinoRatio(r)[1,1],
      MaxDD = maxDrawdown(r),
      Calmar = Return.annualized(r)[1,1] / maxDrawdown(r),
      CumReturn = prod(1 + rv, na.rm = TRUE) - 1)
  }
  do.call(cbind, m)
}

fm <- compute_metrics(all_ports)
cat("\nFull Sample:\n")
print(round(fm, 4))

# ⚠ Conditional decomposition (LS)
ls_vals_p <- as.numeric(coredata(all_ports$`InvVol-LS`))
long_days  <- ls_vals_p[!ls_is_short]
short_days <- ls_vals_p[ls_is_short]

if (length(short_days) >= 20) {
  long_xts  <- xts(long_days, order.by = ret_dates[!ls_is_short])
  short_xts <- xts(short_days, order.by = ret_dates[ls_is_short])
  long_sr  <- SharpeRatio.annualized(long_xts)[1,1]
  short_sr <- SharpeRatio.annualized(short_xts)[1,1]
  long_cagr <- Return.annualized(long_xts)[1,1]
  short_cagr <- Return.annualized(short_xts)[1,1]
  cat(sprintf("\nLS Conditional: Long(%.0f%%) CAGR=%.4f SR=%.4f | Short(%.1f%%) CAGR=%.4f SR=%.4f\n",
              100-short_pct_ls, long_cagr, long_sr, short_pct_ls, short_cagr, short_sr))
  if (short_sr < 0) cat("  → Short leg NEGATIVE Sharpe — aggregate carried by long side.\n")
}

# 🚀 Overlay conditional decomposition
ov_vals_p <- as.numeric(coredata(all_ports$`InvVol-LS+OV`))
ov_normal <- ov_vals_p[!ov_is_overlay]
ov_active  <- ov_vals_p[ov_is_overlay]
if (length(ov_active) >= 5) {
  ov_norm_sr <- SharpeRatio.annualized(xts(ov_normal, order.by = ret_dates[!ov_is_overlay]))[1,1]
  ov_act_sr  <- SharpeRatio.annualized(xts(ov_active, order.by = ret_dates[ov_is_overlay]))[1,1]
  cat(sprintf("\nOverlay Conditional: Normal(%.1f%%) SR=%.4f | Active(%.1f%%) SR=%.4f\n",
              100-overlay_pct, ov_norm_sr, overlay_pct, ov_act_sr))
}

# 🚀 Crash-window comparison table
cat("\n--- Crash-Window Drawdown Comparison ---\n")
for (sw_name in names(FAST_SHOCK_WINDOWS)) {
  sw <- FAST_SHOCK_WINDOWS[[sw_name]]
  win_xts <- all_ports[paste0(sw[1], "/", sw[2])]
  if (nrow(win_xts) < 5) { cat(sprintf("  %s: insufficient data\n", sw_name)); next }
  dd_lo  <- maxDrawdown(win_xts$`InvVol-LO`)
  dd_ls  <- maxDrawdown(win_xts$`InvVol-LS`)
  dd_ov  <- maxDrawdown(win_xts$`InvVol-LS+OV`)
  cat(sprintf("  %s (%d days): LO DD=%.1f%%, LS DD=%.1f%%, LS+OV DD=%.1f%%\n",
              sw_name, nrow(win_xts), dd_lo*100, dd_ls*100, dd_ov*100))
}

# ============================================================================
# PHASE 5 — STATISTICAL ROBUSTNESS
# ============================================================================

cat("\n=== PHASE 5: STATISTICAL ROBUSTNESS ===\n")

# 5a. Block bootstrap LS-LO
set.seed(42)
spread_ls <- all_ports$`InvVol-LS` - all_ports$`InvVol-LO`
sv_ls <- as.numeric(coredata(spread_ls))
obs_ls_diff <- SharpeRatio.annualized(spread_ls)[1,1]

bsize <- 63L; nobs <- length(sv_ls); nblk <- ceiling(nobs/bsize)
B <- 2000L; boot_ls <- numeric(B)
for (b in seq_len(B)) {
  st <- sample(seq_len(nobs - bsize + 1L), nblk, replace = TRUE)
  bs <- numeric(0)
  for (k in seq_len(nblk)) bs <- c(bs, sv_ls[st[k]:(st[k]+bsize-1)])
  bs <- bs[1:nobs]
  boot_ls[b] <- if (sd(bs) == 0) NA else SharpeRatio.annualized(xts(bs, order.by = ret_dates))[1,1]
}
boot_ls <- na.omit(boot_ls)
p_ls <- mean(abs(boot_ls) >= abs(obs_ls_diff))
ci_ls <- quantile(boot_ls, c(0.025, 0.975))
cat(sprintf("Bootstrap LS−LO: obs=%.4f p=%.4f CI=[%.4f,%.4f]\n", obs_ls_diff, p_ls, ci_ls[1], ci_ls[2]))

# ⚠ Short-leg bootstrap
if (length(short_days) >= 20) {
  Bs <- 2000L; boot_sm <- numeric(Bs)
  for (b in seq_len(Bs)) {
    st <- sample(seq_len(max(1, length(short_days)-bsize+1)), ceiling(length(short_days)/bsize), replace = TRUE)
    bs <- numeric(0)
    for (k in seq_along(st)) bs <- c(bs, short_days[st[k]:(st[k]+bsize-1)])
    boot_sm[b] <- mean(bs[1:length(short_days)], na.rm = TRUE)
  }
  p_sm <- mean(abs(boot_sm) >= abs(mean(short_days, na.rm = TRUE)))
  ci_sm <- quantile(boot_sm, c(0.025, 0.975))
  cat(sprintf("Short-leg bootstrap: mean=%.6f p=%.4f CI=[%.6f,%.6f]\n",
              mean(short_days, na.rm = TRUE), p_sm, ci_sm[1], ci_sm[2]))
}

# 5b. Subperiod + grinding-bull
cat("\n--- Subperiod Decomposition ---\n")
all_periods <- list(
  "Pre-2015" = c("2009-01-01","2014-12-31"),
  "2015-2019" = c("2015-01-01","2019-12-31"),
  "COVID2020" = c("2020-01-01","2020-12-31"),
  "2021-2022" = c("2021-01-01","2022-12-31"),
  "2023-Pres" = c("2023-01-01","2026-12-31"),
  "ExclCrisis" = c("2009-01-01","2026-12-31")
)

for (pname in names(all_periods)) {
  pr <- all_periods[[pname]]; sub <- all_ports[paste0(pr[1],"/",pr[2])]
  if (pname == "ExclCrisis") {
    m <- !(index(sub) >= as.Date("2020-01-01") & index(sub) <= as.Date("2020-12-31"))
    m <- m & !(index(sub) >= as.Date("2022-01-01") & index(sub) <= as.Date("2022-12-31"))
    sub <- sub[m]
  }
  if (nrow(sub) < 60) next
  sm <- compute_metrics(sub)
  cat(sprintf("\n  %s (%d days):\n", pname, nrow(sub)))
  for (col in colnames(sm)) {
    cat(sprintf("    %-14s CAGR=%-7.4f Sharpe=%-7.4f MaxDD=%-7.4f\n",
                col, sm["CAGR",col], sm["Sharpe",col], sm["MaxDD",col]))
  }
}

cat("\n--- Grinding-Bull Stress Test ---\n")
for (gb_name in names(GRINDING_BULL_WINDOWS)) {
  gb <- all_ports[paste0(GRINDING_BULL_WINDOWS[[gb_name]][1],"/",
                          GRINDING_BULL_WINDOWS[[gb_name]][2])]
  if (nrow(gb) < 20) next
  gm <- compute_metrics(gb)
  cat(sprintf("  %s (%d days):\n", gb_name, nrow(gb)))
  for (col in colnames(gm))
    cat(sprintf("    %-14s CAGR=%-7.4f Sharpe=%-7.4f\n", col, gm["CAGR",col], gm["Sharpe",col]))
}

# 5c. Parameter sweep (slow signal: window × tilt)
cat("\n--- Slow Signal Sweep (window × tilt) ---\n")
wins <- c(63L, 126L, 189L, 252L); tilts <- c(0.5, 1.0, 1.5, 2.0)
rcache <- list()
for (w in wins) {
  ra <- as.numeric(rollapply(vixXts, w, mean, fill = NA, align = "right"))
  rcache[[as.character(w)]] <- ra
}
sg <- expand.grid(win=wins, tilt=tilts)
cat(sprintf("Running %d combinations...\n", nrow(sg)))
pb <- txtProgressBar(0, nrow(sg), style=3)
sw_sr <- sw_sp <- numeric(nrow(sg))

for (ri in seq_len(nrow(sg))) {
  w <- sg$win[ri]; tlt <- sg$tilt[ri]; ra <- rcache[[as.character(w)]]
  ls_w <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (is.na(ra[i])) next
    prior <- ra[1:i]; prior <- prior[!is.na(prior)]
    if (length(prior) < 20) next
    ls_w[i] <- 2*(ecdf(prior)(ra[i]) - 0.5)
  }
  # trend with base VIX_TREND_WINDOW
  ts_w <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (is.na(roll_slp[i])) next
    ws <- max(1L, i-VIX_TREND_WINDOW+1L)
    wv <- roll_slp[ws:i]; wv <- wv[!is.na(wv)]
    if (length(wv) < 20) next
    mu <- mean(wv); sig <- sd(wv)
    ts_w[i] <- if(sig==0) 0 else tanh((roll_slp[i]-mu)/sig)
  }
  rsw <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (is.na(ls_w[i]) || is.na(ts_w[i])) next
    rsw[i] <- pmax(pmin(max(ls_w[i],0)*max(-ts_w[i],0) - max(-ls_w[i],0)*max(ts_w[i],0), 1), -1)
  }
  swx <- na.omit(xts(cbind(rs=rsw), order.by=dates))
  rdw <- intersect(index(swx), common_dates); rdw <- rdw[-1]
  if (length(rdw) < 100) { sw_sr[ri] <- NA; sw_sp[ri] <- NA; setTxtProgressBar(pb,ri); next }
  r1w <- as.numeric(coredata(n50Ret[rdw])); r2w <- as.numeric(coredata(midRet[rdw]))
  s_a  <- as.numeric(coredata(swx[rdw]$rs)); s_l <- c(0, s_a[-length(s_a)])
  tw_a <- trailing_vix_60[match(rdw, dates)]
  MSw  <- unique(as.Date(format(rdw,"%Y-%m-01"))); MSw <- MSw[MSw >= rdw[1]]
  pl_w <- numeric(length(rdw)); p0_w <- numeric(length(rdw)); ish <- logical(length(rdw))
  for (i in seq_along(rdw)) {
    d <- rdw[i]; ms <- max(MSw[MSw <= d]); we <- ms-1; ws <- we - VOL_LOOKBACK
    is <- which(rdw >= ws)[1]; ie <- max(which(rdw <= we))
    if (is.na(is)||is.na(ie)||ie-is<20) { sw_v <- c(0.5,0.5) }
    else {
      v1 <- sd(r1w[is:ie],na.rm=TRUE); v2 <- sd(r2w[is:ie],na.rm=TRUE)
      if(is.na(v1)||v1==0) v1<-1; if(is.na(v2)||v2==0) v2<-1
      sw_v <- c((1/v1)/(1/v1+1/v2), 1-(1/v1)/(1/v1+1/v2))
    }
    be <- min(TARGET_RISK/max(tw_a[i],2.0,na.rm=TRUE), MAX_WEIGHT)
    if(is.na(be)) be<-0.5
    te <- pmax(pmin(be*(1+tlt*s_l[i]), MAX_WEIGHT), MIN_WEIGHT)
    pl_w[i] <- te*sw_v[1]*r1w[i] + te*sw_v[2]*r2w[i]
    p0_w[i] <- be*sw_v[1]*r1w[i] + be*sw_v[2]*r2w[i]
    ish[i] <- te < 0
  }
  sp_w <- na.omit(xts(pl_w - p0_w, order.by=rdw))
  sw_sr[ri] <- if(nrow(sp_w)>=100 && sd(coredata(sp_w),na.rm=TRUE)>0)
    SharpeRatio.annualized(sp_w)[1,1] else NA
  sw_sp[ri] <- mean(ish)*100
  setTxtProgressBar(pb, ri)
}
close(pb)
sg$SR_diff <- sw_sr; sg$ShortPct <- sw_sp
write.csv(sg, sprintf("%s/sweep_slow.csv", reportPath), row.names=FALSE)

cat(sprintf("\nSweep: %d valid\n", sum(!is.na(sg$SR_diff))))
if (any(!is.na(sg$SR_diff))) {
  bst <- which.max(sg$SR_diff)
  cat(sprintf("  Best: SR=%.4f (win=%d tilt=%.1f short=%.1f%%)\n",
              sg$SR_diff[bst], sg$win[bst], sg$tilt[bst], sg$ShortPct[bst]))
  cat(sprintf("  Mean SR diff: %.4f\n", mean(sg$SR_diff, na.rm=TRUE)))
}

# 5d. Placebo: shuffle overlay trigger timing
cat("\n--- Placebo: Overlay Trigger Randomization ---\n")
if (sum(ov_is_overlay) > 5) {
  n_placebo <- 500L
  placebo_dds <- numeric(n_placebo)
  obs_ov_dd <- maxDrawdown(all_ports$`InvVol-LS+OV`)
  shock_mask <- ret_dates >= as.Date(FAST_SHOCK_WINDOWS[[1]][1]) &
                ret_dates <= as.Date(FAST_SHOCK_WINDOWS[[1]][2])
  for (pb_i in seq_len(n_placebo)) {
    # shuffle overlay active flags within shock window only
    orig <- ov_is_overlay[shock_mask]
    shuffled <- sample(orig)
    ov_placebo <- ov_is_overlay
    ov_placebo[shock_mask] <- shuffled
    # rebuild portfolio with placebo overlay
    pp <- numeric(T_len); pw1 <- pw2 <- numeric(T_len)
    for (i in seq_len(T_len)) {
      sw <- invvol_split(i)
      be <- min(TARGET_RISK/max(tvix_a[i],2.0,na.rm=TRUE), MAX_WEIGHT)
      if(is.na(be)) be<-0.5
      te <- be*(1+TILT_MAGNITUDE*rs_lag[i])
      if(ov_placebo[i]) te <- te*ov_decay_a[i]
      te <- pmax(pmin(te,MAX_WEIGHT),MIN_WEIGHT)
      w1<-te*sw[1]; w2<-te*sw[2]
      to <- if(i>1) abs(w1-pw1[i-1])+abs(w2-pw2[i-1]) else 0
      pp[i]<-w1*n50_a[i]+w2*mid_a[i]-to*TRANSACTION_COST
      pw1[i]<-w1; pw2[i]<-w2
    }
    pp_xts <- xts(pp, order.by=ret_dates)
    placebo_dds[pb_i] <- maxDrawdown(pp_xts)
    if (pb_i %% 100 == 0) cat(".")
  }
  cat(sprintf("\n  Observed LS+OV MaxDD: %.4f\n", obs_ov_dd))
  cat(sprintf("  Placebo mean MaxDD:   %.4f (sd=%.4f)\n", mean(placebo_dds), sd(placebo_dds)))
  p_placebo <- mean(placebo_dds <= obs_ov_dd)
  cat(sprintf("  p(placebo DD ≤ observed): %.3f\n", p_placebo))
}

# 5e. Overlay-specific sensitivity sweep 🚀
cat("\n--- Overlay Sensitivity Sweep ---\n")
ov_sigmas <- c(2.0, 2.5, 3.0, 3.5, 4.0)
ov_cools  <- c(5L, 10L, 20L)
ov_grid <- expand.grid(sigma=ov_sigmas, cool=ov_cools)
ov_dd_red <- ov_fp <- numeric(nrow(ov_grid))

for (ri in seq_len(nrow(ov_grid))) {
  sg <- ov_grid$sigma[ri]; cl <- ov_grid$cool[ri]
  ov_raw <- vix_zscore > sg
  ov_act <- logical(n); ov_dec <- numeric(n)
  dsince <- 0L; crem <- 0L; qdays <- 0L; iov <- FALSE
  for (i in seq_len(n)) {
    if (ov_raw[i] && !is.na(ov_raw[i])) { dsince<-0L; crem<-cl; iov<-TRUE; qdays<-0L }
    if (iov) {
      dsince <- dsince+1L
      if (crem>0) { ov_act[i]<-TRUE; ov_dec[i]<-OVERLAY_REDUCED_WT+(1-OVERLAY_REDUCED_WT)*(cl-crem)/cl; crem<-crem-1L }
      else { if(!ov_raw[i]||is.na(ov_raw[i])) qdays<-qdays+1L else qdays<-0L
        if(qdays>=OVERLAY_REENTRY) { iov<-FALSE; ov_act[i]<-FALSE; ov_dec[i]<-1.0 }
        else { ov_act[i]<-TRUE; ov_dec[i]<-1.0 }
      }
    } else { ov_act[i]<-FALSE; ov_dec[i]<-1.0 }
  }
  ov_a <- ov_act[match(ret_dates, dates)]; ov_d <- ov_dec[match(ret_dates, dates)]
  pp <- numeric(T_len); pw1<-pw2<-numeric(T_len)
  for (i in seq_len(T_len)) {
    sw <- invvol_split(i)
    be <- min(TARGET_RISK/max(tvix_a[i],2.0,na.rm=TRUE), MAX_WEIGHT)
    if(is.na(be)) be<-0.5
    te <- be*(1+TILT_MAGNITUDE*rs_lag[i])
    if(!is.na(ov_a[i])&&ov_a[i]) te<-te*ov_d[i]
    te <- pmax(pmin(te,MAX_WEIGHT),MIN_WEIGHT)
    w1<-te*sw[1]; w2<-te*sw[2]
    to<-if(i>1) abs(w1-pw1[i-1])+abs(w2-pw2[i-1]) else 0
    pp[i]<-w1*n50_a[i]+w2*mid_a[i]-to*TRANSACTION_COST; pw1[i]<-w1; pw2[i]<-w2
  }
  sw_sub <- all_ports[paste0(FAST_SHOCK_WINDOWS[[1]][1],"/",FAST_SHOCK_WINDOWS[[1]][2])]
  pp_xts <- xts(pp, order.by=ret_dates)
  pp_sub <- pp_xts[paste0(FAST_SHOCK_WINDOWS[[1]][1],"/",FAST_SHOCK_WINDOWS[[1]][2])]
  ov_dd_red[ri] <- maxDrawdown(sw_sub$`InvVol-LO`) - maxDrawdown(pp_sub)
  ov_fp[ri] <- sum(ov_a & !shock_mask, na.rm=TRUE) / max(1, sum(ov_a, na.rm=TRUE)) * 100
}
ov_grid$DD_reduction <- ov_dd_red; ov_grid$FP_rate <- ov_fp
write.csv(ov_grid, sprintf("%s/sweep_overlay.csv", reportPath), row.names=FALSE)
cat(sprintf("Overlay sweep: %d combos, DD reduction range [%.4f, %.4f], FP rate [%.0f%%, %.0f%%]\n",
            nrow(ov_grid), min(ov_dd_red,na.rm=TRUE), max(ov_dd_red,na.rm=TRUE),
            min(ov_fp,na.rm=TRUE), max(ov_fp,na.rm=TRUE)))

# ============================================================================
# PHASE 6 — OUT-OF-SAMPLE
# ============================================================================

cat("\n=== PHASE 6: OUT-OF-SAMPLE ===\n")

is_idx <- which(ret_dates <= as.Date(TRAIN_END_DATE))
os_idx <- which(ret_dates > as.Date(TRAIN_END_DATE))

if (length(is_idx) >= 252 && length(os_idx) >= 252) {
  for (col in colnames(all_ports)) {
    is_sr <- SharpeRatio.annualized(all_ports[is_idx, col])[1,1]
    os_sr <- SharpeRatio.annualized(all_ports[os_idx, col])[1,1]
    cat(sprintf("  %-14s IS SR=%.3f  OS SR=%.3f\n", col, is_sr, os_sr))
  }

  # Bug: short-leg conditional Sharpe IS vs OS (for LS)
  os_ls <- as.numeric(coredata(all_ports$`InvVol-LS`[os_idx]))
  os_short <- os_ls[ls_is_short[os_idx]]
  is_short <- ls_vals_p[ls_is_short & (seq_len(T_len) %in% is_idx)]
  if (length(is_short) >= 20 && length(os_short) >= 20) {
    is_ssr <- SharpeRatio.annualized(xts(is_short,
      order.by = ret_dates[is_idx][ls_is_short[is_idx]]))[1,1]
    os_ssr <- SharpeRatio.annualized(xts(os_short,
      order.by = ret_dates[os_idx][ls_is_short[os_idx]]))[1,1]
    cat(sprintf("  LS Short-leg:  IS SR=%.3f  OS SR=%.3f\n", is_ssr, os_ssr))
  }

  # 🚀 Overlay OS: fit overlay params on IS only, test on OS
  cat("\n  --- Overlay Out-of-Sample ---\n")
  is_vix   <- vix_vals[match(ret_dates[is_idx], dates)]
  is_va    <- vix_roll_avg[match(ret_dates[is_idx], dates)]
  is_vs    <- vix_roll_sd[match(ret_dates[is_idx], dates)]
  is_z     <- (is_vix - is_va) / is_vs
  # Compute overlay on OS using IS-fitted z-score but OS VIX data
  os_vix   <- vix_vals[match(ret_dates[os_idx], dates)]
  os_va    <- vix_roll_avg[match(ret_dates[os_idx], dates)]
  os_vs    <- vix_roll_sd[match(ret_dates[os_idx], dates)]
  os_z     <- (os_vix - os_va) / os_vs
  os_raw   <- os_z > OVERLAY_SPIKE_SIGMA
  ov_act_os <- logical(length(os_idx)); ov_dec_os <- numeric(length(os_idx))
  dsince<-0L; crem<-0L; qdays<-0L; iov<-FALSE
  for (i in seq_along(os_idx)) {
    if (os_raw[i] && !is.na(os_raw[i])) { dsince<-0L; crem<-OVERLAY_COOLDOWN; iov<-TRUE; qdays<-0L }
    if (iov) {
      dsince <- dsince+1L
      if(crem>0) { ov_act_os[i]<-TRUE; ov_dec_os[i]<-OVERLAY_REDUCED_WT+(1-OVERLAY_REDUCED_WT)*(OVERLAY_COOLDOWN-crem)/OVERLAY_COOLDOWN; crem<-crem-1L }
      else { if(!os_raw[i]||is.na(os_raw[i])) qdays<-qdays+1L else qdays<-0L
        if(qdays>=OVERLAY_REENTRY) { iov<-FALSE; ov_act_os[i]<-FALSE; ov_dec_os[i]<-1.0 }
        else { ov_act_os[i]<-TRUE; ov_dec_os[i]<-1.0 } }
    } else { ov_act_os[i]<-FALSE; ov_dec_os[i]<-1.0 }
  }
  cat(sprintf("  OS overlay triggers: %d, active days: %d (%.1f%%)\n",
              sum(os_raw,na.rm=TRUE), sum(ov_act_os), mean(ov_act_os)*100))

  # Chart
  source("/mnt/hollandC/StockViz/R/plot.common.r")
  os_xts <- all_ports[os_idx]
  os_sr_txt <- paste0(colnames(os_xts), "=",
    round(sapply(colnames(os_xts), function(nm) SharpeRatio.annualized(os_xts[,nm])[1,1]),2),
    collapse=", ")
  Common.PlotCumReturns(os_xts, "VIX Regime LS+Overlay — Out-of-Sample",
    sprintf("OS (%s → %s) | SR: %s", TRAIN_END_DATE, last(index(os_xts)), os_sr_txt),
    sprintf("%s/cumulative_os.png", reportPath), NULL)
}

# ============================================================================
# PHASE 7 — IMPLEMENTATION ROBUSTNESS
# ============================================================================

cat("\n=== PHASE 7: IMPLEMENTATION ROBUSTNESS ===\n")

sr_lo_val <- SharpeRatio.annualized(all_ports$`InvVol-LO`)[1,1]

# Cost sensitivity
cat("Cost sensitivity:\n")
for (cst in c(0.001, 0.002, 0.005, 0.01)) {
  pc <- numeric(T_len); pw1<-pw2<-numeric(T_len)
  for (i in seq_len(T_len)) {
    sw<-invvol_split(i); be<-min(TARGET_RISK/max(tvix_a[i],2.0,na.rm=TRUE),MAX_WEIGHT)
    if(is.na(be)) be<-0.5; te<-pmax(pmin(be*(1+TILT_MAGNITUDE*rs_lag[i]),MAX_WEIGHT),MIN_WEIGHT)
    if(!is.na(ov_active_a[i])&&ov_active_a[i]) te<-te*ov_decay_a[i]
    te<-pmax(pmin(te,MAX_WEIGHT),MIN_WEIGHT)
    w1<-te*sw[1]; w2<-te*sw[2]
    to<-if(i>1) abs(w1-pw1[i-1])+abs(w2-pw2[i-1]) else 0
    pc[i]<-w1*n50_a[i]+w2*mid_a[i]-to*cst; pw1[i]<-w1; pw2[i]<-w2
  }
  sr <- SharpeRatio.annualized(xts(pc,order.by=ret_dates))[1,1]
  cat(sprintf("  Cost=%.0fbp: LS+OV SR=%.4f  vs LO=%.4f  diff=%.4f\n", cst*100, sr, sr_lo_val, sr-sr_lo_val))
}

# Borrow cost
cat(sprintf("\nBorrow cost (%.0f%% annual):\n", BORROW_COST_ANNUAL*100))
dbc <- (1+BORROW_COST_ANNUAL)^(1/252)-1
pb <- numeric(T_len); pw1<-pw2<-numeric(T_len)
for (i in seq_len(T_len)) {
  sw<-invvol_split(i); be<-min(TARGET_RISK/max(tvix_a[i],2.0,na.rm=TRUE),MAX_WEIGHT)
  if(is.na(be)) be<-0.5; te<-pmax(pmin(be*(1+TILT_MAGNITUDE*rs_lag[i]),MAX_WEIGHT),MIN_WEIGHT)
  if(!is.na(ov_active_a[i])&&ov_active_a[i]) te<-te*ov_decay_a[i]
  te<-pmax(pmin(te,MAX_WEIGHT),MIN_WEIGHT)
  w1<-te*sw[1]; w2<-te*sw[2]
  to<-if(i>1) abs(w1-pw1[i-1])+abs(w2-pw2[i-1]) else 0
  gross<-w1*n50_a[i]+w2*mid_a[i]-to*TRANSACTION_COST
  pb[i]<-gross - max(0,-(w1+w2))*dbc; pw1[i]<-w1; pw2[i]<-w2
}
sr_b <- SharpeRatio.annualized(xts(pb,order.by=ret_dates))[1,1]
cat(sprintf("  LS+OV+bwr SR=%.4f vs LO=%.4f diff=%.4f\n", sr_b, sr_lo_val, sr_b-sr_lo_val))

# Weight bounds
cat("Weight bounds:\n")
for (mw in c(-0.5,-1.0,-1.5,-2.0)) {
  pw<-numeric(T_len); sc<-0L; pw1<-pw2<-numeric(T_len)
  for (i in seq_len(T_len)) {
    sw<-invvol_split(i); be<-min(TARGET_RISK/max(tvix_a[i],2.0,na.rm=TRUE),MAX_WEIGHT)
    if(is.na(be)) be<-0.5; te<-pmax(pmin(be*(1+TILT_MAGNITUDE*rs_lag[i]),MAX_WEIGHT),mw)
    if(!is.na(ov_active_a[i])&&ov_active_a[i]) te<-te*ov_decay_a[i]
    te<-pmax(pmin(te,MAX_WEIGHT),mw)
    w1<-te*sw[1]; w2<-te*sw[2]
    to<-if(i>1) abs(w1-pw1[i-1])+abs(w2-pw2[i-1]) else 0
    pw[i]<-w1*n50_a[i]+w2*mid_a[i]-to*TRANSACTION_COST; pw1[i]<-w1; pw2[i]<-w2
    if((w1+w2)<0) sc<-sc+1L
  }
  sr <- SharpeRatio.annualized(xts(pw,order.by=ret_dates))[1,1]
  cat(sprintf("  MIN=%.1f: SR=%.4f short=%.1f%% diff=%.4f\n", mw, sr, sc/T_len*100, sr-sr_lo_val))
}

# Overlay cooldown sensitivity
cat("Overlay cooldown sensitivity:\n")
for (cl in c(5L, 10L, 20L, 30L)) {
  ov_a_cl <- logical(n); ov_d_cl <- numeric(n)
  dsince<-0L; crem<-0L; qdays<-0L; iov<-FALSE
  for (i in seq_len(n)) {
    if (overlay_raw[i]&&!is.na(overlay_raw[i])) { dsince<-0L; crem<-cl; iov<-TRUE; qdays<-0L }
    if (iov) {
      dsince<-dsince+1L
      if(crem>0) { ov_a_cl[i]<-TRUE; ov_d_cl[i]<-OVERLAY_REDUCED_WT+(1-OVERLAY_REDUCED_WT)*(cl-crem)/cl; crem<-crem-1L }
      else { if(!overlay_raw[i]||is.na(overlay_raw[i])) qdays<-qdays+1L else qdays<-0L
        if(qdays>=OVERLAY_REENTRY) { iov<-FALSE; ov_a_cl[i]<-FALSE; ov_d_cl[i]<-1.0 }
        else { ov_a_cl[i]<-TRUE; ov_d_cl[i]<-1.0 } }
    } else { ov_a_cl[i]<-FALSE; ov_d_cl[i]<-1.0 }
  }
  oa <- ov_a_cl[match(ret_dates,dates)]; od <- ov_d_cl[match(ret_dates,dates)]
  pp <- numeric(T_len); pw1<-pw2<-numeric(T_len)
  for (i in seq_len(T_len)) {
    sw<-invvol_split(i); be<-min(TARGET_RISK/max(tvix_a[i],2.0,na.rm=TRUE),MAX_WEIGHT)
    if(is.na(be)) be<-0.5; te<-pmax(pmin(be*(1+TILT_MAGNITUDE*rs_lag[i]),MAX_WEIGHT),MIN_WEIGHT)
    if(!is.na(oa[i])&&oa[i]) te<-te*od[i]
    te<-pmax(pmin(te,MAX_WEIGHT),MIN_WEIGHT)
    w1<-te*sw[1]; w2<-te*sw[2]
    to<-if(i>1) abs(w1-pw1[i-1])+abs(w2-pw2[i-1]) else 0
    pp[i]<-w1*n50_a[i]+w2*mid_a[i]-to*TRANSACTION_COST; pw1[i]<-w1; pw2[i]<-w2
  }
  pp_x <- xts(pp, order.by=ret_dates)
  sr_cl <- SharpeRatio.annualized(pp_x)[1,1]
  dd_cl <- maxDrawdown(pp_x)
  cat(sprintf("  Cool=%dd: SR=%.4f MaxDD=%.4f active=%.1f%%\n", cl, sr_cl, dd_cl, mean(oa,na.rm=TRUE)*100))
}

# ============================================================================
# PHASE 8 — REPORTING
# ============================================================================

cat("\n=== PHASE 8: REPORTING ===\n")

cat("\n===== LS+OVERLAY BACKTEST SUMMARY =====\n")
cat(sprintf("Data: %s → %s (%d days)\n", first(index(all_ports)), last(index(all_ports)), nrow(all_ports)))
cat(sprintf("Assets: NIFTY 50 TR + NIFTY MIDCAP SELECT TR\n"))
cat(sprintf("Base weight: %.0f%%/VIX, cap [%.1f, %.1f], tilt ±%.1f\n",
            TARGET_RISK*100, MIN_WEIGHT, MAX_WEIGHT, TILT_MAGNITUDE))
cat(sprintf("Overlay: VIX>%.0fσ above %dd avg, cool=%dd, re-entry=%dd\n",
            OVERLAY_SPIKE_SIGMA, OVERLAY_SPIKE_WINDOW, OVERLAY_COOLDOWN, OVERLAY_REENTRY))
cat(sprintf("TC: %.1f%%, borrow: %.1f%%/yr\n", TRANSACTION_COST*100, BORROW_COST_ANNUAL*100))
cat("\n")

cat("=== Full Sample Metrics ===\n")
print(round(fm, 4))
cat("\n")

cat("=== Regime Score Distribution ===\n")
cat(sprintf("  mean=%.3f sd=%.3f, >0: %.1f%% <0: %.1f%% ~0: %.1f%%\n",
            mean(rs,na.rm=TRUE), sd(rs,na.rm=TRUE),
            mean(rs>0,na.rm=TRUE)*100, mean(rs<0,na.rm=TRUE)*100,
            mean(abs(rs)<0.1,na.rm=TRUE)*100))
cat("\n")

cat("=== Short-Leg Diagnostics ===\n")
cat(sprintf("  LS:  %.1f%% net short, exposure: mean=%.3f median=%.3f min=%.3f\n",
            short_pct_ls, mean(ls_w1+ls_w2), median(ls_w1+ls_w2), min(ls_w1+ls_w2)))
cat(sprintf("  LS+OV: %.1f%% net short, overlay active %.1f%%\n", short_pct_ov, overlay_pct))
cat("\n")

cat("=== Conditional Performance ===\n")
if (length(short_days) >= 20)
  cat(sprintf("  LS long-cond: SR=%.4f, short-cond: SR=%.4f\n", long_sr, short_sr))
if (length(ov_active) >= 5)
  cat(sprintf("  LS+OV overlay-normal: SR=%.4f, overlay-active: SR=%.4f\n", ov_norm_sr, ov_act_sr))
cat("\n")

cat("=== Bootstrap ===\n")
cat(sprintf("  LS−LO: %.4f, p=%.4f, CI=[%.4f,%.4f]\n", obs_ls_diff, p_ls, ci_ls[1], ci_ls[2]))
if (length(short_days) >= 20)
  cat(sprintf("  Short-leg: mean=%.6f, p=%.4f, CI=[%.6f,%.6f]\n",
              mean(short_days,na.rm=TRUE), p_sm, ci_sm[1], ci_sm[2]))
cat(sprintf("  Placebo DD p=%.3f\n", if(exists("p_placebo")) p_placebo else NA))
cat("\n")

cat("=== Crticial Findings ===\n")
cat(sprintf("1. Short freq: LS=%.1f%% LS+OV=%.1f%% ", short_pct_ls, short_pct_ov))
if (short_pct_ls < MIN_SHORT_PCT && short_pct_ov < MIN_SHORT_PCT)
  cat("(UNTESTED — below threshold)")
cat("\n")
if (length(short_days) >= 20) {
  cat(sprintf("2. Short-leg Sharpe: %.4f ", short_sr))
  cat(if(short_sr<0)"(NEGATIVE)"else"(POSITIVE)")
  cat("\n")
}
cat(sprintf("3. LS−LO diff: %.4f, p=%.4f\n", obs_ls_diff, p_ls))
cat(sprintf("4. OVERALL: %s\n",
  if(short_pct_ls<MIN_SHORT_PCT&&short_pct_ov<MIN_SHORT_PCT)
    "Short leg barely activates. Long-short is long-only in practice."
  else if(exists("short_sr")&&short_sr<0)
    "Short leg fires but LOSES money. Edge purely from long side."
  else "Short leg fires and contributes."))

cat(sprintf("\n===== END =====\n"))

# GT table
fm_tbl <- as.data.frame(t(fm))
fm_tbl$Strategy <- rownames(fm_tbl); fm_tbl <- fm_tbl |> select(Strategy, everything())
rownames(fm_tbl) <- NULL

gt_tbl <- fm_tbl |> gt() |>
  tab_header(title="VIX Regime LS+Overlay — Full Sample",
    subtitle=sprintf("%s → %s | base=%.0f%%/VIX tilt=±%.1f overlay=%.0fσ",
      first(index(all_ports)), last(index(all_ports)),
      TARGET_RISK*100, TILT_MAGNITUDE, OVERLAY_SPIKE_SIGMA)) |>
  fmt_percent(c(CAGR, Vol, MaxDD, CumReturn), decimals=2) |>
  fmt_number(c(Sharpe, Sortino, Calmar), decimals=2) |>
  tab_style(cell_text(weight="bold"), cells_column_labels()) |>
  tab_source_note("@StockViz") |>
  tab_style(cell_text(align="right"), cells_source_notes()) |>
  tab_style(cell_fill("#fff8e1"), cells_body(rows=fm_tbl$Strategy=="B&H")) |>
  tab_style(cell_fill("#f0fff0"), cells_body(rows=fm_tbl$Strategy=="InvVol-LO")) |>
  tab_style(cell_fill("#f0f4ff"), cells_body(rows=fm_tbl$Strategy=="InvVol-LS")) |>
  tab_style(cell_fill("#ffe0f0"), cells_body(rows=fm_tbl$Strategy=="InvVol-LS+OV"))
gt_tbl |> gtsave(sprintf("%s/metrics_ls.html", reportPath))

Common.PlotCumReturns(all_ports,
  "VIX Regime LS+Overlay — Cumulative Returns",
  sprintf("Full | tilt=±%.1f overlay=%.0fσ cool=%dd", TILT_MAGNITUDE, OVERLAY_SPIKE_SIGMA, OVERLAY_COOLDOWN),
  sprintf("%s/cumulative_full.png", reportPath), NULL)
