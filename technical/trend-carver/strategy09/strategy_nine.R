#' Strategy Nine: Multiple Trend Following Rules
#' (Robert Carver, "Advanced Futures Trading Strategies", 2023)
#'
#' Combines six EWMAC(n, 4n) trend filters of different speeds into a single
#' combined forecast, applies the Forecast Diversification Multiplier (FDM)
#' from Table 36, re-caps the result at ±20, and converts it to a binary
#' long/short signal (1 / 0).
#'
#' EWMAC(n) = EMA(n) - EMA(4n) of price, risk-normalized, scaled, and capped.
#' The six filters are: EWMAC2, EWMAC4, EWMAC8, EWMAC16, EWMAC32, EWMAC64
#' (the number = the *short* EMA span; the long EMA span is always 4x that).
#'
#' Processing pipeline per Carver's Strategy Nine:
#'   1. Compute each filter's risk-adjusted, scaled, capped forecast (±20).
#'   2. Combine with equal forecast weights across surviving filters.
#'   3. Multiply by the FDM (Table 36) to restore the target abs mean of 10.
#'   4. Re-cap the FDM-adjusted combined forecast at ±20.
#'   5. Sign of the final forecast → 1 (long) or 0 (short).
#'
#' The FDM-adjusted combined forecast is also returned as an attribute so it
#' can be plugged directly into Carver's position-sizing formula:
#'   N = (Capital * IDM * w * tau) / (mult * price * FX * vol_daily) * (F / 10)
#'
#' Required packages: xts, zoo (zoo is a dependency of xts and is used for
#' the EMA helper).

library(xts)
library(zoo)

# ---------------------------------------------------------------------------
# Helper: exponentially weighted moving average matching Carver's convention.
# Carver (and most EWMA implementations) use a smoothing constant
#   alpha = 2 / (span + 1)
# applied recursively: EMA_t = alpha * P_t + (1 - alpha) * EMA_{t-1}
# ---------------------------------------------------------------------------
ewma <- function(x, span) {
  alpha <- 2 / (span + 1)
  as.numeric(stats::filter(alpha * x, filter = 1 - alpha, method = "recursive",
                            init = x[1]))
}

# ---------------------------------------------------------------------------
# Helper: exponentially weighted standard deviation of returns, with a span
# of 35 days, blended with a long-run average -- the same volatility
# estimate Carver uses throughout the book for all forecast normalization
# (strategy three onward), regardless of trend-filter speed.
# ---------------------------------------------------------------------------
ewma_vol <- function(returns, span = 35, slow_weight = 0.3, slow_span = 10 * span) {
  fast_var <- ewma(returns^2, span)
  slow_var <- ewma(returns^2, slow_span)
  blended_var <- (1 - slow_weight) * fast_var + slow_weight * slow_var
  sqrt(blended_var)
}

# ---------------------------------------------------------------------------
# Forecast scalars from Table 29 of the book (so that |forecast| averages 10
# across the six EWMACn variations).
# ---------------------------------------------------------------------------
forecast_scalars <- c(
  EWMAC2  = 12.1,
  EWMAC4  = 8.53,
  EWMAC8  = 5.95,
  EWMAC16 = 4.10,
  EWMAC32 = 2.79,
  EWMAC64 = 1.91
)

fast_spans <- c(EWMAC2 = 2, EWMAC4 = 4, EWMAC8 = 8,
                 EWMAC16 = 16, EWMAC32 = 32, EWMAC64 = 64)

# ---------------------------------------------------------------------------
# Average annual turnover per filter (Table 35 of the book), used by the
# cost speed-limit test below.
# ---------------------------------------------------------------------------
filter_turnover <- c(
  EWMAC2  = 98.5,
  EWMAC4  = 50.2,
  EWMAC8  = 25.4,
  EWMAC16 = 13.2,
  EWMAC32 = 7.6,
  EWMAC64 = 5.2
)

# ---------------------------------------------------------------------------
# Forecast Diversification Multiplier (FDM) lookup table (Table 36).
#
# Because the six EWMAC filters are positively correlated, the combined
# forecast averages out to less than the target abs mean of 10. The FDM
# corrects for this. It is keyed on the *set* of active filters (those with
# a non-zero forecast weight), represented here as a sorted comma-separated
# string of filter names for fast lookup.
# ---------------------------------------------------------------------------
fdm_table <- list(
  "EWMAC2,EWMAC4,EWMAC8,EWMAC16,EWMAC32,EWMAC64" = 1.26,
  "EWMAC4,EWMAC8,EWMAC16,EWMAC32,EWMAC64"         = 1.19,
  "EWMAC8,EWMAC16,EWMAC32,EWMAC64"                 = 1.13,
  "EWMAC16,EWMAC32,EWMAC64"                         = 1.08,
  "EWMAC32,EWMAC64"                                 = 1.03,
  "EWMAC64"                                         = 1.00
)

#' Look up the FDM for the active set of filters.
#' Falls back to 1.0 with a warning if the combination isn't in Table 36
#' (e.g. a custom weight vector with a non-standard set of active filters).
lookup_fdm <- function(weights) {
  active <- sort(names(weights[weights > 0]))
  key    <- paste(active, collapse = ",")
  fdm    <- fdm_table[[key]]
  if (is.null(fdm)) {
    warning("FDM not found in Table 36 for filter set: ", key,
            ". Defaulting to FDM = 1.0. Supply a custom `fdm` argument ",
            "to override.")
    fdm <- 1.0
  }
  fdm
}


# ---------------------------------------------------------------------------
# Cost speed-limit test (Carver's "removing expensive trading rules" step).
#
# For a given instrument, annual trading cost in Sharpe Ratio (SR) units is
# approximately:
#     annual_cost_sr = turnover * cost_per_trade_sr
# Carver allows a trading rule to consume at most `speed_limit` SR units/year
# (he uses 0.15, vs. 0.10 for instrument selection -- a filter passes if:
#     turnover_filter * cost_per_trade_sr <= speed_limit
#
# Filters that pass get an equal forecast weight; filters that fail get a
# weight of 0. If no filter passes, every weight is 0 (the instrument
# shouldn't be traded with this strategy at all).
#
# NOTE: this is the simplified relationship he describes in strategy three.
# His actual worked Eurodollar example also folds in extra turnover from
# contract rolls (2 trades per roll, 4 rolls/year for Eurodollar), which
# pushes the true required turnover ceiling a bit lower than the simple
# formula above gives. If you have the instrument's roll frequency and want
# an exact match to his numbers, add the roll-turnover term to
# `cost_per_trade_sr` (or to the cost-per-trade input) before calling this
# function; as written, this is a close approximation rather than an exact
# replication of his arithmetic.
#
# @param cost_per_trade_sr  instrument's cost per trade, in SR units (e.g.
#                            from bid/ask spread + commission, scaled by
#                            instrument volatility -- see strategy three of
#                            the book for how to derive this)
# @param speed_limit         max SR units/year a single filter may cost
#                             (default 0.15, per the book)
# @return                    named numeric vector of forecast weights for
#                             EWMAC2...EWMAC64, summing to 1 (or all 0 if
#                             nothing survives)
cost_speed_limit_weights <- function(cost_per_trade_sr, speed_limit = 0.15) {
  annual_cost <- filter_turnover * cost_per_trade_sr
  survives <- annual_cost <= speed_limit

  if (!any(survives)) {
    w <- setNames(rep(0, length(filter_turnover)), names(filter_turnover))
    warning("No EWMAC filter passes the cost speed-limit test for this instrument; ",
            "all weights are 0 (instrument should not be traded with this strategy).")
    return(w)
  }

  w <- setNames(rep(0, length(filter_turnover)), names(filter_turnover))
  w[survives] <- 1 / sum(survives)
  w
}

# ---------------------------------------------------------------------------
# Main function
# ---------------------------------------------------------------------------
#' @param prices      an xts object of (single-instrument) prices, one column
#' @param forecast_cap absolute cap applied both to each individual filter's
#'                      forecast and to the final FDM-adjusted combined
#'                      forecast (default 20, per the book)
#' @param weights      named numeric vector of forecast weights for
#'                      EWMAC2, EWMAC4, EWMAC8, EWMAC16, EWMAC32, EWMAC64.
#'                      Must sum to 1. Defaults to equal weight on all six.
#'                      Ignored if `use_cost_screen = TRUE`.
#' @param use_cost_screen if TRUE, `weights` is ignored and instead derived
#'                      automatically from the cost speed-limit test (equal
#'                      weight across whichever filters pass; see
#'                      `cost_speed_limit_weights()`). Requires
#'                      `cost_per_trade_sr` to be supplied.
#' @param cost_per_trade_sr instrument's cost per trade, in SR units. Required
#'                      if `use_cost_screen = TRUE`.
#' @param speed_limit   max SR units/year a single filter may cost before
#'                      being dropped by the cost screen (default 0.15, per
#'                      the book). Only used if `use_cost_screen = TRUE`.
#' @param fdm           Forecast Diversification Multiplier. If NULL
#'                      (default), looked up automatically from Table 36
#'                      using the active filter set. Supply a numeric value
#'                      to override (e.g. for a non-standard filter subset).
#' @return             an xts object, same dates as `prices`, with:
#'                       - values 1 (long) or 0 (short)
#'                       - attribute "forecast": the FDM-adjusted, re-capped
#'                         combined forecast as an xts (range −20 to +20),
#'                         ready to plug into the position-sizing formula as F
#'                       - attribute "fdm": the FDM value used
#'                       - attribute "weights": the forecast weights used
#'                      Values are NA before the warm-up period ends (first
#'                      4 × 64 = 256 rows), or throughout if the cost screen
#'                      eliminates every filter.
strategy_nine_signal <- function(prices,
                                  forecast_cap     = 20,
                                  weights          = c(EWMAC2 = 1/6, EWMAC4 = 1/6, EWMAC8 = 1/6,
                                                       EWMAC16 = 1/6, EWMAC32 = 1/6, EWMAC64 = 1/6),
                                  use_cost_screen  = FALSE,
                                  cost_per_trade_sr = NULL,
                                  speed_limit      = 0.15,
                                  fdm              = NULL) {

  stopifnot(is.xts(prices), ncol(prices) == 1)

  if (use_cost_screen) {
    if (is.null(cost_per_trade_sr))
      stop("cost_per_trade_sr must be supplied when use_cost_screen = TRUE")
    weights <- cost_speed_limit_weights(cost_per_trade_sr, speed_limit)
  }

  stopifnot(abs(sum(weights) - 1) < 1e-8 || all(weights == 0))

  if (all(weights == 0)) {
    out <- xts(rep(NA_real_, nrow(prices)), order.by = index(prices))
    attr(out, "forecast") <- out
    attr(out, "fdm")      <- NA_real_
    attr(out, "weights")  <- weights
    return(out)
  }

  # resolve FDM: use supplied value, else look up Table 36
  if (is.null(fdm)) fdm <- lookup_fdm(weights)

  px <- as.numeric(prices)
  n  <- length(px)

  # daily simple returns for the EWMAC raw signal and for volatility estimation
  rets <- c(NA, diff(px) / px[-n])

  vol <- ewma_vol(ifelse(is.na(rets), 0, rets))
  vol[is.na(rets)] <- NA

  # --- step 1 & 2: weighted sum of individual capped forecasts ---------------
  combined_forecast <- rep(0, n)

  for (nm in names(fast_spans)) {
    w <- weights[[nm]]
    if (is.na(w) || w == 0) next

    s <- fast_spans[[nm]]
    l <- 4 * s

    ema_fast <- ewma(px, s)
    ema_slow <- ewma(px, l)

    raw     <- ema_fast - ema_slow                       # EWMAC(s, l) raw crossover
    risk_adj <- raw / (vol * px)                          # risk-normalized
    scaled   <- risk_adj * forecast_scalars[[nm]]         # scale to abs mean ~10
    capped   <- pmax(pmin(scaled, forecast_cap),          # cap individual forecast
                     -forecast_cap)

    combined_forecast <- combined_forecast + w * capped
  }

  # --- step 3: apply FDM -----------------------------------------------------
  fdm_forecast <- combined_forecast * fdm

  # --- step 4: re-cap combined forecast after FDM ----------------------------
  fdm_forecast_capped <- pmax(pmin(fdm_forecast, forecast_cap), -forecast_cap)

  # warm-up: mark rows where the slowest EMA (EWMAC64 long span = 256) has
  # not yet converged
  warm_up <- 4 * max(fast_spans)
  if (n > warm_up) {
    fdm_forecast_capped[1:warm_up] <- NA
  } else {
    fdm_forecast_capped[] <- NA
  }

  # --- step 5: binary signal -------------------------------------------------
  signal <- ifelse(fdm_forecast_capped > 0, 1,
                   ifelse(fdm_forecast_capped < 0, 0, NA))

  out <- xts(signal, order.by = index(prices))
  attr(out, "forecast") <- xts(fdm_forecast_capped, order.by = index(prices))
  attr(out, "fdm")      <- fdm
  attr(out, "weights")  <- weights
  out
}

# ---------------------------------------------------------------------------
# Example usage
# ---------------------------------------------------------------------------
# library(xts)
# data(sample_matrix)
# prices <- as.xts(sample_matrix)[, "Close", drop = FALSE]
#
# # Default: all six filters, equal weight, FDM auto-looked up (1.26)
# sig <- strategy_nine_signal(prices)
# tail(sig, 10)
#
# # Retrieve the FDM-adjusted combined forecast (range -20 to +20) for use
# # in position sizing:  N = base_block * (F / 10)
# forecast <- attr(sig, "forecast")
# fdm_used  <- attr(sig, "fdm")       # 1.26 for all-six case
# weights_used <- attr(sig, "weights")
#
# # Cost speed-limit screen: weights and FDM both derived automatically
# sig2 <- strategy_nine_signal(
#   prices,
#   use_cost_screen   = TRUE,
#   cost_per_trade_sr = 0.0088    # SR-unit cost per trade for this instrument
# )
# attr(sig2, "weights")  # see which filters survived
# attr(sig2, "fdm")      # corresponding FDM from Table 36
#
# # Override FDM manually (e.g. for a custom/non-standard filter subset)
# sig3 <- strategy_nine_signal(prices, fdm = 1.15)
#
# # Inspect which filters survive for a given cost, without running signals:
# cost_speed_limit_weights(cost_per_trade_sr = 0.0088)

