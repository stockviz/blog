# Fixed-rule mechanics. No database or report side effects when sourced.
suppressPackageStartupMessages({
  library(xts)
  library(TTR)
  library(PerformanceAnalytics)
})
source("/mnt/ssd1/stockviz/R2/backtests/common/runtime.R")
source_common("returns")

KALMAN_Q <- diag(c(0.0001, 0.0001) * 0.01^2)
KALMAN_R <- 300 * 0.01^2
WARMUP <- 504L
WINDOWS <- list(pre = c(NA, "2019-12-31"), post = c("2020-05-01", NA),
                full = c(NA, NA))
SMA_LOOKBACKS <- c(20L, 50L, 100L, 200L)
BASE_SYSTEMS <- c("Buy & Hold", paste0("SMA", SMA_LOOKBACKS), "Kalman")
SYSTEMS <- c(BASE_SYSTEMS, "Kalman Calibrated", "Kalman Adaptive")
CALIBRATION_END <- "2019-12-31"
Q_MULTIPLIERS <- 2^seq(-8, 8, by = 2)
VOL_HALF_LIFE <- 20
VOL_BOUNDS <- c(0.25, 4)

kalman_covariance <- function(q = KALMAN_Q, r = KALMAN_R) {
  # Solve the steady-state Joseph covariance without observing market returns.
  f <- matrix(c(1, 0, 1, 1), 2, 2)
  h <- matrix(c(1, 0), 1, 2)
  p <- diag(1, 2)
  for (i in seq_len(20000L)) {
    predicted <- f %*% p %*% t(f) + q
    k <- predicted %*% t(h) / as.numeric(h %*% predicted %*% t(h) + r)
    a <- diag(2) - k %*% h
    updated <- a %*% predicted %*% t(a) + r * tcrossprod(k)
    if (max(abs(updated - p)) < 1e-14) return(updated)
    p <- updated
  }
  stop("Riccati covariance did not converge")
}

kalman_filter <- function(price, q = KALMAN_Q, r = KALMAN_R) {
  # Filter log closes forward; never smooth or fit on future observations.
  # `r` is either a scalar measurement noise (steady-state init at (q, r)) or a
  # length(price) vector of per-row measurement noise. A vector keeps the same
  # steady-state covariance initialization at (q, KALMAN_R) -- the base noise the
  # adaptive arm expresses around -- so scalar and vector paths share their prior.
  stopifnot(length(price) > 1L, all(is.finite(price)), all(price > 0))
  f <- matrix(c(1, 0, 1, 1), 2, 2)
  h <- matrix(c(1, 0), 1, 2)
  if (length(r) == 1L) {
    stopifnot(is.finite(r), r > 0)
    p <- kalman_covariance(q, r)
    r_vec <- rep(r, length(price))
  } else {
    stopifnot(length(r) == length(price), all(is.finite(r)), all(r > 0))
    p <- kalman_covariance(q, KALMAN_R)
    r_vec <- r
  }
  state <- c(log(price[1]), 0)
  ans <- matrix(NA_real_, length(price), 4,
                dimnames = list(NULL, c("LogLevel", "Slope", "SlopeSD", "Z")))
  for (i in seq_along(price)) {
    predicted <- f %*% p %*% t(f) + q
    k <- predicted %*% t(h) / as.numeric(h %*% predicted %*% t(h) + r_vec[i])
    state <- as.numeric(f %*% state + k * (log(price[i]) - as.numeric(h %*% f %*% state)))
    a <- diag(2) - k %*% h
    p <- a %*% predicted %*% t(a) + r_vec[i] * tcrossprod(k)
    ans[i, ] <- c(state, sqrt(p[2, 2]), max(-5, min(5, state[2] / sqrt(p[2, 2]))))
  }
  ans
}

kalman_horizon <- function(q_multiplier = 1, r_multiplier = 1) {
  # Effective horizon in sessions: centroid of the positive slope impulse weights.
  # A single 0.01 log-price step is forward-filtered and the slope response,
  # scaled by the step, gives the weights; the positive-weight centroid is the
  # horizon (the local-linear filter also has a small negative tail).
  impulse <- exp(c(0, rep(0.01, 2999)))
  filtered <- kalman_filter(impulse, q = KALMAN_Q * q_multiplier,
                            r = KALMAN_R * r_multiplier)
  weights <- filtered[-1, "Slope"] / 0.01
  positive <- pmax(weights, 0)
  as.numeric(sum((0:2998) * positive) / sum(positive))
}

rule_state <- function(score) {
  # Strict positive entry / negative exit; equality retains the prior state.
  out <- numeric(length(score))
  held <- 0
  for (i in seq_along(score)) {
    if (is.finite(score[i])) {
      if (score[i] > 0) held <- 1
      if (score[i] < 0) held <- 0
    }
    out[i] <- held
  }
  out
}

target_column <- function(system, names = NULL) {
  # Map a display system name to its target column inside `signals`. Spaces are
  # removed before appending "_Target", so "Kalman Calibrated" resolves to
  # "KalmanCalibrated_Target" and the fixed "Kalman" to "Kalman_Target". Legacy
  # results may store the SMA200 target only under "SMA_Target"; resolve that
  # alias when the canonical "SMA200_Target" column is absent.
  canonical <- paste0(gsub(" ", "", system, fixed = TRUE), "_Target")
  if (!is.null(names) && !(canonical %in% names) && identical(system, "SMA200") &&
      "SMA_Target" %in% names) canonical <- "SMA_Target"
  canonical
}

simulate_index <- function(levels, drag = 0.0025, warmup = WARMUP, calibration = NULL) {
  # Six fixed arms by default; supplying a calibration adds the frozen-Q Kalman
  # Calibrated arm and the volatility-adaptive Kalman Adaptive arm (eight total).
  price <- as.numeric(levels)
  dates <- as.Date(zoo::index(levels))
  stopifnot(length(price) > warmup, !anyDuplicated(dates),
            !is.unsorted(dates, strictly = TRUE), all(is.finite(price)), all(price > 0))
  filtered <- kalman_filter(price)
  sma <- vapply(SMA_LOOKBACKS, function(lb) as.numeric(TTR::SMA(price, n = lb)), numeric(length(price)))
  colnames(sma) <- paste0("SMA", SMA_LOOKBACKS)
  sma_targets <- vapply(seq_along(SMA_LOOKBACKS), function(j) rule_state(price - sma[, j]), numeric(length(price)))
  colnames(sma_targets) <- paste0(colnames(sma), "_Target")
  base_target <- cbind(rep(1, length(price)), sma_targets, rule_state(filtered[, "Slope"]))
  colnames(base_target) <- BASE_SYSTEMS

  if (is.null(calibration)) {
    systems <- BASE_SYSTEMS
    target <- base_target
  } else {
    stopifnot(is.list(calibration))
    # Accept either the full calibration result or its frozen settings payload.
    s <- if (is.list(calibration$settings)) calibration$settings else calibration
    q_mult <- s$q_multiplier
    ref_var <- s$reference_variance
    half_life <- if (is.null(s$vol_half_life)) VOL_HALF_LIFE else s$vol_half_life
    bounds <- if (is.null(s$vol_bounds)) VOL_BOUNDS else s$vol_bounds
    stopifnot(length(q_mult) == 1L, is.finite(q_mult), q_mult > 0,
              length(ref_var) == 1L, is.finite(ref_var), ref_var > 0,
              length(bounds) == 2L, all(is.finite(bounds)), bounds[1] < bounds[2])
    # Prior-close EWMA variance of squared log returns (seed 0.01^2, half-life
    # sessions). Row t uses only returns through close t-1, so it excludes the
    # return at t and never sees current/future prices.
    lambda <- 0.5^(1 / half_life)
    log_ret <- c(NA_real_, diff(log(price)))
    var_at_close <- numeric(length(price))
    var_at_close[1] <- 0.01^2
    if (length(price) > 1L) {
      for (i in 2:length(price)) {
        var_at_close[i] <- lambda * var_at_close[i - 1L] + (1 - lambda) * log_ret[i]^2
      }
    }
    prior_var <- c(0.01^2, var_at_close[-length(var_at_close)])
    r_multiplier <- pmax(bounds[1], pmin(bounds[2], prior_var / ref_var))
    q <- KALMAN_Q * q_mult
    calib_filt <- kalman_filter(price, q = q, r = KALMAN_R)
    adapt_filt <- kalman_filter(price, q = q, r = KALMAN_R * r_multiplier)
    systems <- SYSTEMS
    target <- cbind(base_target,
                    rule_state(calib_filt[, "Slope"]),
                    rule_state(adapt_filt[, "Slope"]))
    colnames(target) <- SYSTEMS
  }

  applied <- rbind(rep(0, length(systems)), target[-nrow(target), , drop = FALSE])
  rows <- seq.int(warmup + 1L, length(price))
  positions <- applied[rows, , drop = FALSE]
  # All arms open from cash on their shared evaluation start, not during warm-up.
  turnover <- abs(positions - rbind(rep(0, length(systems)), positions[-nrow(positions), , drop = FALSE]))
  index_return <- price[rows] / price[rows - 1L] - 1
  gross <- positions * index_return
  cost <- turnover * drag
  net <- gross - cost
  stopifnot(all(is.finite(net)), all(net > -1))

  signals <- data.frame(Date = dates, Close = price, sma, filtered, sma_targets,
                        SMA_Target = target[, "SMA200"], Kalman_Target = target[, "Kalman"])
  if (!is.null(calibration)) {
    signals$CalibratedSlope <- calib_filt[, "Slope"]
    signals$AdaptiveSlope <- adapt_filt[, "Slope"]
    signals$AdaptiveVariance <- prior_var
    signals$AdaptiveRMultiplier <- r_multiplier
    signals$KalmanCalibrated_Target <- target[, "Kalman Calibrated"]
    signals$KalmanAdaptive_Target <- target[, "Kalman Adaptive"]
  }

  list(rets = xts(net, dates[rows]), gross = xts(gross, dates[rows]),
       exposure = xts(positions, dates[rows]), turnover = xts(turnover, dates[rows]),
       costs = xts(cost, dates[rows]), price = levels[rows],
       signals = signals)
}

index_metrics <- function(result, index_name, window, bps) {
  # Compute shared metrics on identical slices; drawdown is reported negative.
  bounds <- WINDOWS[[window]]
  rets <- slice_returns(result$rets, bounds[1], bounds[2])
  stopifnot(NROW(rets) > 20)
  dates <- as.character(zoo::index(rets))
  source_rows <- match(zoo::index(rets), zoo::index(result$rets))
  stopifnot(!anyNA(source_rows))
  systems <- colnames(result$rets)
  do.call(rbind, lapply(seq_along(systems), function(j) {
    x <- rets[, j]
    m <- strategy_metrics(x, include_calmar = TRUE)
    exp <- as.numeric(result$exposure[source_rows, j])
    turns <- as.numeric(result$turnover[source_rows, j])
    previous <- as.numeric(result$exposure[, j])
    exits <- c(FALSE, diff(previous) == -1)[source_rows]
    monthly <- as.numeric(xts::apply.monthly(x, PerformanceAnalytics::Return.cumulative))
    data.frame(Index = index_name, System = systems[j], Window = window, CostBps = bps,
               Start = min(dates), End = max(dates), N = m[["N"]],
               TotalReturn = compound_return(x), CAGR = m[["CAGR"]], Vol = m[["Vol"]],
               Sharpe = m[["Sharpe"]], MaxDD = -m[["MaxDD"]], Calmar = m[["Calmar"]],
               Invested = mean(exp), Turnover = sum(turns), Exits = sum(exits),
               AnnualExits = sum(exits) * 252 / NROW(x),
               CostDrag = sum(as.numeric(result$costs[source_rows, j])),
               WorstMonth = min(monthly), PositiveMonths = mean(monthly > 0))
  }))
}

fall_diagnostics <- function(result, index_name, threshold = 0.15) {
  # Describe every timing arm's historical exits; never use episodes as signals.
  price <- as.numeric(result$price)
  dates <- as.Date(zoo::index(result$price))
  systems <- colnames(result$rets)
  signal_names <- names(result$signals)
  peak <- 1L
  episodes <- list()
  emit <- function(last) {
    span <- seq.int(peak, last)
    trough <- span[which.min(price[span])]
    depth <- price[trough] / price[peak] - 1
    if (depth > -threshold) return(NULL)
    do.call(rbind, lapply(seq.int(2L, length(systems)), function(j) {
      target <- result$signals[[target_column(systems[j], signal_names)]]
      pos <- match(dates, result$signals$Date)
      held <- target[pos]
      change <- c(0, diff(held))
      sell <- span[change[span] == -1 & span <= trough]
      first_exit <- if (length(sell)) sell[1] else NA_integer_
      data.frame(Index = index_name, System = systems[j], Peak = dates[peak],
                 Trough = dates[trough], End = dates[last],
                 Recovered = price[last] >= price[peak], Depth = depth,
                 ExitSignal = if (is.na(first_exit)) as.Date(NA) else dates[first_exit],
                 ExitDelaySessions = first_exit - peak,
                 DeclineAtExit = if (is.na(first_exit)) NA_real_ else price[first_exit] / price[peak] - 1,
                 FalseStarts = if (is.na(first_exit)) 0L else
                   sum(change[seq.int(first_exit, trough)] == 1))
    }))
  }
  for (i in 2:length(price)) {
    if (price[i] >= price[peak]) {
      row <- emit(i)
      if (!is.null(row)) episodes[[length(episodes) + 1L]] <- row
      peak <- i
    }
  }
  if (peak < length(price)) {
    row <- emit(length(price))
    if (!is.null(row)) episodes[[length(episodes) + 1L]] <- row
  }
  do.call(rbind, episodes)
}

calibrate_kalman <- function(levels, training_end = CALIBRATION_END, warmup = WARMUP) {
  # Choose one process-noise multiplier per index from a fixed grid using the
  # highest finite pre-cutoff net Sharpe at 25 bps on the post-warm-up training
  # sample; ties break toward shallower MaxDD, then grid order. The choice is a
  # fitted pre-2020 selection, not a causal estimate, and is frozen after cutoff.
  cutoff <- as.Date(training_end)
  # Exclude the holdout before validation, filtering or reference estimation.
  training_levels <- levels[paste0("/", as.character(cutoff))]
  price <- as.numeric(training_levels)
  dates <- as.Date(zoo::index(training_levels))
  stopifnot(length(price) > warmup, !anyDuplicated(dates),
            !is.unsorted(dates, strictly = TRUE), all(is.finite(price)), all(price > 0))
  train_end_row <- max(which(dates <= cutoff))
  stopifnot(is.finite(train_end_row), train_end_row > warmup)
  # Reference variance is the mean squared log return over the whole training
  # window (including the warm-up), using the squared-return EWMA convention.
  log_ret <- c(NA_real_, diff(log(price)))
  train_log_ret <- log_ret[seq_len(train_end_row)]
  train_log_ret <- train_log_ret[is.finite(train_log_ret)]
  reference_variance <- mean(train_log_ret^2)
  stopifnot(is.finite(reference_variance), reference_variance > 0)
  # Score on the post-warm-up training sample; require a full year of sessions.
  score_rows <- seq.int(warmup + 1L, train_end_row)
  stopifnot(length(score_rows) >= 252L)
  index_return <- price[score_rows] / price[score_rows - 1L] - 1
  score_dates <- dates[score_rows]
  score_one <- function(multiplier) {
    filtered <- kalman_filter(price, q = KALMAN_Q * multiplier, r = KALMAN_R)
    target <- rule_state(filtered[, "Slope"])
    applied <- c(0, target[-length(target)])
    positions <- applied[score_rows]
    turnover <- abs(positions - c(0, positions[-length(positions)]))
    net <- positions * index_return - turnover * 0.0025
    strategy_metrics(xts(net, score_dates))
  }
  scored <- lapply(Q_MULTIPLIERS, score_one)
  cagr <- vapply(scored, function(m) m[["CAGR"]], numeric(1))
  sharpe <- vapply(scored, function(m) m[["Sharpe"]], numeric(1))
  maxdd <- vapply(scored, function(m) m[["MaxDD"]], numeric(1))
  nobs <- vapply(scored, function(m) m[["N"]], numeric(1))
  finite <- which(is.finite(sharpe) & is.finite(maxdd) & is.finite(cagr))
  stopifnot(length(finite) >= 1L)
  best <- finite[order(-sharpe[finite], maxdd[finite], finite)[1L]]
  candidates <- data.frame(
    QMultiplier = Q_MULTIPLIERS,
    CAGR = cagr, Sharpe = sharpe, MaxDD = -maxdd, N = nobs,
    Start = as.character(score_dates[1]),
    End = as.character(score_dates[length(score_dates)]),
    HorizonSessions = vapply(Q_MULTIPLIERS, kalman_horizon, numeric(1)),
    Selected = seq_along(Q_MULTIPLIERS) == best,
    stringsAsFactors = FALSE)
  settings <- list(
    q_multiplier = Q_MULTIPLIERS[best],
    reference_variance = reference_variance,
    training_end = as.character(cutoff),
    vol_half_life = VOL_HALF_LIFE,
    vol_bounds = VOL_BOUNDS)
  summary <- data.frame(
    QMultiplier = Q_MULTIPLIERS[best],
    ReferenceVariance = reference_variance,
    ReferenceVolAnnualized = sqrt(reference_variance) * sqrt(252),
    TrainingStart = as.character(score_dates[1]),
    TrainingEnd = as.character(dates[train_end_row]),
    TrainingRows = nobs[best],
    ReferenceStart = as.character(dates[1]),
    ReferenceRows = train_end_row,
    TrainCAGR = cagr[best],
    TrainSharpe = sharpe[best],
    TrainMaxDD = -maxdd[best],
    HorizonSessions = kalman_horizon(Q_MULTIPLIERS[best]),
    stringsAsFactors = FALSE)
  list(settings = settings, candidates = candidates, summary = summary)
}
