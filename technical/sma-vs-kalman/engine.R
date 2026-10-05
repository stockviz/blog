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
SYSTEMS <- c("Buy & Hold", "SMA200", "Kalman")

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
  stopifnot(length(price) > 1L, all(is.finite(price)), all(price > 0))
  f <- matrix(c(1, 0, 1, 1), 2, 2)
  h <- matrix(c(1, 0), 1, 2)
  p <- kalman_covariance(q, r)
  state <- c(log(price[1]), 0)
  ans <- matrix(NA_real_, length(price), 4,
                dimnames = list(NULL, c("LogLevel", "Slope", "SlopeSD", "Z")))
  for (i in seq_along(price)) {
    predicted <- f %*% p %*% t(f) + q
    k <- predicted %*% t(h) / as.numeric(h %*% predicted %*% t(h) + r)
    state <- as.numeric(f %*% state + k * (log(price[i]) - as.numeric(h %*% f %*% state)))
    a <- diag(2) - k %*% h
    p <- a %*% predicted %*% t(a) + r * tcrossprod(k)
    ans[i, ] <- c(state, sqrt(p[2, 2]), max(-5, min(5, state[2] / sqrt(p[2, 2]))))
  }
  ans
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

simulate_index <- function(levels, drag = 0.0025, warmup = WARMUP) {
  # Close-t target earns close-t to close-(t+1); cost uses applied turnover.
  price <- as.numeric(levels)
  dates <- as.Date(zoo::index(levels))
  stopifnot(length(price) > warmup, !anyDuplicated(dates),
            !is.unsorted(dates, strictly = TRUE), all(is.finite(price)), all(price > 0))
  filtered <- kalman_filter(price)
  sma <- as.numeric(TTR::SMA(price, n = 200L))
  target <- cbind(rep(1, length(price)), rule_state(price - sma), rule_state(filtered[, "Slope"]))
  colnames(target) <- SYSTEMS
  applied <- rbind(rep(0, 3), target[-nrow(target), , drop = FALSE])
  rows <- seq.int(warmup + 1L, length(price))
  positions <- applied[rows, , drop = FALSE]
  # All arms open from cash on their shared evaluation start, not during warm-up.
  turnover <- abs(positions - rbind(rep(0, 3), positions[-nrow(positions), , drop = FALSE]))
  index_return <- price[rows] / price[rows - 1L] - 1
  gross <- positions * index_return
  cost <- turnover * drag
  net <- gross - cost
  stopifnot(all(is.finite(net)), all(net > -1))
  list(rets = xts(net, dates[rows]), gross = xts(gross, dates[rows]),
       exposure = xts(positions, dates[rows]), turnover = xts(turnover, dates[rows]),
       costs = xts(cost, dates[rows]), price = levels[rows],
       signals = data.frame(Date = dates, Close = price, SMA200 = sma,
                            filtered, SMA_Target = target[, 2], Kalman_Target = target[, 3]))
}

index_metrics <- function(result, index_name, window, bps) {
  # Compute shared metrics on identical slices; drawdown is reported negative.
  bounds <- WINDOWS[[window]]
  rets <- slice_returns(result$rets, bounds[1], bounds[2])
  stopifnot(NROW(rets) > 20)
  dates <- as.character(zoo::index(rets))
  source_rows <- match(zoo::index(rets), zoo::index(result$rets))
  stopifnot(!anyNA(source_rows))
  do.call(rbind, lapply(seq_along(SYSTEMS), function(j) {
    x <- rets[, j]
    m <- strategy_metrics(x, include_calmar = TRUE)
    exp <- as.numeric(result$exposure[source_rows, j])
    turns <- as.numeric(result$turnover[source_rows, j])
    previous <- as.numeric(result$exposure[, j])
    exits <- c(FALSE, diff(previous) == -1)[source_rows]
    monthly <- as.numeric(xts::apply.monthly(x, PerformanceAnalytics::Return.cumulative))
    data.frame(Index = index_name, System = SYSTEMS[j], Window = window, CostBps = bps,
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
  # Describe historical record-peak drawdowns, without using episodes as signals.
  price <- as.numeric(result$price)
  dates <- as.Date(zoo::index(result$price))
  peak <- 1L
  episodes <- list()
  emit <- function(last) {
    span <- seq.int(peak, last)
    trough <- span[which.min(price[span])]
    depth <- price[trough] / price[peak] - 1
    if (depth > -threshold) return(NULL)
    do.call(rbind, lapply(2:3, function(j) {
      target <- result$signals[[paste0(if (j == 2) "SMA" else "Kalman", "_Target")]]
      pos <- match(dates, result$signals$Date)
      held <- target[pos]
      change <- c(0, diff(held))
      sell <- span[change[span] == -1 & span <= trough]
      first_exit <- if (length(sell)) sell[1] else NA_integer_
      data.frame(Index = index_name, System = SYSTEMS[j], Peak = dates[peak],
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
