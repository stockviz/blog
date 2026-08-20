suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(TTR)
})

classify_regime <- function(vix_sma20, vix_sma40) {
  ifelse(is.na(vix_sma20) | is.na(vix_sma40), NA_character_,
         ifelse(vix_sma40 <= 18, "Green",
                ifelse(vix_sma20 < 32, "Yellow", "Red")))
}

regime_lookback <- function(regime) {
  unname(c(Green = 10L, Yellow = 3L, Red = 1L)[regime])
}

month_momentum <- function(month_levels, lookback) {
  month_levels / lag(month_levels, lookback) - 1
}

weights_from_momentum <- function(scores, top_n, asset_order = names(scores), cash_name = "CASH") {
  if (is.null(names(scores))) stop("Momentum scores must be named")
  if (!cash_name %in% asset_order) stop("Cash must be present in asset order")
  if (top_n < 1L || top_n > length(scores)) stop("Invalid top_n")
  scores <- scores[asset_order]
  scores[!is.finite(scores)] <- -Inf
  ord <- order(-scores, match(names(scores), asset_order))
  picked <- names(scores)[ord[seq_len(top_n)]]
  slot_weight <- 1 / top_n
  weights <- setNames(rep(0, length(asset_order)), asset_order)
  for (asset in picked) {
    destination <- if (asset != cash_name && scores[asset] < 0) cash_name else asset
    weights[destination] <- weights[destination] + slot_weight
  }
  if (abs(sum(weights) - 1) > 1e-12) stop("Portfolio weights do not sum to one")
  weights
}

# Number of assets eligible for selection: only finite-momentum assets count.
# top_n is capped so a staggered universe with fewer live sectors than the
# requested slot count still produces a fully-invested portfolio.
effective_top_n <- function(scores, top_n) {
  min(top_n, sum(is.finite(scores)))
}

holding_dates_after_signal <- function(signal_date, trading_dates) {
  signal_date <- as.Date(signal_date)
  trading_dates <- as.Date(trading_dates)
  current_month <- as.Date(format(signal_date, "%Y-%m-01"))
  next_month <- seq(current_month, by = "month", length.out = 2L)[2]
  after_next <- seq(next_month, by = "month", length.out = 2L)[2]
  trading_dates[trading_dates >= next_month & trading_dates < after_next]
}

# Held (non-zero weight) assets drive the return. Selecting only held columns
# avoids 0 * NA == NA from staggered indices that are still NA this month.
apply_month_weights <- function(month_returns, weights, old_weights, cost_rate = 0) {
  held <- names(weights)[weights > 0]
  if (length(held) == 0L) stop("No held assets")
  missing_assets <- setdiff(held, colnames(month_returns))
  if (length(missing_assets) > 0) stop("Missing return columns: ", paste(missing_assets, collapse = ", "))
  old_weights <- old_weights[names(weights)]
  turnover <- 0.5 * sum(abs(weights - old_weights))
  gross_values <- as.numeric(as.matrix(month_returns[, held, drop = FALSE]) %*% weights[held])
  gross <- xts(gross_values, index(month_returns))
  net <- gross
  if (NROW(net) > 0) net[1] <- net[1] - turnover * cost_rate
  colnames(gross) <- "gross"
  colnames(net) <- "net"
  list(gross = gross, net = net, turnover = turnover, cost = turnover * cost_rate)
}

sample_last_available <- function(x, dates) {
  source_dates <- as.Date(index(x))
  pos <- findInterval(as.Date(dates), source_dates)
  vals <- rep(NA_real_, length(dates))
  valid <- pos > 0L
  vals[valid] <- as.numeric(x[pos[valid]])
  vals
}

apply_percentile_regime <- function(vix_sma20, vix_sma40, thresholds) {
  ifelse(is.na(vix_sma20) | is.na(vix_sma40), NA_character_,
         ifelse(vix_sma40 <= thresholds$green, "Green",
                ifelse(vix_sma20 >= thresholds$red, "Red", "Yellow")))
}

calibrate_percentile_thresholds <- function(signal_data, train_end,
                                             green_prob = 0.5, red_prob = 0.9) {
  train <- signal_data$dates <= as.Date(train_end)
  list(
    green = as.numeric(quantile(signal_data$vix_sma40[train], green_prob, na.rm = TRUE)),
    red = as.numeric(quantile(signal_data$vix_sma20[train], red_prob, na.rm = TRUE)),
    green_prob = green_prob,
    red_prob = red_prob,
    calibrated_through = as.Date(train_end)
  )
}

build_signal_data <- function(cache, regime_thresholds = NULL) {
  levels <- cache$month_ends
  signal_dates <- as.Date(index(levels))
  sma20 <- TTR::SMA(cache$vix, n = 20)
  sma40 <- TTR::SMA(cache$vix, n = 40)
  v20 <- sample_last_available(sma20, signal_dates)
  v40 <- sample_last_available(sma40, signal_dates)
  regime <- if (is.null(regime_thresholds)) {
    classify_regime(v20, v40)
  } else {
    apply_percentile_regime(v20, v40, regime_thresholds)
  }
  lookback <- regime_lookback(regime)
  moms <- list(
    `1` = month_momentum(levels, 1L),
    `3` = month_momentum(levels, 3L),
    `10` = month_momentum(levels, 10L)
  )
  list(dates = signal_dates, levels = levels, vix_sma20 = v20, vix_sma40 = v40,
       regime = regime, lookback = lookback, momentum = moms)
}

run_equal_weight <- function(cache, start_date, end_date) {
  x <- cache$index_returns[paste0(start_date, "/", end_date)]
  if (NROW(x) == 0L) stop("No returns for equal-weight benchmark")
  values <- numeric(NROW(x))
  months <- format(index(x), "%Y-%m")
  for (month in unique(months)) {
    positions <- which(months == month)
    first_rets <- as.numeric(x[positions[1], ])
    available <- which(is.finite(first_rets))
    if (length(available) == 0L) {
      values[positions] <- 0
      next
    }
    weights <- rep(0, NCOL(x))
    weights[available] <- 1 / length(available)
    for (i in positions) {
      day_rets <- as.numeric(x[i, ])
      # Portfolio return uses only available sleeve; unavailable weight is 0
      # so 0*NA is avoided by subsetting to available
      avail_rets <- day_rets[available]
      avail_rets[!is.finite(avail_rets)] <- 0
      port_ret <- sum(weights[available] * avail_rets)
      values[i] <- port_ret
      # Drift weights within the month among available names only
      grown <- weights[available] * (1 + avail_rets)
      s <- sum(grown)
      if (s > 0) weights[available] <- grown / s
    }
  }
  out <- xts(values, index(x))
  colnames(out) <- "Equal Weight B&H"
  out
}

empty_audit_row <- function(signal_date, holding_dates, signal, lookback, strategy, cost_rate) {
  data.frame(
    strategy = strategy,
    cost_bps = cost_rate * 10000,
    signal_date = as.Date(signal_date),
    holding_month = format(first(holding_dates), "%Y-%m"),
    holding_start_date = as.Date(first(holding_dates)),
    holding_end_date = as.Date(last(holding_dates)),
    vix_sma20 = signal$vix_sma20,
    vix_sma40 = signal$vix_sma40,
    regime = signal$regime,
    lookback_months = lookback,
    stringsAsFactors = FALSE
  )
}

run_cross_portfolio <- function(cache, adaptive = TRUE, top_n = 1L, cost_rate = 0.0025,
                                regime_thresholds = NULL) {
  signal_data <- build_signal_data(cache, regime_thresholds)
  all_returns <- merge(cache$index_returns, cache$cash_returns, join = "inner")
  colnames(all_returns) <- c(colnames(cache$index_returns), "CASH")
  asset_order <- colnames(cache$month_ends)
  strategy <- sprintf("%s Top %d", if (adaptive) "VIX" else "10M", top_n)
  old_weights <- setNames(rep(0, length(asset_order)), asset_order)
  old_weights["CASH"] <- 1
  daily_parts <- list()
  audit_parts <- list()
  out_i <- 0L

  for (i in seq_along(signal_data$dates)) {
    signal_date <- signal_data$dates[i]
    holding_dates <- holding_dates_after_signal(signal_date, as.Date(index(all_returns)))
    if (length(holding_dates) == 0L) next
    lookback <- if (adaptive) signal_data$lookback[i] else 10L
    if (is.na(lookback) || i <= lookback) next
    scores <- as.numeric(signal_data$momentum[[as.character(lookback)]][i, ])
    names(scores) <- asset_order
    available <- names(scores)[is.finite(scores)]
    if (!"CASH" %in% available) next
    if (length(available) <= 1L) next
    top_n_eff <- effective_top_n(scores, top_n)
    weights <- weights_from_momentum(scores, top_n_eff, asset_order)
    month_returns <- all_returns[as.character(holding_dates)]
    pnl <- apply_month_weights(month_returns, weights, old_weights, cost_rate)

    out_i <- out_i + 1L
    colnames(pnl$net) <- strategy
    daily_parts[[out_i]] <- pnl$net
    row <- empty_audit_row(signal_date, holding_dates,
                           list(vix_sma20 = signal_data$vix_sma20[i],
                                vix_sma40 = signal_data$vix_sma40[i],
                                regime = signal_data$regime[i]),
                           lookback, strategy, cost_rate)
    for (asset in asset_order) {
      row[[paste0("momentum_", make.names(asset))]] <- scores[asset]
      row[[paste0("weight_", make.names(asset))]] <- weights[asset]
    }
    row$available_count <- length(available) - 1L
    row$selected_assets <- paste(names(weights)[weights > 0], collapse = ";")
    row$turnover <- pnl$turnover
    row$gross_return <- prod(1 + as.numeric(pnl$gross)) - 1
    row$cost <- pnl$cost
    row$net_return <- prod(1 + as.numeric(pnl$net)) - 1
    audit_parts[[out_i]] <- row
    old_weights <- weights
  }
  if (length(daily_parts) == 0L) stop("No valid holding months for ", strategy)
  daily <- do.call(rbind, daily_parts)
  audit <- do.call(rbind, audit_parts)
  stopifnot(all(audit$signal_date < audit$holding_start_date))
  stopifnot(!anyDuplicated(audit$signal_date))
  stopifnot(all(is.finite(as.numeric(daily))))
  weight_cols <- grep("^weight_", names(audit), value = TRUE)
  stopifnot(all(abs(rowSums(audit[, weight_cols, drop = FALSE]) - 1) < 1e-12))
  list(daily = daily, audit = audit, strategy = strategy, cost_rate = cost_rate)
}
