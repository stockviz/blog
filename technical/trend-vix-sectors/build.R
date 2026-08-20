suppressPackageStartupMessages({
  library(RODBC)
  library(xts)
  library(zoo)
  library(digest)
})
source("/mnt/hollandC/StockViz/R/config.r")

REPORT_PATH <- "/mnt/data/blog/technical/trend-vix-sectors"
CACHE_PATH <- file.path(REPORT_PATH, "cache.rds")
CLEAN_REPORT_PATH <- file.path(REPORT_PATH, "data-quality-report.txt")
CASH_NAME <- "CASH"
CASH_SCHEME_CODE <- 103734L
TRAIN_END <- as.Date("2019-12-31")
TEST_START <- as.Date("2020-05-01")
REGIME_WINDOWS <- c(20L, 40L)
MOMENTUM_LOOKBACKS <- c(1L, 3L, 10L)
CLEAN_THRESHOLD <- 0.30
QUERY_VERSION <- "trend-vix-sectors-v1"

# Sectoral total-return indices (classification == "sectoral-indices"), sorted
# alphabetically. Each series enters the investable universe only once it has
# enough history for the selected momentum lookback (staggered inclusion).
INDEX_NAMES <- c(
  "NIFTY AUTO TR",
  "NIFTY BANK TR",
  "NIFTY CAPITAL GOODS TR",
  "NIFTY CEMENT TR",
  "NIFTY CHEMICALS TR",
  "NIFTY COMMERCIAL & TRANSPORT SERVICES TR",
  "NIFTY CONSTRUCTION TR",
  "NIFTY CONSUMER DURABLES TR",
  "NIFTY CONSUMER SERVICES TR",
  "NIFTY FINANCIAL SERVICES 25/50 TR",
  "NIFTY FINANCIAL SERVICES EX-BANK TR",
  "NIFTY FINANCIAL SERVICES TR",
  "NIFTY FMCG TR",
  "NIFTY HEALTHCARE TR",
  "NIFTY HOSPITALS TR",
  "NIFTY HOUSING FINANCE TR",
  "NIFTY INSURANCE TR",
  "NIFTY IT TR",
  "NIFTY MEDIA TR",
  "NIFTY METAL TR",
  "NIFTY MIDSMALL FINANCIAL SERVICES TR",
  "NIFTY MIDSMALL HEALTHCARE TR",
  "NIFTY MIDSMALL IT & TELECOM TR",
  "NIFTY NBFC TR",
  "NIFTY OIL & GAS TR",
  "NIFTY PHARMA TR",
  "NIFTY POWER TR",
  "NIFTY PRIVATE BANK TR",
  "NIFTY PSU BANK TR",
  "NIFTY REALTY TR",
  "NIFTY REITS & REALTY TR",
  "NIFTY RETAIL TR",
  "NIFTY TELECOMMUNICATIONS TR",
  "NIFTY500 HEALTHCARE TR"
)

INDEX_SQL <- paste0(
  "SELECT index_name, time_stamp, px_close FROM bhav_index WHERE index_name IN (",
  paste(sprintf("'%s'", INDEX_NAMES), collapse = ","),
  ") ORDER BY time_stamp, index_name"
)
VIX_SQL <- "SELECT time_stamp, px_close FROM vix_history ORDER BY time_stamp"
CASH_SQL <- sprintf(
  "SELECT AS_OF, NAV FROM mf_nav_history WHERE SCHEME_CODE = %d ORDER BY AS_OF",
  CASH_SCHEME_CODE
)

month_end_rows <- function(x) {
  if (NROW(x) == 0) return(x)
  x[!duplicated(format(index(x), "%Y-%m"), fromLast = TRUE)]
}

simple_returns <- function(levels) {
  out <- levels / lag(levels, 1) - 1
  out[-1, , drop = FALSE]
}

map_point_in_time <- function(target_dates, nav_xts) {
  target_dates <- as.Date(target_dates)
  source_dates <- as.Date(index(nav_xts))
  pos <- findInterval(target_dates, source_dates)
  if (any(pos == 0L)) stop("Cash NAV does not cover the first target date")
  mapped <- xts(as.numeric(nav_xts[pos]), target_dates)
  colnames(mapped) <- CASH_NAME
  attr(mapped, "source_dates") <- source_dates[pos]
  mapped
}

validate_unique_positive <- function(x, label) {
  if (anyDuplicated(as.Date(index(x)))) stop(label, " contains duplicate dates")
  if (any(!is.finite(as.numeric(x))) || any(as.numeric(x) <= 0)) {
    stop(label, " contains non-positive or non-finite values")
  }
  if (is.unsorted(as.Date(index(x)), strictly = TRUE)) stop(label, " dates are not strictly increasing")
  invisible(TRUE)
}

# Returns are validated per-finite-observation: staggered indices legitimately
# carry NA returns before inception, so NAs are excluded from the range check.
validate_return_range <- function(x, lower, upper, label) {
  vals <- as.numeric(x)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(invisible(TRUE))
  bad <- which(vals < lower | vals > upper)
  if (length(bad) > 0) {
    stop(sprintf("%s has %d returns outside [%.3f, %.3f]", label, length(bad), lower, upper))
  }
  invisible(TRUE)
}

# Neutralize single-day data artifacts in a raw index level series.
#
# bhav_index carries occasional bad ticks for some sectoral TR series: a level
# that spikes >threshold and reverts the next trading day (spike-revert), plus
# one-off re-basings that jump one-way and never revert.
#
#  - spike-revert: the spike day's level is replaced with the previous day's
#    level, so the spike day's return is zero and the next day's return
#    recomputes against the pre-spike level (correct, and gap-free).
#  - one-way re-base: all earlier levels are rescaled by the jump ratio so the
#    level path is continuous (equivalent to an adjusted series).
clean_index_levels <- function(x, label, threshold = CLEAN_THRESHOLD) {
  lvls <- as.numeric(x)
  dates <- as.Date(index(x))
  n <- length(lvls)
  notes <- character()
  if (n < 3L) return(list(x = x, notes = notes))

  returns <- c(NA_real_, lvls[-1] / lvls[-n] - 1)
  bad <- which(!is.na(returns) & abs(returns) > threshold)
  skip <- rep(FALSE, n)
  for (t in bad) {
    if (skip[t] || is.na(lvls[t])) next
    prev <- lvls[t - 1]
    if (is.na(prev)) next
    reverted <- FALSE
    if (t + 1L <= n && !is.na(lvls[t + 1])) {
      reverted <- isTRUE(abs(lvls[t + 1] / prev - 1) < 0.05)
    }
    if (reverted) {
      notes <- c(notes, sprintf("%s: neutralized spike tick %s (%.1f%%)",
                                label, dates[t], returns[t] * 100))
      lvls[t] <- prev  # carry forward: zero return on the spike day, gap-free
      skip[t + 1L] <- TRUE  # the revert day is auto-corrected by the carry-forward
    } else {
      ratio <- lvls[t] / prev
      lvls[seq_len(t - 1)] <- lvls[seq_len(t - 1)] * ratio
      notes <- c(notes, sprintf("%s: back-adjusted %d levels before %s by %.5f (re-base)",
                                label, t - 1, dates[t], ratio))
    }
  }
  list(x = xts(lvls, dates), notes = notes)
}

cache_fingerprint_inputs <- function(raw_source_ranges) {
  list(
    query_version = QUERY_VERSION,
    index_sql = INDEX_SQL,
    vix_sql = VIX_SQL,
    cash_sql = CASH_SQL,
    index_names = INDEX_NAMES,
    cash_scheme_code = CASH_SCHEME_CODE,
    regime_windows = REGIME_WINDOWS,
    momentum_lookbacks = MOMENTUM_LOOKBACKS,
    clean_threshold = CLEAN_THRESHOLD,
    train_end = TRAIN_END,
    test_start = TEST_START,
    source_ranges = raw_source_ranges
  )
}

validate_cache_fingerprint <- function(cache) {
  if (is.null(cache$raw_source_ranges)) stop("Cache is missing raw source ranges")
  current_inputs <- cache_fingerprint_inputs(cache$raw_source_ranges)
  if (!identical(cache$fingerprint_inputs, current_inputs)) {
    stop("Cache parameters differ from current build configuration")
  }
  expected <- digest(current_inputs, algo = "sha256")
  if (!identical(cache$fingerprint, expected)) stop("Cache fingerprint mismatch")
  invisible(TRUE)
}

query_or_stop <- function(con, sql, label) {
  ans <- sqlQuery(con, sql, stringsAsFactors = FALSE)
  if (is.character(ans)) stop(label, " query failed: ", paste(ans, collapse = " | "))
  if (nrow(ans) == 0) stop(label, " query returned no rows")
  ans
}

build_cache <- function(cache_path = CACHE_PATH) {
  con <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, "StockViz", ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE
  )
  on.exit(odbcClose(con), add = TRUE)

  index_df <- query_or_stop(con, INDEX_SQL, "Index")
  vix_df <- query_or_stop(con, VIX_SQL, "VIX")
  cash_df <- query_or_stop(con, CASH_SQL, "Cash")

  index_df$time_stamp <- as.Date(index_df$time_stamp)
  vix_df$time_stamp <- as.Date(vix_df$time_stamp)
  cash_df$AS_OF <- as.Date(cash_df$AS_OF)

  if (!setequal(unique(index_df$index_name), INDEX_NAMES)) stop("Unexpected index coverage")
  if (anyDuplicated(cash_df$AS_OF)) stop("Cash NAV has duplicate dates")

  clean_notes <- character()
  index_list <- lapply(INDEX_NAMES, function(nm) {
    d <- index_df[index_df$index_name == nm, c("time_stamp", "px_close")]
    x <- xts(d$px_close, d$time_stamp)
    colnames(x) <- nm
    validate_unique_positive(x, nm)
    cleaned <- clean_index_levels(x, nm)
    clean_notes <<- c(clean_notes, cleaned$notes)
    x <- cleaned$x
    colnames(x) <- nm
    validate_unique_positive(x, nm)
    x
  })
  raw_source_ranges <- list(
    index = setNames(lapply(index_list, function(x) as.character(range(as.Date(index(x))))),
                     INDEX_NAMES),
    cash = as.character(range(cash_df$AS_OF)),
    vix = as.character(range(vix_df$time_stamp))
  )

  # Staggered inclusion: outer join so each index keeps its own inception date.
  # Cells before an index's first trade are NA and are excluded from ranking.
  index_levels <- do.call(merge, index_list)
  colnames(index_levels) <- INDEX_NAMES

  cash_raw <- xts(cash_df$NAV, cash_df$AS_OF)
  colnames(cash_raw) <- CASH_NAME
  validate_unique_positive(cash_raw, "Cash NAV")

  vix <- xts(vix_df$px_close, vix_df$time_stamp)
  colnames(vix) <- "INDIA VIX"
  validate_unique_positive(vix, "India VIX")

  completed_cutoff <- as.Date(format(Sys.Date(), "%Y-%m-01")) - 1
  final_date <- min(max(as.Date(index(index_levels))), completed_cutoff)
  index_levels <- index_levels[paste0("/", final_date)]

  # The cash proxy bounds the common start (same binding constraint as the
  # market-cap study). India VIX warm-up bounds the effective signal start.
  common_start <- max(as.Date(first(index(cash_raw))), as.Date(first(index(index_levels))))
  index_levels <- index_levels[paste0(common_start, "/")]
  cash_nav <- map_point_in_time(as.Date(index(index_levels)), cash_raw)
  source_dates <- attr(cash_nav, "source_dates")
  if (any(source_dates > as.Date(index(cash_nav)))) stop("Cash mapping used a future NAV")

  index_returns <- simple_returns(index_levels)
  cash_returns <- simple_returns(cash_nav)
  validate_return_range(index_returns, -CLEAN_THRESHOLD, CLEAN_THRESHOLD, "Sector index")
  validate_return_range(cash_returns, -0.01, 0.01, "Cash")

  all_levels <- merge(index_levels, cash_nav, join = "inner")
  colnames(all_levels) <- c(INDEX_NAMES, CASH_NAME)
  month_ends <- month_end_rows(all_levels)
  coverage <- data.frame(
    series = c(INDEX_NAMES, "India VIX", "Quantum Liquid Fund-Growth"),
    first_date = as.Date(c(
      vapply(index_list, function(x) as.character(first(index(x))), character(1)),
      as.character(first(index(vix))), as.character(first(index(cash_raw)))
    )),
    last_date = as.Date(c(
      vapply(index_list, function(x) as.character(last(index(x))), character(1)),
      as.character(last(index(vix))), as.character(last(index(cash_raw)))
    )),
    rows = c(vapply(index_list, NROW, integer(1)), NROW(vix), NROW(cash_raw)),
    stringsAsFactors = FALSE
  )

  if (length(clean_notes) > 0) {
    writeLines(clean_notes, CLEAN_REPORT_PATH)
    cat(sprintf("Data-quality corrections: %d\n", length(clean_notes)))
    cat(paste0("  ", clean_notes, collapse = "\n"), "\n")
  }

  fingerprint_inputs <- cache_fingerprint_inputs(raw_source_ranges)
  cache <- list(
    fingerprint = digest(fingerprint_inputs, algo = "sha256"),
    fingerprint_inputs = fingerprint_inputs,
    raw_source_ranges = raw_source_ranges,
    clean_notes = clean_notes,
    index_levels = index_levels,
    index_returns = index_returns,
    cash_nav = cash_nav,
    cash_returns = cash_returns,
    vix = vix,
    month_ends = month_ends,
    coverage = coverage,
    cash_source_dates = source_dates,
    completed_through = final_date
  )
  validate_cache_fingerprint(cache)
  saveRDS(cache, cache_path)
  print(coverage, row.names = FALSE)
  cat(sprintf("Cache saved: %s (%d union daily rows; %d month ends)\n",
              cache_path, NROW(index_levels), NROW(month_ends)))
  invisible(cache)
}

if (sys.nframe() == 0L) {
  build_cache()
}
