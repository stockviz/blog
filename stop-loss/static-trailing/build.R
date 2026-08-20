suppressPackageStartupMessages({
  library(RODBC)
  library(DBI)
  library(RPostgres)
  library(digest)
})

source("/mnt/hollandC/StockViz/R/config.r")

REPORT_PATH <- "/mnt/data/blog/stop-loss/static-trailing"
CACHE_PATH <- file.path(REPORT_PATH, "cache.rds")
RESULTS_PATH <- file.path(REPORT_PATH, "results.rds")

MODEL_IDS <- c(
  "1A6C40B8-BDF1-43E5-829C-E3265BDB7F1A",
  "AFD0DFFF-2EA7-4E4D-BA50-D9CC0E4B5052"
)
HORIZONS <- c(1L, 5L, 10L, 20L)
QUERY_VERSION <- "static-trailing-v1"

validate_unique_positive <- function(x, label) {
  if (anyDuplicated(as.Date(index(x)))) stop(label, " duplicate dates")
  if (any(!is.finite(as.numeric(x))) || any(as.numeric(x) <= 0)) stop(label, " non-positive")
  invisible(TRUE)
}

cache_fingerprint_inputs <- function(sl_ranges, model_ids, horizons) {
  list(
    query_version = QUERY_VERSION,
    model_ids = sort(model_ids),
    horizons = sort(horizons),
    sl_ranges = sl_ranges
  )
}

validate_cache_fingerprint <- function(cache) {
  expected_inputs <- cache_fingerprint_inputs(cache$sl_ranges, cache$model_ids, cache$horizons)
  if (!identical(cache$fingerprint_inputs, expected_inputs)) stop("Cache parameters differ from build configuration")
  expected <- digest(expected_inputs, algo = "sha256")
  if (!identical(cache$fingerprint, expected)) stop("Cache fingerprint mismatch")
  invisible(TRUE)
}

query_or_stop <- function(con, sql, label) {
  ans <- sqlQuery(con, sql, stringsAsFactors = FALSE)
  if (is.character(ans)) stop(label, " query failed: ", paste(ans, collapse = " | "))
  ans
}

build_cache <- function(cache_path = CACHE_PATH) {
  con <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, "StockViz", ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE
  )
  on.exit(odbcClose(con), add = TRUE)

  pgCon <- dbConnect(
    RPostgres::Postgres(),
    host = "sweden", dbname = "StockVizDyn",
    user = ldbuser2, password = ldbpassword2,
    sslmode = "allow"
  )
  on.exit(dbDisconnect(pgCon), add = TRUE, after = TRUE)

  # ------------------------------------------------------------------
  # 1. SL events — distinct model/symbol/date (deduplicate intra-day repeats)
  mids_sql <- paste(sprintf("'%s'", MODEL_IDS), collapse = ",")
  sl_sql <- sprintf("
    SELECT DISTINCT MODEL_ID, SYMBOL, CAST(TIME_STAMP as date) as SL_DATE
    FROM ADVISOR_MODEL_PORTFOLIO_SL
    WHERE MODEL_ID IN (%s)
  ", mids_sql)
  sl <- query_or_stop(con, sl_sql, "SL")
  sl$SL_DATE <- as.Date(sl$SL_DATE)
  sl$MODEL_ID <- as.character(sl$MODEL_ID)
  sl$SYMBOL <- as.character(sl$SYMBOL)
  if (nrow(sl) == 0) stop("No SL events")
  if (anyDuplicated(paste(sl$MODEL_ID, sl$SYMBOL, sl$SL_DATE))) stop("Duplicate SL after DISTINCT")

  coverage <- data.frame(
    model = MODEL_IDS,
    events = sapply(MODEL_IDS, function(m) sum(sl$MODEL_ID == m)),
    stringsAsFactors = FALSE
  )
  cat(sprintf("SL events: %d total (%s)\n", nrow(sl), paste(coverage$events, collapse = "/")))
  cat(sprintf("Distinct symbols: %d\n", length(unique(sl$SYMBOL))))
  cat(sprintf("Date range: %s to %s\n", min(sl$SL_DATE), max(sl$SL_DATE)))

  distinct_symbols <- sort(unique(sl$SYMBOL))
  sl_ranges <- list(
    min_date = as.character(min(sl$SL_DATE)),
    max_date = as.character(max(sl$SL_DATE)),
    n_events = nrow(sl),
    n_symbols = length(distinct_symbols)
  )

  # ------------------------------------------------------------------
  # 2. Price history from eod_adjusted_nse (PG) — chunked IN queries
  fetch_pg <- function(symbols) {
    if (length(symbols) == 0) return(data.frame())
    placeholders <- paste(sprintf("$%d", seq_along(symbols)), collapse = ",")
    sql <- sprintf("SELECT ticker, date_stamp, c FROM eod_adjusted_nse WHERE ticker IN (%s) ORDER BY ticker, date_stamp", placeholders)
    dbGetQuery(pgCon, sql, params = as.list(symbols))
  }
  pg_chunks <- split(distinct_symbols, ceiling(seq_along(distinct_symbols) / 300))
  pg_list <- lapply(pg_chunks, fetch_pg)
  pg <- do.call(rbind, pg_list)
  pg$date_stamp <- as.Date(pg$date_stamp)
  pg$ticker <- as.character(pg$ticker)
  cat(sprintf("PG rows: %d (%d tickers, %d missing)\n", nrow(pg), length(unique(pg$ticker)),
              length(distinct_symbols) - length(unique(pg$ticker))))
  if (nrow(pg) > 0) {
    dup_pg <- anyDuplicated(paste(pg$ticker, pg$date_stamp))
    if (dup_pg) stop("Duplicate ticker/date in PG")
  }

  # ------------------------------------------------------------------
  # 3. Fallback daily returns from RETURN_SERIES_ALL (SQL Server)
  fetch_rsa <- function(symbols) {
    if (length(symbols) == 0) return(data.frame())
    in_list <- paste(sprintf("'%s'", gsub("'", "''", symbols)), collapse = ",")
    sql <- sprintf("SELECT SYMBOL, TIME_STAMP, DAILY_RETURN FROM RETURN_SERIES_ALL WHERE SYMBOL IN (%s) ORDER BY SYMBOL, TIME_STAMP", in_list)
    query_or_stop(con, sql, "RSA")
  }
  rsa_chunks <- split(distinct_symbols, ceiling(seq_along(distinct_symbols) / 500))
  rsa_list <- lapply(rsa_chunks, fetch_rsa)
  rsa <- do.call(rbind, rsa_list)
  rsa$TIME_STAMP <- as.Date(rsa$TIME_STAMP)
  rsa$SYMBOL <- as.character(rsa$SYMBOL)
  cat(sprintf("RSA rows: %d (%d symbols)\n", nrow(rsa), length(unique(rsa$SYMBOL))))

  # ------------------------------------------------------------------
  # 4. Build lookup: ticker -> data.frame sorted by date
  pg_index <- split(pg, pg$ticker)
  pg_index <- lapply(pg_index, function(d) d[order(d$date_stamp), ])

  rsa_index <- split(rsa, rsa$SYMBOL)
  rsa_index <- lapply(rsa_index, function(d) d[order(d$TIME_STAMP), ])

  forward_for_event <- function(symbol, sl_date, horizon) {
    # PG path — price ratio
    if (symbol %in% names(pg_index)) {
      d <- pg_index[[symbol]]
      dates <- d$date_stamp
      closes <- d$c
      pos <- findInterval(sl_date, dates)
      if (pos > 0 && pos + horizon <= nrow(d)) {
        # findInterval returns position of sl_date or previous trading day
        # forward is pos + horizon (horizon trading days after base)
        return(list(ret = closes[pos + horizon] / closes[pos] - 1, src = "pg"))
      }
    }
    # RSA fallback — cumulative daily returns after sl_date (exclusive)
    if (symbol %in% names(rsa_index)) {
      d <- rsa_index[[symbol]]
      dates <- d$TIME_STAMP
      rets <- d$DAILY_RETURN
      pos <- findInterval(sl_date, dates)
      # next trading day after sl_date is pos+1
      start <- pos + 1
      end <- start + horizon - 1
      if (start >= 1 && end <= nrow(d) && !any(!is.finite(rets[start:end]))) {
        return(list(ret = prod(1 + rets[start:end]) - 1, src = "rsa"))
      }
    }
    list(ret = NA_real_, src = NA_character_)
  }

  # ------------------------------------------------------------------
  # 5. Compute forward returns for every event x horizon
  horizons <- sort(HORIZONS)
  out_list <- vector("list", nrow(sl) * length(horizons))
  k <- 1L
  for (i in seq_len(nrow(sl))) {
    sym <- sl$SYMBOL[i]
    d <- sl$SL_DATE[i]
    mid <- sl$MODEL_ID[i]
    for (h in horizons) {
      fw <- forward_for_event(sym, d, h)
      out_list[[k]] <- data.frame(
        model = mid, symbol = sym, sl_date = d,
        horizon = h, ret = fw$ret, src = fw$src,
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
    if (i %% 2000 == 0) cat(sprintf("  forward %d / %d\n", i, nrow(sl)))
  }
  forward <- do.call(rbind, out_list)
  forward$sl_date <- as.Date(forward$sl_date)
  forward$horizon <- as.integer(forward$horizon)

  # Coverage diagnostics
  for (h in horizons) {
    sub <- forward[forward$horizon == h, ]
    cat(sprintf("H=%2d: N=%5d non-NA=%5d pg=%5d rsa=%4d\n",
                h, nrow(sub), sum(is.finite(sub$ret)),
                sum(sub$src == "pg", na.rm = TRUE),
                sum(sub$src == "rsa", na.rm = TRUE)))
  }

  fingerprint_inputs <- cache_fingerprint_inputs(sl_ranges, MODEL_IDS, HORIZONS)
  cache <- list(
    fingerprint = digest(fingerprint_inputs, algo = "sha256"),
    fingerprint_inputs = fingerprint_inputs,
    sl_ranges = sl_ranges,
    model_ids = MODEL_IDS,
    horizons = HORIZONS,
    sl = sl,
    pg = pg,
    rsa = rsa,
    forward = forward,
    completed_at = Sys.time()
  )
  validate_cache_fingerprint(cache)
  saveRDS(cache, cache_path)
  cat(sprintf("Cache saved: %s (%d SL distinct, %d forward rows)\n", cache_path, nrow(sl), nrow(forward)))
  invisible(cache)
}

if (sys.nframe() == 0L) {
  build_cache()
}
