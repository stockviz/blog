suppressPackageStartupMessages({
  library(RODBC)
  library(DBI)
  library(RPostgres)
  library(digest)
})

source("/mnt/hollandC/StockViz/R/config.r")

REPORT_PATH <- "/mnt/data/blog/stop-loss/replacements"
CACHE_PATH <- file.path(REPORT_PATH, "cache.rds")
RESULTS_PATH <- file.path(REPORT_PATH, "results.rds")

MODEL_IDS <- c(
  "1A6C40B8-BDF1-43E5-829C-E3265BDB7F1A",
  "AFD0DFFF-2EA7-4E4D-BA50-D9CC0E4B5052"
)
HORIZONS <- c(1L, 5L, 10L, 20L)
QUERY_VERSION <- "replacements-v1"

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
  # Azure remote: portfolio snapshots (ADVISOR_MODEL_PORTFOLIO)
  con_az <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            dbserver, "stockviz", dbuser, dbpassword),
    case = "nochange", believeNRows = TRUE
  )
  on.exit(odbcClose(con_az), add = TRUE)

  # Norway local: SL table (ADVISOR_MODEL_PORTFOLIO_SL)
  con_nw <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, "StockViz", ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE
  )
  on.exit(odbcClose(con_nw), add = TRUE, after = TRUE)

  pgCon <- dbConnect(
    RPostgres::Postgres(),
    host = "sweden", dbname = "StockVizDyn",
    user = ldbuser2, password = ldbpassword2,
    sslmode = "allow"
  )
  on.exit(dbDisconnect(pgCon), add = TRUE, after = TRUE)

  # ------------------------------------------------------------------
  # 1. SL events — distinct model/symbol/date and seq
  mids_sql <- paste(sprintf("'%s'", MODEL_IDS), collapse = ",")
  sl_sql <- sprintf("SELECT MODEL_ID, SYMBOL, SEQ_ID, TIME_STAMP FROM ADVISOR_MODEL_PORTFOLIO_SL WHERE MODEL_ID IN (%s)", mids_sql)
  sl_raw <- query_or_stop(con_nw, sl_sql, "SL")
  sl_raw$MODEL_ID <- as.character(sl_raw$MODEL_ID)
  sl_raw$SYMBOL <- as.character(sl_raw$SYMBOL)
  sl_raw$SEQ_ID <- as.numeric(sl_raw$SEQ_ID)
  sl_raw$TIME_STAMP <- as.POSIXct(sl_raw$TIME_STAMP)
  sl_raw$SL_DATE <- as.Date(sl_raw$TIME_STAMP)

  # distinct portfolio seqs ordered per model
  port_sql <- sprintf("SELECT MODEL_ID, SEQ_ID, SYMBOL, TIME_STAMP FROM ADVISOR_MODEL_PORTFOLIO WHERE MODEL_ID IN (%s)", mids_sql)
  port_raw <- query_or_stop(con_az, port_sql, "Portfolio")
  port_raw$MODEL_ID <- as.character(port_raw$MODEL_ID)
  port_raw$SYMBOL <- as.character(port_raw$SYMBOL)
  port_raw$SEQ_ID <- as.numeric(port_raw$SEQ_ID)
  port_raw$TIME_STAMP <- as.POSIXct(port_raw$TIME_STAMP)
  # filter CASH rows for replacement logic (CASH is not a tradable replacement)
  # keep for diff but exclude from replacement set later

  # Build per-model ordered seq list and portfolio map
  models <- MODEL_IDS
  all_replacements <- list()
  sl_ranges_list <- list()

  for (mid in models) {
    port_mid <- port_raw[port_raw$MODEL_ID == mid, ]
    sl_mid <- sl_raw[sl_raw$MODEL_ID == mid, ]
    seqs <- sort(unique(port_mid$SEQ_ID))
    seq_time <- setNames(port_mid$TIME_STAMP[match(seqs, port_mid$SEQ_ID)], as.character(seqs))
    # portfolio holdings per seq (exclude CASH for holdings comparison, but keep mapping)
    port_by_seq <- split(port_mid$SYMBOL, port_mid$SEQ_ID)
    # include all symbols including CASH for diff, but filter later
    sl_by_seq_symbols <- split(sl_mid$SYMBOL, sl_mid$SEQ_ID)

    # For each SL seq, find successor seq and compute added = next - cur
    repl_rows <- list()
    k <- 1
    for (sl_seq in names(sl_by_seq_symbols)) {
      sl_seq_num <- as.numeric(sl_seq)
      if (!as.character(sl_seq_num) %in% names(port_by_seq)) next
      idx <- match(sl_seq_num, seqs)
      if (is.na(idx) || idx >= length(seqs)) next
      nxt_seq <- seqs[idx + 1]
      cur_set <- unique(port_by_seq[[as.character(sl_seq_num)]])
      nxt_set <- unique(port_by_seq[[as.character(nxt_seq)]])
      added <- setdiff(nxt_set, cur_set)
      # exclude CASH from replacements (not tradable)
      added <- setdiff(added, c("CASH", "LIQUIDBEES"))
      if (length(added) == 0) next
      entry_date <- as.Date(seq_time[as.character(nxt_seq)])
      sl_date <- as.Date(seq_time[as.character(sl_seq_num)])
      for (sym in added) {
        repl_rows[[k]] <- data.frame(
          model = mid,
          sl_seq = sl_seq_num,
          next_seq = nxt_seq,
          sl_date = sl_date,
          entry_date = entry_date,
          symbol = sym,
          stringsAsFactors = FALSE
        )
        k <- k + 1
      }
    }
    if (length(repl_rows) > 0) {
      repl_mid <- do.call(rbind, repl_rows)
      all_replacements[[mid]] <- repl_mid
    }
    sl_ranges_list[[mid]] <- list(
      n_sl_events = nrow(sl_mid),
      n_sl_seqs = length(unique(sl_mid$SEQ_ID)),
      n_replacements = if (length(repl_rows) > 0) length(repl_rows) else 0,
      min_sl = as.character(min(sl_mid$SL_DATE)),
      max_sl = as.character(max(sl_mid$SL_DATE))
    )
    cat(sprintf("Model %s: %d SL rows, %d distinct SL seqs, %d replacements (added at next seq)\n",
                substr(mid, 1, 8), nrow(sl_mid), length(unique(sl_mid$SEQ_ID)),
                if (length(repl_rows) > 0) length(repl_rows) else 0))
  }

  replacements <- do.call(rbind, all_replacements)
  replacements$entry_date <- as.Date(replacements$entry_date)
  replacements$sl_date <- as.Date(replacements$sl_date)
  cat(sprintf("Total replacements: %d (%d distinct symbols)\n", nrow(replacements), length(unique(replacements$symbol))))
  cat(sprintf("Replacement date range: %s to %s\n", min(replacements$entry_date), max(replacements$entry_date)))
  cat(sprintf("Distinct replacement symbols: %d sample %s\n", length(unique(replacements$symbol)), paste(head(sort(unique(replacements$symbol)), 10), collapse = ", ")))

  distinct_symbols <- sort(unique(replacements$symbol))
  sl_ranges <- list(
    overall = list(
      n_replacements = nrow(replacements),
      n_symbols = length(distinct_symbols),
      min_entry = as.character(min(replacements$entry_date)),
      max_entry = as.character(max(replacements$entry_date))
    ),
    per_model = sl_ranges_list
  )

  # ------------------------------------------------------------------
  # 2. Price history from eod_adjusted_nse (PG)
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
  if (nrow(pg) > 0 && anyDuplicated(paste(pg$ticker, pg$date_stamp))) stop("Duplicate PG")

  # 3. Fallback RSA
  fetch_rsa <- function(symbols) {
    if (length(symbols) == 0) return(data.frame())
    in_list <- paste(sprintf("'%s'", gsub("'", "''", symbols)), collapse = ",")
    sql <- sprintf("SELECT SYMBOL, TIME_STAMP, DAILY_RETURN FROM RETURN_SERIES_ALL WHERE SYMBOL IN (%s) ORDER BY SYMBOL, TIME_STAMP", in_list)
    query_or_stop(con_nw, sql, "RSA")
  }
  rsa_chunks <- split(distinct_symbols, ceiling(seq_along(distinct_symbols) / 500))
  rsa_list <- lapply(rsa_chunks, fetch_rsa)
  rsa <- do.call(rbind, rsa_list)
  rsa$TIME_STAMP <- as.Date(rsa$TIME_STAMP)
  rsa$SYMBOL <- as.character(rsa$SYMBOL)
  cat(sprintf("RSA rows: %d (%d symbols)\n", nrow(rsa), length(unique(rsa$SYMBOL))))

  pg_index <- split(pg, pg$ticker)
  pg_index <- lapply(pg_index, function(d) d[order(d$date_stamp), ])
  rsa_index <- split(rsa, rsa$SYMBOL)
  rsa_index <- lapply(rsa_index, function(d) d[order(d$TIME_STAMP), ])

  forward_for <- function(symbol, entry_date, horizon) {
    if (symbol %in% names(pg_index)) {
      d <- pg_index[[symbol]]
      dates <- d$date_stamp
      closes <- d$c
      pos <- findInterval(entry_date, dates)
      if (pos > 0 && pos + horizon <= nrow(d)) {
        return(list(ret = closes[pos + horizon] / closes[pos] - 1, src = "pg"))
      }
    }
    if (symbol %in% names(rsa_index)) {
      d <- rsa_index[[symbol]]
      dates <- d$TIME_STAMP
      rets <- d$DAILY_RETURN
      pos <- findInterval(entry_date, dates)
      start <- pos + 1
      end <- start + horizon - 1
      if (start >= 1 && end <= nrow(d) && !any(!is.finite(rets[start:end]))) {
        return(list(ret = prod(1 + rets[start:end]) - 1, src = "rsa"))
      }
    }
    list(ret = NA_real_, src = NA_character_)
  }

  horizons <- sort(HORIZONS)
  out_list <- vector("list", nrow(replacements) * length(horizons))
  k <- 1
  for (i in seq_len(nrow(replacements))) {
    sym <- replacements$symbol[i]
    ed <- replacements$entry_date[i]
    mid <- replacements$model[i]
    sl_seq <- replacements$sl_seq[i]
    nxt_seq <- replacements$next_seq[i]
    for (h in horizons) {
      fw <- forward_for(sym, ed, h)
      out_list[[k]] <- data.frame(
        model = mid, symbol = sym,
        sl_seq = sl_seq, next_seq = nxt_seq,
        sl_date = replacements$sl_date[i],
        entry_date = ed,
        horizon = h, ret = fw$ret, src = fw$src,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
    if (i %% 2000 == 0) cat(sprintf("  forward %d / %d\n", i, nrow(replacements)))
  }
  forward <- do.call(rbind, out_list)
  forward$sl_date <- as.Date(forward$sl_date)
  forward$entry_date <- as.Date(forward$entry_date)
  forward$horizon <- as.integer(forward$horizon)

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
    replacements = replacements,
    pg = pg,
    rsa = rsa,
    forward = forward,
    completed_at = Sys.time()
  )
  validate_cache_fingerprint(cache)
  saveRDS(cache, cache_path)
  cat(sprintf("Cache saved: %s (%d replacements, %d forward rows)\n", cache_path, nrow(replacements), nrow(forward)))
  invisible(cache)
}

if (sys.nframe() == 0L) {
  build_cache()
}
