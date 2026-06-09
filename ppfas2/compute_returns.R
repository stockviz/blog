#!/usr/bin/env Rscript
#
# PPFAS Flexi Cap Fund — Indian Equity Portfolio Returns
#
# Reads indian_equity_lo_portfolio.csv, looks up symbols via equity_ticker,
# fetches 1-year forward prices from eod_adjusted_nse (PostgreSQL) or
# RETURN_SERIES_ALL (SQL Server fallback), computes position and
# NIFTY MIDCAP 150 TR returns, and writes the enriched CSV.
#

suppressPackageStartupMessages({
  library('RODBC')
  library('RPostgres')
  library('tidyverse')
  library('xts')
  library('zoo')
})

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# Paths and config
# ---------------------------------------------------------------------------
# Determine script directory
argv <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", argv, value = TRUE)
if (length(file_arg) > 0) {
  SCRIPT_DIR <- dirname(sub("^--file=", "", file_arg))
} else {
  SCRIPT_DIR <- getwd()
}
DATA_DIR   <- SCRIPT_DIR
CSV_IN     <- file.path(DATA_DIR, "indian_equity_lo_portfolio.csv")
CSV_OUT    <- file.path(DATA_DIR, "indian_equity_lo_portfolio.csv")

source("/mnt/hollandC/StockViz/R/config.r")   # db creds

# ---------------------------------------------------------------------------
# Database connections
# ---------------------------------------------------------------------------
# SQL Server (equity_ticker, RETURN_SERIES_ALL, BHAV_INDEX)
mssqlCon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

# PostgreSQL (eod_adjusted_nse)
pgCon <- dbConnect(
  RPostgres::Postgres(),
  host     = ldbserver2,
  user     = ldbuser2,
  password = ldbpassword2,
  dbname   = 'StockVizDyn',
  sslmode  = 'allow'
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Look up NSE symbol from ISIN via equity_ticker.
#' Falls back to name-based lookup if ISIN match fails.
#' Returns the first SERIES='EQ' symbol, or any symbol if no EQ series found.
lookup_symbol <- function(isin_code, stock_name) {
  # Try ISIN first
  sql <- sprintf(
    "SELECT SYMBOL, SERIES FROM equity_ticker WHERE ISIN = '%s'",
    isin_code
  )
  df <- tryCatch(
    sqlQuery(mssqlCon, sql),
    error = function(e) NULL
  )

  if (is.data.frame(df) && nrow(df) > 0) {
    eq_rows <- df[df$SERIES == "EQ", ]
    if (nrow(eq_rows) > 0) return(eq_rows$SYMBOL[1])
    return(df$SYMBOL[1])
  }

  # Fallback: match by name (clean both sides for fuzzy matching)
  if (is.na(stock_name) || nchar(stock_name) < 3) return(NA_character_)

  # Normalize: strip Ltd/Limited/Company, dots, extra spaces
  clean_name <- function(x) {
    x <- gsub("\\s+", " ", x)
    x <- gsub("\\s+Limited$", "", x, ignore.case = TRUE)
    x <- gsub("\\s+Ltd\\.?$", "", x, ignore.case = TRUE)
    x <- gsub("\\s+Company$", "", x, ignore.case = TRUE)
    x <- gsub("\\s+Co\\.?$", "", x, ignore.case = TRUE)
    x <- gsub("\\s*\\(.*\\)", "", x)            # drop (India), (I) etc
    x <- gsub("\\s+India$", "", x, ignore.case = TRUE)
    x <- gsub("\\s+&\\s+", " and ", x)          # normalize & → and
    x <- gsub("\\s+and\\s+", " and ", x, ignore.case = TRUE)
    x <- gsub("\\.", "", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  search_name <- clean_name(stock_name)

  sql <- sprintf(
    "SELECT SYMBOL, SERIES, NAME FROM equity_ticker WHERE NAME LIKE '%%%s%%'",
    gsub("'", "''", search_name)
  )
  df <- tryCatch(
    sqlQuery(mssqlCon, sql),
    error = function(e) NULL
  )

  if (!is.data.frame(df) || nrow(df) == 0) return(NA_character_)

  # Rank by name similarity (exact after cleaning is best)
  df$clean <- vapply(df$NAME, clean_name, "")
  df$score <- nchar(df$clean)  # shorter = closer match when using LIKE
  exact <- df[df$clean == search_name, ]
  if (nrow(exact) > 0) df <- exact

  # Prefer EQ series
  eq_rows <- df[df$SERIES == "EQ", ]
  if (nrow(eq_rows) > 0) return(eq_rows$SYMBOL[1])

  return(df$SYMBOL[1])
}

#' Fetch adjusted close prices from eod_adjusted_nse (PostgreSQL).
#' Returns an xts of close prices, or NULL if not found / error.
fetch_prices_pg <- function(symbol, from_date, to_date) {
  df <- tryCatch(
    dbGetQuery(pgCon,
      "SELECT date_stamp, c FROM eod_adjusted_nse
       WHERE ticker = $1 AND date_stamp >= $2 AND date_stamp <= $3
       ORDER BY date_stamp",
      params = list(symbol, from_date, to_date)
    ),
    error = function(e) NULL
  )
  if (!is.data.frame(df) || nrow(df) < 2) return(NULL)

  xts(df$c, order.by = as.Date(df$date_stamp))
}

#' Fetch daily returns from RETURN_SERIES_ALL (SQL Server).
#' Returns an xts of cumulative price series (from returns), or NULL.
fetch_returns_mssql <- function(symbol, from_date, to_date) {
  sql <- sprintf(
    "SELECT TIME_STAMP, DAILY_RETURN FROM RETURN_SERIES_ALL
     WHERE SYMBOL = '%s' AND TIME_STAMP >= '%s' AND TIME_STAMP <= '%s'
     ORDER BY TIME_STAMP",
    symbol,
    format(from_date, "%Y-%m-%d"),
    format(to_date,   "%Y-%m-%d")
  )
  df <- tryCatch(
    sqlQuery(mssqlCon, sql),
    error = function(e) NULL
  )
  if (!is.data.frame(df) || nrow(df) < 2) return(NULL)

  # Build cumulative price series from returns (start at 1)
  rets <- xts(df$DAILY_RETURN, order.by = as.Date(df$TIME_STAMP))
  rets <- rets[!is.na(rets)]
  if (nrow(rets) < 2) return(NULL)

  cumprod(1 + rets)
}

#' Calculate return: (last / first) - 1.
#' Uses coredata() to avoid xts timestamp alignment issues.
#' Returns NA for impossible values (data errors).
calc_return <- function(price_series) {
  if (is.null(price_series) || nrow(price_series) < 2) return(NA_real_)
  first_val <- as.numeric(coredata(first(price_series)))
  last_val  <- as.numeric(coredata(last(price_series)))
  if (is.na(first_val) || is.na(last_val) || first_val == 0) return(NA_real_)
  if (first_val < 0 || last_val < 0) return(NA_real_)  # negative price = bad data
  ret <- (last_val / first_val) - 1
  if (ret < -0.99 || ret > 100) return(NA_real_)       # sanity bounds
  ret
}

#' Fetch NIFTY MIDCAP 150 TR close prices and compute 1-year return.
fetch_index_return <- function(from_date, to_date) {
  sql <- sprintf(
    "SELECT TIME_STAMP, PX_CLOSE FROM BHAV_INDEX
     WHERE INDEX_NAME = 'NIFTY MIDCAP 150 TR'
       AND TIME_STAMP >= '%s' AND TIME_STAMP <= '%s'
     ORDER BY TIME_STAMP",
    format(from_date, "%Y-%m-%d"),
    format(to_date,   "%Y-%m-%d")
  )
  df <- tryCatch(
    sqlQuery(mssqlCon, sql),
    error = function(e) NULL
  )
  if (!is.data.frame(df) || nrow(df) < 2) return(NA_real_)

  px <- xts(df$PX_CLOSE, order.by = as.Date(df$TIME_STAMP))
  calc_return(px)
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

cat(sprintf("Reading %s ...\n", CSV_IN))
portfolio <- read_csv(CSV_IN, show_col_types = FALSE)

# Deduplicate by (date, isin) — compute return once per holding per month
# Keep the first name seen for each ISIN (names drift slightly over time)
unique_positions <- portfolio |>
  distinct(date, isin, .keep_all = TRUE) |>
  select(date, isin, name) |>
  arrange(date, isin)

cat(sprintf("Computing returns for %d unique (date, isin) pairs ...\n",
            nrow(unique_positions)))

# Caches
symbol_cache  <- new.env(hash = TRUE, parent = emptyenv())
index_cache   <- new.env(hash = TRUE, parent = emptyenv())
return_cache  <- new.env(hash = TRUE, parent = emptyenv())

total   <- nrow(unique_positions)
last_pct <- 0

for (i in seq_len(total)) {
  dt   <- unique_positions$date[i]
  isin <- unique_positions$isin[i]
  nm   <- unique_positions$name[i]
  key  <- paste(dt, isin, sep = "|")

  pct_done <- floor(100 * i / total)
  if (pct_done %% 10 == 0 && pct_done != last_pct) {
    cat(sprintf("\r  [%d/%d] %d%% ...", i, total, pct_done))
    last_pct <- pct_done
  }

  portfolio_date <- as.Date(dt)
  forward_date   <- seq(portfolio_date, by = "1 year", length.out = 2)[2]

  # --- Lookup symbol ---
  sym <- symbol_cache[[isin]]
  if (is.null(sym)) {
    sym <- lookup_symbol(isin, nm)
    symbol_cache[[isin]] <- sym
  }

  # --- Compute position return ---
  if (is.na(sym)) {
    return_cache[[key]] <- NA_real_
  } else {
    # Try PostgreSQL eod_adjusted_nse first
    px <- fetch_prices_pg(sym, portfolio_date, forward_date)

    if (!is.null(px) && nrow(px) >= 2) {
      return_cache[[key]] <- calc_return(px)
    } else {
      # Fallback: RETURN_SERIES_ALL via SQL Server
      cum_px <- fetch_returns_mssql(sym, portfolio_date, forward_date)
      return_cache[[key]] <- calc_return(cum_px)
    }
  }

  # --- Compute index return (once per date) ---
  if (is.null(index_cache[[as.character(dt)]])) {
    idx_ret <- fetch_index_return(portfolio_date, forward_date)
    index_cache[[as.character(dt)]] <- idx_ret
  }
}
cat(sprintf("\r  [%d/%d] 100%% done.\n", total, total))

# --- Enrich portfolio with returns ---
cat("Enriching portfolio with returns ...\n")

# Pre-compute vectors for faster assignment
pos_rets <- vapply(seq_len(nrow(portfolio)), function(i) {
  key <- paste(portfolio$date[i], portfolio$isin[i], sep = "|")
  val <- return_cache[[key]]
  if (is.null(val)) NA_real_ else val
}, numeric(1))

idx_rets <- vapply(portfolio$date, function(d) {
  val <- index_cache[[as.character(d)]]
  if (is.null(val)) NA_real_ else val
}, numeric(1))

symbols <- vapply(portfolio$isin, function(isin) {
  sym <- symbol_cache[[isin]]
  if (is.null(sym)) NA_character_ else sym
}, character(1))

portfolio$symbol                        <- symbols
portfolio$position_return_1y            <- pos_rets
portfolio$nifty_midcap150_tr_return_1y  <- idx_rets

# --- Write CSV ---
cat(sprintf("Writing %s ...\n", CSV_OUT))
write_csv(portfolio, CSV_OUT)

# --- Summary ---
n_positions  <- nrow(unique_positions)
n_with_ret   <- sum(!is.na(portfolio$position_return_1y))
n_index_ret  <- length(unique(portfolio$date[!is.na(portfolio$nifty_midcap150_tr_return_1y)]))

cat(sprintf(
  "\nDone!\n  Unique (date, isin) pairs: %d\n  With returns: %d (%.1f%%)\n  Index dates covered: %d\n",
  n_positions, n_with_ret, 100 * n_with_ret / n_positions, n_index_ret
))

# Quick sanity: sample rows
cat("\nSample rows:\n")
print(head(portfolio |> select(date, name, pct_net_assets, position_return_1y, nifty_midcap150_tr_return_1y), 10))

# Cleanup
odbcClose(mssqlCon)
dbDisconnect(pgCon)
