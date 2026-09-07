#!/usr/bin/env Rscript
#
# SEBI surveillance event-return report.
#
# Reads SEBI_SURVEILLANCE and measures the return on surveillance entry and
# exit dates, separating first entry from later grade changes. It joins each
# symbol to the DECILE_CONSTITUENTS market-cap decile at its first entry and
# writes event-level data, summary tables, and charts.
#

suppressPackageStartupMessages({
  library(RODBC)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(gt)
  library(webshot2)
})

options("scipen" = 100)
options(stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# Paths and configuration
# ---------------------------------------------------------------------------

argv <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", argv, value = TRUE)
SCRIPT_DIR <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  getwd()
}

REPORT_PATH <- SCRIPT_DIR
source("/mnt/hollandC/StockViz/R/config.r")

# ---------------------------------------------------------------------------
# Database helpers
# ---------------------------------------------------------------------------

#' Stop with the database error returned by RODBC, otherwise return its data.
query_or_stop <- function(con, sql, label) {
  # RODBC returns query errors as character vectors instead of raising them.
  # Convert that unconventional result into a normal, actionable error here.
  ans <- sqlQuery(con, sql, stringsAsFactors = FALSE)
  if (is.character(ans)) {
    stop(label, " query failed: ", paste(ans, collapse = " | "))
  }
  ans
}

#' Open the SQL Server and PostgreSQL connections used by this report.
open_connections <- function() {
  # Surveillance, decile, and fallback-return data are in SQL Server.
  mssql <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, ldbname, ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE
  )

  # Adjusted NSE closes are stored in StockVizDyn on PostgreSQL.
  pg <- dbConnect(
    RPostgres::Postgres(), host = ldbserver2, user = ldbuser2,
    password = ldbpassword2, dbname = "StockVizDyn", sslmode = "allow"
  )
  list(mssql = mssql, pg = pg)
}

#' Return the first matching table column from a list of known aliases.
find_column <- function(actual, aliases, required = TRUE) {
  normalized <- toupper(gsub("[^A-Z0-9]", "", actual))
  wanted <- toupper(gsub("[^A-Z0-9]", "", aliases))
  hit <- match(wanted, normalized)
  if (all(is.na(hit))) {
    if (required) stop("Could not identify a required column. Tried: ", paste(aliases, collapse = ", "))
    return(NA_character_)
  }
  actual[hit[which(!is.na(hit))[1]]]
}

#' Identify surveillance-table columns while allowing established schema aliases.
detect_surveillance_columns <- function(df) {
  # The current table uses SYMBOL, TIME_STAMP, and STAGE. Aliases keep the
  # script usable if a refreshed extract exposes a slightly different name.
  nms <- names(df)
  list(
    symbol = find_column(nms, c("SYMBOL", "TICKER", "SECURITY")),
    date = find_column(nms, c("TIME_STAMP", "SURVEILLANCE_DATE", "DATE_STAMP", "DATE", "EVENT_DATE")),
    grade = find_column(nms, c("GRADE", "STAGE", "SURVEILLANCE_STAGE", "SURVEILLANCE_INDICATOR", "INDICATOR", "MEASURE")),
    status = find_column(nms, c("STATUS", "ACTION", "EVENT", "EVENT_TYPE", "STATE"), required = FALSE)
  )
}

#' Quote SQL Server string literals without allowing symbols to alter the query.
sql_literal <- function(x) sprintf("'%s'", gsub("'", "''", as.character(x), fixed = TRUE))

#' Fetch historical decile membership in bounded symbol chunks.
fetch_deciles <- function(con, symbols, max_date) {
  if (length(symbols) == 0) return(data.frame())

  # Query all history through the latest first-entry date. The lower bound is
  # intentionally omitted: each symbol needs its latest decile *before* its
  # own first entry, which may be earlier than the earliest event in the set.
  chunks <- split(symbols, ceiling(seq_along(symbols) / 500L))
  rows <- lapply(chunks, function(chunk) {
    in_list <- paste(sql_literal(chunk), collapse = ",")
    sql <- sprintf(
      "SELECT SYMBOL, DECILE, TIME_STAMP FROM DECILE_CONSTITUENTS
       WHERE SYMBOL IN (%s) AND TIME_STAMP <= '%s'",
      in_list, format(max_date, "%Y-%m-%d")
    )
    query_or_stop(con, sql, "DECILE_CONSTITUENTS")
  })
  out <- do.call(rbind, rows)
  if (nrow(out) > 0) {
    out$SYMBOL <- as.character(out$SYMBOL)
    out$DECILE <- as.character(out$DECILE)
    out$TIME_STAMP <- as.Date(out$TIME_STAMP)
  }
  out
}

#' Fetch adjusted closes and traded volumes for all symbols and event dates.
fetch_adjusted_prices <- function(pg, symbols, min_date, max_date) {
  if (length(symbols) == 0) return(data.frame())

  # A 14-day look-back is requested by the caller so weekends and holidays
  # still leave enough history to calculate the prior trading-day close.
  chunks <- split(symbols, ceiling(seq_along(symbols) / 300L))
  rows <- lapply(chunks, function(chunk) {
    placeholders <- paste(sprintf("$%d", seq_along(chunk)), collapse = ",")
    dbGetQuery(
      pg,
      sprintf("SELECT ticker, date_stamp, c, v FROM eod_adjusted_nse
               WHERE ticker IN (%s) AND date_stamp >= $%d AND date_stamp <= $%d
               ORDER BY ticker, date_stamp", placeholders, length(chunk) + 1L, length(chunk) + 2L),
      params = c(as.list(chunk), list(min_date, max_date))
    )
  })
  out <- do.call(rbind, rows)
  if (nrow(out) > 0) {
    out$ticker <- as.character(out$ticker)
    out$date_stamp <- as.Date(out$date_stamp)
    out$c <- as.numeric(out$c)
    out$v <- as.numeric(out$v)
  }
  out
}

#' Fetch fallback daily returns from SQL Server.
fetch_fallback_returns <- function(con, symbols, min_date, max_date) {
  if (length(symbols) == 0) return(data.frame())

  # This series is used only when an adjusted close pair cannot be formed.
  chunks <- split(symbols, ceiling(seq_along(symbols) / 500L))
  rows <- lapply(chunks, function(chunk) {
    in_list <- paste(sql_literal(chunk), collapse = ",")
    query_or_stop(con, sprintf(
      "SELECT SYMBOL, TIME_STAMP, DAILY_RETURN FROM RETURN_SERIES_ALL
       WHERE SYMBOL IN (%s) AND TIME_STAMP >= '%s' AND TIME_STAMP <= '%s'
       ORDER BY SYMBOL, TIME_STAMP",
      in_list, format(min_date, "%Y-%m-%d"), format(max_date, "%Y-%m-%d")
    ), "RETURN_SERIES_ALL")
  })
  out <- do.call(rbind, rows)
  if (nrow(out) > 0) {
    out$SYMBOL <- as.character(out$SYMBOL)
    out$TIME_STAMP <- as.Date(out$TIME_STAMP)
    out$DAILY_RETURN <- as.numeric(out$DAILY_RETURN)
  }
  out
}

#' Classify a surveillance row as active or an explicit exit.
classify_active <- function(grade, status) {
  # In the live table, STAGE = "0" denotes no active surveillance. Some
  # extracts instead carry a textual exit/status marker, so handle both forms.
  grade_text <- trimws(tolower(ifelse(is.na(grade), "", grade)))
  text <- paste(grade_text, ifelse(is.na(status), "", tolower(status)))
  !grade_text %in% c("", "0", "na", "none") &
    !grepl("\\b(exit|exited|out|remove|removed|withdraw|inactive|ceased|closed)\\b", text)
}

#' Convert raw surveillance records into entry, transition, and exit events.
make_events <- function(raw, cols) {
  # Reduce the source to the fields needed by the state machine. Keeping the
  # original grade text preserves the label used in the source table.
  x <- data.frame(
    symbol = as.character(raw[[cols$symbol]]),
    event_date = as.Date(raw[[cols$date]]),
    grade = as.character(raw[[cols$grade]]),
    status = if (is.na(cols$status)) NA_character_ else as.character(raw[[cols$status]]),
    stringsAsFactors = FALSE
  )
  x <- x[!is.na(x$symbol) & nzchar(x$symbol) & !is.na(x$event_date), ]
  x$active <- classify_active(x$grade, x$status)
  x$grade_key <- tolower(trimws(ifelse(is.na(x$grade), "", x$grade)))

  # Daily snapshots repeat the same state. Remove exact duplicates before
  # walking the state changes, while retaining separate same-day grades.
  x <- x[order(x$symbol, x$event_date, x$grade_key, x$status), ]
  x <- x[!duplicated(paste(x$symbol, x$event_date, x$grade_key, x$active)), ]

  event_rows <- list()
  k <- 1L
  for (symbol in unique(x$symbol)) {
    d <- x[x$symbol == symbol, ]
    prev_active <- FALSE
    prev_grade <- NA_character_

    # Treat each ticker as a small finite-state machine:
    # inactive -> active is entry, active -> inactive is exit, and a changed
    # active grade is a transition. This avoids counting repeated snapshots.
    for (i in seq_len(nrow(d))) {
      is_entry <- d$active[i] && !prev_active
      is_transition <- d$active[i] && prev_active && !is.na(prev_grade) && d$grade_key[i] != prev_grade
      is_exit <- !d$active[i] && prev_active
      if (is_entry || is_transition || is_exit) {
        event_type <- if (is_entry && is.na(prev_grade)) "first_entry" else if (is_entry) "re_entry" else if (is_transition) "grade_transition" else "exit"
        event_rows[[k]] <- data.frame(
          symbol = symbol, event_date = d$event_date[i], grade = d$grade[i],
          status = d$status[i], event_type = event_type,
          from_grade = if (is.na(prev_grade)) NA_character_ else prev_grade,
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
      prev_active <- d$active[i]
      if (d$active[i]) prev_grade <- d$grade_key[i]
    }
  }
  if (length(event_rows) == 0) return(data.frame())
  do.call(rbind, event_rows)
}

#' Assign the latest known market-cap decile on or before first surveillance entry.
join_original_deciles <- function(events, deciles) {
  # All later transitions and exits inherit the decile at the first entry;
  # the decile is not re-estimated at each event.
  firsts <- events[events$event_type == "first_entry", c("symbol", "event_date")]
  names(firsts)[2] <- "first_entry_date"
  firsts <- firsts[!duplicated(firsts$symbol), ]
  if (nrow(deciles) == 0) {
    firsts$original_decile <- NA_character_
  } else {
    firsts$original_decile <- vapply(seq_len(nrow(firsts)), function(i) {
      # Use only information known by first entry to avoid look-ahead bias.
      d <- deciles[deciles$SYMBOL == firsts$symbol[i] & deciles$TIME_STAMP <= firsts$first_entry_date[i], ]
      if (nrow(d) == 0) return(NA_character_)
      d$DECILE[which.max(d$TIME_STAMP)]
    }, character(1))
  }
  merge(events, firsts, by = "symbol", all.x = TRUE, sort = FALSE)
}

#' Compute event-day return from adjusted closes, then use daily returns as fallback.
attach_event_returns <- function(events, prices, fallback) {
  if (nrow(events) == 0) return(events)

  # Index the fetched data once. Without these splits, each event would scan
  # the complete price/return table again.
  pidx <- split(prices, prices$ticker)
  ridx <- split(fallback, fallback$SYMBOL)
  out <- events
  out$return <- NA_real_
  out$return_source <- NA_character_
  out$volume <- NA_real_
  out$prior_volume <- NA_real_
  out$volume_change <- NA_real_
  for (i in seq_len(nrow(out))) {
    sym <- out$symbol[i]
    dt <- out$event_date[i]

    # Adjusted close: use the event date's close and the latest prior close.
    # findInterval naturally handles events recorded on non-trading days.
    if (sym %in% names(pidx)) {
      d <- pidx[[sym]][order(pidx[[sym]]$date_stamp), ]
      pos <- findInterval(dt, d$date_stamp)
      if (pos >= 2L && is.finite(d$v[pos]) && is.finite(d$v[pos - 1L]) && d$v[pos - 1L] > 0) {
        out$volume[i] <- d$v[pos]
        out$prior_volume[i] <- d$v[pos - 1L]
        out$volume_change[i] <- d$v[pos] / d$v[pos - 1L] - 1
      }
      if (pos >= 2L && is.finite(d$c[pos]) && is.finite(d$c[pos - 1L]) && d$c[pos - 1L] != 0) {
        out$return[i] <- d$c[pos] / d$c[pos - 1L] - 1
        out$return_source[i] <- "eod_adjusted_nse"
        next
      }
    }

    # Fallback: RETURN_SERIES_ALL stores the daily return directly on the
    # event date, so no cumulative reconstruction is needed here.
    if (sym %in% names(ridx)) {
      d <- ridx[[sym]]
      hit <- which(d$TIME_STAMP == dt & is.finite(d$DAILY_RETURN))
      if (length(hit) > 0L) {
        out$return[i] <- d$DAILY_RETURN[hit[1]]
        out$return_source[i] <- "RETURN_SERIES_ALL"
      }
    }
  }
  out
}

#' Return decile labels in numeric order, including multi-digit labels.
decile_levels <- function(x) {
  labels <- unique(as.character(x))
  numeric_labels <- suppressWarnings(as.numeric(labels))
  labels[order(is.na(numeric_labels), numeric_labels, labels)]
}

#' Calculate summary statistics by event type and original market-cap decile.
event_stats <- function(events) {
  groups <- events[is.finite(events$return) & !is.na(events$original_decile), ]
  if (nrow(groups) == 0) return(data.frame())
  split_groups <- split(groups, list(groups$event_type, groups$original_decile), drop = TRUE)
  rows <- lapply(split_groups, function(d) {
    x <- d$return
    s <- sd(x)

    # The one-sample t-statistic is descriptive: it tests the group mean
    # against zero, not whether two deciles differ from each other.
    t_stat <- if (length(x) > 1L && is.finite(s) && s > 0) mean(x) / (s / sqrt(length(x))) else NA_real_
    data.frame(
      event_type = d$event_type[1], original_decile = d$original_decile[1],
      N = length(x), mean = mean(x) * 100, median = median(x) * 100,
      sd = s * 100, positive_rate = mean(x > 0) * 100,
      t_stat = t_stat,
      p_value = if (is.finite(t_stat)) 2 * pt(-abs(t_stat), df = length(x) - 1L) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$decile_order <- match(out$original_decile, decile_levels(out$original_decile))
  out <- out[order(out$event_type, out$decile_order), ]
  out$decile_order <- NULL
  out
}

#' Test whether event-day returns differ across the original market-cap deciles.
decile_effect_stats <- function(events) {
  groups <- events[is.finite(events$return) & !is.na(events$original_decile), ]
  if (nrow(groups) == 0) return(data.frame())
  rows <- lapply(split(groups, groups$event_type), function(d) {
    # Kruskal-Wallis tests compare return distributions across deciles without
    # assuming normality. They are reported separately for each event type.
    d$decile_factor <- factor(d$original_decile)
    kw <- if (nlevels(d$decile_factor) > 1L) kruskal.test(d$return, d$decile_factor) else NULL
    data.frame(
      event_type = d$event_type[1], N = nrow(d), deciles = nlevels(d$decile_factor),
      kruskal_statistic = if (is.null(kw)) NA_real_ else unname(kw$statistic),
      kruskal_p_value = if (is.null(kw)) NA_real_ else kw$p.value,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Summarize the event-day volume change by event type and original decile.
volume_stats <- function(events) {
  groups <- events[is.finite(events$volume_change) & !is.na(events$original_decile), ]
  if (nrow(groups) == 0) return(data.frame())
  split_groups <- split(groups, list(groups$event_type, groups$original_decile), drop = TRUE)
  rows <- lapply(split_groups, function(d) {
    x <- d$volume_change
    data.frame(
      event_type = d$event_type[1], original_decile = d$original_decile[1],
      N = length(x), mean_change = mean(x) * 100, median_change = median(x) * 100,
      sd_change = sd(x) * 100, increased_rate = mean(x > 0) * 100,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$decile_order <- match(out$original_decile, decile_levels(out$original_decile))
  out <- out[order(out$event_type, out$decile_order), ]
  out$decile_order <- NULL
  out
}

#' Test whether event-day volume changes differ across original deciles.
volume_decile_effect_stats <- function(events) {
  groups <- events[is.finite(events$volume_change) & !is.na(events$original_decile), ]
  if (nrow(groups) == 0) return(data.frame())
  rows <- lapply(split(groups, groups$event_type), function(d) {
    decile_factor <- factor(d$original_decile)
    kw <- if (nlevels(decile_factor) > 1L) kruskal.test(d$volume_change, decile_factor) else NULL
    data.frame(
      event_type = d$event_type[1], N = nrow(d), deciles = nlevels(decile_factor),
      kruskal_statistic = if (is.null(kw)) NA_real_ else unname(kw$statistic),
      kruskal_p_value = if (is.null(kw)) NA_real_ else kw$p.value,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Save a gt table as HTML and PNG using the established StockViz presentation.
save_gt_table <- function(df, stem, title, subtitle) {
  if (nrow(df) == 0) return(invisible(NULL))
  tbl <- gt(df) |>
    tab_header(title = title, subtitle = subtitle) |>
    tab_source_note("@StockViz") |>
    tab_style(cell_text(weight = "bold"), cells_column_labels()) |>
    opt_row_striping()
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(numeric_cols) > 0) {
    tbl <- tbl |> fmt_number(columns = all_of(numeric_cols), decimals = 2)
  }
  if ("N" %in% names(df)) {
    tbl <- tbl |> fmt_number(columns = N, decimals = 0, use_seps = FALSE)
  }
  html <- file.path(REPORT_PATH, paste0(stem, ".html"))
  png <- file.path(REPORT_PATH, paste0(stem, ".png"))
  gtsave(tbl, html)
  webshot2::webshot(html, png, selector = "table.gt_table", expand = c(10, 10, 10, 10))
  invisible(tbl)
}

#' Write event-return charts split by first entries, transitions, exits, and decile.
save_charts <- function(events) {
  plot_df <- events[is.finite(events$return) & !is.na(events$original_decile), ]
  if (nrow(plot_df) == 0) return(invisible(NULL))
  # Explicit factor levels prevent lexical ordering such as 1, 10, 2.
  plot_df$original_decile <- factor(
    plot_df$original_decile,
    levels = decile_levels(plot_df$original_decile)
  )
  p <- ggplot(plot_df, aes(x = original_decile, y = return * 100, fill = event_type)) +
    geom_boxplot(outlier.alpha = 0.15, position = position_dodge(width = 0.8)) +
    geom_hline(yintercept = 0, linetype = 2, color = "red") +
    labs(title = "SEBI Surveillance Event-Day Returns by Original Market-Cap Decile",
         subtitle = sprintf("N=%d events with usable returns | adjusted close, else RETURN_SERIES_ALL", nrow(plot_df)),
         x = "Original market-cap decile", y = "Event-day return (%)", fill = "Event type", caption = "@StockViz") +
    theme_minimal(base_size = 11)
  ggsave(file.path(REPORT_PATH, "event-day-returns-by-decile.png"), p, width = 13, height = 7, dpi = 130)

  means <- plot_df |>
    group_by(event_type, original_decile) |>
    summarise(mean_return = mean(return) * 100, N = n(), .groups = "drop")
  p2 <- ggplot(means, aes(x = original_decile, y = mean_return, color = event_type, group = event_type)) +
    geom_hline(yintercept = 0, linetype = 2, color = "grey40") +
    geom_line(linewidth = 1) + geom_point(size = 2.5) +
    labs(title = "Mean SEBI Surveillance Event-Day Return by Original Market-Cap Decile",
         subtitle = "Means are shown only for decile/event-type groups with usable event-day returns",
         x = "Original market-cap decile", y = "Mean event-day return (%)", color = "Event type", caption = "@StockViz") +
    theme_minimal(base_size = 11)
  ggsave(file.path(REPORT_PATH, "mean-event-day-returns-by-decile.png"), p2, width = 12, height = 7, dpi = 130)

  # Volume is shown as a percentage change from the prior trading day. This
  # avoids comparing raw share counts across companies of very different size.
  volume_df <- events[is.finite(events$volume_change) & !is.na(events$original_decile), ]
  if (nrow(volume_df) > 0) {
    volume_df$original_decile <- factor(
      volume_df$original_decile,
      levels = decile_levels(volume_df$original_decile)
    )
    p3 <- ggplot(volume_df, aes(x = original_decile, y = volume_change * 100, fill = event_type)) +
      geom_boxplot(outlier.alpha = 0.15, position = position_dodge(width = 0.8)) +
      geom_hline(yintercept = 0, linetype = 2, color = "red") +
      labs(title = "SEBI Surveillance Event-Day Volume Change by Original Market-Cap Decile",
           subtitle = sprintf("N=%d events with usable volume | event-day volume relative to prior trading day", nrow(volume_df)),
           x = "Original market-cap decile", y = "Volume change (%)", fill = "Event type", caption = "@StockViz") +
      theme_minimal(base_size = 11)
    ggsave(file.path(REPORT_PATH, "event-day-volume-change-by-decile.png"), p3, width = 13, height = 7, dpi = 130)

    volume_means <- volume_df |>
      group_by(event_type, original_decile) |>
      summarise(mean_change = mean(volume_change) * 100, N = n(), .groups = "drop")
    p4 <- ggplot(volume_means, aes(x = original_decile, y = mean_change, color = event_type, group = event_type)) +
      geom_hline(yintercept = 0, linetype = 2, color = "grey40") +
      geom_line(linewidth = 1) + geom_point(size = 2.5) +
      labs(title = "Mean SEBI Surveillance Event-Day Volume Change by Decile",
           subtitle = "Event-day volume relative to the prior trading day",
           x = "Original market-cap decile", y = "Mean volume change (%)", color = "Event type", caption = "@StockViz") +
      theme_minimal(base_size = 11)
    ggsave(file.path(REPORT_PATH, "mean-event-day-volume-change-by-decile.png"), p4, width = 12, height = 7, dpi = 130)
  }
  invisible(NULL)
}

#' Run the complete surveillance event-return report.
run_report <- function() {
  conns <- open_connections()
  on.exit({ odbcClose(conns$mssql); dbDisconnect(conns$pg) }, add = TRUE)

  # Load the surveillance history first because its dates define every later
  # query window and the set of symbols required from the other databases.
  raw <- query_or_stop(
    conns$mssql, "SELECT * FROM SEBI_SURVEILLANCE", "SEBI_SURVEILLANCE"
  )
  if (nrow(raw) == 0) stop("SEBI_SURVEILLANCE contains no records")
  cols <- detect_surveillance_columns(raw)
  cat(sprintf("SEBI_SURVEILLANCE: %d rows; symbol=%s date=%s grade=%s status=%s\n",
              nrow(raw), cols$symbol, cols$date, cols$grade,
              ifelse(is.na(cols$status), "<none>", cols$status)))

  # Derive state changes before querying market data; daily snapshots are not
  # themselves events and would unnecessarily enlarge the price queries.
  events <- make_events(raw, cols)
  if (nrow(events) == 0) stop("No entry, transition, or exit events could be derived")
  first_dates <- events$event_date[events$event_type == "first_entry"]
  if (length(first_dates) == 0) stop("No first-entry events could be derived")

  symbols <- sort(unique(events$symbol))
  deciles <- fetch_deciles(conns$mssql, symbols, max(first_dates))
  events <- join_original_deciles(events, deciles)

  # Include a small pre-event buffer for the prior trading-day close.
  min_date <- min(events$event_date) - 14L
  max_date <- max(events$event_date)
  prices <- fetch_adjusted_prices(conns$pg, symbols, min_date, max_date)
  fallback <- fetch_fallback_returns(conns$mssql, symbols, min_date, max_date)
  events <- attach_event_returns(events, prices, fallback)

  events <- events[order(events$event_date, events$symbol, events$event_type), ]
  stats <- event_stats(events)
  decile_tests <- decile_effect_stats(events)
  volume_summary <- volume_stats(events)
  volume_tests <- volume_decile_effect_stats(events)
  # CSV files are convenient for inspection; the RDS preserves the complete
  # typed result bundle for downstream analysis.
  write.csv(events, file.path(REPORT_PATH, "surveillance-events.csv"),
            row.names = FALSE, na = "")
  write.csv(stats, file.path(REPORT_PATH, "surveillance-event-stats-by-decile.csv"),
            row.names = FALSE, na = "")
  write.csv(decile_tests, file.path(REPORT_PATH, "surveillance-decile-effect-tests.csv"),
            row.names = FALSE, na = "")
  write.csv(volume_summary, file.path(REPORT_PATH, "surveillance-volume-stats-by-decile.csv"),
            row.names = FALSE, na = "")
  write.csv(volume_tests, file.path(REPORT_PATH, "surveillance-volume-decile-effect-tests.csv"),
            row.names = FALSE, na = "")
  saveRDS(
    list(events = events, stats = stats, decile_tests = decile_tests,
         volume_summary = volume_summary, volume_tests = volume_tests, columns = cols),
    file.path(REPORT_PATH, "surveillance-report.rds")
  )

  save_gt_table(stats, "surveillance-event-stats-by-decile",
                "SEBI Surveillance Event-Day Returns by Original Market-Cap Decile",
                sprintf("First entries, grade transitions, re-entries, and exits | N=%d event rows | return = event close / prior close - 1",
                        nrow(events)))
  save_gt_table(decile_tests, "surveillance-decile-effect-tests",
                "Does Original Market-Cap Decile Explain Event-Day Returns?",
                "Kruskal-Wallis test across original deciles; small p-values indicate evidence of a distributional difference")
  save_gt_table(volume_summary, "surveillance-volume-stats-by-decile",
                "SEBI Surveillance Event-Day Volume Change by Original Market-Cap Decile",
                "Volume change = event-day traded volume / prior trading-day volume - 1")
  save_gt_table(volume_tests, "surveillance-volume-decile-effect-tests",
                "Does Original Market-Cap Decile Explain Event-Day Volume Changes?",
                "Kruskal-Wallis test across original deciles")
  save_charts(events)

  cat(sprintf("Events: %d (first entries %d, grade transitions %d, exits %d, re-entries %d)\n",
              nrow(events), sum(events$event_type == "first_entry"),
              sum(events$event_type == "grade_transition"), sum(events$event_type == "exit"),
              sum(events$event_type == "re_entry")))
  cat(sprintf("Returns: %d/%d adjusted-close, %d fallback, %d missing\n",
              sum(events$return_source == "eod_adjusted_nse", na.rm = TRUE),
              nrow(events), sum(events$return_source == "RETURN_SERIES_ALL", na.rm = TRUE),
              sum(!is.finite(events$return))))
  cat(sprintf("Volumes: %d/%d with event-day and prior-day volume\n",
              sum(is.finite(events$volume_change)), nrow(events)))
  cat(sprintf("Wrote report files to %s\n", REPORT_PATH))
  invisible(list(events = events, stats = stats))
}

if (sys.nframe() == 0L) run_report()
