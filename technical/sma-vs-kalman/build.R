#!/usr/bin/env Rscript
# Run: Rscript build.R [--refresh] [--no-png-tables]
# Offline uses recorded local source caches; --refresh requires StockViz config.
suppressPackageStartupMessages({
  library(RODBC)
  library(xts)
  library(zoo)
  library(PerformanceAnalytics)
  library(tidyverse)
  library(ggthemes)
  library(viridis)
  library(ggrepel)
  library(patchwork)
  library(gt)
  library(webshot2)
  library(digest)
})
SCRIPT_ARG <- grep("^--file=", commandArgs(), value = TRUE)
ROOT <- if (length(SCRIPT_ARG)) dirname(normalizePath(sub("^--file=", "", SCRIPT_ARG[1]))) else getwd()
source(file.path(ROOT, "engine.R"))
source_common("charts")
CONFIG_PATH <- Sys.getenv("STOCKVIZ_R_CONFIG", "/mnt/hollandC/StockViz/R/config.r")
# Keep config evaluation at script top and never print credential values.
if (file.exists(CONFIG_PATH)) source(CONFIG_PATH)
ARGS <- commandArgs(trailingOnly = TRUE)
OUT <- file.path(ROOT, "output")
INDEX_NAMES <- c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR",
                 "NIFTY BANK TR")
CACHE_PATHS <- c(broad = "/mnt/data/blog/technical/trend-vix-lookback/cache.rds",
                 bank = "/mnt/data/blog/technical/trend-vix-sectors/cache.rds")
COST_BPS <- c(25, 2)
PRIMARY_BPS <- 25

validate_levels <- function(x, label) {
  # Refuse missing, duplicate, unsorted or invalid closes instead of filling gaps.
  dates <- as.Date(index(x))
  stopifnot(inherits(x, "xts"), NCOL(x) == 1L, NROW(x) > WARMUP + 20L,
            !anyDuplicated(dates), !is.unsorted(dates, strictly = TRUE),
            all(is.finite(as.numeric(x))), all(as.numeric(x) > 0))
  colnames(x) <- label
  x
}

load_levels <- function(refresh = FALSE) {
  # Fetch exact SQL series or explicitly reuse cached observed index levels.
  if (refresh) {
    if (!file.exists(CONFIG_PATH)) stop("Live refresh blocked: set STOCKVIZ_R_CONFIG to an existing config.r")
    con <- odbcDriverConnect(sprintf(
      "Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=StockViz;Uid=%s;Pwd=%s;",
      ldbserver, ldbuser, ldbpassword), case = "nochange", believeNRows = TRUE)
    if (con < 0) stop("Live SQL Server connection failed")
    on.exit(odbcClose(con), add = TRUE)
    sql <- paste0("SELECT index_name, time_stamp, px_close FROM bhav_index WHERE index_name IN (",
                  paste(sprintf("'%s'", INDEX_NAMES), collapse = ","),
                  ") ORDER BY index_name, time_stamp")
    d <- sqlQuery(con, sql, stringsAsFactors = FALSE)
    if (!is.data.frame(d) || !nrow(d)) stop("Index query failed or returned no rows")
    names(d) <- tolower(names(d))
    if (!setequal(unique(d$index_name), INDEX_NAMES)) stop("SQL index identifiers do not match requested universe")
    levels <- setNames(lapply(INDEX_NAMES, function(nm) {
      z <- d[d$index_name == nm, ]
      validate_levels(sort(xts(z$px_close, as.Date(z$time_stamp))), nm)
    }), INDEX_NAMES)
    provenance <- data.frame(Index = INDEX_NAMES, Source = "StockViz.bhav_index live", SHA256 = NA_character_)
  } else {
    if (!all(file.exists(CACHE_PATHS))) stop("Missing offline source cache; use --refresh with configured database")
    broad <- readRDS(CACHE_PATHS[["broad"]])$index_levels
    bank <- readRDS(CACHE_PATHS[["bank"]])$index_levels
    levels <- setNames(lapply(INDEX_NAMES, function(nm) {
      if (nm %in% colnames(broad)) x <- broad[, nm]
      else if (nm %in% colnames(bank)) x <- bank[, nm]
      else stop("Requested primary index is absent from the source caches: ", nm)
      validate_levels(sort(x), nm)
    }), INDEX_NAMES)
    sources <- c(rep(CACHE_PATHS[["broad"]], 3), CACHE_PATHS[["bank"]])
    provenance <- data.frame(Index = INDEX_NAMES, Source = sources,
                             SHA256 = vapply(sources, digest, character(1), file = TRUE, algo = "sha256"))
  }
  provenance$FirstClose <- vapply(levels, function(x) as.character(min(as.Date(index(x)))), character(1))
  provenance$LastClose <- vapply(levels, function(x) as.character(max(as.Date(index(x)))), character(1))
  provenance$Rows <- vapply(levels, NROW, integer(1))
  list(levels = levels, provenance = provenance)
}

save_metric_table <- function(d, stem, title) {
  # Blue index groups, numeric within-index red-to-green ranks and green winners.
  display <- d[, c("Index", "System", "CAGR", "Vol", "Sharpe", "MaxDD", "Calmar",
                    "Invested", "Exits", "AnnualExits", "Turnover", "CostDrag")]
  tbl <- gt(display, groupname_col = "Index") |>
    tab_header(title = title, subtitle = "TR indices | cash earns 0 | 25 bps per entry/exit | 504-session warm-up") |>
    tab_style(cell_fill("#E3F2FD"), cells_row_groups()) |>
    tab_style(cell_text(weight = "bold"), cells_row_groups()) |>
    fmt_percent(columns = c(CAGR, Vol, MaxDD, Invested, CostDrag), decimals = 2) |>
    fmt_number(columns = c(Sharpe, Calmar, AnnualExits, Turnover), decimals = 2) |>
    fmt_integer(columns = Exits) |>
    tab_options(table.font.size = px(13), data_row.padding = px(4)) |>
    tab_source_note("@StockViz | MaxDD is negative; nearer zero is better. CostDrag is summed daily cost, not compounded wealth loss.")
  for (nm in unique(d$Index)) {
    rows <- which(d$Index == nm)
    for (metric in c("CAGR", "Sharpe", "MaxDD")) {
      vals <- d[[metric]][rows]
      ranks <- rank(vals, ties.method = "average")
      colors <- grDevices::colorRampPalette(c("#FFCDD2", "#FFF9C4", "#C8E6C9"))(length(rows))
      for (k in seq_along(rows)) {
        tbl <- tbl |> tab_style(cell_fill(colors[round(ranks[k])]),
                               cells_body(columns = all_of(metric), rows = rows[k]))
      }
      winners <- rows[vals == max(vals)]
      tbl <- tbl |> tab_style(cell_text(weight = "bold", color = "#1B5E20"),
                             cells_body(columns = all_of(metric), rows = winners))
    }
  }
  gtsave(tbl, file.path(OUT, paste0(stem, ".html")))
  if (!"--no-png-tables" %in% ARGS) {
    gtsave(tbl, file.path(OUT, paste0(stem, ".png")), vwidth = 1600, vheight = 2200)
  }
}

update_readme <- function(metrics, provenance) {
  # Refresh one generated results section while preserving hand-written docs.
  text <- c("## Results", "", "### Source coverage", "")
  text <- c(text, "| Index | First cached close | Last cached close | Rows |",
            "|---|---|---|---:|")
  for (i in seq_len(nrow(provenance))) {
    text <- c(text, sprintf("| %s | %s | %s | %s |", provenance$Index[i], provenance$FirstClose[i],
                            provenance$LastClose[i], provenance$Rows[i]))
  }
  for (w in names(WINDOWS)) {
    d <- metrics[metrics$Window == w, ]
    text <- c(text, "", paste0("### ", w, " results"), "",
              "| Index | System | CAGR | Sharpe | MaxDD | Invested | Exits |",
              "|---|---|---:|---:|---:|---:|---:|")
    for (i in seq_len(nrow(d))) text <- c(text, sprintf("| %s | %s | %.2f%% | %.2f | %.2f%% | %.1f%% | %d |",
      d$Index[i], d$System[i], 100*d$CAGR[i], d$Sharpe[i], 100*d$MaxDD[i], 100*d$Invested[i], d$Exits[i]))
    for (nm in INDEX_NAMES) {
      z <- d[d$Index == nm, ]
      k <- z[z$System == "Kalman", ]; s <- z[z$System == "SMA200", ]
      stem <- gsub("[^A-Za-z0-9]+", "_", nm)
      text <- c(text, "", sprintf("#### %s", nm), "",
                sprintf("![%s %s cumulative returns and drawdown](output/cum_dd_%s_%s.png)", nm, w, w, stem), "",
                sprintf("Kalman minus SMA200: CAGR %+.2f percentage points, Sharpe %+.2f, MaxDD %+.2f points (positive means shallower). Exits: %d vs %d. Lower turnover is not itself evidence of higher return or better drawdown control.",
                        100*(k$CAGR-s$CAGR), k$Sharpe-s$Sharpe, 100*(k$MaxDD-s$MaxDD), k$Exits, s$Exits))
    }
  }
  path <- file.path(ROOT, "README.md")
  readme <- readLines(path, encoding = "UTF-8")
  begin_marker <- "<!-- BEGIN GENERATED RESULTS -->"
  end_marker <- "<!-- END GENERATED RESULTS -->"
  begin <- which(readme == begin_marker)
  end <- which(readme == end_marker)
  block <- c(begin_marker, text, end_marker)
  if (!length(begin) && !length(end)) {
    readme <- c(readme, "", block)
  } else {
    stopifnot(length(begin) == 1L, length(end) == 1L, begin < end)
    before <- if (begin > 1L) readme[seq_len(begin - 1L)] else character()
    after <- if (end < length(readme)) readme[seq.int(end + 1L, length(readme))] else character()
    readme <- c(before, block, after)
  }
  writeLines(readme, path, useBytes = TRUE)
}

run_study <- function() {
  # Produce checkpoints before rendering, and assert the full artifact grid.
  dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
  source_data <- load_levels("--refresh" %in% ARGS)
  levels <- source_data$levels
  write.csv(source_data$provenance, file.path(OUT, "source_provenance.csv"), row.names = FALSE)
  results <- setNames(lapply(levels, simulate_index), INDEX_NAMES)
  checkpoint <- list(schema_version = 1L, built = as.character(Sys.time()), results = results,
                     provenance = source_data$provenance, q = KALMAN_Q, r = KALMAN_R,
                     warmup = WARMUP, windows = WINDOWS, costs_bps = COST_BPS)
  saveRDS(checkpoint, file.path(OUT, "checkpoint.rds"))
  # Verify the article's approximate horizon from a deterministic return impulse.
  weights <- kalman_filter(exp(c(0, rep(0.01, 2999))))[-1, "Slope"] / 0.01
  impulse <- data.frame(Lag = 0:2998, KalmanWeight = weights,
                        SMA200Weight = pmax(199 - (0:2998), 0) / sum(1:199))
  write.csv(impulse, file.path(OUT, "impulse_weights.csv"), row.names = FALSE)
  metrics <- bind_rows(lapply(INDEX_NAMES, function(nm) bind_rows(lapply(names(WINDOWS), function(w)
    index_metrics(results[[nm]], nm, w, PRIMARY_BPS)))))
  stopifnot(nrow(metrics) == length(INDEX_NAMES) * length(SYSTEMS) * length(WINDOWS),
            all(is.finite(as.matrix(metrics[, c("CAGR", "Sharpe", "MaxDD")]))))
  write.csv(metrics, file.path(OUT, "metrics.csv"), row.names = FALSE)
  sensitivity <- bind_rows(lapply(COST_BPS, function(bps) bind_rows(lapply(INDEX_NAMES, function(nm) {
    result <- if (bps == PRIMARY_BPS) results[[nm]] else simulate_index(levels[[nm]], bps / 10000)
    bind_rows(lapply(names(WINDOWS), function(w) index_metrics(result, nm, w, bps)))
  }))))
  write.csv(sensitivity, file.path(OUT, "cost_sensitivity.csv"), row.names = FALSE)
  daily <- bind_rows(lapply(INDEX_NAMES, function(nm) {
    r <- results[[nm]]
    bind_rows(lapply(seq_along(SYSTEMS), function(j) data.frame(
      Index = nm, Date = as.Date(index(r$rets)), System = SYSTEMS[j],
      Return = as.numeric(r$rets[, j]), Gross = as.numeric(r$gross[, j]),
      Exposure = as.numeric(r$exposure[, j]), Turnover = as.numeric(r$turnover[, j]),
      Cost = as.numeric(r$costs[, j]))))
  }))
  write.csv(daily, file.path(OUT, "daily_returns.csv"), row.names = FALSE)
  write.csv(daily[, c("Index", "Date", "System", "Exposure", "Turnover", "Cost")],
            file.path(OUT, "daily_exposure.csv"), row.names = FALSE)
  signals <- bind_rows(lapply(INDEX_NAMES, function(nm) mutate(results[[nm]]$signals, Index = nm)))
  write.csv(signals, file.path(OUT, "signals.csv"), row.names = FALSE)
  annual <- daily |> mutate(Year = as.integer(format(Date, "%Y"))) |>
    group_by(Index, System, Year) |> summarise(Return = prod(1 + Return) - 1,
      N = n(), Start = min(Date), End = max(Date), .groups = "drop") |>
    mutate(Partial = Year == as.integer(format(min(daily$Date), "%Y")) |
             Year == as.integer(format(max(daily$Date), "%Y")))
  # Mark each index's own opening year, including staggered histories.
  annual <- annual |> group_by(Index) |> mutate(Partial = Year == min(Year) | Year == max(Year)) |> ungroup()
  write.csv(annual, file.path(OUT, "annual_returns.csv"), row.names = FALSE)
  falls <- bind_rows(lapply(INDEX_NAMES, function(nm) fall_diagnostics(results[[nm]], nm)))
  write.csv(falls, file.path(OUT, "falls.csv"), row.names = FALSE)
  manifest <- list()
  for (w in names(WINDOWS)) {
    d <- metrics[metrics$Window == w, ]
    write.csv(d, file.path(OUT, paste0("metrics_", w, ".csv")), row.names = FALSE)
    save_metric_table(d, paste0("metrics_", w), paste("Kalman vs SMA200:", w))
    for (nm in INDEX_NAMES) {
      r <- slice_returns(results[[nm]]$rets, WINDOWS[[w]][1], WINDOWS[[w]][2])
      parts <- setNames(lapply(seq_along(SYSTEMS), function(j) r[, j]), SYSTEMS)
      stem <- gsub("[^A-Za-z0-9]+", "_", nm)
      path <- file.path(OUT, paste0("cum_dd_", w, "_", stem, ".png"))
      plot <- plotCumDrawdown(parts, title = paste(nm, "|", w), subtitle = "", outPath = path,
                              logScale = TRUE, save = FALSE)
      # Log wealth must have strictly positive label lanes; three arms use linear
      # wealth if the shared helper's proportional padding places a lane <= 0.
      if (any(plot[[1]]$layers[[3]]$data$labelY <= 0)) {
        plot <- plotCumDrawdown(parts, title = paste(nm, "|", w), subtitle = "", outPath = path,
                                logScale = FALSE, save = FALSE)
      }
      # Space cumulative-label lanes in log wealth, not raw wealth. The shared
      # raw padding can put a lane near zero and waste most of a log chart.
      if (identical(plot[[1]]$scales$get_scales("y")$trans$name, "log-10")) {
        wealth_range <- range(plot[[1]]$data$Value)
        label_lanes <- exp(seq(log(wealth_range[1] / 1.08),
                               log(wealth_range[2] * 1.08), length.out = length(SYSTEMS)))
        for (layer in 2:3) {
          data <- plot[[1]]$layers[[layer]]$data
          data$labelY <- label_lanes[data$rk]
          plot[[1]]$layers[[layer]]$data <- data
        }
      }
      dd_layer <- plot[[2]]$layers[[3]]
      dd_layer$data$lab <- sprintf("%s  %.1f%%", dd_layer$data$System, 100 * dd_layer$data$Drawdown)
      plot[[2]]$layers[[3]] <- dd_layer
      # Darker viridis endpoint keeps every end label readable on economist blue.
      palette <- setNames(viridis::viridis(3, end = 0.8), SYSTEMS)
      plot[[1]] <- suppressMessages(plot[[1]] + scale_color_manual(values = palette))
      plot[[2]] <- suppressMessages(plot[[2]] + scale_color_manual(values = palette))
      ggsave(path, plot, width = 13, height = 8.5, dpi = 120)
      manifest[[length(manifest) + 1L]] <- data.frame(Index = nm, Window = w, Start = min(as.Date(index(r))),
                                                     End = max(as.Date(index(r))), N = NROW(r), Path = path)
    }
  }
  manifest <- bind_rows(manifest)
  stopifnot(nrow(manifest) == length(INDEX_NAMES) * length(WINDOWS), all(file.exists(manifest$Path)))
  write.csv(manifest, file.path(OUT, "chart_manifest.csv"), row.names = FALSE)
  update_readme(metrics, source_data$provenance)
  cat(sprintf("Build complete: %d indices, %d metric rows, %d cum_dd charts.\n", length(INDEX_NAMES), nrow(metrics), nrow(manifest)))
}

if (sys.nframe() == 0L) run_study()
