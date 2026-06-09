#!/usr/bin/env Rscript
#
# PPFAS Flexi Cap Fund — Performance of Newly Added Positions
#
# Detects contiguous holding periods. A gap > 2 months between appearances
# counts as an exit + re-entry. Charts first-year excess return for every entry.
#

suppressPackageStartupMessages({
  library('tidyverse')
  library('ggthemes')
  library('viridis')
  library('scales')
})

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
argv <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", argv, value = TRUE)
SCRIPT_DIR <- if (length(file_arg) > 0) dirname(sub("^--file=", "", file_arg)) else getwd()
DATA_DIR   <- SCRIPT_DIR

CSV_IN    <- file.path(DATA_DIR, "indian_equity_lo_portfolio.csv")
CHART_DIR <- file.path(DATA_DIR, "charts")
dir.create(CHART_DIR, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
cat(sprintf("Reading %s ...\n", CSV_IN))
df <- read_csv(CSV_IN, show_col_types = FALSE) |>
  filter(!is.na(position_return_1y) & position_return_1y != "NA") |>
  mutate(
    date            = as.Date(date),
    position_return = as.numeric(position_return_1y),
    index_return    = as.numeric(nifty_midcap150_tr_return_1y),
    excess_return   = position_return - index_return,
    label           = if_else(is.na(symbol) | symbol == "NA",
                              str_trunc(name, 30), symbol)
  )

# ---------------------------------------------------------------------------
# Detect contiguous holding periods (gap > 60 days = new period)
# ---------------------------------------------------------------------------
find_entries <- function(df) {
  entries <- tibble()
  for (lbl in unique(df$label)) {
    pos_df <- df |> filter(label == lbl) |> arrange(date)
    if (nrow(pos_df) == 0) next

    # Mark period breaks where gap > 60 days
    pos_df <- pos_df |>
      mutate(
        prev_date = lag(date),
        gap_days  = as.numeric(date - prev_date),
        new_period = row_number() == 1 | is.na(gap_days) | gap_days > 60
      )

    # Each contiguous period starts at a "new_period" row
    period_starts <- pos_df |> filter(new_period)
    entries <- bind_rows(entries, period_starts)
  }
  entries
}

entries <- find_entries(df) |>
  arrange(excess_return) |>
  mutate(label = factor(label, levels = unique(label)))

cat(sprintf("%d rows, %d entry events\n", nrow(df), nrow(entries)))

# ---------------------------------------------------------------------------
# Chart: Excess return at each entry
# ---------------------------------------------------------------------------
cat("Chart: Excess return at entry ...\n")

p <- ggplot(entries, aes(x = label, y = excess_return)) +
  geom_col(aes(fill = excess_return), width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  scale_fill_viridis(option = "D", labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Excess Return After Entry",
    subtitle = sprintf(
      "PPFAS Flexi Cap Fund — %d entry events · 1-year forward vs NIFTY MIDCAP 150 TR",
      nrow(entries)
    ),
    x        = NULL,
    y        = "Excess Return vs NIFTY MIDCAP 150 TR\n(1-year forward from entry)",
    fill     = NULL,
    caption  = "@StockViz"
  ) +
  theme_economist() +
  theme(
    axis.text.x          = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    legend.position      = "none",
    plot.caption         = element_text(size = 8, color = "grey50", hjust = 1),
    plot.caption.position = "plot"
  )

ggsave(file.path(CHART_DIR, "new_positions_bar.png"),
       p, width = 14, height = 8, units = "in")

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat(sprintf("\nDone! Saved to %s/\n", CHART_DIR))
cat(sprintf("\nTotal entry events: %d across %d symbols\n",
            nrow(entries), n_distinct(entries$label)))

repeats <- entries |> count(label) |> filter(n > 1)
if (nrow(repeats) > 0) {
  cat(sprintf("Re-entries: %d\n", nrow(repeats)))
  for (i in seq_len(nrow(repeats))) {
    cat(sprintf("  %-15s %d entries\n", repeats$label[i], repeats$n[i]))
  }
}

cat("\nBest entries:\n")
best <- entries |> slice_max(excess_return, n = 5)
for (i in seq_len(nrow(best))) {
  cat(sprintf("  %-15s %s  %+5.1f%%\n",
              best$label[i], best$date[i], best$excess_return[i] * 100))
}
