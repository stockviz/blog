#!/usr/bin/env Rscript
#
# PPFAS Flexi Cap Fund — Position Out/Under-Performance vs NIFTY MIDCAP 150 TR
#
# Reads indian_equity_lo_portfolio.csv (with returns) and creates charts
# showing aggregate 1-year excess returns per position.
#

suppressPackageStartupMessages({
  library('tidyverse')
  library('ggthemes')
  library('viridis')
  library('ggrepel')
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

CSV_IN  <- file.path(DATA_DIR, "indian_equity_lo_portfolio.csv")
CHART_DIR <- file.path(DATA_DIR, "charts")
dir.create(CHART_DIR, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
cat(sprintf("Reading %s ...\n", CSV_IN))
df <- read_csv(CSV_IN, show_col_types = FALSE) |>
  filter(!is.na(position_return_1y) & position_return_1y != "NA") |>
  mutate(
    date              = as.Date(date),
    position_return   = as.numeric(position_return_1y),
    index_return      = as.numeric(nifty_midcap150_tr_return_1y),
    excess_return     = position_return - index_return
  )

# Use resolved symbol as label; fall back to cleaned name when symbol is NA
df <- df |>
  mutate(
    label = if_else(is.na(symbol) | symbol == "NA",
                    str_trunc(name, 30),
                    symbol)
  )

cat(sprintf("Loaded %d rows, %d unique positions\n", nrow(df), n_distinct(df$label)))

# ---------------------------------------------------------------------------
# Aggregate stats per position
# ---------------------------------------------------------------------------
pos_stats <- df |>
  group_by(label) |>
  summarise(
    n_months      = n(),
    avg_excess    = mean(excess_return),
    med_excess    = median(excess_return),
    sd_excess     = sd(excess_return),
    avg_position  = mean(position_return),
    avg_index     = mean(index_return),
    date_first    = min(date),
    date_last     = max(date),
    .groups       = "drop"
  ) |>
  mutate(
    # Categorize: consistent outperformer / underperformer / mixed
    category = case_when(
      avg_excess >  0.05 ~ "Outperformer (>5%)",
      avg_excess < -0.05 ~ "Underperformer (<-5%)",
      TRUE               ~ "Market-like (±5%)"
    )
  )

# ---------------------------------------------------------------------------
# Chart 1: Average excess return per position (bar chart, sorted)
# ---------------------------------------------------------------------------
cat("Chart 1: Average excess return per position ...\n")

plot_data <- pos_stats |>
  arrange(avg_excess)

plot_data <- plot_data |>
  mutate(label = factor(label, levels = label))

p1 <- ggplot(plot_data, aes(x = label, y = avg_excess)) +
  geom_col(aes(fill = avg_excess), width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  scale_fill_viridis(option = "D", direction = 1,
                     labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Excess Return by Position",
    subtitle = sprintf(
      "PPFAS Flexi Cap Fund — %d holdings · 1-year forward vs NIFTY MIDCAP 150 TR",
      nrow(plot_data)
    ),
    x        = NULL,
    y        = "Average Excess Return\n(1-year forward)",
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

ggsave(file.path(CHART_DIR, "position_excess_return_bar.png"),
       p1, width = 14, height = 8, units = "in")

# ---------------------------------------------------------------------------
# Chart 2: Excess return distribution — sorted dot plot with ranges
# ---------------------------------------------------------------------------
cat("Chart 2: Excess return distribution ...\n")

# Show min/median/max per position as a dot + range
dist_data <- df |>
  group_by(label) |>
  summarise(
    n        = n(),
    min_ex   = min(excess_return),
    q25_ex   = quantile(excess_return, 0.25),
    med_ex   = median(excess_return),
    q75_ex   = quantile(excess_return, 0.75),
    max_ex   = max(excess_return),
    .groups  = "drop"
  ) |>
  arrange(med_ex) |>
  mutate(label = factor(label, levels = label))

p2 <- ggplot(dist_data) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50", linetype = "dashed") +
  geom_linerange(
    aes(x = label, ymin = min_ex, ymax = max_ex),
    color = "grey70", linewidth = 0.3
  ) +
  geom_linerange(
    aes(x = label, ymin = q25_ex, ymax = q75_ex),
    color = "grey40", linewidth = 1.2
  ) +
  geom_point(
    aes(x = label, y = med_ex, color = med_ex),
    size = 2.5
  ) +
  scale_color_viridis(option = "D", direction = 1,
                      labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Excess Return Distribution",
    subtitle = sprintf(
      "PPFAS Flexi Cap Fund — %d holdings · line = min–max, bar = IQR, dot = median",
      nrow(dist_data)
    ),
    x        = NULL,
    y        = "Excess Return vs NIFTY MIDCAP 150 TR\n(1-year forward)",
    color    = "Median Excess Return",
    caption  = "@StockViz"
  ) +
  theme_economist() +
  theme(
    axis.text.x          = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    legend.title         = element_text(size = 8),
    legend.text          = element_text(size = 7, angle = 45, hjust = 1),
    plot.caption         = element_text(size = 8, color = "grey50", hjust = 1),
    plot.caption.position = "plot"
  )

ggsave(file.path(CHART_DIR, "position_excess_return_range.png"),
       p2, width = 14, height = 8, units = "in")

# ---------------------------------------------------------------------------
# Chart 3: Top/bottom performers over time — excess return heat stripes
# ---------------------------------------------------------------------------
cat("Chart 3: Excess return heat stripes over time ...\n")

# Pick top and bottom 10 by average excess return
top_bottom <- pos_stats |>
  slice_max(avg_excess, n = 10) |>
  bind_rows(pos_stats |> slice_min(avg_excess, n = 10)) |>
  distinct(label)

heat_data <- df |>
  inner_join(top_bottom, by = "label") |>
  # Keep the original order from pos_stats
  mutate(label = factor(label, levels = pos_stats$label[pos_stats$label %in% top_bottom$label]))

# Order labels by avg_excess
label_order <- pos_stats |>
  filter(label %in% top_bottom$label) |>
  arrange(avg_excess) |>
  pull(label)

heat_data <- heat_data |>
  mutate(label = factor(label, levels = label_order))

p3 <- ggplot(heat_data, aes(x = date, y = label, fill = excess_return)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis(
    option  = "D",
    limits  = c(-1, max(heat_data$excess_return, 1)),
    oob     = scales::squish,
    labels  = percent_format(),
    name    = "Excess Return"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Excess Return Over Time",
    subtitle = "PPFAS Flexi Cap Fund — top & bottom 10 by average excess return",
    x        = NULL,
    y        = NULL,
    caption  = "@StockViz"
  ) +
  theme_economist() +
  theme(
    axis.text.x          = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y          = element_text(size = 8),
    legend.position      = "right",
    legend.text          = element_text(size = 6),
    plot.caption         = element_text(size = 8, color = "grey50", hjust = 1),
    plot.caption.position = "plot"
  )

ggsave(file.path(CHART_DIR, "position_excess_return_heatmap.png"),
       p3, width = 14, height = 7, units = "in")

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat(sprintf(
  "\nDone! Charts saved to %s/\n",
  CHART_DIR
))

cat("\nPosition performance summary:\n")
cat(sprintf("  Outperformers (>5%% excess): %d\n",
            sum(pos_stats$avg_excess > 0.05)))
cat(sprintf("  Market-like (±5%%):         %d\n",
            sum(abs(pos_stats$avg_excess) <= 0.05)))
cat(sprintf("  Underperformers (<-5%%):     %d\n",
            sum(pos_stats$avg_excess < -0.05)))

# Top and bottom 5
cat("\nTop 5 by average excess return:\n")
top5_print <- pos_stats |> slice_max(avg_excess, n = 5)
for (i in seq_len(nrow(top5_print))) {
  cat(sprintf("  %-20s %+6.1f%%  (%d months)\n",
              top5_print$label[i],
              top5_print$avg_excess[i] * 100,
              top5_print$n_months[i]))
}

cat("\nBottom 5 by average excess return:\n")
bot5_print <- pos_stats |> slice_min(avg_excess, n = 5)
for (i in seq_len(nrow(bot5_print))) {
  cat(sprintf("  %-20s %+6.1f%%  (%d months)\n",
              bot5_print$label[i],
              bot5_print$avg_excess[i] * 100,
              bot5_print$n_months[i]))
}
