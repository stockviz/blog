#!/usr/bin/env Rscript

# Compare sentiment classifications produced by the Qwen and Jev pipelines.
# The script reads both SQLite result databases and writes progressively more
# detailed charts to sentiment-analysis/ under this project directory.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(viridis)
})

options(stringsAsFactors = FALSE)

project_dir <- normalizePath("/mnt/data/blog/media-sentiment", mustWork = FALSE)

output_dir <- file.path(project_dir, "sentiment-analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

qwen_db <- file.path(project_dir, "qwen", "SENTIMENT.db")
jev_db <- file.path(project_dir, "jev", "SENTIMENT.db")
index_csv <- Sys.getenv("INDEX_RETURNS_CSV", file.path(project_dir, "index_returns.csv"))

sentiment_levels <- c("POSITIVE", "NEUTRAL", "NEGATIVE")
sentiment_colors <- setNames(viridis(3, option = "D"), sentiment_levels)
model_colors <- setNames(viridis(2, option = "C"), c("QWEN", "JEV"))

read_sentiment <- function(path, model_name) {
  # Read and close each result database before any plotting starts.
  if (!file.exists(path)) {
    stop(sprintf("Missing sentiment database: %s", path), call. = FALSE)
  }

  connection <- dbConnect(SQLite(), path)
  on.exit(dbDisconnect(connection), add = TRUE)
  result <- dbReadTable(connection, "SENTIMENT")

  result %>%
    mutate(
      MODEL = model_name,
      SENTIMENT = factor(as.character(SENTIMENT), levels = sentiment_levels),
      SOURCE_TYPE = factor(as.character(SOURCE_TYPE), levels = c("VIDEO", "PRINT")),
      SOURCE = as.character(SOURCE)
    )
}

qwen <- read_sentiment(qwen_db, "QWEN")
jev <- read_sentiment(jev_db, "JEV")
data <- bind_rows(qwen, jev)

if (any(is.na(data$SENTIMENT))) {
  stop("Unexpected sentiment label found in one of the databases.", call. = FALSE)
}

write.csv(
  data %>% count(MODEL, SENTIMENT, name = "COUNT"),
  file.path(output_dir, "aggregate_sentiment.csv"),
  row.names = FALSE
)
write.csv(
  data %>% count(MODEL, SOURCE_TYPE, SENTIMENT, name = "COUNT"),
  file.path(output_dir, "sentiment_by_source_type.csv"),
  row.names = FALSE
)
write.csv(
  data %>% count(MODEL, SOURCE, SENTIMENT, name = "COUNT"),
  file.path(output_dir, "sentiment_by_source.csv"),
  row.names = FALSE
)

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey30"),
    plot.caption = element_text(color = "grey45", hjust = 1),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

percent_labels <- function(plot_data) {
  plot_data %>%
    mutate(label = ifelse(PERCENT == 0, "", sprintf("%.1f%%", PERCENT)))
}

plot_distribution <- function(plot_data, x, title, subtitle, flip = FALSE) {
  # Build a consistently coloured percentage chart for one model or comparison.
  plot_data <- percent_labels(plot_data)
  chart <- ggplot(plot_data, aes(x = .data[[x]], y = PERCENT, fill = SENTIMENT)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    geom_text(
      aes(label = label),
      position = position_dodge(width = 0.8),
      vjust = -0.25,
      size = 3,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = sentiment_colors, drop = FALSE) +
    scale_y_continuous(
      labels = percent_format(scale = 1),
      breaks = seq(0, 100, by = 20),
      limits = c(0, 105),
      expand = expansion(mult = c(0, 0.04))
    ) +
    labs(
      x = NULL,
      y = "Share of articles",
      title = title,
      subtitle = subtitle,
      caption = "@StockViz",
      fill = "Sentiment"
    ) +
    base_theme

  if (flip) {
    chart <- chart + coord_flip()
  }
  chart
}

# 1. Aggregate sentiment: Qwen and Jev side by side.
aggregate_counts <- data %>%
  count(MODEL, SENTIMENT, name = "N") %>%
  complete(MODEL = c("QWEN", "JEV"), SENTIMENT = factor(sentiment_levels, levels = sentiment_levels), fill = list(N = 0)) %>%
  group_by(MODEL) %>%
  mutate(PERCENT = 100 * N / sum(N)) %>%
  ungroup() %>%
  mutate(MODEL = factor(MODEL, levels = c("QWEN", "JEV")))

write.csv(aggregate_counts, file.path(output_dir, "aggregate_sentiment.csv"), row.names = FALSE)

aggregate_plot <- plot_distribution(
  aggregate_counts,
  "MODEL",
  "Aggregate sentiment: Qwen vs. Jev",
  sprintf("Qwen n=%s; Jev n=%s", comma(nrow(qwen)), comma(nrow(jev)))
)
ggsave(
  file.path(output_dir, "01_aggregate_qwen_vs_jev.png"),
  aggregate_plot,
  width = 10,
  height = 6,
  units = "in",
  dpi = 160
)

# 2. Sentiment by VIDEO vs. PRINT, with Qwen and Jev panels stacked vertically.
by_type <- data %>%
  count(MODEL, SOURCE_TYPE, SENTIMENT, name = "N") %>%
  complete(
    MODEL = c("QWEN", "JEV"),
    SOURCE_TYPE = factor(c("VIDEO", "PRINT"), levels = c("VIDEO", "PRINT")),
    SENTIMENT = factor(sentiment_levels, levels = sentiment_levels),
    fill = list(N = 0)
  ) %>%
  group_by(MODEL, SOURCE_TYPE) %>%
  mutate(PERCENT = 100 * N / sum(N)) %>%
  ungroup()

write.csv(by_type, file.path(output_dir, "sentiment_by_source_type.csv"), row.names = FALSE)

qwen_type_plot <- plot_distribution(
  by_type %>% filter(MODEL == "QWEN"),
  "SOURCE_TYPE",
  "Qwen sentiment by source type",
  sprintf("n=%s", comma(nrow(qwen)))
)
jev_type_plot <- plot_distribution(
  by_type %>% filter(MODEL == "JEV"),
  "SOURCE_TYPE",
  "Jev sentiment by source type",
  sprintf("n=%s", comma(nrow(jev)))
)

type_panel <- qwen_type_plot / jev_type_plot + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(
  file.path(output_dir, "02_sentiment_by_source_type.png"),
  type_panel,
  width = 10,
  height = 11,
  units = "in",
  dpi = 160
)

# 3. Sentiment by source, with Qwen and Jev panels stacked vertically.
source_order <- data %>%
  count(SOURCE, wt = 1, name = "N") %>%
  group_by(SOURCE) %>%
  summarise(N = sum(N), .groups = "drop") %>%
  arrange(N, SOURCE) %>%
  pull(SOURCE)

by_source <- data %>%
  count(MODEL, SOURCE, SENTIMENT, name = "N") %>%
  complete(
    MODEL = c("QWEN", "JEV"),
    SOURCE = source_order,
    SENTIMENT = factor(sentiment_levels, levels = sentiment_levels),
    fill = list(N = 0)
  ) %>%
  group_by(MODEL, SOURCE) %>%
  mutate(PERCENT = 100 * N / sum(N)) %>%
  ungroup() %>%
  mutate(SOURCE = factor(SOURCE, levels = source_order))

write.csv(by_source, file.path(output_dir, "sentiment_by_source.csv"), row.names = FALSE)

qwen_source_plot <- plot_distribution(
  by_source %>% filter(MODEL == "QWEN"),
  "SOURCE",
  "Qwen sentiment by source",
  "Percentage of each source's articles; sources ordered by combined article count",
  flip = TRUE
)
jev_source_plot <- plot_distribution(
  by_source %>% filter(MODEL == "JEV"),
  "SOURCE",
  "Jev sentiment by source",
  "Percentage of each source's articles; sources ordered by combined article count",
  flip = TRUE
)

source_panel <- qwen_source_plot / jev_source_plot + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(
  file.path(output_dir, "03_sentiment_by_source.png"),
  source_panel,
  width = 12,
  height = max(12, length(source_order) * 0.65),
  units = "in",
  dpi = 160
)

# 4. Daily positive sentiment versus daily index returns.
#
# The optional index CSV must contain DATE (or DATE_STAMP), INDEX (or
# INDEX_NAME), and RETURN columns. If RETURN is absent, PX_CLOSE is accepted
# and converted to a same-day close-to-close return within each index.
read_index_returns <- function(path) {
  # Read daily total-return index observations and normalize their column names.
  if (!file.exists(path)) {
    warning(sprintf(
      "Skipping index scatterplot: %s does not exist. Set INDEX_RETURNS_CSV to a CSV with DATE, INDEX, RETURN.",
      path
    ))
    return(NULL)
  }

  raw <- read.csv(path, check.names = FALSE)
  names(raw) <- toupper(gsub("[^A-Z0-9]+", "_", names(raw)))

  date_col <- intersect(c("DATE", "DATE_STAMP", "TIME_STAMP"), names(raw))[1]
  index_col <- intersect(c("INDEX", "INDEX_NAME"), names(raw))[1]
  return_col <- intersect(c("RETURN", "RET", "DAILY_RETURN"), names(raw))[1]
  close_col <- intersect(c("PX_CLOSE", "CLOSE"), names(raw))[1]
  if (is.na(date_col) || is.na(index_col) || (is.na(return_col) && is.na(close_col))) {
    stop("Index CSV needs DATE, INDEX, and RETURN columns; PX_CLOSE may replace RETURN.", call. = FALSE)
  }

  result <- data.frame(
    DATE = as.Date(raw[[date_col]]),
    INDEX = as.character(raw[[index_col]])
  )
  if (!is.na(return_col)) {
    result$RETURN = as.numeric(raw[[return_col]])
  } else {
    result$PX_CLOSE = as.numeric(raw[[close_col]])
    result <- result %>%
      arrange(INDEX, DATE) %>%
      group_by(INDEX) %>%
      mutate(RETURN = PX_CLOSE / lag(PX_CLOSE) - 1) %>%
      ungroup()
  }

  result %>%
    filter(INDEX %in% c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR")) %>%
    select(DATE, INDEX, RETURN) %>%
    filter(!is.na(DATE), !is.na(RETURN))
}

index_returns <- read_index_returns(index_csv)
if (!is.null(index_returns)) {
  daily_sentiment <- data %>%
    mutate(DATE = as.Date(DATE_STAMP)) %>%
    group_by(MODEL, DATE) %>%
    summarise(
      POSITIVE_PERCENT = 100 * mean(SENTIMENT == "POSITIVE"),
      ARTICLES = n(),
      .groups = "drop"
    )

  scatter_data <- daily_sentiment %>%
    inner_join(index_returns, by = "DATE", relationship = "many-to-many") %>%
    mutate(INDEX = factor(INDEX, levels = c(
      "NIFTY 50 TR",
      "NIFTY MIDCAP 150 TR",
      "NIFTY SMALLCAP 250 TR"
    )))

  write.csv(
    scatter_data,
    file.path(output_dir, "daily_positive_sentiment_vs_index_returns.csv"),
    row.names = FALSE
  )

  index_colors <- setNames(viridis(3, option = "C"), c("NIFTY 50 TR", "NIFTY MIDCAP 150 TR", "NIFTY SMALLCAP 250 TR"))

  plot_sentiment_returns <- function(plot_data, model_name) {
    # Plot one model's daily positive sentiment against each index return.
    ggplot(plot_data, aes(x = RETURN, y = POSITIVE_PERCENT, colour = INDEX)) +
      geom_point(alpha = 0.35, size = 1.2) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.8, na.rm = TRUE) +
      facet_wrap(~INDEX, nrow = 1, scales = "free_x") +
      scale_colour_manual(values = index_colors, drop = FALSE) +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      scale_y_continuous(
        labels = label_percent(scale = 1),
        breaks = seq(0, 100, by = 20),
        limits = c(0, 105)
      ) +
      labs(
        x = "Daily total-return index return",
        y = "Daily positive sentiment",
        title = sprintf("%s: daily positive sentiment vs. index returns", model_name),
        subtitle = "Same-date observations; line is an ordinary least-squares fit",
        caption = "@StockViz",
        colour = "Index"
      ) +
      base_theme +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  }

  qwen_scatter <- plot_sentiment_returns(scatter_data %>% filter(MODEL == "QWEN"), "Qwen")
  jev_scatter <- plot_sentiment_returns(scatter_data %>% filter(MODEL == "JEV"), "Jev")
  scatter_panel <- qwen_scatter / jev_scatter + plot_layout(guides = "collect") & theme(legend.position = "bottom")

  ggsave(
    file.path(output_dir, "04_daily_positive_sentiment_vs_index_returns.png"),
    scatter_panel,
    width = 16,
    height = 11,
    units = "in",
    dpi = 160
  )
}

# 5 onward. Paired and diagnostic charts.
# These charts use the intersection of Qwen and Jev item keys where a paired
# comparison is required. This keeps model disagreement separate from coverage
# differences between the two result databases.
paired <- qwen %>%
  transmute(
    SOURCE_TYPE,
    ITEM_ID,
    DATE_STAMP,
    SOURCE,
    SENTIMENT_QWEN = as.character(SENTIMENT)
  ) %>%
  inner_join(
    jev %>% transmute(
      SOURCE_TYPE,
      ITEM_ID,
      SENTIMENT_JEV = as.character(SENTIMENT)
    ),
    by = c("SOURCE_TYPE", "ITEM_ID")
  )

paired_long <- bind_rows(
  paired %>% transmute(SOURCE_TYPE, ITEM_ID, SOURCE, MODEL = "QWEN", SENTIMENT = SENTIMENT_QWEN),
  paired %>% transmute(SOURCE_TYPE, ITEM_ID, SOURCE, MODEL = "JEV", SENTIMENT = SENTIMENT_JEV)
)

# 5. Paired confusion matrix and agreement summary.
confusion <- paired %>%
  count(SENTIMENT_QWEN, SENTIMENT_JEV, name = "N") %>%
  complete(
    SENTIMENT_QWEN = sentiment_levels,
    SENTIMENT_JEV = sentiment_levels,
    fill = list(N = 0)
  ) %>%
  group_by(SENTIMENT_QWEN) %>%
  mutate(PERCENT_OF_QWEN_ROW = 100 * N / sum(N)) %>%
  ungroup()
write.csv(confusion, file.path(output_dir, "paired_confusion_matrix.csv"), row.names = FALSE)

observed_agreement <- mean(paired$SENTIMENT_QWEN == paired$SENTIMENT_JEV)
qwen_marginal <- prop.table(table(factor(paired$SENTIMENT_QWEN, levels = sentiment_levels)))
jev_marginal <- prop.table(table(factor(paired$SENTIMENT_JEV, levels = sentiment_levels)))
expected_agreement <- sum(qwen_marginal * jev_marginal)
agreement_summary <- data.frame(
  PAIRED_RECORDS = nrow(paired),
  AGREEMENT_RATE = observed_agreement,
  COHENS_KAPPA = (observed_agreement - expected_agreement) / (1 - expected_agreement)
)
write.csv(agreement_summary, file.path(output_dir, "paired_agreement_summary.csv"), row.names = FALSE)

confusion_plot <- ggplot(confusion, aes(x = SENTIMENT_JEV, y = SENTIMENT_QWEN, fill = PERCENT_OF_QWEN_ROW)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%s\n%.1f%%", comma(N), PERCENT_OF_QWEN_ROW)), size = 3.5) +
  scale_fill_viridis_c(option = "C") +
  labs(
    x = "Jev label",
    y = "Qwen label",
    fill = "Qwen row share",
    title = "Paired Qwen–Jev confusion matrix",
    subtitle = sprintf("n=%s paired records; agreement=%.1f%%; Cohen's kappa=%.3f", comma(nrow(paired)), 100 * observed_agreement, agreement_summary$COHENS_KAPPA),
    caption = "@StockViz"
  ) +
  base_theme +
  theme(panel.grid = element_blank(), legend.position = "none")
ggsave(file.path(output_dir, "05_paired_confusion_matrix.png"), confusion_plot, width = 8, height = 7, units = "in", dpi = 160)

paired_percent <- function(group_name) {
  result <- paired_long %>%
    group_by(.data[[group_name]], MODEL, SENTIMENT) %>%
    summarise(N = n(), .groups = "drop") %>%
    group_by(.data[[group_name]], MODEL) %>%
    mutate(PERCENT = 100 * N / sum(N)) %>%
    ungroup()

  qwen_part <- result %>% filter(MODEL == "QWEN") %>% select(all_of(group_name), SENTIMENT, QWEN_PERCENT = PERCENT)
  jev_part <- result %>% filter(MODEL == "JEV") %>% select(all_of(group_name), SENTIMENT, JEV_PERCENT = PERCENT)
  qwen_part %>%
    full_join(jev_part, by = c(group_name, "SENTIMENT")) %>%
    mutate(
      QWEN_PERCENT = replace_na(QWEN_PERCENT, 0),
      JEV_PERCENT = replace_na(JEV_PERCENT, 0),
      DIFFERENCE = QWEN_PERCENT - JEV_PERCENT
    )
}

# 6. Qwen-minus-Jev percentage differences by source type and source.
type_difference <- paired_percent("SOURCE_TYPE")
source_difference <- paired_percent("SOURCE")
write.csv(type_difference, file.path(output_dir, "qwen_minus_jev_by_source_type.csv"), row.names = FALSE)
write.csv(source_difference, file.path(output_dir, "qwen_minus_jev_by_source.csv"), row.names = FALSE)

plot_difference <- function(plot_data, x, title, subtitle, flip = FALSE) {
  chart <- ggplot(plot_data, aes(x = .data[[x]], y = DIFFERENCE, fill = SENTIMENT)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    geom_hline(yintercept = 0, colour = "grey35") +
    scale_fill_manual(values = sentiment_colors, drop = FALSE) +
    scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0.05, 0.12))) +
    labs(x = NULL, y = "Qwen share minus Jev share", title = title, subtitle = subtitle, fill = "Sentiment", caption = "@StockViz") +
    base_theme
  if (flip) chart <- chart + coord_flip()
  chart
}

ggsave(
  file.path(output_dir, "06_qwen_minus_jev_by_source_type.png"),
  plot_difference(type_difference, "SOURCE_TYPE", "Qwen minus Jev sentiment share by source type", "Paired records only"),
  width = 10, height = 6, units = "in", dpi = 160
)
ggsave(
  file.path(output_dir, "07_qwen_minus_jev_by_source.png"),
  plot_difference(source_difference, "SOURCE", "Qwen minus Jev sentiment share by source", "Paired records only; positive values mean Qwen assigns a larger share", TRUE),
  width = 12, height = max(10, length(unique(source_difference$SOURCE)) * 0.7), units = "in", dpi = 160
)

# 7. Daily sentiment mix, rolling sentiment, and daily article volume.
daily_full <- data %>%
  mutate(DATE = as.Date(DATE_STAMP)) %>%
  group_by(MODEL, DATE) %>%
  summarise(
    ARTICLES = n(),
    POSITIVE_PERCENT = 100 * mean(SENTIMENT == "POSITIVE"),
    NEUTRAL_PERCENT = 100 * mean(SENTIMENT == "NEUTRAL"),
    NEGATIVE_PERCENT = 100 * mean(SENTIMENT == "NEGATIVE"),
    .groups = "drop"
  )

daily_long <- daily_full %>%
  select(MODEL, DATE, ARTICLES, ends_with("_PERCENT")) %>%
  pivot_longer(ends_with("_PERCENT"), names_to = "SENTIMENT", values_to = "PERCENT") %>%
  mutate(SENTIMENT = sub("_PERCENT$", "", SENTIMENT)) %>%
  group_by(MODEL, SENTIMENT) %>%
  arrange(DATE) %>%
  mutate(ROLLING_20 = as.numeric(stats::filter(PERCENT, rep(1 / 20, 20), sides = 1))) %>%
  ungroup()

# Start the long time-series charts once the archive first reaches meaningful
# daily coverage. The threshold marks the start date only; all later dates are
# retained, including dates with fewer records. It is applied separately to
# the all-model and paired series.
MIN_DAILY_ARTICLES <- 50L
chart08_start_date <- daily_full %>%
  group_by(DATE) %>%
  summarise(TOTAL_ARTICLES = sum(ARTICLES), .groups = "drop") %>%
  filter(TOTAL_ARTICLES >= MIN_DAILY_ARTICLES) %>%
  summarise(START_DATE = min(DATE)) %>%
  pull(START_DATE)
daily_long_chart08 <- daily_long %>% filter(DATE >= chart08_start_date)
daily_full_chart08 <- daily_full %>% filter(DATE >= chart08_start_date)
write.csv(daily_long_chart08, file.path(output_dir, "daily_sentiment_series.csv"), row.names = FALSE)

daily_sentiment_plot <- ggplot(daily_long_chart08, aes(x = DATE, colour = SENTIMENT)) +
  geom_line(aes(y = PERCENT), alpha = 0.18, linewidth = 0.35) +
  geom_line(aes(y = ROLLING_20), linewidth = 0.9, na.rm = TRUE) +
  facet_wrap(~MODEL, ncol = 1) +
  scale_colour_manual(values = sentiment_colors, drop = FALSE) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 105)) +
  labs(
    x = NULL, y = "Sentiment share", colour = "Sentiment",
    title = "Daily sentiment mix",
    subtitle = sprintf("From %s; start marked by first date with %s combined records; later dates retained; bold lines are 20-observation rolling averages", chart08_start_date, comma(MIN_DAILY_ARTICLES)),
    caption = "@StockViz"
  ) +
  base_theme
volume_plot <- ggplot(daily_full_chart08, aes(x = DATE, y = ARTICLES, colour = MODEL)) +
  geom_col(alpha = 0.75, width = 1) +
  scale_colour_manual(values = model_colors) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Articles per day", colour = "Model", title = "Daily article volume", caption = "@StockViz") +
  base_theme
ggsave(file.path(output_dir, "08_daily_sentiment_and_volume.png"), daily_sentiment_plot / volume_plot, width = 13, height = 11, units = "in", dpi = 160)

# 8. Daily disagreement rate and paired volume.
daily_disagreement <- paired %>%
  mutate(DATE = as.Date(DATE_STAMP), DISAGREE = SENTIMENT_QWEN != SENTIMENT_JEV) %>%
  group_by(DATE) %>%
  summarise(DISAGREEMENT_PERCENT = 100 * mean(DISAGREE), PAIRED_ARTICLES = n(), .groups = "drop")
chart09_start_date <- daily_disagreement %>%
  filter(PAIRED_ARTICLES >= MIN_DAILY_ARTICLES) %>%
  summarise(START_DATE = min(DATE)) %>%
  pull(START_DATE)
daily_disagreement_chart09 <- daily_disagreement %>% filter(DATE >= chart09_start_date)
write.csv(daily_disagreement_chart09, file.path(output_dir, "daily_model_disagreement.csv"), row.names = FALSE)
disagreement_plot <- ggplot(daily_disagreement_chart09, aes(DATE, DISAGREEMENT_PERCENT)) +
  geom_line(colour = viridis(1, option = "D"), alpha = 0.35) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = viridis(1, option = "D"), linewidth = 0.9, na.rm = TRUE) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 105)) +
  labs(x = NULL, y = "Disagreement", title = "Daily Qwen–Jev disagreement", subtitle = sprintf("From %s; start marked by first date with %s paired records; later dates retained", chart09_start_date, comma(MIN_DAILY_ARTICLES)), caption = "@StockViz") +
  base_theme
paired_volume_plot <- ggplot(daily_disagreement_chart09, aes(DATE, PAIRED_ARTICLES)) +
  geom_col(fill = viridis(1, option = "D"), alpha = 0.75, width = 1) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Paired articles", title = "Daily paired coverage", caption = "@StockViz") +
  base_theme
ggsave(file.path(output_dir, "09_daily_disagreement_and_volume.png"), disagreement_plot / paired_volume_plot, width = 13, height = 9, units = "in", dpi = 160)

# 9. Source heatmaps for Qwen and Jev.
source_heatmap_data <- paired_long %>%
  count(MODEL, SOURCE, SENTIMENT, name = "N") %>%
  group_by(MODEL, SOURCE) %>%
  mutate(PERCENT = 100 * N / sum(N)) %>%
  ungroup()
write.csv(source_heatmap_data, file.path(output_dir, "paired_source_sentiment_heatmap.csv"), row.names = FALSE)
source_heatmap <- ggplot(source_heatmap_data, aes(SENTIMENT, reorder(SOURCE, PERCENT, FUN = sum), fill = PERCENT)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.1f%%", PERCENT)), size = 3) +
  facet_wrap(~MODEL, ncol = 1) +
  scale_fill_viridis_c(option = "C", limits = c(0, 100)) +
  labs(x = NULL, y = NULL, fill = "Share", title = "Sentiment share by source", subtitle = "Paired records only", caption = "@StockViz") +
  base_theme +
  theme(panel.grid = element_blank())
ggsave(file.path(output_dir, "10_source_sentiment_heatmap.png"), source_heatmap, width = 10, height = 9, units = "in", dpi = 160)

# 10. Coverage by source and entropy of each source's label distribution.
coverage <- paired_long %>% count(MODEL, SOURCE, name = "PAIRED_ARTICLES")
write.csv(coverage, file.path(output_dir, "paired_coverage_by_source.csv"), row.names = FALSE)
coverage_plot <- ggplot(coverage, aes(reorder(SOURCE, PAIRED_ARTICLES), PAIRED_ARTICLES, fill = MODEL)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Paired articles", fill = "Model", title = "Paired coverage by source", caption = "@StockViz") +
  base_theme
ggsave(file.path(output_dir, "11_paired_coverage_by_source.png"), coverage_plot, width = 11, height = 8, units = "in", dpi = 160)

entropy <- source_heatmap_data %>%
  group_by(MODEL, SOURCE) %>%
  summarise(ENTROPY_BITS = -sum(ifelse(PERCENT > 0, PERCENT / 100 * log2(PERCENT / 100), 0)), .groups = "drop")
write.csv(entropy, file.path(output_dir, "sentiment_entropy_by_source.csv"), row.names = FALSE)
entropy_plot <- ggplot(entropy, aes(reorder(SOURCE, ENTROPY_BITS), ENTROPY_BITS, fill = MODEL)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = model_colors) +
  labs(x = NULL, y = "Entropy (bits)", fill = "Model", title = "Sentiment entropy by source", subtitle = "Higher values indicate a more balanced label distribution", caption = "@StockViz") +
  base_theme
ggsave(file.path(output_dir, "12_sentiment_entropy_by_source.png"), entropy_plot, width = 11, height = 8, units = "in", dpi = 160)

# 11. Sentiment share versus daily article volume.
volume_sentiment_plot <- ggplot(daily_long, aes(ARTICLES, PERCENT, colour = SENTIMENT)) +
  geom_point(alpha = 0.3, size = 1.1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.7, na.rm = TRUE) +
  facet_wrap(~MODEL, ncol = 1) +
  scale_colour_manual(values = sentiment_colors, drop = FALSE) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 105)) +
  labs(x = "Articles published on date", y = "Sentiment share", colour = "Sentiment", title = "Sentiment share versus daily article volume", caption = "@StockViz") +
  base_theme
ggsave(file.path(output_dir, "13_sentiment_vs_daily_volume.png"), volume_sentiment_plot, width = 11, height = 9, units = "in", dpi = 160)

# 12. Disagreement categories by source type.
disagreement_categories <- paired %>%
  mutate(
    CATEGORY = ifelse(
      SENTIMENT_QWEN == SENTIMENT_JEV,
      "Agree",
      paste("Qwen", SENTIMENT_QWEN, "vs Jev", SENTIMENT_JEV)
    )
  ) %>%
  count(SOURCE_TYPE, CATEGORY, name = "N") %>%
  group_by(SOURCE_TYPE) %>%
  mutate(PERCENT = 100 * N / sum(N)) %>%
  ungroup()
write.csv(disagreement_categories, file.path(output_dir, "disagreement_categories_by_source_type.csv"), row.names = FALSE)
disagreement_category_plot <- ggplot(disagreement_categories, aes(SOURCE_TYPE, PERCENT, fill = CATEGORY)) +
  geom_col() +
  scale_fill_viridis_d(option = "D") +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 105)) +
  labs(x = NULL, y = "Share of paired records", fill = "Outcome", title = "How Qwen and Jev disagree", subtitle = "Agreement and directional/neutral disagreements", caption = "@StockViz") +
  base_theme
ggsave(file.path(output_dir, "14_disagreement_categories.png"), disagreement_category_plot, width = 12, height = 7, units = "in", dpi = 160)

# 13. Market-return diagnostics: return buckets, lagged coefficients, and events.
if (!is.null(index_returns)) {
  index_forward <- index_returns %>%
    group_by(INDEX) %>%
    arrange(DATE) %>%
    mutate(
      FWD_0 = RETURN,
      FWD_1 = lead(RETURN, 1),
      FWD_2 = (1 + lead(RETURN, 1)) * (1 + lead(RETURN, 2)) - 1,
      FWD_5 = (1 + lead(RETURN, 1)) * (1 + lead(RETURN, 2)) * (1 + lead(RETURN, 3)) * (1 + lead(RETURN, 4)) * (1 + lead(RETURN, 5)) - 1
    ) %>%
    ungroup()

  market_daily <- daily_full %>%
    inner_join(index_forward, by = "DATE", relationship = "many-to-many")

  bucket_data <- market_daily %>%
    group_by(INDEX) %>%
    mutate(
      RETURN_BUCKET = factor(ntile(RETURN, 5), levels = 1:5, labels = c("Bottom 20%", "20–40%", "40–60%", "60–80%", "Top 20%"))
    ) %>%
    ungroup() %>%
    group_by(MODEL, INDEX, RETURN_BUCKET) %>%
    summarise(POSITIVE_PERCENT = mean(POSITIVE_PERCENT), N = n(), .groups = "drop")
  write.csv(bucket_data, file.path(output_dir, "positive_sentiment_by_return_bucket.csv"), row.names = FALSE)
  bucket_plot <- ggplot(bucket_data, aes(RETURN_BUCKET, POSITIVE_PERCENT, colour = MODEL, group = MODEL)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    facet_wrap(~INDEX, nrow = 1) +
    scale_colour_manual(values = model_colors) +
    scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 105)) +
    labs(x = "Daily return quintile", y = "Mean positive sentiment", colour = "Model", title = "Positive sentiment by daily return bucket", subtitle = "Same-date return quintiles", caption = "@StockViz") +
    base_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(file.path(output_dir, "15_positive_sentiment_by_return_bucket.png"), bucket_plot, width = 15, height = 6, units = "in", dpi = 160)

  lag_long <- market_daily %>%
    select(MODEL, DATE, INDEX, POSITIVE_PERCENT, FWD_0, FWD_1, FWD_2, FWD_5) %>%
    pivot_longer(starts_with("FWD_"), names_to = "HORIZON", values_to = "FORWARD_RETURN") %>%
    mutate(HORIZON = factor(HORIZON, levels = c("FWD_0", "FWD_1", "FWD_2", "FWD_5"), labels = c("Same day", "Next day", "2 days", "5 days"))) %>%
    filter(!is.na(FORWARD_RETURN))

  lag_stats <- lag_long %>%
    group_by(MODEL, INDEX, HORIZON) %>%
    group_modify(~ {
      fit <- lm(FORWARD_RETURN ~ POSITIVE_PERCENT, data = .x)
      coefficient <- summary(fit)$coefficients["POSITIVE_PERCENT", ]
      interval <- confint(fit, "POSITIVE_PERCENT")
      tibble(
        N = nrow(.x),
        SLOPE_BPS_PER_PP = 10000 * coefficient[["Estimate"]],
        LOW_BPS_PER_PP = 10000 * interval[1],
        HIGH_BPS_PER_PP = 10000 * interval[2],
        R_SQUARED = summary(fit)$r.squared
      )
    }) %>%
    ungroup()
  write.csv(lag_stats, file.path(output_dir, "lagged_sentiment_return_coefficients.csv"), row.names = FALSE)
  lag_plot <- ggplot(lag_stats, aes(HORIZON, SLOPE_BPS_PER_PP, colour = MODEL)) +
    geom_hline(yintercept = 0, colour = "grey45") +
    geom_pointrange(aes(ymin = LOW_BPS_PER_PP, ymax = HIGH_BPS_PER_PP), position = position_dodge(width = 0.35)) +
    facet_wrap(~INDEX, nrow = 1) +
    scale_colour_manual(values = model_colors) +
    labs(x = NULL, y = "Slope (basis points per +1 pp sentiment)", colour = "Model", title = "Lagged sentiment–return regression", subtitle = "Points are OLS slopes; bars are 95% confidence intervals", caption = "@StockViz") +
    base_theme +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(file.path(output_dir, "16_lagged_sentiment_return_coefficients.png"), lag_plot, width = 15, height = 6, units = "in", dpi = 160)

  # Use daily class shares directly so the event definition remains explicit.
  event_base <- data %>%
    mutate(DATE = as.Date(DATE_STAMP)) %>%
    group_by(MODEL, DATE) %>%
    summarise(
      ARTICLES = n(),
      POSITIVE_PERCENT = 100 * mean(SENTIMENT == "POSITIVE"),
      NEGATIVE_PERCENT = 100 * mean(SENTIMENT == "NEGATIVE"),
      .groups = "drop"
    ) %>%
    group_by(MODEL) %>%
    mutate(
      POSITIVE_CUTOFF = quantile(POSITIVE_PERCENT, 0.90, na.rm = TRUE),
      NEGATIVE_CUTOFF = quantile(NEGATIVE_PERCENT, 0.90, na.rm = TRUE)
    ) %>%
    ungroup()
  events <- bind_rows(
    event_base %>% filter(POSITIVE_PERCENT >= POSITIVE_CUTOFF) %>% transmute(MODEL, EVENT_DATE = DATE, EVENT = "High positive"),
    event_base %>% filter(NEGATIVE_PERCENT >= NEGATIVE_CUTOFF) %>% transmute(MODEL, EVENT_DATE = DATE, EVENT = "High negative")
  ) %>% distinct()

  make_event_window <- function(index_data, event_row) {
    index_data <- index_data %>% arrange(DATE)
    event_position <- match(event_row$EVENT_DATE, index_data$DATE)
    if (is.na(event_position) || event_position <= 5 || event_position + 10 > nrow(index_data)) return(NULL)
    positions <- (event_position - 5):(event_position + 10)
    window <- index_data[positions, ]
    window$RELATIVE_DAY <- -5:10
    baseline <- cumprod(1 + window$RETURN)[window$RELATIVE_DAY == -1]
    window$CUM_RETURN = 100 * (cumprod(1 + window$RETURN) / baseline - 1)
    window %>% transmute(INDEX, RELATIVE_DAY, CUM_RETURN)
  }
  event_windows <- bind_rows(lapply(seq_len(nrow(events)), function(i) {
    event <- events[i, ]
    bind_rows(lapply(split(index_returns, index_returns$INDEX), make_event_window, event_row = event)) %>%
      mutate(MODEL = event$MODEL, EVENT = event$EVENT, EVENT_DATE = event$EVENT_DATE)
  }))
  if (nrow(event_windows) > 0) {
    event_summary <- event_windows %>%
      group_by(MODEL, INDEX, EVENT, RELATIVE_DAY) %>%
      summarise(MEAN_RETURN = mean(CUM_RETURN), SE = sd(CUM_RETURN) / sqrt(n()), .groups = "drop")
    write.csv(event_summary, file.path(output_dir, "sentiment_event_study.csv"), row.names = FALSE)
    event_colors <- setNames(viridis(2, option = "D"), c("High positive", "High negative"))
    event_plot <- ggplot(event_summary, aes(RELATIVE_DAY, MEAN_RETURN, colour = EVENT, fill = EVENT)) +
      geom_hline(yintercept = 0, colour = "grey45") +
      geom_ribbon(aes(ymin = MEAN_RETURN - 1.96 * SE, ymax = MEAN_RETURN + 1.96 * SE), alpha = 0.15, colour = NA) +
      geom_line(linewidth = 0.9) +
      facet_grid(MODEL ~ INDEX) +
      scale_colour_manual(values = event_colors) +
      scale_fill_manual(values = event_colors) +
      labs(x = "Trading days relative to event", y = "Cumulative return (%)", colour = "Event", fill = "Event", title = "Index returns around extreme sentiment days", subtitle = "Events are model-specific top-decile positive or negative days; bands are 95% intervals", caption = "@StockViz") +
      base_theme
    ggsave(file.path(output_dir, "17_sentiment_event_study.png"), event_plot, width = 15, height = 8, units = "in", dpi = 160)
  }
}

message(sprintf("Wrote charts and CSV summaries to %s", output_dir))
