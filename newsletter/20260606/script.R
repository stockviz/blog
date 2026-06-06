library('tidyverse')
library('ggthemes')
library('viridis')
library('patchwork')
library('scales')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
df <- read_csv("dates_with_funds_and_aum.csv", show_col_types = FALSE,
                col_types = cols(
                  BaseDate = col_date("%Y-%m-%d"),
                  LaunchDate = col_date("%Y-%m-%d"),
                  FundStartDate = col_character(),
                  TotalAUM_Cr = col_double(),
                  IndexSchemeCount = col_integer(),
                  .default = col_character()
                )) |>
  mutate(
    FundStartDate = as.Date(if_else(FundStartDate == "" | FundStartDate == "0", NA_character_, FundStartDate)),
    TotalAUM_Cr = as.numeric(TotalAUM_Cr),
    IndexSchemeCount = as.integer(IndexSchemeCount)
  )

# Parse index category from URL
df <- df |>
  mutate(
    Category = case_when(
      str_detect(URL, "/broad-based-indices/") ~ "Broad-Based",
      str_detect(URL, "/sectoral-indices/")    ~ "Sectoral",
      str_detect(URL, "/thematic-indices/")    ~ "Thematic",
      str_detect(URL, "/strategy-indices/")    ~ "Strategy",
      str_detect(URL, "/strategic-indices/")   ~ "Strategy",
      TRUE ~ "Other"
    )
  )

# Filter to indices with AUM data
aum_df <- df |> filter(TotalAUM_Cr > 0)

# ---------------------------------------------------------------------------
# Chart 1: Top 20 indices by AUM
# ---------------------------------------------------------------------------
top20 <- aum_df |>
  slice_max(TotalAUM_Cr, n = 20) |>
  mutate(IndexName = fct_reorder(IndexName, TotalAUM_Cr))

ggplot(top20, aes(x = TotalAUM_Cr, y = IndexName, fill = Category)) +
  theme_economist() +
  theme(axis.text.y = element_text(size = 7)) +
  geom_col() +
  scale_fill_viridis_d() +
  scale_x_continuous(labels = comma_format()) +
  labs(
    x = "Total AUM (₹ Cr)", y = "",
    title = "Top 20 Nifty Indices by Total Index Fund AUM",
    subtitle = sprintf("As of %s | %d indices with AUM data",
                       first(aum_df$AUM_Period), nrow(aum_df)),
    caption = "@StockViz"
  )

ggsave(sprintf("%s/index-aum-top20.png", reportPath), width = 12, height = 8, units = "in")

# Aggregate AUM by category (used by multiple charts)
cat_aum <- aum_df |>
  group_by(Category) |>
  summarise(
    TotalAUM = sum(TotalAUM_Cr),
    IndexCount = n(),
    .groups = "drop"
  ) |>
  mutate(Pct = TotalAUM / sum(TotalAUM))

# Consistent ordering by TotalAUM for all category charts
cat_aum <- cat_aum |> mutate(Category = fct_reorder(Category, TotalAUM))

# ---------------------------------------------------------------------------
# Chart 3: Count of AUM-bearing indices by category
# ---------------------------------------------------------------------------
ggplot(cat_aum, aes(x = Category, y = IndexCount, fill = Category)) +
  theme_economist() +
  geom_col() +
  scale_fill_viridis_d() +
  guides(fill = "none") +
  labs(
    x = "", y = "Number of Indices",
    title = "Indices with Index Fund AUM by Category",
    subtitle = sprintf("Out of %d total Nifty indices", nrow(df)),
    caption = "@StockViz"
  )

ggsave(sprintf("%s/index-count-by-category.png", reportPath), width = 8, height = 6, units = "in")

# ---------------------------------------------------------------------------
# Chart 4: Launch Date vs AUM
# ---------------------------------------------------------------------------
ld_df <- aum_df |>
  filter(!is.na(LaunchDate), LaunchDate > as.Date("1990-01-01")) |>
  mutate(Age_Years = as.numeric(Sys.Date() - LaunchDate) / 365.25)

ggplot(ld_df, aes(x = LaunchDate, y = TotalAUM_Cr, size = IndexSchemeCount, color = Category)) +
  theme_economist() +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d() +
  scale_y_log10(labels = comma_format()) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_size_continuous(range = c(2, 8)) +
  guides(color = guide_legend(nrow = 1, order = 1), size = guide_legend(nrow = 1, order = 2)) +
  theme(legend.box = "vertical") +
  labs(
    x = "Launch Date", y = "Total AUM (₹ Cr, log scale)",
    size = "Schemes",
    title = "Index Launch Date vs Current AUM",
    subtitle = "Older indices tend to have more AUM, but not always",
    caption = "@StockViz"
  )

ggsave(sprintf("%s/index-launch-vs-aum.png", reportPath), width = 12, height = 7, units = "in")

# ---------------------------------------------------------------------------
# Chart 5: Scheme count distribution
# ---------------------------------------------------------------------------
ggplot(aum_df, aes(x = IndexSchemeCount, fill = Category)) +
  theme_economist() +
  geom_histogram(binwidth = 2, color = "white") +
  scale_fill_viridis_d() +
  labs(
    x = "Number of Index Fund Schemes", y = "Count of Indices",
    title = "How Many Schemes Track Each Index?",
    subtitle = sprintf("Mean: %.1f | Median: %d schemes per index",
                       mean(aum_df$IndexSchemeCount), median(aum_df$IndexSchemeCount)),
    caption = "@StockViz"
  )

ggsave(sprintf("%s/index-scheme-count-dist.png", reportPath), width = 10, height = 6, units = "in")

# ---------------------------------------------------------------------------
# Chart 6: Dashboard summary (patchwork)
# ---------------------------------------------------------------------------
p1 <- ggplot(cat_aum, aes(x = Category, y = TotalAUM, fill = Category)) +
  theme_economist() +
  theme(legend.position = "none") +
  geom_col() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = comma_format()) +
  labs(x = "", y = "AUM (₹ Cr)", title = "AUM by Category")

p2 <- ggplot(aum_df, aes(x = TotalAUM_Cr)) +
  theme_economist() +
  geom_histogram(bins = 30, fill = viridis_pal()(1), color = "white") +
  scale_x_log10(labels = comma_format()) +
  labs(x = "Total AUM (₹ Cr, log)", y = "Count", title = "AUM Distribution")

p3 <- aum_df |>
  count(Category) |>
  mutate(Category = factor(Category, levels = levels(cat_aum$Category))) |>
  ggplot(aes(x = Category, y = n, fill = Category)) +
  theme_economist() +
  theme(legend.position = "none") +
  geom_col() +
  scale_fill_viridis_d() +
  labs(x = "", y = "Count", title = "Indices by Category")

p4 <- aum_df |>
  filter(!is.na(LaunchDate), LaunchDate > as.Date("1990-01-01")) |>
  mutate(LaunchYear = year(LaunchDate)) |>
  count(LaunchYear) |>
  ggplot(aes(x = LaunchYear, y = n)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_col(fill = viridis_pal()(1)) +
  scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 2)) +
  labs(x = "", y = "Count", title = "Indices Launched per Year")

(p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Nifty Index Fund AUM Dashboard",
    subtitle = sprintf("Data as of %s | %d indices with AUM across %d categories",
                       first(aum_df$AUM_Period), nrow(aum_df), n_distinct(aum_df$Category)),
    caption = "@StockViz",
    theme = theme_economist()
  )

ggsave(sprintf("%s/index-aum-dashboard.png", reportPath), width = 14, height = 10, units = "in")

# ---------------------------------------------------------------------------
# Summary stats to stdout
# ---------------------------------------------------------------------------
cat(sprintf("\nTotal Nifty indices: %d\n", nrow(df)))
cat(sprintf("Indices with AUM:    %d\n", nrow(aum_df)))
cat(sprintf("Total AUM:           ₹%s Cr\n", comma(round(sum(aum_df$TotalAUM_Cr), 0))))
cat(sprintf("AUM Period:          %s\n", first(aum_df$AUM_Period)))
cat(sprintf("Mean schemes/index:  %.1f\n", mean(aum_df$IndexSchemeCount)))
cat(sprintf("\nBy Category:\n"))
cat_aum |>
  arrange(desc(TotalAUM)) |>
  mutate(Line = sprintf("  %-15s ₹%s Cr  (%d indices, %.1f%%)",
                        Category, comma(round(TotalAUM, 0)), IndexCount, Pct * 100)) |>
  pull(Line) |>
  cat(sep = "\n")
cat("\n")
