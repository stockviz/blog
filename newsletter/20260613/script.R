#!/usr/bin/env Rscript
#
# AMFI Monthly Fund Flows — Large Cap, Mid Cap, Small Cap,
#   Sectoral/Thematic, and Gold ETF
#
# Data: StockViz.dbo.AMFI_MONTHLY_STATS
# Output: /mnt/data/blog/newsletter/20260613/

library('RODBC')
library('tidyverse')
library('xts')
library('ggthemes')
library('viridis')
library('patchwork')
library('scales')
library('zoo')
library('ggrepel')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "/mnt/data/blog/newsletter/20260613"
source("/mnt/hollandC/StockViz/R/config.r")

# ---------------------------------------------------------------------------
# Database
# ---------------------------------------------------------------------------

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

categories <- c('Large Cap Fund', 'Mid Cap Fund', 'Small Cap Fund',
                'Flexi Cap Fund', 'Sectoral/Thematic Funds', 'GOLD ETF')

cats_csv <- paste(sprintf("'%s'", categories), collapse = ", ")

sql <- sprintf("
  SELECT PERIOD, SCHEME_CATEGORY,
         FUNDS_MOBILIZED, REPURCHASES, NET_FLOW, AUM
  FROM AMFI_MONTHLY_STATS
  WHERE SCHEME_CATEGORY IN (%s)
    AND SECTION = 'A'
  ORDER BY SCHEME_CATEGORY, PERIOD
", cats_csv)

df <- sqlQuery(lcon, sql)

if (!is.data.frame(df)) {
  stop("Query failed: ", df[1])
}

odbcClose(lcon)

# ---------------------------------------------------------------------------
# Wrangle
# ---------------------------------------------------------------------------

df <- df |>
  mutate(
    PERIOD = as.Date(PERIOD),
    SCHEME_CATEGORY = factor(SCHEME_CATEGORY, levels = categories),
    FUNDS_MOBILIZED = as.numeric(FUNDS_MOBILIZED),
    REPURCHASES = as.numeric(REPURCHASES),
    NET_FLOW = as.numeric(NET_FLOW),
    AUM = as.numeric(AUM)
  ) |>
  arrange(SCHEME_CATEGORY, PERIOD) |>
  group_by(SCHEME_CATEGORY) |>
  mutate(
    CUM_NET_FLOW = cumsum(NET_FLOW),
    ROLL12_NET = rollapply(NET_FLOW, 12, sum, fill = NA, align = "right")
  ) |>
  ungroup()

dateRange <- range(df$PERIOD)
dateLabel <- sprintf("%s:%s", format(dateRange[1], "%b %Y"), format(dateRange[2], "%b %Y"))

# Short labels with preserved order
short_labels <- c(
  'Large Cap Fund' = 'Large Cap',
  'Mid Cap Fund' = 'Mid Cap',
  'Small Cap Fund' = 'Small Cap',
  'Flexi Cap Fund' = 'Flexi Cap',
  'Sectoral/Thematic Funds' = 'Sectoral/Thematic',
  'GOLD ETF' = 'Gold ETF'
)
short_label_order <- unname(short_labels[categories])

df <- df |>
  mutate(cat_label = factor(
    short_labels[as.character(SCHEME_CATEGORY)],
    levels = short_label_order
  ))

# ---------------------------------------------------------------------------
# Chart 1: Monthly Net Flow — faceted
# ---------------------------------------------------------------------------

p1 <- df |>
  ggplot(aes(x = PERIOD, y = NET_FLOW, fill = cat_label)) +
  geom_area(alpha = 0.35) +
  geom_line(aes(color = cat_label), linewidth = 0.5) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  facet_wrap(~ cat_label, scales = "free_y", ncol = 2) +
  scale_fill_viridis_d(guide = "none") +
  scale_color_viridis_d(guide = "none") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  theme_economist() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  labs(
    x = '', y = 'Net Flow (₹ Crore)',
    title = "Monthly Net Fund Flows by Category",
    subtitle = dateLabel,
    caption = '@StockViz'
  )

ggsave(file.path(reportPath, "flow-net-monthly.png"), p1,
       width = 12, height = 10, units = "in")

# ---------------------------------------------------------------------------
# Chart 2: Cumulative Net Flow
# ---------------------------------------------------------------------------

p2 <- df |>
  ggplot(aes(x = PERIOD, y = CUM_NET_FLOW, color = cat_label)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50", linetype = "dashed") +
  geom_text_repel(
    data = df |> filter(PERIOD == max(PERIOD)),
    aes(label = cat_label),
    direction = "y", nudge_x = 30, hjust = 0,
    size = 3.5, fontface = "bold", segment.color = NA
  ) +
  scale_color_viridis_d(guide = "none") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
               expand = expansion(mult = c(0.02, 0.25))) +
  coord_cartesian(clip = "off") +
  theme_economist() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
    plot.margin = margin(5, 70, 5, 5)
  ) +
  labs(
    x = '', y = 'Cumulative Net Flow (₹ Crore)',
    title = "Cumulative Net Fund Flows",
    subtitle = dateLabel,
    caption = '@StockViz'
  )

ggsave(file.path(reportPath, "flow-net-cumulative.png"), p2,
       width = 12, height = 7, units = "in")

# ---------------------------------------------------------------------------
# Chart 3: Rolling 12-month Net Flow
# ---------------------------------------------------------------------------

p3 <- df |>
  filter(!is.na(ROLL12_NET)) |>
  ggplot(aes(x = PERIOD, y = ROLL12_NET, color = cat_label)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50", linetype = "dashed") +
  geom_text_repel(
    data = df |> filter(PERIOD == max(PERIOD) & !is.na(ROLL12_NET)),
    aes(label = cat_label),
    direction = "y", nudge_x = 30, hjust = 0,
    size = 3.5, fontface = "bold", segment.color = NA
  ) +
  scale_color_viridis_d(guide = "none") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
               expand = expansion(mult = c(0.02, 0.25))) +
  coord_cartesian(clip = "off") +
  theme_economist() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
    plot.margin = margin(5, 70, 5, 5)
  ) +
  labs(
    x = '', y = 'Net Flow (₹ Crore)',
    title = "Rolling 12-Month Net Fund Flows",
    subtitle = dateLabel,
    caption = '@StockViz'
  )

ggsave(file.path(reportPath, "flow-net-rolling12.png"), p3,
       width = 12, height = 7, units = "in")

# ---------------------------------------------------------------------------
# Chart 4: Inflow vs Outflow — stacked bar (latest 24 months)
# ---------------------------------------------------------------------------

df_bar <- df |>
  filter(PERIOD >= max(PERIOD) - 365*2) |>
  select(PERIOD, cat_label, FUNDS_MOBILIZED, REPURCHASES) |>
  pivot_longer(cols = c(FUNDS_MOBILIZED, REPURCHASES),
               names_to = "flow_type", values_to = "amount") |>
  mutate(flow_type = ifelse(flow_type == "FUNDS_MOBILIZED", "Inflow", "Outflow"))

p4 <- df_bar |>
  ggplot(aes(x = PERIOD, y = amount, fill = flow_type)) +
  geom_col(position = "dodge", alpha = 0.85) +
  facet_wrap(~ cat_label, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Inflow" = "#2ecc71", "Outflow" = "#e74c3c")) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  theme_economist() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    x = '', y = 'Amount (₹ Crore)',
    title = "Monthly Inflows vs Outflows (Last 24 Months)",
    subtitle = dateLabel,
    caption = '@StockViz'
  )

ggsave(file.path(reportPath, "flow-in-out-monthly.png"), p4,
       width = 12, height = 10, units = "in")

# ---------------------------------------------------------------------------
# Chart 5: Net Flow as % of AUM
# ---------------------------------------------------------------------------

p5 <- df |>
  mutate(flow_pct = (NET_FLOW / AUM) * 100) |>
  ggplot(aes(x = PERIOD, y = flow_pct, fill = cat_label)) +
  geom_area(alpha = 0.35) +
  geom_line(aes(color = cat_label), linewidth = 0.5) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  facet_wrap(~ cat_label, scales = "free_y", ncol = 2) +
  scale_fill_viridis_d(guide = "none") +
  scale_color_viridis_d(guide = "none") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  theme_economist() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  labs(
    x = '', y = 'Net Flow (% of AUM)',
    title = "Monthly Net Flow as Percentage of AUM",
    subtitle = dateLabel,
    caption = '@StockViz'
  )

ggsave(file.path(reportPath, "flow-net-pct.png"), p5,
       width = 12, height = 10, units = "in")

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

cat("=== Fund Flow Charts Generated ===\n")
cat(sprintf("Period: %s to %s\n", dateRange[1], dateRange[2]))
cat(sprintf("Categories: %s\n", paste(categories, collapse = ", ")))
cat(sprintf("Total rows: %d\n", nrow(df)))
cat("\nOutput files:\n")
for (f in list.files(reportPath, pattern = "flow-.*\\.png$")) {
  cat(sprintf("  %s\n", f))
}
