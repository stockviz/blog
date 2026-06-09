#!/usr/bin/env Rscript
#
# Gold ETF Flows
#
# Data: StockViz.dbo.AMFI_MONTHLY_STATS
# Output: /mnt/data/blog/newsletter/20260613/
#

library('RODBC')
library('tidyverse')
library('ggthemes')
library('viridis')
library('scales')
library('zoo')

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

gold_flows <- sqlQuery(lcon, "
  SELECT PERIOD, FUNDS_MOBILIZED, REPURCHASES, NET_FLOW, AUM
  FROM AMFI_MONTHLY_STATS
  WHERE SCHEME_CATEGORY = 'GOLD ETF'
    AND SECTION = 'A'
  ORDER BY PERIOD
")

if (!is.data.frame(gold_flows)) stop("AMFI_MONTHLY_STATS query failed")

gold_flows <- gold_flows |>
  mutate(
    PERIOD = as.Date(PERIOD),
    FUNDS_MOBILIZED = as.numeric(FUNDS_MOBILIZED),
    REPURCHASES = as.numeric(REPURCHASES),
    NET_FLOW = as.numeric(NET_FLOW),
    AUM = as.numeric(AUM)
  )

odbcClose(lcon)

# GOLDBEES monthly close (last trading day of each month)
gold_px <- sqlQuery(
  odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, ldbname, ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE),
  "SELECT TIME_STAMP, PX_CLOSE FROM PX_HISTORY
   WHERE SYMBOL = 'GOLDBEES' AND SERIES = 'EQ'
   ORDER BY TIME_STAMP"
)

gold_px <- gold_px |>
  mutate(
    TIME_STAMP = as.Date(TIME_STAMP),
    YM = format(TIME_STAMP, "%Y-%m")
  ) |>
  group_by(YM) |>
  slice_max(TIME_STAMP, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(PERIOD = as.Date(paste0(YM, "-01")) + months(1) - days(1)) |>
  select(PERIOD, PX_CLOSE) |>
  arrange(PERIOD)

# ---------------------------------------------------------------------------
# Merge and wrangle
# ---------------------------------------------------------------------------

df <- gold_flows |>
  left_join(gold_px, by = "PERIOD") |>
  filter(!is.na(PX_CLOSE)) |>
  arrange(PERIOD) |>
  mutate(
    CUM_NET_FLOW = cumsum(NET_FLOW),
    GOLD_RET     = c(NA, diff(log(PX_CLOSE))),
    ROLL12_RET   = rollapply(GOLD_RET, 12, sum, fill = NA, align = "right"),
    ROLL12_FLOW  = rollapply(NET_FLOW, 12, sum, fill = NA, align = "right")
  )

dateRange <- range(df$PERIOD)
dateLabel <- sprintf("%s:%s", format(dateRange[1], "%b %Y"), format(dateRange[2], "%b %Y"))

# ---------------------------------------------------------------------------
# Chart 1: Monthly Inflow vs Outflow (last 24 months)
# ---------------------------------------------------------------------------

df_bar <- df |>
  filter(PERIOD >= max(PERIOD) - 365*2) |>
  select(PERIOD, FUNDS_MOBILIZED, REPURCHASES) |>
  pivot_longer(cols = c(FUNDS_MOBILIZED, REPURCHASES),
               names_to = "flow_type", values_to = "amount") |>
  mutate(flow_type = ifelse(flow_type == "FUNDS_MOBILIZED", "Inflow", "Outflow"))

p1 <- df_bar |>
  ggplot(aes(x = PERIOD, y = amount, fill = flow_type)) +
  geom_col(position = "dodge", alpha = 0.85, width = 15) +
  scale_fill_manual(values = c("Inflow" = "#2ecc71", "Outflow" = "#e74c3c")) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  theme_economist() +
  theme(
    axis.text.x    = element_text(angle = 0, hjust = 0.5, size = 7),
    legend.position = "top",
    legend.title    = element_blank(),
    plot.caption    = element_text(size = 8, color = "grey50", hjust = 1),
    plot.caption.position = "plot"
  ) +
  labs(
    x        = '',
    y        = 'Amount (₹ Crore)',
    title    = "Gold ETF Monthly Inflows vs Outflows",
    subtitle = "Last 24 months",
    caption  = '@StockViz'
  )

ggsave(file.path(reportPath, "gold-flow-in-out-monthly.png"), p1,
       width = 12, height = 7, units = "in")

# ---------------------------------------------------------------------------
# Chart 2: Cumulative Net Flow
# ---------------------------------------------------------------------------

p2 <- df |>
  ggplot(aes(x = PERIOD, y = CUM_NET_FLOW)) +
  geom_area(fill = "#f1c40f", alpha = 0.3) +
  geom_line(color = "#e67e22", linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  theme_economist() +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
    plot.caption   = element_text(size = 8, color = "grey50", hjust = 1),
    plot.caption.position = "plot"
  ) +
  labs(
    x        = '',
    y        = 'Cumulative Net Flow (₹ Crore)',
    title    = "Gold ETF Cumulative Net Flows",
    subtitle = dateLabel,
    caption  = '@StockViz'
  )

ggsave(file.path(reportPath, "gold-cumulative-flow.png"), p2,
       width = 12, height = 7, units = "in")

# ---------------------------------------------------------------------------
# Chart 3: Monthly GOLDBEES return vs Monthly Net Flow scatter
# ---------------------------------------------------------------------------

df_scatter <- df |>
  filter(!is.na(GOLD_RET), PERIOD >= as.Date("2020-05-01")) |>
  mutate(YEAR = factor(format(PERIOD, "%Y")))

p3 <- df_scatter |>
  ggplot(aes(x = GOLD_RET, y = NET_FLOW)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_vline(xintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_point(aes(color = YEAR), size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40",
              linewidth = 0.5, fill = "grey80", alpha = 0.3) +
  scale_color_viridis_d(option = "D", name = NULL) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = comma) +
  theme_economist() +
  theme(
    legend.position = "right",
    legend.text     = element_text(size = 7),
    plot.caption    = element_text(size = 8, color = "grey50", hjust = 1),
    plot.caption.position = "plot"
  ) +
  labs(
    x        = "Monthly GOLDBEES Return",
    y        = "Monthly Net Flow (₹ Crore)",
    title    = "Gold ETF Flows vs GOLDBEES Returns",
    subtitle = "May 2020 onward · monthly",
    caption  = '@StockViz'
  )

ggsave(file.path(reportPath, "gold-scatter-monthly.png"), p3,
       width = 10, height = 7, units = "in")

# ---------------------------------------------------------------------------
# Chart 4: 12-month GOLDBEES return vs 12-month Net Flow scatter
# ---------------------------------------------------------------------------

df_scatter12 <- df |>
  filter(!is.na(ROLL12_RET), PERIOD >= as.Date("2021-04-01")) |>
  mutate(YEAR = factor(format(PERIOD, "%Y")))

p4 <- df_scatter12 |>
  ggplot(aes(x = ROLL12_RET, y = ROLL12_FLOW)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_vline(xintercept = 0, linewidth = 0.3, color = "grey50") +
  geom_point(aes(color = YEAR), size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40",
              linewidth = 0.5, fill = "grey80", alpha = 0.3) +
  scale_color_viridis_d(option = "D", name = NULL) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = comma) +
  theme_economist() +
  theme(
    legend.position = "right",
    legend.text     = element_text(size = 7),
    plot.caption    = element_text(size = 8, color = "grey50", hjust = 1),
    plot.caption.position = "plot"
  ) +
  labs(
    x        = "12-Month GOLDBEES Return",
    y        = "12-Month Net Flow (₹ Crore)",
    title    = "Gold ETF Flows vs GOLDBEES Returns",
    subtitle = "12-month rolling · from April 2021",
    caption  = '@StockViz'
  )

ggsave(file.path(reportPath, "gold-scatter-12m.png"), p4,
       width = 10, height = 7, units = "in")

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

cat("=== Gold ETF Charts Generated ===\n")
cat(sprintf("Period: %s to %s\n", dateRange[1], dateRange[2]))
cat(sprintf("Total rows: %d\n", nrow(df)))
cat("\nOutput files:\n")
for (f in list.files(reportPath, pattern = "gold-.*\\.png$")) {
  cat(sprintf("  %s\n", f))
}
