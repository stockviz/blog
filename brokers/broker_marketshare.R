#!/usr/bin/env Rscript
#
# broker_marketshare.R — 100% Stacked Area Chart of broker market share over time
#
# Data sources:
#   People.dbo.broker_check     — per-broker UCC counts per FY
#   People.dbo.broker_aggregate — total active clients per FY
#
# Output:
#   broker_marketshare.png — static 100% stacked area chart
#

library('RODBC')
library('tidyverse')
library('ggthemes')
library('viridis')
library('scales')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "/mnt/data/blog/brokers"
source("/mnt/hollandC/StockViz/R/config.r")

# ---------------------------------------------------------------------------
# Database
# ---------------------------------------------------------------------------
lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=People;Uid=%s;Pwd=%s;",
          ldbserver, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

cat("Fetching data...\n")
check <- sqlQuery(lcon, "
  SELECT FIN_YEAR, BROKER_NAME, NUM_ACTIVE
  FROM broker_check
  WHERE FIN_YEAR >= 201415
  ORDER BY FIN_YEAR, BROKER_NAME
")
agg <- sqlQuery(lcon, "
  SELECT FIN_YEAR, TOTAL_ACTIVE
  FROM broker_aggregate
  WHERE FIN_YEAR >= 201415
  ORDER BY FIN_YEAR
")
odbcClose(lcon)

# ---------------------------------------------------------------------------
# Wrangle
# ---------------------------------------------------------------------------
fy_label <- function(fy) {
  fy_chr <- as.character(fy)
  sprintf("%s-%s", substr(fy_chr, 1, 4), substr(fy_chr, 5, 6))
}

df <- check |>
  left_join(agg, by = "FIN_YEAR") |>
  filter(TOTAL_ACTIVE > 0) |>
  mutate(
    FY_LABEL = fy_label(FIN_YEAR),
    MARKET_SHARE = NUM_ACTIVE / TOTAL_ACTIVE,
    FY_ORDER = as.integer(FIN_YEAR)
  )

# Short names
# Normalize names for lookup (uppercase)
name_map <- c(
  "RAISE SECURITIES PRIVATE LIMITED (FORMERLY KNOWN AS MONEYLICIOUS SECURITIES PRIVATE LIMITED) (DHAN APP)" = "Raise Securities (Dhan)",
  "GROWW INVEST TECH PRIVATE LIMITED" = "Groww",
  "NEXTBILLION TECHNOLOGY PRIVATE LIMITED" = "Groww",
  "ZERODHA BROKING LIMITED" = "Zerodha",
  "ZERODHA" = "Zerodha",
  "ZERODHA SECURITIES PRIVATE LIMITED" = "Zerodha",
  "ANGEL ONE LIMITED" = "Angel One",
  "ANGEL BROKING LIMITED" = "Angel One",
  "ANGEL BROKING PRIVATE LIMITED" = "Angel One",
  "ICICI SECURITIES LIMITED" = "ICICI Securities",
  "ICICI SECURITIES PRIMARY DEALERSHIP LIMITED" = "ICICI Securities",
  "UPSTOX SECURITIES PRIVATE LIMITED" = "Upstox",
  "RKSV SECURITIES INDIA PRIVATE LIMITED" = "Upstox",
  "KOTAK SECURITIES LTD." = "Kotak Securities",
  "KOTAK MAHINDRA SECURITIES LTD." = "Kotak Securities",
  "HDFC SECURITIES LTD." = "HDFC Securities",
  "SBICAP SECURITIES LIMITED" = "SBICAP Securities",
  "MOTILAL OSWAL FINANCIAL SERVICES LIMITED" = "Motilal Oswal",
  "MOTILAL OSWAL SECURITIES LTD." = "Motilal Oswal",
  "MOTILAL OSWAL CAPITAL MARKETS PRIVATE LIMITED" = "Motilal Oswal",
  "PAYTM MONEY LTD." = "Paytm Money",
  "INDSTOCKS PRIVATE LIMITED" = "Indstocks",
  "SHAREKHAN LTD." = "Sharekhan",
  "AXIS SECURITIES LIMITED" = "Axis Securities",
  "IIFL CAPITAL SERVICES LTD." = "IIFL Capital",
  "INDIA INFOLINE LTD." = "IIFL Capital",
  "PHONEPE WEALTH BROKING PRIVATE LIMITED" = "PhonePe Wealth",
  "FYERS SECURITIES PRIVATE LIMITED" = "Fyers",
  "5PAISA CAPITAL LIMITED" = "5paisa",
  "CHOICE EQUITY BROKING PRIVATE LIMITED" = "Choice Equity",
  "GEOJIT INVESTMENTS LIMITED" = "Geojit",
  "GEOJIT BNP PARIBAS FINANCIAL SERVICES LIMITED" = "Geojit",
  "RELIGARE BROKING LIMITED" = "Religare",
  "NUVAMA WEALTH AND INVESTMENT LIMITED." = "Nuvama Wealth",
  "SMC GLOBAL SECURITIES LTD." = "SMC Global",
  "MIRAE ASSET CAPITAL MARKETS ( INDIA ) PRIVATE LIMITED" = "Mirae Asset",
  "FINVASIA SECURITIES PRIVATE LIMITED" = "Finvasia",
  "ANAND RATHI SHARE AND STOCK BROKERS LIMITED" = "Anand Rathi",
  "ALICE BLUE FIN SVCS P LTD" = "Alice Blue",
  "JM FINANCIAL SERVICES LIMITED" = "JM Financial",
  "NIRMAL BANG SECURITIES PVT. LTD." = "Nirmal Bang",
  "RELIANCE SECURITIES LIMITED" = "Reliance Securities",
  "KOTAK MAHINDRA BANK LTD." = "Kotak Bank"
)

df <- df |>
  mutate(
    NAME_UPPER = toupper(BROKER_NAME),
    BROKER_SHORT = ifelse(NAME_UPPER %in% names(name_map),
                          name_map[NAME_UPPER], BROKER_NAME)
  )

# Identify top N brokers by average market share across all FYs
broker_avg <- df |>
  group_by(BROKER_SHORT) |>
  summarise(AVG_SHARE = mean(MARKET_SHARE), .groups = "drop") |>
  arrange(desc(AVG_SHARE))

# Top 8 + group rest as "Others"
TOP_N <- 8
top_brokers <- broker_avg$BROKER_SHORT[1:TOP_N]
cat(sprintf("Top %d brokers: %s\n", TOP_N, paste(top_brokers, collapse = ", ")))

df <- df |>
  mutate(DISPLAY = ifelse(BROKER_SHORT %in% top_brokers, BROKER_SHORT, "Others"))

# Aggregate: sum market share per DISPLAY per FY
plot_df <- df |>
  group_by(FY_LABEL, FY_ORDER, DISPLAY) |>
  summarise(SHARE = sum(MARKET_SHARE), .groups = "drop")

# FY levels in order
fy_levels <- plot_df |>
  distinct(FY_LABEL, FY_ORDER) |>
  arrange(FY_ORDER) |>
  pull(FY_LABEL)

plot_df$FY_LABEL <- factor(plot_df$FY_LABEL, levels = fy_levels)

# Order DISPLAY: top brokers then "Others" last
display_order <- c(top_brokers, "Others")
plot_df$DISPLAY <- factor(plot_df$DISPLAY, levels = rev(display_order))

# "Unattributed" = the gap between report total and sum of all broker UCCs
fy_unattributed <- plot_df |>
  group_by(FY_LABEL) |>
  summarise(TOTAL_SHOWN = sum(SHARE), .groups = "drop") |>
  mutate(UNATTRIBUTED = 1 - TOTAL_SHOWN)

if (any(fy_unattributed$UNATTRIBUTED > 0.01)) {
  cat("\nUnattributed share (exchange-level clients not in individual broker rows):\n")
  for (i in 1:nrow(fy_unattributed)) {
    if (fy_unattributed$UNATTRIBUTED[i] > 0.005) {
      cat(sprintf("  %s: %.2f%%\n", fy_unattributed$FY_LABEL[i],
                  fy_unattributed$UNATTRIBUTED[i] * 100))
    }
  }
}

# ---------------------------------------------------------------------------
# Color palette
# ---------------------------------------------------------------------------
broker_colors <- c(
  setNames(viridis_pal(option = "D")(TOP_N), top_brokers),
  c("Others" = "#7CB5D0")
)

# ---------------------------------------------------------------------------
# 100% Stacked Area Chart
# ---------------------------------------------------------------------------
cat(sprintf("\nPlotting %d FYs, %d categories...\n",
            length(fy_levels), length(display_order)))

p <- ggplot(plot_df, aes(x = FY_LABEL, y = SHARE, fill = DISPLAY, group = DISPLAY)) +
  geom_area(position = "fill", alpha = 0.9, color = "white", linewidth = 0.3) +
  scale_y_continuous(
    labels = percent_format(),
    expand = c(0, 0.005),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_fill_manual(values = broker_colors) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    title = "NSE Broker Market Share",
    subtitle = sprintf("Share of Total Active Clients (UCC) — Top %d Brokers + Others", TOP_N),
    x = "", y = "Market Share",
    fill = "",
    caption = "@StockViz"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "right",
    legend.text = element_text(size = 9, margin = margin(l = 2, r = 2)),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(l = 2, r = 2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 2, 10, 2)
  )

outfile <- file.path(reportPath, "broker_marketshare.png")
ggsave(outfile, p, width = 12, height = 7, units = "in", dpi = 120)
cat(sprintf("Done: %s\n", outfile))
