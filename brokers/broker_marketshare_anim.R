#!/usr/bin/env Rscript
#
# broker_marketshare_anim.R — Animated bar chart race of per-year top 8 brokers
#
# Each frame shows that year's actual top 8 by market share.
# Output: broker_marketshare.gif
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

# ---------------------------------------------------------------------------
# Name normalization (same map as broker_marketshare.R)
# ---------------------------------------------------------------------------
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

# Aggregate by short name
df_agg <- df |>
  group_by(FY_LABEL, FY_ORDER, BROKER_SHORT) |>
  summarise(
    SHARE_PCT = sum(MARKET_SHARE) * 100,
    .groups = "drop"
  )

# Others = sum of remaining after top 8 per FY
fy_levels <- df_agg |>
  distinct(FY_LABEL, FY_ORDER) |>
  arrange(FY_ORDER)

N_TOP <- 8

# Build per-FY frames: top 8 + Others
all_frames <- list()
all_brokers_seen <- c()

for (fy_name in fy_levels$FY_LABEL) {
  fy_df <- df_agg |>
    filter(FY_LABEL == fy_name) |>
    arrange(desc(SHARE_PCT))

  tops <- fy_df |> slice_head(n = N_TOP)
  others_share <- fy_df |> slice_tail(n = max(0, nrow(fy_df) - N_TOP)) |> pull(SHARE_PCT) |> sum()

  frame <- tops |>
    select(BROKER_SHORT, SHARE_PCT)

  if (others_share > 0) {
    frame <- frame |>
      add_row(BROKER_SHORT = "Others", SHARE_PCT = others_share)
  }

  frame <- frame |>
    mutate(
      FY_LABEL = fy_name,
      FY_ORDER = first(fy_df$FY_ORDER),
      RANK = row_number()
    )

  all_frames[[fy_name]] <- frame
  all_brokers_seen <- union(all_brokers_seen, frame$BROKER_SHORT)
}

plot_df <- bind_rows(all_frames)
plot_df$FY_LABEL <- factor(plot_df$FY_LABEL, levels = fy_levels$FY_LABEL)

# Pad names to fixed width so axis labels don't shift between frames
max_name_width <- max(nchar(plot_df$BROKER_SHORT))
plot_df$BROKER_SHORT <- stringr::str_pad(plot_df$BROKER_SHORT, max_name_width, "right")
all_brokers_seen <- stringr::str_pad(all_brokers_seen, max_name_width, "right")

# ---------------------------------------------------------------------------
# Colors — consistent across frames
# ---------------------------------------------------------------------------
all_brokers <- setdiff(sort(all_brokers_seen), "Others")
broker_colors <- setNames(viridis_pal(option = "D")(length(all_brokers)), all_brokers)
broker_colors["Others"] <- "#7CB5D0"

# ---------------------------------------------------------------------------
# Generate frames
# ---------------------------------------------------------------------------
cat(sprintf("Generating %d frames (per-year top %d)...\n", length(fy_levels$FY_LABEL), N_TOP))

fy_list <- fy_levels$FY_LABEL
global_max <- max(plot_df$SHARE_PCT) * 1.18

for (i in seq_along(fy_list)) {
  fy_name <- fy_list[i]
  frame_df <- plot_df |> filter(FY_LABEL == fy_name) |> arrange(desc(SHARE_PCT))

  p <- ggplot(frame_df, aes(x = reorder(BROKER_SHORT, SHARE_PCT), y = SHARE_PCT,
                              fill = BROKER_SHORT)) +
    geom_col(width = 0.7) +
    geom_text(aes(y = SHARE_PCT + global_max * 0.02,
                  label = sprintf("%.1f%%", SHARE_PCT)),
              hjust = 0, size = 4.5) +
    coord_flip() +
    scale_y_continuous(limits = c(0, global_max), expand = c(0, 0),
                       labels = function(x) sprintf("%.0f%%", x)) +
    scale_fill_manual(values = broker_colors, guide = "none") +
    labs(
      title = sprintf("NSE Broker Market Share — %s", fy_name),
      subtitle = sprintf("Top %d by Unique Client Codes (UCC)", N_TOP),
      x = "", y = "Market Share",
      caption = "@StockViz"
    ) +
    theme_economist() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 10, hjust = 0),
      axis.text.y = element_text(size = 10, family = "mono"),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(10, 5, 10, 5)
    )

  fname <- sprintf("%s/frame_%02d.png", reportPath, i)
  ggsave(fname, p, width = 10, height = 6, units = "in", dpi = 100)
  cat(sprintf("  [%2d/%2d] %s\n", i, length(fy_list), fy_name))
}

# ---------------------------------------------------------------------------
# Stitch into GIF
# ---------------------------------------------------------------------------
cat("\nStitching frames...\n")
frame_pattern <- file.path(reportPath, "frame_*.png")
out_gif <- file.path(reportPath, "broker_marketshare.gif")

cmd <- sprintf("convert -delay 200 -loop 0 '%s' '%s'", frame_pattern, out_gif)
system(cmd)

unlink(Sys.glob(file.path(reportPath, "frame_*.png")))
cat(sprintf("Done: %s\n", out_gif))
