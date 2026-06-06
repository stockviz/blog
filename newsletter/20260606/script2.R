library('RODBC')
library('tidyverse')
library('xts')
library('ggthemes')
library('viridis')
library('patchwork')
library('scales')

pdf(NULL)
options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")

lcon <- odbcDriverConnect(
  sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
          ldbserver, ldbname, ldbuser, ldbpassword),
  case = "nochange", believeNRows = TRUE)

# ---------------------------------------------------------------------------
# Load metadata
# ---------------------------------------------------------------------------
df <- read_csv("dates_with_funds_and_aum.csv", show_col_types = FALSE) |>
  mutate(
    BaseDate = as.Date(BaseDate),
    LaunchDate = as.Date(LaunchDate),
    FundStartDate = as.Date(FundStartDate),
    TotalAUM_Cr = as.numeric(TotalAUM_Cr)
  )

valid <- df |>
  filter(!is.na(SchemeCode), SchemeCode != "", SchemeCode != "0",
         !is.na(FundStartDate))

cat(sprintf("Indices with mapped fund: %d\n", nrow(valid)))

# ---------------------------------------------------------------------------
# Map to TR index names
# ---------------------------------------------------------------------------
all_tr <- sqlQuery(lcon, "
  SELECT DISTINCT INDEX_NAME FROM BHAV_INDEX
  WHERE INDEX_NAME LIKE '% TR' AND INDEX_NAME NOT LIKE '%TR %'
")[,1]

match_tr <- function(idx_name, tr_list) {
  direct <- paste0(toupper(idx_name), " TR")
  if (direct %in% tr_list) return(direct)
  clean <- gsub("\\s+", " ", toupper(idx_name))
  if (paste0(clean, " TR") %in% tr_list) return(paste0(clean, " TR"))
  if (!grepl("^NIFTY", clean)) {
    nifty_name <- paste0("NIFTY ", clean, " TR")
    if (nifty_name %in% tr_list) return(nifty_name)
  }
  pattern <- gsub("\\s+", ".*", clean)
  matches <- grep(pattern, tr_list, value = TRUE, ignore.case = TRUE)
  matches <- matches[grepl("TR$", matches)]
  if (length(matches) > 0) return(matches[1])
  return(NA)
}

valid$TR_NAME <- sapply(valid$IndexName, match_tr, tr_list = all_tr)
mapped <- valid |> filter(!is.na(TR_NAME))
cat(sprintf("Mapped to TR index: %d\n", nrow(mapped)))

# ---------------------------------------------------------------------------
# Fetch returns for a date window
# ---------------------------------------------------------------------------
get_tr_return <- function(tr_name, from_date, to_date) {
  from_str <- format(from_date, "%Y-%m-%d")
  to_str   <- format(to_date, "%Y-%m-%d")
  px <- sqlQuery(lcon, sprintf(
    "SELECT TIME_STAMP, PX_CLOSE FROM BHAV_INDEX
     WHERE INDEX_NAME = '%s' AND TIME_STAMP >= '%s' AND TIME_STAMP <= '%s'
     ORDER BY TIME_STAMP", tr_name, from_str, to_str))
  if (!is.data.frame(px) || nrow(px) < 20) return(NA_real_)
  tryCatch({
    x <- xts(px$PX_CLOSE, px$TIME_STAMP)
    as.numeric(coredata(last(x))) / as.numeric(coredata(first(x))) - 1
  }, error = function(e) NA_real_)
}

get_fund_return <- function(scheme_code, from_date, to_date) {
  from_str <- format(from_date, "%Y-%m-%d")
  to_str   <- format(to_date, "%Y-%m-%d")
  nav <- sqlQuery(lcon, sprintf(
    "SELECT AS_OF, NAV FROM MF_NAV_HISTORY
     WHERE SCHEME_CODE = %d AND AS_OF >= '%s' AND AS_OF <= '%s'
     ORDER BY AS_OF", scheme_code, from_str, to_str))
  if (!is.data.frame(nav) || nrow(nav) < 20) return(NA_real_)
  tryCatch({
    x <- xts(nav$NAV, nav$AS_OF)
    as.numeric(coredata(last(x))) / as.numeric(coredata(first(x))) - 1
  }, error = function(e) NA_real_)
}

# ---------------------------------------------------------------------------
# Compute returns
# ---------------------------------------------------------------------------
results <- list()

for (i in seq_len(nrow(mapped))) {
  idx_name   <- mapped$IndexName[i]
  tr_name    <- mapped$TR_NAME[i]
  idx_launch <- as.Date(mapped$LaunchDate[i])
  fund_start <- as.Date(mapped$FundStartDate[i])
  scheme_code <- as.integer(mapped$SchemeCode[i])

  if (is.na(idx_launch) || is.na(fund_start)) next

  # Index: ±1 year around index launch date
  idx_pre  <- get_tr_return(tr_name, idx_launch - 365, idx_launch)
  idx_post <- get_tr_return(tr_name, idx_launch, idx_launch + 365)

  # Fund: 1 year before fund start (use index TR as proxy)
  fund_pre <- get_tr_return(tr_name, fund_start - 365, fund_start)

  # Fund: 1 year after fund start (use actual fund NAV)
  fund_post <- get_fund_return(scheme_code, fund_start, fund_start + 365)

  results[[length(results) + 1]] <- data.frame(
    IndexName    = idx_name,
    IndexLaunch  = idx_launch,
    FundStart    = fund_start,
    IdxPre       = idx_pre,
    IdxPost      = idx_post,
    FundPre      = fund_pre,
    FundPost     = fund_post,
    stringsAsFactors = FALSE
  )
}

res_df <- bind_rows(results)
cat(sprintf("Computed returns for %d indices\n", nrow(res_df)))

if (nrow(res_df) == 0) {
  cat("No return data available.\n")
  quit(save = "no", status = 0)
}

# Filter to those with at least some data
res_df <- res_df |>
  filter(!is.na(IdxPre) | !is.na(IdxPost) | !is.na(FundPre) | !is.na(FundPost))

cat(sprintf("With return data: %d\n", nrow(res_df)))

# ---------------------------------------------------------------------------
# Chart 1: Index ±1yr around launch (where data exists)
# ---------------------------------------------------------------------------
idx_returns <- res_df |>
  filter(!is.na(IdxPre), !is.na(IdxPost)) |>
  select(IndexName, IdxPre, IdxPost) |>
  pivot_longer(-IndexName, names_to = "Period", values_to = "Return") |>
  mutate(
    Return = Return * 100,
    Period = if_else(Period == "IdxPre", "Index Return\n(year before)", "Index Return\n(year after)")
  )

n_idx <- n_distinct(idx_returns$IndexName)

ggplot(idx_returns, aes(x = Period, y = Return, fill = Period)) +
  theme_economist() +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  scale_fill_viridis_d() +
  guides(fill = "none") +
  labs(
    x = "", y = "Return (%)",
    title = "Index TR Returns: The Year Before vs After Launch",
    subtitle = sprintf("%d indices | Median: pre-launch %.1f%%, post-launch %.1f%%",
                       n_idx,
                       median(res_df$IdxPre, na.rm = TRUE) * 100,
                       median(res_df$IdxPost, na.rm = TRUE) * 100),
    caption = "@StockViz"
  )

ggsave(sprintf("%s/index-launch-returns.png", reportPath), width = 8, height = 6, units = "in")

# ---------------------------------------------------------------------------
# Chart 2: Fund ±1yr around fund start
# ---------------------------------------------------------------------------
fund_returns <- res_df |>
  filter(!is.na(FundPre), !is.na(FundPost)) |>
  select(IndexName, FundPre, FundPost) |>
  pivot_longer(-IndexName, names_to = "Period", values_to = "Return") |>
  mutate(
    Return = Return * 100,
    Period = if_else(Period == "FundPre", "Index Return\n(year before)", "Fund Return\n(first year)")
  )

n_fund <- n_distinct(fund_returns$IndexName)

ggplot(fund_returns, aes(x = Period, y = Return, fill = Period)) +
  theme_economist() +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  scale_fill_viridis_d() +
  guides(fill = "none") +
  labs(
    x = "", y = "Return (%)",
    title = "Index Funds: The Year Before vs Their First Year",
    subtitle = sprintf("%d funds | Median: pre-launch %.1f%%, post-launch %.1f%%",
                       n_fund,
                       median(res_df$FundPre, na.rm = TRUE) * 100,
                       median(res_df$FundPost, na.rm = TRUE) * 100),
    caption = "@StockViz"
  )

ggsave(sprintf("%s/fund-launch-returns.png", reportPath), width = 8, height = 6, units = "in")

# ---------------------------------------------------------------------------
# Chart 3: All indices — pre vs post launch return scatter
# ---------------------------------------------------------------------------
prepost_all <- res_df |>
  filter(!is.na(IdxPre), !is.na(IdxPost)) |>
  mutate(
    PrePct = IdxPre * 100,
    PostPct = IdxPost * 100
  )

ggplot(prepost_all, aes(x = PrePct, y = PostPct)) +
  theme_economist() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey70") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey70") +
  geom_point(aes(color = cut(IndexLaunch, breaks = "4 years")), size = 3, alpha = 0.8) +
  scale_color_viridis_d() +
  guides(color = guide_legend(nrow = 1)) +
  labs(
    x = "Index Return\n(year before, %)", 
    y = "Index Return\n(year after, %)",
    color = "Launch",
    title = "Index TR Returns: Year Before vs After Launch",
    subtitle = sprintf("%d indices | Above diagonal = post > pre | Median: pre=%.1f%%, post=%.1f%%",
                       nrow(prepost_all),
                       median(prepost_all$PrePct), median(prepost_all$PostPct)),
    caption = "@StockViz"
  )

ggsave(sprintf("%s/index-pre-post-scatter.png", reportPath), width = 10, height = 7, units = "in")

# ---------------------------------------------------------------------------
# Chart 4: Fund launch — pre vs post scatter
# ---------------------------------------------------------------------------
fund_prepost <- res_df |>
  filter(!is.na(FundPre), !is.na(FundPost)) |>
  mutate(
    PrePct = FundPre * 100,
    PostPct = FundPost * 100
  )

ggplot(fund_prepost, aes(x = PrePct, y = PostPct)) +
  theme_economist() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey70") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey70") +
  geom_point(aes(color = cut(FundStart, breaks = "4 years")), size = 3, alpha = 0.8) +
  scale_color_viridis_d() +
  guides(color = guide_legend(nrow = 1)) +
  labs(
    x = "Index Return\n(year before, %)", 
    y = "Fund Return\n(first year, %)",
    color = "Fund Start",
    title = "First Year of an Index Fund vs the Year Before It Launched",
    subtitle = sprintf("%d funds | Above diagonal = first year > year before | Median: pre=%.1f%%, post=%.1f%%",
                       nrow(fund_prepost),
                       median(fund_prepost$PrePct), median(fund_prepost$PostPct)),
    caption = "@StockViz"
  )

ggsave(sprintf("%s/fund-pre-post-scatter.png", reportPath), width = 10, height = 7, units = "in")

# ---------------------------------------------------------------------------
# Summary table
# ---------------------------------------------------------------------------
cat(sprintf("\n=== Index Launch Returns (TR) ===\n"))
cat(sprintf("  Indices with data:  %d\n",
            sum(!is.na(res_df$IdxPre) & !is.na(res_df$IdxPost))))
cat(sprintf("  Median pre-launch:  %.1f%%\n",
            median(res_df$IdxPre, na.rm = TRUE) * 100))
cat(sprintf("  Median post-launch: %.1f%%\n",
            median(res_df$IdxPost, na.rm = TRUE) * 100))

cat(sprintf("\n=== Fund Launch Returns ===\n"))
cat(sprintf("  Indices with data:  %d\n",
            sum(!is.na(res_df$FundPre) & !is.na(res_df$FundPost))))
cat(sprintf("  Median pre (idx TR): %.1f%%\n",
            median(res_df$FundPre, na.rm = TRUE) * 100))
cat(sprintf("  Median post (fund):  %.1f%%\n",
            median(res_df$FundPost, na.rm = TRUE) * 100))
