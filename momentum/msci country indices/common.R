library('RODBC')
library('quantmod')
library('PerformanceAnalytics')
library('tidyverse')
library('gt')
library('webshot2')
library('xts')

options("scipen" = 100)
options(stringsAsFactors = FALSE)

reportPath <- "."
source("/mnt/hollandC/StockViz/R/config.r")
source("/mnt/data/blog/common/plot.common.r")

pdf(NULL)
drag <- 0.2 / 100

# ── shared cache ───────────────────────────────────────────────────────────────
cache_file <- sprintf("%s/cache.Rdata", reportPath)
if (file.exists(cache_file)) {
  print("shared cache found — loading...")
  load(cache_file)
  cat(sprintf("  %d countries, %d month-end dates\n",
              length(valid_countries), length(monthEndDates)))
} else {
  print("no shared cache — computing from scratch...")

  # ── connect ─────────────────────────────────────────────────────────────────
  lconUS2 <- odbcDriverConnect(
    sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            ldbserver, "StockVizUs2", ldbuser, ldbpassword),
    case = "nochange", believeNRows = TRUE
  )

  # ── discover MSCI country equity universe ───────────────────────────────────
  benchIndexName <- "MSCI ACWI Index"
  benchIndexCode <- sqlQuery(lconUS2,
    sprintf("select index_code from msci_meta where index_name='%s'", benchIndexName))[[1]]

  ignoreList <- c('-1')
  countryCounts <- sqlQuery(lconUS2, "
    select index_code, count(c1.[value]) cnt
    from msci_meta cross apply openjson(details, '$.country') c1
    group by index_code")
  countryCounts <- countryCounts[countryCounts[, 2] == 1, ]

  countryMeta <- sqlQuery(lconUS2, sprintf("
    select index_name, index_code, json_value(details, '$.country[0]') country
    from msci_meta
    where json_value(details, '$.assetClass') = 'EQUITY'
      and json_value(details, '$.indexType') = 'Flat Index Type'
      and json_value(details, '$.sizes[0]') = 'LARGE_CAP'
      and json_value(details, '$.taxonomyGroups[0]') <> 'Sector'
      and json_value(details, '$.taxonomyCategories[0]') = 'Market Cap'
      and index_code in (%s)
      and index_name not like '%% ex %%'
      and index_name not like '%% imi%%'
      and index_name not like '%% large %%'
      and index_code not in (%s)
    order by country",
    paste(countryCounts$index_code, collapse = ','),
    paste(ignoreList, collapse = ',')))

  countryMeta <- countryMeta[!is.na(countryMeta$country), ]

  countryDates <- sqlQuery(lconUS2, sprintf("
    select ID, min(TIME_STAMP) stdt, max(TIME_STAMP) eddt
    from msci_data
    where INDEX_TYPE = 'G' and ID in (%s)
    group by ID", paste(countryMeta$index_code, collapse = ",")))

  countryUniverse <- countryDates |>
    filter(stdt <= median(stdt)) |>
    inner_join(countryMeta, join_by(ID == index_code))

  cat(sprintf("  %d country indices in universe\n", nrow(countryUniverse)))

  # ── load prices ─────────────────────────────────────────────────────────────
  print("  loading prices...")
  daily_prices  <- list()
  daily_rets    <- list()
  monthly_rets  <- list()

  # benchmark
  print("  loading benchmark...")
  benchPx <- sqlQuery(lconUS2, sprintf("
    select VAL, TIME_STAMP
    from msci_data
    where INDEX_TYPE = 'G' and ID=%s
    order by TIME_STAMP", benchIndexCode))
  bench_xts <- xts(benchPx$VAL, benchPx$TIME_STAMP)
  bench_monthly_rets <- monthlyReturn(bench_xts)
  names(bench_monthly_rets) <- c(benchIndexName)
  cat(sprintf("    ACWI: %d rows\n", nrow(bench_xts)))

  for (i in 1:nrow(countryUniverse)) {
    idx_code <- countryUniverse$ID[i]
    idx_name <- countryUniverse$country[i]
    px <- sqlQuery(lconUS2, sprintf("
      select VAL, TIME_STAMP
      from msci_data
      where INDEX_TYPE = 'G' and ID=%s
      order by TIME_STAMP", idx_code))
    if (nrow(px) < 260) {
      cat(sprintf("    %s: too few rows (%d), skipping\n", idx_name, nrow(px)))
      next
    }
    daily_prices[[idx_name]]  <- xts(px$VAL, px$TIME_STAMP)
    daily_rets[[idx_name]]    <- dailyReturn(daily_prices[[idx_name]])
    monthly_rets[[idx_name]]  <- monthlyReturn(daily_prices[[idx_name]])
    cat(sprintf("    %s: %d rows\n", idx_name, nrow(px)))
  }

  valid_countries <- names(daily_prices)
  cat(sprintf("  %d countries loaded\n", length(valid_countries)))

  # ── compute rolling Sharpe + SMA (always both — shared cache) ───────────────
  positionSize <- 10
  momLbs <- c(50, 100, 200)  # days
  periodSRs  <- list()
  sma_prices <- list()

  for (momLb in momLbs) {
    print(sprintf("  computing rolling Sharpe + SMA: %d-day...", momLb))
    periodSRs[[momLb]] <- do.call(merge.xts, lapply(
      valid_countries, \(vc) rollapply(daily_rets[[vc]], momLb, \(x) SharpeRatio(x))))
    names(periodSRs[[momLb]]) <- valid_countries
    periodSRs[[momLb]] <- na.omit(periodSRs[[momLb]])

    sma_prices[[momLb]] <- do.call(merge.xts, lapply(
      valid_countries, \(vc) SMA(daily_prices[[vc]], momLb)))
    names(sma_prices[[momLb]]) <- valid_countries
  }

  monthEndDates <- index(to.period(periodSRs[[max(momLbs)]][, 1], OHLC = FALSE))
  cat(sprintf("  %d month-end dates\n", length(monthEndDates)))

  # ── save shared cache ───────────────────────────────────────────────────────
  print("saving shared cache...")
  save(valid_countries, daily_prices, daily_rets, monthly_rets,
       bench_monthly_rets, benchIndexName,
       periodSRs, sma_prices, momLbs, monthEndDates,
       drag, positionSize,
       file = cache_file)
  cat("  cache saved\n")
  odbcClose(lconUS2)
}
