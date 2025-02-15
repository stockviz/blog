library('RODBC')
library('duckdb')
library('tidyverse')
library('dplyr')
library('ggthemes')
library('viridis')
library('patchwork')

duckdbPath <- "/mnt/siberia/crypto"
reportPath <- "."

pdf(NULL)

stableCoins <- "USDT|USDC|USDS|USDe|DAI|FDUSD|USDD|PYUSD|TUSD|USD0|USDY|USDB|BUSD|EUR|BRL"
filenames <- Sys.glob(sprintf("%s/binance.*.db", duckdbPath))
filenames <- tail(sort(filenames)[-length(filenames)], 2)

allData <- tibble()
for(fn in filenames){
  print(paste("loading", fn))
  con <- dbConnect(duckdb::duckdb(), dbdir = fn, read_only = TRUE, config = list("memory_limit"="32GB"))
  allData <- rbind(allData, tbl(con, 'bsnap') |> collect())
  dbDisconnect(con)
}

summStats <- allData |> filter(str_starts(symbol, stableCoins, negate=TRUE)) |> group_by(symbol) |> 
  summarise(MED_SPREAD = 100*median(askPrice/bidPrice - 1), MED_VAL = median(bidPrice * bidQty + askPrice * askQty)) |>
  arrange(MED_SPREAD)

rm(allData)

numNoLiquidty <- (summStats |> filter(MED_SPREAD < 0 | is.na(MED_SPREAD) | !is.finite(MED_SPREAD)) |> summarise(CNT = n()))[[1]]
print(paste(numNoLiquidty, "of", nrow(summStats)))
print(100*round(numNoLiquidty/nrow(summStats), 5))

decileStats <- summStats |> filter(MED_SPREAD >= 0 & !is.na(MED_SPREAD) & is.finite(MED_SPREAD)) |> 
  mutate(DECILE = ntile(MED_SPREAD, 10)) |> 
  group_by(DECILE) |>
  summarise(MEAN_SPREAD = median(MED_SPREAD), MAX_SPREAD = max(MED_SPREAD),
            MEAN_VAL = median(MED_VAL), MAX_VAL = max(MED_VAL))

p1 <- decileStats |>
  ggplot(aes(x=factor(DECILE), y = MEAN_SPREAD)) +
  theme_economist() +
  geom_bar(stat = "identity", fill=viridis_pal()(2)[1]) + 
  geom_text(aes(label = paste0("[", round(MAX_SPREAD, 2), "]")), vjust=0) +
  labs(x = '', y = 'median spread [max] (%)', subtitle = "spread")

p2 <- decileStats |> mutate(MEAN_VAL = MEAN_VAL/1000) |>
  ggplot(aes(x=factor(DECILE), y = MEAN_VAL)) +
  theme_economist() +
  geom_bar(stat = "identity", fill=viridis_pal()(2)[1]) + 
  geom_text(aes(label = paste0("[", format(round(MAX_VAL, 0), big.mark=",", trim=TRUE,scientific=FALSE), "]")), vjust=-0.5, size=3) +
  labs(x = 'decile', y = "median book [max] ($ '000)", subtitle = "book value")

p1/p2 + plot_annotation(title = "Binance Quotes by bid/ask decile", 
                        caption = '@StockViz',
                        theme = theme_economist())
ggsave(sprintf("%s/binance.quote.aggregate.png", reportPath), width = 12, height = 12,units = "in")
  
summStats |> filter(MED_SPREAD >= 0 & !is.na(MED_SPREAD) & is.finite(MED_SPREAD)) |> 
  mutate(DECILE = ntile(MED_SPREAD, 10)) |>
  filter(DECILE < 10) |> 
    ggplot(aes(x=factor(DECILE, levels = unique(DECILE)), y = MED_SPREAD)) +
    theme_economist() +
    geom_violin() +
  labs(x = 'decile', y = 'median spread(%)', title = "Binance bid/ask spread by decile", caption='@StockViz')

ggsave(sprintf("%s/binance.quote.spread.distribution.png", reportPath), width = 12, height = 6,units = "in")
