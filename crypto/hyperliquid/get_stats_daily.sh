#! /bin/bash

#### apt install bash-builtins

enable -f /usr/lib/bash/csv csv

skip_headers=1
while IFS=, read -r line
do
csv -a aVar "$line"
if ((skip_headers))
  then
((skip_headers--))
else
  echo "${aVar[0]}"
  sqlcmd="SET memory_limit = '32GB';select dt, 100*median(sellPx1/buyPx1-1) as med_spread, median(wavgBuyPx*totalBuyQty) as med_buy_sz, median(wavgSellPx*totalSellQty) as med_sell_sz from l2 where coin='${aVar[0]}' and sellPx1 > 0 and buyPx1 > 0 and dt >= 20240401 group by dt" 
  file_name="${aVar[0]}_daily_stats.csv"
  duckdb /mnt/data/crypto/hyperliquid/hyper-historical.db --readonly --csv "$sqlcmd" > "$file_name"
fi
done < book_median_2024q1.csv #created by liquidity_stats.R