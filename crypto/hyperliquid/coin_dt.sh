#! /bin/bash


sqlcmd="SET memory_limit = '32GB';select coin, dt, avg(wavgBuyPx*totalBuyQty) as avg_buy from l2 group by coin, dt"
duckdb /mnt/data/crypto/hyperliquid/hyper-historical.db --readonly --csv "$sqlcmd" > coin_book_dt_buy.csv

sqlcmd="SET memory_limit = '32GB';select coin, dt, avg(wavgSellPx*totalSellQty) as avg_sell from l2 group by coin, dt"
duckdb /mnt/data/crypto/hyperliquid/hyper-historical.db --readonly --csv "$sqlcmd" > coin_book_dt_sell.csv
