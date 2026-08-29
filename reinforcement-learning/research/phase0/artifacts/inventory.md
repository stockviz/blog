# DataInventory — verified coverage

> built 2026-08-28T07:17:30.479467+00:00

## Daily [CLOSE] — SQL Server StockViz
| Table | Scope | Start | End | Rows |
|---|---|---|---|---|
| bhav_index | NIFTY 50 | 1990-07-03 | 2026-08-27 | 8,790 |
| bhav_index | NIFTY 50 TR | 1999-06-30 | 2026-08-27 | 6,757 |
| bhav_index | NIFTY MIDCAP 150 TR | 2005-04-01 | 2026-08-27 | 5,310 |
| bhav_index | NIFTY SMALLCAP 250 TR | 2005-04-01 | 2026-08-27 | 5,310 |
| VIX_HISTORY | all | 2009-03-03 | 2026-08-27 | 4,304 |
| BHAV_EQ_FUT | BANKNIFTY fut | 2005-06-10 | 2026-08-27 | 15,670 |
| BHAV_EQ_FUT | NIFTY fut | 2000-06-12 | 2026-08-27 | 19,275 |
| MOMENTUM_PROB | 2824 symbols | 2010-01-04 | 2026-08-27 | — |
| MOMENTUM_ABS | 2448 symbols | 2015-09-02 | 2026-08-27 | — |

## Intraday [INTRADAY] — Postgres StockVizDyn
| Table | Scope | Start (UTC) | End (UTC) | Rows | Days | tick epoch |
|---|---|---|---|---|---|---|
| zd_index_bars | INDIA VIX | 2015-01-09 03:45:00+00:00 | 2026-08-27 09:59:00+00:00 | 1,075,719 | 2880 | 1970 |
| zd_index_bars | NIFTY 100 | 2015-01-09 03:45:00+00:00 | 2026-08-27 09:59:00+00:00 | 1,075,686 | 2880 | 1970 |
| zd_index_bars | NIFTY 50 | 2015-01-09 03:45:00+00:00 | 2026-08-27 09:59:00+00:00 | 1,076,110 | 2881 | 1970 |
| zd_index_bars | NIFTY BANK | 2015-01-09 03:45:00+00:00 | 2026-08-27 09:59:00+00:00 | 1,076,063 | 2881 | 1970 |
| zd_index_bars | NIFTY IT | 2015-01-09 03:45:00+00:00 | 2026-08-27 09:59:00+00:00 | 1,076,032 | 2881 | 1970 |
| zd_index_bars | NIFTY MIDCAP 50 | 2015-01-09 03:45:00+00:00 | 2026-08-27 09:59:00+00:00 | 1,076,028 | 2881 | 1970 |
| zd_option_bars | est 2,358,447,104 rows | 2025-01-28 09:15:00+00:00 | 2026-08-27 15:39:00+00:00 | sample 31,532 (tok 17512194) | — | 1990 |
| zd_bars_mcx | est 64,419,240 rows | 2025-09-17 17:19:00+00:00 | 2026-08-27 17:59:00+00:00 | sample 230,722 (tok 120761863) | — | 1970 |