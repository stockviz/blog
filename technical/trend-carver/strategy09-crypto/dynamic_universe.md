# Dynamic Universe Walk-Forward Backtest

Two scripts that run a monthly walk-forward backtest of Robert Carver's
Strategy Nine on the evolving Binance USDT coin universe.

## Common Structure

Both scripts follow the same framework:

```
1. PARSE ARGUMENTS
   - min_daily_bars  (default 500, min 256)
   - sharpe_lookback = min_daily_bars
   - min_sharpe_rows = floor(min_daily_bars / 2)
   - min_coins = 5

2. CONNECT to PostgreSQL (StockVizDyn on sweden)

3. FIND EARLIEST DATA
   - Query min(close_time) from binance_crypto_historical_1h
   - Generate month-end dates from earliest + 1 month to today

4. FOR EACH MONTH-END:
   a. DISCOVER current USDT coins (query DB for symbols active up to month-end)
   b. LOAD any new coins on demand (hourly → daily close, cache in memory)
   c. SELECT coins that qualify:
      - ≥ min_daily_bars of daily price history
      - Run Strategy Nine on historical data (no look-forward)
      - Compute binary strategy returns on historical data
      - Positive Sharpe ratio over last sharpe_lookback days
   d. LOG selection to CSV (date, coins, count)
   e. If ≥ min_coins selected → START TRADING
   f. For the NEXT month, each trading day:
      - For each selected coin:
        * Compute signal from data up to day-1 (no look-forward)
        * Get next-day return
        * Apply strategy rules
      - Average across coins (equal-weight)
   g. APPEND daily portfolio returns

5. SAVE portfolio returns CSV + cumulative chart vs BTCUSDT B&H
```

## script_dynamic_universe_BLS-EW.R

**Binary Long-Short, Equal-Weight**

Selection and trading rules:
- signal = 1 → go LONG (+next-day return)
- signal = 0 → go SHORT (−next-day return)
- Drag = 0.2% applied on binary signal changes

Outputs:
- `dynamic-universe-selection-bls-ew.csv`
- `dynamic-universe-returns-bls-ew.csv`
- `dynamic-universe-bls-ew.cumret.png`

## script_dynamic_universe_BLO-EW.R

**Binary Long-Only, Equal-Weight**

Selection and trading rules:
- signal = 1 → go LONG (+next-day return)
- signal = 0 → stay FLAT (no position, 0 return)
- Drag = 0.2% applied on binary signal changes

Outputs:
- `dynamic-universe-selection-blo-ew.csv`
- `dynamic-universe-returns-blo-ew.csv`
- `dynamic-universe-blo-ew.cumret.png`

## Key Design Decisions

- **No look-forward**: Signal for day D uses only data up to D−1. Selection
  for month M uses only data up to month-end M.
- **Dynamic universe**: Coins are re-discovered from the DB each month. Newly
  listed coins enter automatically once they accumulate enough history.
- **Sharpe filter on recent window**: Only the last N days (sharpe_lookback)
  are used for selection, not the entire history. This favors coins with
  recent positive trend-following performance.
- **Minimum coin threshold**: Trading only begins when ≥5 coins qualify,
  preventing single-coin concentration.
- **Equal-weight across selected coins**: Simple average of daily returns,
  rebalanced monthly as the selection changes.
