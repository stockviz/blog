# Strategy Nine — Robert Carver Backtests

**Blog post:** [Strategy 9](https://stockviz.biz/2026/06/23/strategy-9/)

> In Rob Carver's Advanced Futures Trading Strategies, there's a chapter,
> "Strategy Nine: Multiple Trend Following Rules," that uses composite
> trend-following rules to drive a long-short strategy. We explore the
> strategy through an Indian market participant's lens.

## What's here

Two R scripts that implement and backtest **Strategy Nine** from Robert
Carver's book *Advanced Futures Trading Strategies* (2023).

- `strategy_nine.R` — the strategy implementation (a reusable function)
- `backtest.R` — runs the strategy on four Nifty indices and generates
  charts and tables

## The strategy in plain English

Strategy Nine combines **six trend-following filters** of different speeds
into a single trading signal. Each filter compares a short-term moving
average to a long-term moving average:

| Filter | Fast EMA | Slow EMA | Speed |
|--------|----------|----------|-------|
| EWMAC2 | 2 days | 8 days | Very fast |
| EWMAC4 | 4 days | 16 days | Fast |
| EWMAC8 | 8 days | 32 days | Medium |
| EWMAC16 | 16 days | 64 days | Medium-slow |
| EWMAC32 | 32 days | 128 days | Slow |
| EWMAC64 | 64 days | 256 days | Very slow |

All six are combined with **equal weight** (each contributes 1/6th of the
final signal). The idea: if most filters agree the trend is up, you go long.
If most agree it's down, you stay out or go short.

## Cost screen

Faster filters trade more often, which costs more. An optional **cost screen**
drops any filter whose expected annual trading cost exceeds a threshold
(0.15 Sharpe-ratio units per year). With typical costs (`cost_per_trade_sr
= 0.0088`), this usually eliminates EWMAC2 and EWMAC4, leaving only the
slower filters active.

## What backtest.R does

1. **Fetches price data** for NIFTY 50 TR, NIFTY MIDCAP 150 TR, NIFTY
   SMALLCAP 250 TR, and NIFTY BANK TR from a SQL Server database
2. **Runs Strategy Nine** in several configurations:
   - Equal weight vs cost screen
   - Long/flat (binary signal: go long or stay out)
   - Long/short (binary signal: go long or go short)
   - Scaled long-only (position size proportional to signal strength)
   - Scaled long-short (direction + size from signal strength)
3. **Compares against benchmarks**: Buy & Hold, 50/20-day SMA strategies,
   and a 50-50 blend of SMA + Strategy Nine
4. **Calculates** annualized returns, Sharpe ratios, and annual max
   drawdowns for every scenario
5. **Generates** cumulative return charts, annual performance tables, and
   a summary gt table

## Running it

```r
cd /mnt/data/blog/technical/trend-carver/strategy09
Rscript backtest.R
```

Requires access to the StockViz SQL Server database (`norway`).

## Output files

### Summary
- `strategy09-results.html` / `.png` — master gt table with all strategies

### Per-index charts and tables
- `{Index}.strategy09.cumret.png` — long-only cumulative returns
- `{Index}.strategy09.ls.cumret.png` — long/short cumulative returns
- `{Index}.strategy09.compare.cumret.png` — long-only vs long/short vs B&H
- `{Index}.annual.returns.table.png` — annual returns (5 scenarios)
- `{Index}.annual.drawdowns.table.png` — annual max drawdowns
- `{Index}.annual.sharpe.table.png` — annual Sharpe ratios

### SMA comparison
- `{Index}.sma.compare.cumret.png`
- `{Index}.sma.annual.returns.table.png`
- `{Index}.sma.annual.drawdowns.table.png`
- `{Index}.sma.annual.sharpe.table.png`

### 50-50 blend
- `{Index}.blend.compare.cumret.png`
- `{Index}.blend.annual.returns.table.png`
- `{Index}.blend.annual.drawdowns.table.png`
- `{Index}.blend.annual.sharpe.table.png`

### Scaled forecast strategies
- `{Index}.scaled.equal_weight.cumret.png`
- `{Index}.scaled.cost_screen.cumret.png`

## Table conventions

- **Dark red** numbers = negative returns or drawdowns > 20%
- **Dark green** numbers = drawdowns < 10%
- **Boxed** numbers = beats B&H by more than 2 percentage points
- **Light green row** = equal weight; **light orange row** = cost screen
- All tables carry an `@StockViz` source note

## The winning strategy

After testing every combination, the **best risk-adjusted outcome** comes
from a simple 50-50 blend of two things:

1. **Strategy Nine Scaled Long-Only** on NIFTY MIDCAP 150 TR
2. **Strategy Nine Scaled Long-Only** on NIFTY SMALLCAP 250 TR

Both use equal-weight filters (no cost screen). The "scaled" part means the
position size is proportional to the forecast strength — you invest more
when the signal is strong and less when it's weak, rather than going
all-in or all-out.

### Why this wins

| Metric | 50-50 ScLO | At 2× Leverage |
|--------|-----------|----------------|
| Annualized Return | 9.4% | 18.9% |
| Sharpe Ratio | 1.18 | 1.18 |
| Max Drawdown | 13.1% | 26.1% |

Compared to the binary long-only strategy (which has similar Sharpe around
1.3), the scaled approach has **half the drawdown**. At 2× leverage, the
binary strategy would hit a 55% drawdown — a portfolio killer — while the
scaled version stays at a manageable 26%.

### What's excluded and why

- **NIFTY 50 TR**: Sharpe never exceeds 0.56 across all strategy variants.
  The strategy simply doesn't trend well on large caps.
- **NIFTY BANK TR**: Same story — best Sharpe is 0.68, and drawdowns are
  severe.
- **Cost screen**: Reduces returns by 30-40% on MIDCAP/SMALLCAP without
  meaningfully improving risk-adjusted metrics. Not worth it for these
  indices.
- **Long/short**: Adds complexity and higher drawdowns without improving
  Sharpe enough to justify it. The short side frequently loses money.

### Winning strategy output files

- `winning-strategy.cumret.png` — cumulative returns (6 lines comparing
  individual and combined strategies)
- `winning-strategy.annual.returns.png` — annual returns table
- `winning-strategy.annual.drawdowns.png` — annual max drawdown table
- `winning-strategy.annual.sharpe.png` — annual Sharpe ratio table
