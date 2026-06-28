# Robert Carver Backtests

## Strategy Nine

**Intro:** [Strategy 9](https://stockviz.biz/2026/06/23/strategy-9/)
> Explores Carver's composite trend-following strategy through an Indian lens — long-only vs long-short, binary vs scaled, equal-weight vs cost-screen across NIFTY indices. Cost screens hurt more than help; MIDCAP and SMALLCAP show promise while NIFTY 50 and BANK underperform.

**15 Instruments:** [Strategy 9 with 15 Instruments](https://stockviz.biz/2026/06/25/strategy-9-with-15-instruments/)
> Expands to 15 instruments across NIFTY, crypto, MCX commodities and US ETFs. Tests scaled vs binary × long-only vs long-short × equal-weight vs inverse-vol. Only scaled long-only equal-weight holds up; crypto dominates contributions.

**Crypto Universe:** [Strategy 9 with Crypto](https://stockviz.biz/2026/06/26/strategy-9-with-crypto/)
> Expands to all 21 pre-2019 Binance USDT coins. Binary long-only equal-weight delivers the highest returns but with a 60% drawdown, ruling out leverage. Adding more coins beyond the Big 3 does not improve the portfolio; many have negative trend-following contributions.

**Dynamic Universe:** [Strategy 9 with Dynamic Universe Selection](https://stockviz.biz/2026/06/27/strategy-9-with-dynamic-universe-selection/)
> Walk-forward monthly re-selection from the full Binance USDT universe. Coins qualify with ≥500 days history and positive Sharpe (or Hurst > 0.55). Neither approach improves on the static universe; dynamic selection adds complexity without better risk-adjusted returns.

**MSCI Equity Indices:** [Strategy 9 with Equity Indices](https://stockviz.biz/2026/06/28/strategy-9-with-equity-indices/)
> Applies Strategy 9 trend-following to a basket of MSCI country equity indices. A Binary Long-only strategy earns a higher Sharpe than buy & hold, but its drawdown profile makes it unleverageable — so it ultimately trails B&H returns. More importantly, yearly returns suggest something stopped working in 2009; the backtest performance appears back-loaded. Once again, Strategy 9 fails us.

## What We Learned

After running Strategy 9 across five increasingly broad universes — NIFTY indices, a 15-instrument multi-asset basket, 21 crypto coins, a dynamic walk-forward crypto universe, and MSCI country equity indices — a few patterns became impossible to ignore.

### 1. Scaled long-only is the only variant worth keeping

Across every experiment, scaled long-only (position size proportional to forecast strength) produced the best risk-adjusted returns. The short side of binary long-short strategies consistently loses money or adds drawdown without compensating Sharpe improvement. Binary long-only earns higher raw returns in some universes but with drawdowns that rule out leverage — making it strictly inferior to buy & hold on an absolute return basis.

### 2. The strategy output is unleverageable

This is the central failure. The whole point of trend-following is to produce a return stream smooth enough that you can apply leverage and beat buy & hold. Strategy 9 never achieves this. Even its best variants have drawdowns in the 25–60% range. At 2× leverage, a 30% unlevered drawdown becomes 60% — a portfolio killer. Without the ability to lever up safely, you trail B&H on absolute returns.

### 3. Expanding the universe yields diminishing returns

The Big 3 crypto coins (BTC, ETH, SOL) already capture most of what trend-following can extract from crypto. Adding 18 more coins, or dynamically re-selecting from the full universe each month, adds complexity without improving the portfolio. The best-performing 3–5 instruments drive the results; the rest contribute noise or outright negative returns.

### 4. Performance may be period-dependent

The MSCI equity experiment revealed that yearly returns effectively stopped working around 2009. What looks like a decent full-sample Sharpe may be entirely back-loaded — an artifact of pre-2009 returns that never recurred. The same question hangs over the other universes; we did not slice them the same way.

### 5. Cost screens hurt; inverse-vol weighting doesn't help

Carver's cost screen eliminates the faster filters (EWMAC2, EWMAC4), making the strategy sluggish and cutting returns 30–40% without meaningfully improving risk metrics. Inverse-volatility weighting, which should theoretically down-weight volatile losers and improve risk-adjusted returns, made no material difference versus simple equal-weight.

### 6. The best risk-adjusted result is narrow, scaled, long-only

A 50-50 equal-weight blend of Scaled Long-Only on NIFTY MIDCAP 150 and NIFTY SMALLCAP 250 produced a Sharpe of 1.18 with a 13.1% drawdown — the cleanest equity curve across all experiments. But even this trails B&H on unlevered absolute returns. At 2× leverage it earns 18.9% annualized with a 26% drawdown — the closest we got to a genuinely usable strategy.

### Bottom line

Strategy 9 does not survive real-world scrutiny. It finds trends, it earns positive Sharpe, but it cannot produce a return stream smooth enough to lever into genuine outperformance. The drawdowns are always too deep, the universe expansion never helps, and the equity index variant suggests whatever edge existed may have expired. This is a strategy that looks promising in theory and in plots — and fails on closer inspection.

