# Summary: Breadth Momentum and Vigilant Asset Allocation (VAA)

**Authors:** Wouter J. Keller and Jan Willem Keuning (2017)
**Source:** SSRN 3002624

## Overview

VAA (Vigilant Asset Allocation) is a tactical, long-only asset allocation strategy that combines relative momentum (picking the best-performing assets in a universe) with a fast, aggressive form of crash protection the authors call *breadth momentum*. It is a follow-up to the authors' earlier PAA (Protective Asset Allocation) model, with the explicit design goal of "winning more by losing less": target annual returns above 10%, with maximum drawdowns kept below 20%, and ideally below 15%.

## Core Mechanics

**Momentum filter (13612W):** Instead of using simple trailing 12-month returns, VAA scores each asset using a weighted blend of average annualized returns over the past 1, 3, 6, and 12 months, with heavy weight (40%) placed on the most recent month. This makes the signal much faster/more responsive than traditional momentum filters like SMA12 or the standard "13612" filter. The same filter is used for both relative momentum (ranking assets) and absolute momentum (classifying an asset as "good" or "bad" based on whether its score is positive or non-positive).

**Breadth-based crash protection:** Rather than replacing individual "bad" assets with cash one at a time (as traditional dual momentum does), VAA counts the total number of bad assets (b) in the universe and compares it to a breadth threshold (B). The cash fraction is CF = b/B (capped at 100%). This means the whole portfolio can be pushed entirely into cash even when only one or a few assets in the universe have gone negative — a far more aggressive and "vigilant" trigger than conventional trend-following overlays. An "Easy Trading" rounding rule maps this cash fraction onto whole-asset replacements (rather than partially trimming every position) to reduce unnecessary turnover.

**Cash universe:** When out of the market, VAA doesn't sit in a single risk-free asset. It selects the best-performing of several bond-like instruments (e.g., short Treasuries, intermediate Treasuries, investment-grade corporates), using the same momentum filter but without an absolute-momentum sign filter.

**Optimization metric (RAD):** Rather than optimizing Sharpe ratio or the Calmar/MAR ratio, the authors introduce "Returns Adjusted for Drawdowns" (RAD), a metric that harshly penalizes strategies as maximum drawdown approaches 50% (the rough threshold at which a hedge fund would face redemptions/liquidation). This reflects their view that most investors care more about drawdown than volatility.

## Results

Testing across four asset universes — two global (12 and 4 assets, back to 1969) and two US-only (15 and 6 assets, back to 1925) — VAA achieved out-of-sample annual returns above 10% with maximum drawdowns below 15% in all four cases, while sitting in cash roughly 50-60% of the time on average. It generally outperformed both a traditional dual-momentum benchmark and simple buy-and-hold/60-40 benchmarks on drawdown-adjusted metrics, though not always on raw return.

## The "Knee" Pattern

One of the more notable empirical findings in the paper is a recurring bend, or "knee," in the relationship between return (R) and maximum drawdown (D) as the breadth threshold B is varied while holding the number of top assets (T) fixed.

Tracing R against D as B increases from 0 up toward N produces a distinctly kinked (not smooth) curve:

- **Before the knee** (low B, very aggressive/vigorous crash protection): increasing B causes return to climb quickly while drawdown stays roughly flat. Each extra unit of B "buys" a lot of return for almost no added risk.
- **At the knee**: this is the point of maximum efficiency — the best return achievable without incurring much additional drawdown.
- **After the knee** (higher B, looser/slower crash protection): the curve goes nearly flat — return barely improves further, while drawdown expands rapidly as B approaches N (i.e., approaches "never go to cash").

For example, in the VAA-G12 universe (with T=2), the knee occurs at B=4: moving from B=1 to B=4, return rises from about 13% to 21% while drawdown stays around 6-7%; moving from B=4 to B=6, return stays near 21% while drawdown balloons to about 21%. Similar knees appear in all four tested universes (VAA-G4 at B=1, VAA-U15 at B=3, VAA-U6 at B=1). Notably, the knee tends to sit at the **same B value in both the in-sample and out-of-sample periods**, which the authors take as evidence the pattern is not simply a data-mining artifact.

This knee is essentially an "elbow point" on a risk/return efficiency frontier for the breadth parameter — it marks the threshold beyond which loosening crash protection stops buying meaningful extra return and just adds drawdown. This is why RAD-based optimization (which penalizes drawdown heavily) tends to select B values right at or near the knee.

Importantly, the authors are explicit that they do not have a theoretical explanation for *why* this bend appears, or why its location is so stable across different universes and time periods. They describe it as "some remarkable timing characteristic" of breadth momentum and flag it as an open question for future research, alongside the need for genuine forward "live" testing beyond backtesting.

## Drawbacks and Limitations

1. **Datasnooping / overfitting risk.** VAA optimizes two free parameters (T and B) in-sample out of 16-36 possible combinations. Although the authors apply an in-sample/out-of-sample split and cite Harvey and Liu's (2013) haircut methodology to partially address this, the underlying choice of momentum filter (13612W itself), the four specific universes tested, and the cash-universe composition were not similarly out-of-sample validated — these were chosen based on prior work and some in-sample tuning, which the authors themselves acknowledge as a residual source of bias.

2. **Return is capped by heavy cash exposure.** Being out of the market roughly half to two-thirds of the time means a large share of the strategy's return comes from low-yielding cash instruments rather than risk premia. In the smallest US universe (VAA-U6), this cash drag combined with an extremely protective parameter setting (B=1) actually caused VAA to underperform simple buy-and-hold on raw return.

3. **False alarms / whipsaw.** Because the crash-protection trigger is deliberately "vigorous" — capable of moving the entire portfolio to cash when just one or two assets in a 12-asset universe turn negative — it will frequently exit the market ahead of ordinary corrections that don't turn into full crashes, giving up the subsequent recovery.

4. **High turnover and transaction costs.** The strategy's monthly turnover is substantial (roughly 50-60% per month in the backtests), with total annual transaction costs of around 1.2-1.4% even at a modest assumed 0.10% per-trade cost — a cost that would be even more punishing with wider bid-ask spreads, less liquid ETFs, or a smaller account size.

5. **Sensitivity to universe composition.** The results depend heavily on which specific ETFs/assets are included (the paper's own footnotes show materially different Sharpe, drawdown, and RAD figures for a similar GEM-style strategy depending on whether Vanguard or iShares ETFs were used), meaning practical implementation is sensitive to fund selection, fee structure, and tracking error.

6. **Limited independent history for statistical confidence.** Even with backtests spanning up to 90 years, the number of truly independent "crash" events (the 1930s, 2008-09, etc.) is small. A single severe drawdown episode can disproportionately shape both the optimal parameters and the perceived effectiveness of the crash-protection mechanism, meaning the strategy's true edge is difficult to establish with high statistical confidence.

7. **Unresolved "why" question.** The authors themselves note they cannot fully explain *why* the breadth-momentum mechanism produces the "knee" pattern they observe in the return/drawdown tradeoff, and explicitly flag this, along with the strategy's real-world persistence, as an open question requiring further research — including out-of-sample "live" testing beyond backtesting.
