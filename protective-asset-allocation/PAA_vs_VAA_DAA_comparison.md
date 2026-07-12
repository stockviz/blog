# PAA vs. VAA vs. DAA: A Comparison

*Based on Keller & Keuning's paper series: Protective Asset Allocation (PAA, 2016), Vigilant Asset Allocation (VAA, 2017), and Defensive Asset Allocation (DAA, 2018)*

## 1. How the three models differ

All three are by the same authors and share the same DNA: dual momentum (absolute + relative) plus a "breadth" indicator that scales the bond/cash fraction based on how many assets in a universe currently have positive momentum. Where they diverge:

| Feature | PAA (2016) | VAA (2017) | DAA (2018) |
|---|---|---|---|
| Momentum filter | SMA(12) — a slow, smoothed 12-month filter | "13612W" — a fast, recency-weighted blend of 1/3/6/12-month returns (1-month return weighted 12x, 12-month weighted 1x) | Same 13612W fast filter as VAA |
| Portfolio construction | Equal-weight basket of the Top-6 (out of 12) risky assets, blended with a bond fraction (BF) | Winner-take-all: 100% of the portfolio in a single best asset (offensive or defensive), no blending | Top-N risky assets by relative momentum, but crash-protection is decided by a separate universe |
| Crash-protection signal | Breadth of the *same* 12-asset risky universe you invest in | Breadth of the *same* risky universe you invest in | Breadth of a small, separate **"canary" universe** (e.g., VWO/EEM + BND/AGG) that is not otherwise invested in |
| Bond allocation style | Gradual: BF is a continuous fraction (0–100%) depending on the number of bad assets | Binary/aggressive: either fully invested in risky assets or fully in defensive assets — no partial blend | Gradual like PAA, but sized off the canary universe rather than the full risky universe |
| Turnover | Moderate (slow filter, blended weights) | Very high (~700%/year reported for the aggressive variant) — fast filter reacts to every wiggle | High, but somewhat lower than VAA since only a couple of canary assets need to flip |
| Design goal | Meet or beat a 1-year term deposit ("absolute return") with high win-rates and small drawdowns | Maximize offensive return while still cutting large drawdowns — more aggressive than PAA | Keep VAA's crash protection but reduce the average cash drag by decoupling the protection signal from the investable universe |

In short: **PAA** blends into bonds gradually and smooths its momentum signal, prioritizing steadiness. **VAA** swaps the smooth filter for a twitchy one and goes all-in on a single winning asset each month, prioritizing return. **DAA** keeps VAA's aggressive style but tries to fix its biggest side effect — being in cash too often — by only asking a small "canary" universe whether trouble is brewing, rather than the whole risky universe.

## 2. Drawbacks of PAA relative to VAA and DAA

- **Lower returns, generally.** In the authors' own tests, VAA and DAA post higher full-sample CAGRs than PAA2, because PAA2's aggressive bond-shifting (up to 100% bonds) sacrifices some upside to buy its low drawdowns.
- **Blunter protection trigger.** PAA ties its bond fraction to the very same universe it trades. If the risky universe as a whole drifts bearish for idiosyncratic reasons (not a real "risk-off" event), PAA still de-risks — this can mean unnecessary time out of the market. DAA's canary-universe approach is explicitly designed to reduce this false-positive tendency and lower average cash drag.
- **Diversification vs. concentration trade-off cuts both ways.** PAA's Top-6 equal-weight sleeve is more diversified than VAA's single-asset, winner-take-all approach — but that diversification is also why PAA gives up some of VAA's offensive punch. It's not a strict downside, but it is the reason PAA underperforms VAA/DAA on raw return.
- **Slower-reacting filter (SMA12) vs. 13612W.** This cuts turnover and cost, but also means PAA can be slower to re-enter after a crash or slower to protect against a fast, sudden drawdown compared to VAA/DAA's fast filter.

## 3. Obvious faults / weaknesses in the PAA approach itself

1. **In-sample optimization / data snooping.** The paper openly selects L=12, Top=6, a=2 by scanning 72 parameter combinations on the in-sample period and picking the best. Even with the authors' own Sharpe-ratio haircut adjustments, this is a classic overfitting risk — the "best" parameters are, by construction, flattering for that specific historical window.
2. **Small number of independent market regimes tested.** 45 years sounds like a lot of months, but it contains only one real secular rate-hike era (1970s–1981) and a handful of major bear markets (1973–74, 2000–02, 2008–09). The "absolute return" claim rests heavily on how PAA handled these few episodes, not a large sample of independent crash events.
3. **The Win0/Win5 criteria are somewhat arbitrarily chosen thresholds** (95%/99%) and were used as the *selection criterion* for choosing the winning parameter set. This is circular: the model is praised for meeting a bar it was explicitly tuned to meet.
4. **Survivorship and construction issues in the underlying data.** Most of the ETF price histories before the funds actually existed are synthetic proxies built from index/yield data (explained in Appendix A). Proxy construction always introduces some model risk (tracking-error minimization, fee assumptions, roll-yield assumptions for bond proxies) that a live, tradeable version of PAA didn't actually experience.
5. **Transaction costs are a simplifying assumption (flat 0.1% one-way), acknowledged by the authors as too low for early decades and possibly too high for later ones** — a small, uniform fee assumption is unlikely to reflect real slippage, bid/ask spreads, or market impact, especially for the less liquid assets (GSG, HYG, EEM in the 1970s).
6. **Regime dependence on the "cash" proxy (IEF).** A large share of PAA2's protective benefit comes from IEF behaving as a negatively-correlated hedge during equity sell-offs. That correlation is a historical regularity, not a law — in a regime where bonds and stocks sell off together (e.g., 2022-style), the "safety" of the bond sleeve is not guaranteed, and the paper's own section 5 shows results are sensitive to which bond is used as "cash."
7. **Discrete jumps in allocation.** Both n (number of good assets) and the bond fraction move in coarse steps (twelfths of the universe), so small changes in momentum near a threshold can cause outsized month-to-month portfolio shifts — a form of trigger-happy discreteness rather than a smooth risk dial.
8. **Look-ahead-adjacent design choices.** Choosing to test the IS period 1970–1992 specifically because it contains both a rising- and falling-rate regime (rather than, say, a random split) means the in-sample period was hand-picked with the benefit of hindsight about macro history — reasonable for research purposes, but it weakens claims of a truly "blind" backtest methodology.
9. **The strategy is long-only and un-levered**, and by design gives up return in strong bull markets (explicitly acknowledged in the paper) — so its favorable Sharpe/MAR ratios partly reflect a structurally more conservative posture rather than a free lunch.

## Bottom line

PAA is the more conservative, steadier member of the family — it trades some return for very consistent, bond-like risk metrics. VAA pushes the same idea further into offense (fast filter, all-or-nothing positioning) at the cost of far higher turnover and choppier month-to-month behavior. DAA tries to keep VAA's aggressiveness while fixing its main weakness (excessive time spent in cash) using a decoupled canary signal. None of the three escapes the deeper issues common to backtested tactical models: in-sample parameter selection, a limited number of independent stress-test episodes, reliance on historical stock/bond correlation, and simplified cost assumptions.
