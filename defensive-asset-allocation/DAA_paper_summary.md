# Summary: Breadth Momentum and the Canary Universe: Defensive Asset Allocation (DAA)
**Keller & Keuning (2018, rev. 2018)**

## Overview

This paper proposes Defensive Asset Allocation (DAA), an evolution of the authors' earlier Vigilant Asset Allocation (VAA) strategy. The key innovation is separating the "risky" asset universe (used for return generation via relative momentum/asset rotation) from a distinct "canary" universe (used solely to signal crash protection via breadth momentum). Crash protection is triggered when canary assets (identified as VWO and BND) show non-positive momentum, with the cash allocation scaling as b/B (number of bad canary assets over a breadth parameter B).

The stated aim: keep drawdown protection roughly comparable to VAA while substantially lowering the average cash fraction — improving returns, tracking error, and turnover in the process. Backtests span Dec 1926–Mar 2018, with the canary universe selected in-sample (1926–1970) and tested out-of-sample (1970–2018) across several risky-asset universes (G12, U6, U15, G4).

## Key Claimed Results

- The VWO/BND canary combination roughly halves average cash fraction versus VAA (e.g., ~25–30% vs. ~55–70%) while maintaining or improving returns and risk-adjusted metrics (K50, K25, UPI) for most universes.
- DAA underperforms VAA specifically on the small G4 universe, which the authors attribute to insufficient diversification.
- An "aggressive" variant (B=1, alternate cash/leverage instruments) produces higher returns with only modestly higher drawdowns.

## Drawbacks and Limitations

**1. Small, narrow in-sample canary search window.**
The canary universe (VWO, BND) was selected using only ~44 years of monthly data (Dec 1926–Dec 1970), and even more narrowly, it was chosen from a candidate set of only four assets (SPY, VEA, VWO, BND). This is a very limited search space and history from which to generalize a "universal" crash-prediction signal, especially since VWO (emerging markets) as an ETF/index concept didn't meaningfully exist over most of that historical period — the backtest necessarily relies on proxy/index data rather than tradable instruments.

**2. Overlapping/reused test periods across sections.**
While the authors emphasize avoiding "datasnooping" by separating the IS canary-selection period from the OS test period, the same OS period (1970–2018) is then reused repeatedly to select other free parameters (Top T for each universe, breadth B, cash universe variants, "aggressive" leveraged variants). Each additional round of in-sample optimization on subsets of this same broad OS window raises multiple-testing/overfitting risk that isn't fully addressed.

**3. Heavy reliance on non-tradable historical proxies.**
Much of the backtest (pre-1970, and in places pre-2000s) uses index proxies rather than actual tradable fund prices, and even the "own ETF proxies" for 1970 onward are approximations including estimated fees. This is disclosed by the authors but materially weakens confidence in reported returns, drawdowns, and cash fractions for the earlier decades that anchor the whole methodology.

**4. Weak economic rationale for the canary signal.**
The paper explicitly admits (in its own conclusion) that it has no worked-out theoretical explanation for *why* bad momentum in VWO/EEM specifically should presage crashes in developed-market equities, beyond speculative narratives (USD strength, EM currency sensitivity). Without a causal mechanism, there's meaningful risk that VWO/BND was selected partly because it fit the historical data well (data mining) rather than because it captures a persistent, structural relationship.

**5. Inconsistent performance across universes.**
DAA notably underperforms VAA for the G4 universe (small, concentrated) on return and risk metrics, and the authors' fix (enlarging to G6) is itself an ad hoc adjustment introduced after the fact rather than a principled design choice — again raising overfitting concerns.

**6. Parameter sensitivity and manual tuning.**
Multiple free parameters (Top T, breadth B, choice of cash universe C3, optional leverage) are each optimized per risky-asset universe via one-dimensional sweeps on K25/K50. The strategy's performance is thus the product of several nested optimization choices, which makes it harder to assess how much of the outperformance is signal versus fitting.

**7. Use of leveraged ETF proxies in the "aggressive" variant.**
Section 9's leveraged-asset extension (2x ETFs like SSO, QLD, UST) is based on simulated/fitted proxies rather than live leveraged fund histories for much of the period, and the authors themselves flag concerns about liquidity and the unrealistic assumption of stable borrowing costs across very different interest-rate regimes (e.g., 1980s vs. 2009+). The eye-catching return/drawdown figures in this section should be read with this caveat prominently in mind.

**8. Transaction costs are simplified.**
A flat one-way transaction cost of 0.1% is assumed throughout; this doesn't capture bid-ask spread variation, slippage in less liquid instruments (especially relevant for the leveraged variants), or the cost of historical periods with wider spreads.

**9. No out-of-sample validation beyond the paper's own end date.**
All results end at March 2018, and there's no live/forward-tested track record included in the paper itself to confirm the strategy continued to perform as backtested once published (a general concern for any factor/timing strategy, since publication itself can erode an edge).

**10. Survivorship and universe-construction choices are not deeply interrogated.**
The specific composition of the G12/U6/U15/G4 "risky" universes (which ETFs/asset classes are included) is inherited from the earlier VAA paper without extensive fresh justification here, so any biases in those original choices propagate into DAA's results.

## Bottom Line

DAA's core contribution — decoupling the crash-signal universe from the return-generating universe — is a reasonable and intuitively appealing idea, and the reported reduction in average cash fraction (while preserving much of VAA's drawdown protection) is the paper's most credible and useful finding. However, the result rests on a narrow historical search for the canary assets, reuses the out-of-sample period for multiple rounds of parameter tuning, leans on non-tradable proxy data for large stretches of the backtest, and offers no firm economic theory for why the chosen canary assets should keep working going forward. These are typical weaknesses of backtested tactical asset allocation research and warrant real caution before relying on the strategy with live capital.
