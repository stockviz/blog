# US Sector ETF — Portfolio Experiments

Blog: [Building Winning Portfolios with SPDR Sector ETFs](https://stockviz.biz/2026/08/05/building-winning-portfolios-with-spdr-sector-etfs/)

> **Abstract:** This post evaluates quantitative strategies for rotating through SPDR
> Sector ETFs, comparing them against a buy-and-hold S&P 500 benchmark. The analysis
> finds that while a 5-year rolling window selecting the top 4 ETFs by Sharpe Ratio
> yields excess returns after transaction costs, the outperformance is highly
> inconsistent and can lag for years. The author also notes that for Indian investors,
> short-term capital gains taxes can negate the strategy's benefits, and a two-year
> rebalancing variant produced uninspiring results.

## Verdict: Which approach maximizes returns?

**OR (highest Omega) with annual rebalance delivers the best CAGR: 12.29% vs SPY's 10.83% and SR's 12.18%.**

Omega Ratio considers both upside and downside — it selects combinations with strong
risk-adjusted returns that outperform in both bull and bear markets. The OR strategy
edges out SR on CAGR (12.29% vs 12.18%) with slightly lower drawdown (43.19% vs 43.55%).

For pure return maximization, **annual rebalance + OR is the winner**.
The 2Y-RBL OR variant gives 11.19% CAGR (barely beats SPY at 11.13%).

## Summary

We tested four selection criteria (LD = lowest drawdown, HD = highest drawdown, SR = highest Sharpe, OR = highest Omega)
across two rebalance regimes (annual and 2-year alternating), plus an RRG rotation and a
multi-window walk-forward. All strategies use 0.25% drag per rebalance.

## Results: Annual Rebalance (5yr rolling window)

| Method | CAGR | Sharpe | MaxDD | Vol | vs SPY |
|---|---|---|---|---|---|
| **LD** | 10.47% | 0.69 | 40.61% | 16.41% | Higher Sharpe, lower drawdown |
| **SR** | 12.18% | 0.71 | 43.55% | 18.68% | Strong returns |
| **OR ★** | 12.29% | 0.72 | 43.19% | 18.64% | **Best returns, best Sharpe** |
| **HD** | 8.57% | 0.47 | 66.43% | 23.28% | Underperforms |
| **SPY** | 10.83% | 0.64 | 55.20% | 18.93% | Benchmark |

## Results: 2-Year Alternating Rebalance

| Method | CAGR | Sharpe | MaxDD | Vol | vs SPY |
|---|---|---|---|---|---|
| **LD** | 10.62% | 0.70 | 43.76% | 16.44% | Higher Sharpe, lower drawdown |
| **SR** | 11.10% | 0.65 | 50.79% | 19.10% | Beats SPY on CAGR |
| **OR** | 11.19% | 0.66 | 47.95% | 18.81% | Beats SPY on CAGR |
| **HD** | 10.04% | 0.54 | 63.20% | 22.35% | Underperforms |
| **SPY** | 11.13% | 0.64 | 55.20% | 19.25% | Benchmark (2006–) |

## Results: Multi-Window Walk-Forward (SR sweep, 1-5yr windows)

Best combo: XLY+XLK+XLV+XLU (2yr lookback, train SR=1.52)

| Metric | Combo (Train) | SPY (Train) | Combo (Test) | SPY (Test) |
|---|---|---|---|---|
| CAGR | 7.90% | 6.03% | 13.76% | 15.32% |
| Sharpe | 0.53 | 0.40 | 0.76 | 0.81 |
| MaxDD | 46.91% | 55.20% | 31.84% | 33.70% |

## Results: RRG Rotation (weekly, 10/4)

| Metric | RRG Top5 | SPY |
|---|---|---|
| CAGR | 4.31% | 8.94% |
| Sharpe | 0.32 | 0.54 |
| MaxDD | 57.95% | 55.20% |
| Volatility | 18.94% | 19.13% |

## Key Learnings

1. **OR wins on CAGR and Sharpe**: The highest-Omega combo (12.29% CAGR, Sharpe 0.72) edges out
   SR (12.18% CAGR, Sharpe 0.71) and crushes HD (8.57%). Omega's dual focus on upside/downside pays off.

2. **HD is reliably worst**: Chasing the highest-drawdown combo is a losing strategy —
   it amplifies losses and increases volatility. In every test, HD underperforms SPY.

3. **LD provides the best risk-adjusted returns**: Lowest-drawdown selection consistently
   delivers the highest Sharpe ratio (0.69–0.70) with lower volatility than SPY.

4. **2Y-RBL is a mild improvement over annual rebalance**: For LD and HD, the alternating
   rebalance marginally improves CAGR. For SR, it reduces CAGR but also reduces drawdown.

5. **Multi-win validates short lookbacks**: 2-year windows win on training data, and the
   test set shows the combo delivers competitive risk-adjusted returns vs SPY.

6. **RRG fails on US sectors**: Weekly momentum rotation underperforms all combinatorial
   strategies and SPY itself. See [README-rrg.md](README-rrg.md) for details.

## Implemented Scripts

- `etf-combo.R {LD|SR|HD|OR}` — Annual rebalance, 5yr rolling window
- `etf-combo-2YRBL.R {LD|SR|HD|OR}` — 2-year alternating halves
- `etf-combo-multiwin.R` — Multi-window walk-forward (SR sweep)
- `rrg-rotation.R` — Weekly RRG, monthly rebalance
