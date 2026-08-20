# Efficacy of a Non-Adaptive Trailing Stop Loss 

Two advisor models that use the **same static trailing stop** were audited:

* **Model A** [Momo (Relative) v1.1](https://stockviz.biz/theme-eq/1A6C40B8-BDF1-43E5-829C-E3265BDB7F1A) — 5,003 distinct symbol-dates
* **Model B** [Momo (Velocity) v1.0](https://stockviz.biz/theme-eq/AFD0DFFF-2EA7-4E4D-BA50-D9CC0E4B5052) — 4,893 distinct symbol-dates

Total **9,896 distinct SL triggers** covering 918 symbols from 2016-08-23 to
2026-08-19. For every trigger the forward return of the *same symbol* was measured **1, 5,
10 and 20 trading days after the SL date**.

This is an *opportunity-cost* test: **ret > 0 means the stop was costly**
(holding would have made money); ret < 0 means the stop saved a loss.

## What the statics say

### Combined (both models) — the headline

| Horizon | N    | Mean   | Median | SD     | % > 0 (costly) | % < 0 (saved) | Mean if >0 | Mean if <0 | t-stat | p     |
|--------:|-----:|-------:|-------:|-------:|----------------:|--------------:|-----------:|-----------:|-------:|-------|
| 1d      | 9889 | 0.12%  | 0.06%  | 2.87%  | 51.2%           | 48.5%         | 2.09%      | −1.97%     | 4.09  | 4.4e-05 |
| 5d      | 9878 | 0.48%  | 0.25%  | 6.57%  | 52.3%           | 47.5%         | 4.88%      | −4.37%     | 7.21  | 6.1e-13 |
| 10d     | 9869 | 1.12%  | 0.55%  | 9.33%  | 53.4%           | 46.3%         | 7.24%      | −5.93%     | 11.93 | 1.4e-32 |
| 20d     | 9850 | 2.17%  | 1.12%  | 13.28% | 54.8%           | 45.1%         | 10.69%     | −8.18%     | 16.21 | 2.4e-58 |

[`metrics-combined.png`](./metrics-combined.png) ([`.html`](./metrics-combined.html)) — [`tail-rates.png`](./tail-rates.png) ([`.html`](./tail-rates.html)) quantifies tails at the horizons
that matter for a holding decision:

| Horizon | % > +3% | % > +5% | % > +10% | % < −3% | % < −5% | % < −10% |
|--------:|--------:|--------:|---------:|--------:|--------:|---------:|
| 10d     | 26.0%   | 17.0%   | 7.0%     | 18.2%   | 12.3%   | 5.6%     |
| 20d     | 32.4%   | 23.4%   | 11.5%    | 21.0%   | 15.5%   | 8.5%     |

At 20 days you were almost **1.5× more likely to miss a >5% rally than to
avoid a >5% slide** (23.4% vs 15.5%). The right tail dominates: winners
average +10.7% when positive, losers average −8.2% when negative — the stop
is cutting the winners faster than it is cutting the losers.

**Interpretation: on average the static trailing stop was not worth it.**
Every horizon mean is *positive* and strongly significant (all p < 1e-4,
20d p ~ 1e-58). The median is positive at every horizon and rises with it
(0.06% → 1.12%). Buying and holding for 20 trading days after the trigger
would have beaten the stop by 2.2% on average, 1.1% at the median. The 1-day
effect is tiny (12 bps, barely distinguishable from noise), but it compounds
— the cost accumulates the longer the forgone holding period.

### By year (20d, combined) — where it did and did not pay

| Year | N    | Mean   | % > 0 | Regime in brief |
|-----:|-----:|-------:|------:|-----------------|
| 2016 | 422  | 1.08%  | 51.9% | post-Brexit drift |
| 2017 | 991  | 6.13%  | 65.7% | broad bull — stop highly costly |
| 2018 | 1096 | −1.76% | 41.1% | bear — only year where stop clearly saved |
| 2019 | 847  | 1.39%  | 55.7% | range-bound recovery |
| 2020 | 1042 | 2.25%  | 58.9% | Covid rebound — costly |
| 2021 | 1051 | 4.20%  | 59.8% | continuation bull |
| 2022 | 1068 | −0.28% | 46.1% | sideways/bear — wash |
| 2023 | 830  | 6.02%  | 68.7% | bull — most costly |
| 2024 | 1064 | 1.75%  | 50.7% | mixed |
| 2025 | 893  | 0.34%  | 48.5% | mixed |
| 2026 | 546  | 3.60%  | 60.3% | YTD bull |

[`metrics-annual-20d.png`](./metrics-annual-20d.png) ([`.html`](./metrics-annual-20d.html)) / [`annual-20d.png`](./annual-20d.png) (also [`annual-5d.png`](./annual-5d.png) / [`metrics-annual-5d.png`](./metrics-annual-5d.png) and [`annual-10d.png`](./annual-10d.png) / [`metrics-annual-10d.png`](./metrics-annual-10d.png) for 5- and 10-trading-day horizons) — 8 of 11 years the mean
20d forward return is positive (holding wins). Only 2018 (−1.76%,
41% positive, the single clean vindication) and marginally 2022 (−0.28%)
rewarded the stop on average. In strong bull years (2017, 2023) the drag
is 6% in 20 trading days — the classic trailing-stop problem of selling
into momentum that keeps trending.

## Were you better off holding?

**Yes, on average, at every horizon from 1 to 20 trading days.** The
evidence is:

* Mean and median > 0 at all horizons, with monotonic increase.
* Majority of triggers (51–55%) would have profited by holding; majority
  grows with horizon.
* Gains when right are larger than losses when wrong (10.7% vs 8.2% at
  20d).
* Statistical significance is overwhelming (t > 4 at 1d, t > 16 at 20d).

The stop did avoid large individual losses (8.5% of 20d windows fell >
10%, 5.6% of 10d windows), but it forwent even more large rallies.
A non-adaptive trailing level that triggers on normal volatility is
functionally a *profit cap* that converts a 918-stock diversified
momentum tail into a truncated distribution — you keep the 1–2% whipsaw
protection at 1d, but pay 1–2% in forgone return by 10–20d.

This does not prove stops are useless — it proves *this* static,
trailing formulation is. An adaptive stop (ATR-scaled, volatility-scaled,
or regime-conditioned), a wider buffer, or a time-stop (re-enter after
N days) would need to be tested to see if the 2018/2022 benefit can be
kept without the 2017/2023 drag. The forward-returns file is provided to
allow exactly that — every SL event is listed with 1/5/10/20d returns and
source flag.

## Files

* [`build.R`](./build.R) — SQL Server + Postgres extraction, price-ratio / RSA
  cumulative logic, fingerprinted [`cache.rds`](./cache.rds) (query version
  `static-trailing-v1`, 9,896 distinct SL events)
* [`analysis.R`](./analysis.R) — `compute_horizon_stats`, gt tables ([`metrics-combined`](./metrics-combined.html) / [`metrics-combined.png`](./metrics-combined.png),
  [`metrics-annual-5d`](./metrics-annual-5d.html) / [`metrics-annual-5d.png`](./metrics-annual-5d.png), [`metrics-annual-10d`](./metrics-annual-10d.html) / [`metrics-annual-10d.png`](./metrics-annual-10d.png), [`metrics-annual-20d`](./metrics-annual-20d.html) / [`metrics-annual-20d.png`](./metrics-annual-20d.png), [`tail-rates`](./tail-rates.html) / [`tail-rates.png`](./tail-rates.png)), ggplot
  histograms ([`hist-5d.png`](./hist-5d.png), [`hist-10d.png`](./hist-10d.png), [`hist-20d.png`](./hist-20d.png)) / box ([`box-by-horizon.png`](./box-by-horizon.png)) / mean ([`mean-by-horizon.png`](./mean-by-horizon.png)) / annual violins ([`annual-5d.png`](./annual-5d.png), [`annual-10d.png`](./annual-10d.png), [`annual-20d.png`](./annual-20d.png)) (all with `@StockViz` caption and
  `tab_source_note`)
* [`tests.R`](./tests.R) — synthetic price-ratio, RSA cumulative, weekend fallback,
  fingerprint tests
* [`run.R`](./run.R) — `Rscript tests.R && Rscript build.R && Rscript analysis.R`
* [`cache.rds`](./cache.rds) — SL table, PG/RSA prices, forward returns
* [`forward-returns.csv`](./forward-returns.csv) — audit table: `model,symbol,sl_date,horizon,ret,src`
* [`results.rds`](./results.rds) — aggregated stats bundles
* `*.png` / `*.html` — tables and charts (see links above)

## Reproduce

```bash
Rscript tests.R
Rscript build.R
Rscript analysis.R
# or
Rscript run.R
```

Generated from StockViz 
