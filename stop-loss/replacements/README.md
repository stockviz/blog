# Replacements Added After Stop Loss

For the same two advisor models audited in [`../static-trailing`](../static-trailing)
([Momo (Relative) v1.1](https://stockviz.biz/theme-eq/1A6C40B8-BDF1-43E5-829C-E3265BDB7F1A) and
[Momo (Velocity) v1.0](https://stockviz.biz/theme-eq/AFD0DFFF-2EA7-4E4D-BA50-D9CC0E4B5052)),
this note studies the **replacements**: the symbols that were *added* at the next
portfolio snapshot after a stop-loss exit.

This is the same opportunity-cost test applied to what was *bought*: ret > 0
means the replacement gained after entry; ret < 0 means it fell.

## What the statics say — replacements

### Combined (both models)

| Horizon | N    | Mean   | Median | SD     | % > 0 | % < 0 | Mean if >0 | Mean if <0 | t-stat | p     |
|--------:|-----:|-------:|-------:|-------:|------:|------:|-----------:|-----------:|-------:|-------|
| 1d      | 9270 | 0.08%  | -0.06% | 2.83%  | 48.2% | 51.2% | 2.07%      | −1.79%     | 2.72  | 6.5e-03 |
| 5d      | 9257 | 0.60%  | 0.13%  | 6.33%  | 51.2% | 48.5% | 4.94%      | −3.97%     | 9.10  | 1.1e-19 |
| 10d     | 9248 | 1.16%  | 0.44%  | 9.06%  | 52.4% | 47.5% | 7.29%      | −5.60%     | 12.26 | 2.6e-34 |
| 20d     | 9225 | 2.07%  | 1.07%  | 12.91% | 54.0% | 45.9% | 10.54%     | −7.89%     | 15.44 | 4.3e-53 |

[`metrics-combined.png`](./metrics-combined.png) ([`.html`](./metrics-combined.html)) — [`tail-rates.png`](./tail-rates.png) ([`.html`](./tail-rates.html)) for 10/20d tails:

| Horizon | % > +3% | % > +5% | % > +10% | % < −3% | % < −5% | % < −10% |
|--------:|--------:|--------:|---------:|--------:|--------:|---------:|
| 10d     | 25.2%   | 16.6%   | 6.8%     | 17.3%   | 11.4%   | 4.9%     |
| 20d     | 31.4%   | 22.3%   | 10.8%    | 20.1%   | 14.6%   | 7.6%     |

Replacements are also drift-positive: mean and median rise with horizon
(−0.06% at 1d → 1.07% at 20d), majority positive from 5d onward (51–54%),
winners +10.5% vs losers −7.9% at 20d. The 1-day window is the only
marginally weak one (48% positive, though still mean +8 bps, p=0.006).

### By entry year (20d)

| Year | N    | Mean   | % > 0 |
|-----:|-----:|-------:|------:|
| 2016 | 418  | 0.92%  | 54.5% |
| 2017 | 992  | 5.22%  | 64.6% |
| 2018 | 1027 | -1.12% | 42.2% |
| 2019 | 808  | 0.77%  | 50.1% |
| 2020 | 984  | 2.46%  | 59.1% |
| 2021 | 999  | 4.52%  | 60.0% |
| 2022 | 1042 | -0.47% | 46.8% |
| 2023 | 795  | 6.07%  | 67.8% |
| 2024 | 1014 | 1.56%  | 52.2% |
| 2025 | 834  | 0.22%  | 49.6% |
| 2026 | 312  | 3.27%  | 59.9% |

[`metrics-annual-20d.png`](./metrics-annual-20d.png) ([`.html`](./metrics-annual-20d.html)) / [`annual-20d.png`](./annual-20d.png)
(also [`annual-5d.png`](./annual-5d.png) / [`metrics-annual-5d.png`](./metrics-annual-5d.png) and [`annual-10d.png`](./annual-10d.png) / [`metrics-annual-10d.png`](./metrics-annual-10d.png)) — the same
2017/2023 bull drag and 2018 bear benefit seen for the stops appear for
the replacements, because the replacement *is* a momentum pick entering
into the same regime.

## Files

* [`build.R`](./build.R) — Azure `ADVISOR_MODEL_PORTFOLIO` + Norway `ADVISOR_MODEL_PORTFOLIO_SL`
  diff (`SEQ_ID` → `NEXT_SEQ` → `added`), price-ratio / RSA cumulative, fingerprinted
  [`cache.rds`](./cache.rds) (`replacements-v1`, 9,277 replacements)
* [`analysis.R`](./analysis.R) — `compute_horizon_stats`, gt tables ([`metrics-combined`](./metrics-combined.html) / [`metrics-combined.png`](./metrics-combined.png),
  [`metrics-annual-5d`](./metrics-annual-5d.html) / [`metrics-annual-5d.png`](./metrics-annual-5d.png), [`metrics-annual-10d`](./metrics-annual-10d.html) / [`metrics-annual-10d.png`](./metrics-annual-10d.png), [`metrics-annual-20d`](./metrics-annual-20d.html) / [`metrics-annual-20d.png`](./metrics-annual-20d.png), [`tail-rates`](./tail-rates.html) / [`tail-rates.png`](./tail-rates.png)), ggplot
  histograms ([`hist-5d.png`](./hist-5d.png), [`hist-10d.png`](./hist-10d.png), [`hist-20d.png`](./hist-20d.png)) / box ([`box-by-horizon.png`](./box-by-horizon.png)) / mean ([`mean-by-horizon.png`](./mean-by-horizon.png)) / annual violins ([`annual-5d.png`](./annual-5d.png), [`annual-10d.png`](./annual-10d.png), [`annual-20d.png`](./annual-20d.png)) (all with `@StockViz` caption and
  `tab_source_note`)
* [`tests.R`](./tests.R) — synthetic price-ratio, RSA cumulative, weekend fallback, fingerprint tests
* [`run.R`](./run.R) — `Rscript tests.R && Rscript build.R && Rscript analysis.R`
* [`cache.rds`](./cache.rds) — SL table, portfolio snapshots, replacements, forward returns
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
