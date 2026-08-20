# Should You Let Winners Run? — What Two Stop-Loss Audits Say

**Bottom line:** for the static trailing stop used by these two StockViz advisor
models, you would have made more money by doing nothing — letting the stopped
positions ride and not replacing them — in about **5 out of 10 cases after 1 day
and 5.5 out of 10 after 20 days**, by about **2% on average over 20 trading
days**.

That is not an opinion. It is what 9,896 real stop-loss exits and 9,277 real
replacements did next, measured with adjusted prices. 98–99% resolve. 

## The two audits (same models, two sides of one trade)

Both studies use the same two live advisor models that share **one identical
static trailing stop** (a fixed-percent trailing level, not scaled to volatility
or regime):

* [Momo (Relative) v1.1](https://stockviz.biz/theme-eq/1A6C40B8-BDF1-43E5-829C-E3265BDB7F1A) — 5,003 stops / 4,741 replacements
* [Momo (Velocity) v1.0](https://stockviz.biz/theme-eq/AFD0DFFF-2EA7-4E4D-BA50-D9CC0E4B5052) — 4,893 stops / 4,536 replacements

| Study | What was measured | N | Period |
|-------|-------------------|--:|--------|
| [**1. What you sold**](./static-trailing/README.md) — `../static-trailing` | Forward return of the **stopped symbol itself** 1, 5, 10, 20 trading days after the stop date | 9,896 stops (918 symbols) | 2016-08-23 → 2026-08-19 |
| [**2. What you bought instead**](./replacements/README.md) — `../replacements` | Forward return of the **replacement symbol** 1, 5, 10, 20 trading days after its entry date | 9,277 replacements (914 symbols) | 2016-08-23 → 2026-08-19 |


> Think of it as an *opportunity-cost* test: `ret > 0` = the stop was costly
> (you would have been better holding); `ret < 0` = the stop saved you.

## What happened — the numbers, in plain language

### 1. The stock you sold usually went *up* after you sold it

From [`static-trailing/README.md`](./static-trailing/README.md) —

| Horizon (trading days) | Avg gain if you had held | How often holding would have won | Median |
|------------------------|-------------------------:|---------------------------------:|-------:|
| 1  | **+0.12%** | **51.2%** | +0.06% |
| 5  | **+0.48%** | **52.3%** | +0.25% |
| 10 | **+1.12%** | **53.4%** | +0.55% |
| 20 | **+2.17%** | **54.8%** | +1.12% |

All are statistically decisive (t = 4.1 at 1d, 16.2 at 20d, p < 0.0001).
Winners averaged +10.7% at 20d when they won; losers averaged −8.2% when they
lost — the wins were bigger and more frequent. At 20d you were
**1.5× more likely to miss a >5% rally than to avoid a >5% slide**
(23.4% vs 15.5% — see [`tail-rates.png`](./static-trailing/tail-rates.png)).

*Translation:* if you had just held the stopped stock for a month, you would
have been up 2% more on average, and up at the median too. The stop did save
some 8–10% drawdowns (8.5% of windows fell >10%), but it missed even more
10%+ rallies (11.5%).

Year-by-year (20d, [`annual-20d.png`](./static-trailing/annual-20d.png)) hammers
this home: **8 of 11 years** holding wins, often by 4–6% (2017 +6.13%,
2023 +6.02%). Only **2018 (−1.76%, 41% wins)** — the one clean bear year —
was the stop a clear saviour; 2022 was a wash (−0.28%). The static level
helped in a bear and hurt in every bull.

### 2. The stock you bought instead did *about the same*

From [`replacements/README.md`](./replacements/README.md) —

| Horizon | Avg gain of replacement | How often it went up |
|--------|------------------------:|---------------------:|
| 1  | **+0.08%** | 48.2% |
| 5  | **+0.60%** | 51.2% |
| 10 | **+1.16%** | 52.4% |
| 20 | **+2.07%** | 54.0% |

Compare side-by-side at 20d:

* What you **sold**: +2.17% avg, 54.8% wins
* What you **bought**: +2.07% avg, 54.0% wins — **a 10 bp difference**, well
  within noise.

You replaced a drift-positive basket with another drift-positive basket.
The churn bought you nothing — same distribution, same 1–6% annual drag in
bull years, just with turnover cost and slippage on top.

## Why “let it run” was better — a simple story

Imagine you own a mango tree. The static trailing stop is a rule that says
*“if a mango drops 8% from its high, shake the whole tree and sell it,
then immediately buy a neighbour’s tree.”*

In a real orchard (the market):

1. **Wind is not the same as disease.** A fixed 8% drop does not tell you
   whether the tree is sick (real trend break) or just windy (normal
   volatility). High-volatility stocks swing 2–3% a day; 8% can be a two-day
   wobble. An *adaptive* rule would scale with wind (ATR, realised vol,
   VIX regime) — this one did not, so it shook healthy trees.

2. **Mango trees bounce.** Stocks that just fell 8% are more likely to
   bounce than to keep falling — prices are noisy in the short run and
   trending in the medium run. That is why 51–55% of stopped stocks were
   *up* a few days later, and the up moves (+10.7%) out-sized the down moves
   (−8.2%). Cutting a dip often cuts the recovery.

3. **You swapped one mango tree for another mango tree.** The replacement
   was picked by the same momentum logic that picked the original. On average
   it did exactly what the sold one would have done (+2% in a month). You paid
   brokerage, spread, and the bid-ask to swap — and for the privilege of
   resetting the same bet.

4. **The bill compounds.** Missing 0.12% in one day sounds trivial. Miss it
   across 9,896 trades, let the winners be a little bigger than the losers,
   and hold for 20 days instead of 1 — and it becomes 2.2% per trade on
   average, 6% in a strong bull year like 2017 or 2023. That is the
   opportunity cost of a profit cap.

5. **Diversification already did the job.** With ~20 holdings across 914
   names over 10 years, the portfolio was already diversified. The stop
   trimmed a diversified, momentum-tilted tail into a *truncated* tail —
   you kept the small 1-day whipsaw protection and paid 1–2% for it by
   10–20 days.

None of this says “never use a stop.” It says *this* static, fixed-percent
trailing formulation is a **profit cap disguised as a risk control**.
A wider buffer, a volatility-scaled buffer (e.g. `2 × ATR`), a regime filter,
or a time-stop that buys back after N days would need to be tested to see
if the one good year (2018) can be kept without paying the eight bad ones.

## How to read the detail

* Start with [`static-trailing/README.md`](./static-trailing/README.md) for the full
  stop-loss distributions: [`metrics-combined.png`](./static-trailing/metrics-combined.png),
  [`hist-5d.png`](./static-trailing/hist-5d.png) / [`hist-10d.png`](./static-trailing/hist-10d.png) / [`hist-20d.png`](./static-trailing/hist-20d.png),
  [`box-by-horizon.png`](./static-trailing/box-by-horizon.png), and violins
  [`annual-5d.png`](./static-trailing/annual-5d.png) / [`annual-10d.png`](./static-trailing/annual-10d.png) / [`annual-20d.png`](./static-trailing/annual-20d.png).
* Then [`replacements/README.md`](./replacements/README.md) for the same charts but for
  replacements — note how similar the histograms and violins look.
* All tables are linked as `*.html` (interactive) and `*.png` (screenshots):
  [`metrics-combined`](./static-trailing/metrics-combined.html),
  [`metrics-annual-20d`](./static-trailing/metrics-annual-20d.html),
  [`tail-rates`](./static-trailing/tail-rates.html) — and the `replacements/` mirrors.
* Code: [`static-trailing/build.R`](./static-trailing/build.R) / [`analysis.R`](./static-trailing/analysis.R) and
  [`replacements/build.R`](./replacements/build.R) / [`analysis.R`](./replacements/analysis.R) are
  R pipelines with fingerprinted caches (`cache.rds`), `Rscript run.R` reproduces
  everything. See `*README.md#reproduce`.

## Caveats (read before trading this)

* This is a **forward-return audit**, not a portfolio backtest. It does not
  simulate what the NAV would have been without stops (that needs turnover,
  weight, and cash-drag modelling).
* Results are **unconditional** — averaged over all stops. Conditioning on
  VIX, drawdown, or stock-level volatility might find pockets where the stop
  helped.
* 2018 shows the stop *can* help; the question is whether you can harvest
  that without the 2017/2023 cost. A non-adaptive level cannot.
* Past adjusted-price behaviour (2016–2026) is not a guarantee.

---
*Generated from StockViz 
