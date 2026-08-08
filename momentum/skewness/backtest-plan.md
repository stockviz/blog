Given the data you have — **NSE-only stocks, adjusted daily prices, industry classification, and free-float market cap** — I would design this as a **long-only Indian momentum strategy with an expected-skewness overlay**, while keeping the skewness forecasting procedure as close as possible to the paper.

The key distinction is that the paper uses skewness on **both the long and short legs**. For a long-only strategy, there is no short leg. So the natural adaptation is:

> **First identify momentum winners; then, among those winners, prefer stocks with high predicted next-month skewness.**

That preserves the paper's economic intuition: on the long side, we want stocks with greater potential for extreme positive returns. The paper explicitly uses high expected skewness for its long leg. 

---

# 1. Strategy we're going to backtest

I would make the **primary strategy**:

[
\boxed{
\text{NSE Momentum + Expected Skewness}
}
]

with this sequence:

```text
NSE universe
    ↓
Data-quality filters
    ↓
Calculate 12-2 momentum
    ↓
Select momentum winners
    ↓
Estimate expected next-month skewness
    ↓
Select high-skewness stocks among momentum winners
    ↓
Construct long-only portfolio
    ↓
Hold for one month
    ↓
Rebalance
```

The critical point is that **momentum determines eligibility**, while **expected skewness determines which momentum stocks we actually own**.

---

# 2. What I would NOT do

I would not simply rank every Indian stock on:

[
Momentum + Skewness
]

and combine the two scores arbitrarily.

That would make it difficult to determine whether the result comes from momentum or skewness.

Instead, use **sequential sorts**, consistent with the paper's methodology. The paper first sorts on the anomaly and then sorts within the anomaly portfolios on expected skewness. 

For our long-only version:

[
\boxed{
\text{Momentum sort}
\rightarrow
\text{Expected-skewness sort}
}
]

---

# 3. Data requirements

You already have four critical ingredients:

### Required

1. **Adjusted daily prices**
2. **NSE security identifier**
3. **Industry classification**
4. **Free-float market capitalization**

I'd additionally want:

5. Trading dates
6. Security listing/delisting dates if available
7. Corporate-action-adjusted prices
8. A reliable way to identify suspended/non-trading stocks
9. Ideally daily volume/turnover for robustness and transaction-cost analysis

The first four are enough for the **core signal**, but volume/liquidity data becomes very valuable for implementation.

---

# 4. Define the investment universe

At each month-end (t), construct:

[
U_t={\text{NSE-listed eligible equities at }t}
]

I would **not use today's surviving NSE stocks for the entire historical backtest**.

This is essential.

A stock that disappeared in 2012 must be present in the 2008–2012 universe if it was eligible then.

Otherwise you introduce survivorship bias.

---

# 5. Basic security filters

I'd start with the following.

### Include

* NSE ordinary equity shares
* active/listed securities
* stocks with valid adjusted daily prices
* stocks with sufficient daily observations

### Exclude

* ETFs
* mutual funds
* derivatives
* preference shares
* warrants
* obviously non-equity securities
* duplicate/security-class problems

I'd also maintain a separate treatment for IPOs.

---

# 6. Minimum daily-data requirement

The paper calculates realized skewness and volatility from daily returns within a month. 

For India, I'd impose:

[
N_{i,t}\geq15
]

valid daily observations in month (t).

This is an **India implementation choice**, not something I'm claiming the paper explicitly requires for its baseline skewness calculation.

Why 15?

Because otherwise an illiquid stock with only a handful of trades can produce absurdly unstable skewness estimates.

Later I'd test:

[
N\geq10,\quad15,\quad20
]

as robustness checks.

---

# 7. Daily return calculation

For each stock:

[
r_{i,d}=\frac{P_{i,d}}{P_{i,d-1}}-1
]

where (P) is the **adjusted daily price**.

This is extremely important.

We don't want a split or bonus issue to appear as a +100%, +200%, etc. return.

Those artificial extreme observations would directly contaminate the skewness predictor.

---

# 8. Calculate monthly realized skewness

For each stock-month:

[
RV_{i,t}
========

\sqrt{
\frac{1}{N_{i,t}}
\sum_{d=1}^{N_{i,t}}r_{i,d}^{2}
}
]

and:

[
RS_{i,t}
========

\frac{
\frac{1}{N_{i,t}}\sum_{d=1}^{N_{i,t}}r_{i,d}^{3}
}{
RV_{i,t}^{3}
}
]

This follows the paper's raw-return skewness definition. 

**Important:** this is the paper's raw third-moment measure, not conventional centered sample skewness.

I would reproduce the paper exactly for the baseline.

---

# 9. Construct momentum

The paper defines momentum as the cumulative return over the previous year **excluding the most recent month**. 

For our Indian strategy:

[
MOM_{i,t}
=========

\prod_{k=2}^{12}(1+R_{i,t-k+1})-1
]

Conceptually:

```text
Forecast/formation month:       August

Excluded:
July

Included:
June
May
April
...
August previous year
```

So this is essentially **12-2 momentum**.

---

# 10. The monthly timeline is critical

Suppose the portfolio is formed at the end of **July** for holding during **August**.

Our information set must look like:

```text
                 JULY
                   │
                   ├── July daily returns
                   │       ↓
                   │    RS July
                   │    RV July
                   │
                   ├── July monthly return
                   │
                   ├── July market cap
                   │
                   ├── July industry
                   │
                   └── momentum through June
                   │
                   ▼
            Expected skewness
              for AUGUST
                   │
                   ▼
            Portfolio formed
                   │
                   ▼
                AUGUST
              holding period
```

**Nothing from August may enter the July decision.**

This is the single most important implementation rule.

---

# 11. Expected-skewness predictor

The paper estimates a monthly cross-sectional regression:

[
RS_{i,t}
========

\alpha_t+
\lambda_t'X_{i,t-1}
+
\epsilon_{i,t}
]

and then uses the estimated coefficients to forecast the next month's skewness. 

The important feature is:

### This is NOT a rolling 36/60/120-month regression.

They estimate the relationship **cross-sectionally every month**.

So for July:

[
RS_{i,July}
===========

\alpha_{July}
+
\beta_{July}'X_{i,June}
+
\epsilon
]

Then:

[
\widehat{RS}_{i,August}
=======================

\hat\alpha_{July}
+
\hat\beta_{July}'X_{i,July}
]

That's the forecast we use.

---

# 12. Indian version of the skewness model

For the baseline India replication, I would use:

[
\boxed{
RS_{i,t}
========

\alpha_t
+
\beta_{1,t}RV_{i,t-1}
+
\beta_{2,t}RS_{i,t-1}
+
\beta_{3,t}MOM_{i,t-1}
+
\beta_{4,t}PRIOR_{i,t-1}
+
\beta_{5,t}SMALL_{i,t-1}
+
\beta_{6,t}MEDIUM_{i,t-1}
+
IndustryFE
+
\epsilon_{i,t}
}
]

The paper's final Model 7 uses realized volatility, realized skewness, momentum, prior-month return, size indicators and industry indicators; it excludes turnover and book-to-market from the final specification. 

For India, I would **remove the NASDAQ dummy**, obviously.

---

# 13. Why free-float market cap is useful

The paper uses lagged market capitalization for value-weighted portfolio returns. 

Since your Indian dataset has **free-float market cap**, I'd use that consistently.

For size:

[
Size_{i,t}=FFMC_{i,t}
]

Then divide stocks into:

* Small
* Medium
* Large

using monthly cross-sectional terciles.

Large is the reference category.

---

# 14. Industry controls

Use your available industry classification as fixed effects.

For each month:

[
Industry_{i,t}=j
]

and include industry dummy variables in the skewness regression.

I would **not try to recreate the U.S. Fama-French 48 industries** because your Indian classification is already available and is more economically appropriate for the Indian universe.

---

# 15. Estimate the skewness model each month

Suppose we are at the end of July 2020.

We take all eligible stocks and run:

[
RS_{i,July}
===========

\alpha_{July}
+
\beta_{RV,July}RV_{i,June}
+
\beta_{RS,July}RS_{i,June}
+\cdots
]

Then save:

[
\hat\alpha_{July},
\hat\beta_{RV,July},
\hat\beta_{RS,July},...
]

We then calculate:

[
\boxed{
ExpectedRS_{i,August}
=====================

\hat\alpha_{July}
+
\hat\beta_{RV,July}RV_{i,July}
+
\hat\beta_{RS,July}RS_{i,July}
+\cdots
}
]

The paper emphasizes that the monthly estimation allows the predictive relationship to vary over time. 

---

# 16. Don't use the paper's average coefficients

This is worth emphasizing again.

The paper reports average Model 7 coefficients—for example, an average volatility coefficient of approximately 2.31—but those are **descriptive averages across the monthly regressions**. 

For the backtest, we use:

[
\boxed{\hat\beta_t}
]

for each individual month.

Not:

[
\bar{\beta}
]

---

# 17. Now construct the momentum universe

Once we have:

[
MOM_{i,t}
]

rank all eligible stocks.

For the primary strategy, I would use **deciles** to stay close to the paper.

Define:

[
D_{10}=\text{highest momentum decile}
]

Then:

[
MomentumUniverse_t=D_{10}
]

This gives us the strongest momentum stocks.

---

# 18. Apply the skewness overlay

Within the highest momentum decile, rank stocks on:

[
ExpectedRS_{i,t+1}
]

into three groups:

* Low expected skewness
* Medium expected skewness
* High expected skewness

Then our **primary portfolio** is:

[
\boxed{
Momentum10 \cap HighExpectedSkewness
}
]

This is the cleanest long-only adaptation of the paper.

---

# 19. Why not use low skewness?

Because we're long-only.

The paper's logic is:

> seek high-skewness stocks on the long side and avoid high-skewness stocks on the short side. 

Since we have no short leg, we only need the first half:

[
\boxed{\text{Long high expected skewness}}
]

---

# 20. Primary portfolio definition

I'd therefore define:

### Portfolio L1

**Momentum Decile 10**

### Portfolio L2

**Momentum Decile 10 + Medium expected skewness**

### Portfolio L3

**Momentum Decile 10 + High expected skewness**

Then compare:

[
L1
]

against:

[
L3
]

The incremental effect is:

[
\boxed{
L3-L1
}
]

This is a very useful diagnostic because it tells us whether the skewness predictor is actually adding value **within momentum winners**.

---

# 21. Don't make the skewness overlay too restrictive initially

If D10 contains 10% of the universe and then we take the top skewness tercile:

[
10%\times33%\approx3.3%
]

of the total universe.

That could become quite concentrated.

Therefore I'd run several portfolio definitions.

### Strategy A — Primary

Top momentum decile + top skewness tercile.

### Strategy B

Top momentum decile + top 50% skewness.

### Strategy C

Top 20% momentum + top 50% skewness.

### Strategy D

Top 20% momentum + top 33% skewness.

This gives us a concentration/return tradeoff.

---

# 22. Weighting schemes

I would test three.

### Equal-weight

[
w_i=\frac{1}{N}
]

This gives the purest signal test.

### Free-float-market-cap weighted

[
w_i=
\frac{FFMC_i}{\sum FFMC_i}
]

This is likely to be more realistic for an Indian institutional strategy.

### Volatility-scaled

Potentially:

[
w_i\propto\frac{1}{\sigma_i}
]

but I would **not make this part of the primary strategy**.

It introduces another optimization layer.

---

# 23. My preferred primary weighting

I'd actually make the main result:

[
\boxed{\text{Free-float market-cap weighted}}
]

with:

* individual-stock maximum weight, perhaps 5%
* optional sector maximum
* cash = 0

and show equal-weight results separately.

Why?

Because an equal-weight strategy can become heavily exposed to tiny Indian stocks, which could make the result look spectacular but be impossible to implement.

---

# 24. Rebalancing frequency

The paper's skewness-management strategy is constructed monthly. 

I would therefore use:

[
\boxed{\text{Monthly rebalance}}
]

rather than weekly.

At each month-end:

1. Update momentum
2. Update skewness forecast
3. Re-rank
4. Rebalance

---

# 25. Execution assumption

Don't assume we transact at month-end closing prices.

That creates a subtle look-ahead/implementation problem.

I'd define:

### Signal date

Last trading day of month (t).

### Execution date

First trading day of month (t+1).

### Execution price

Preferably:

* VWAP, if available; or
* next-day open; or
* next-day close as a conservative alternative.

For a clean backtest, I'd use **next trading day's open** as the baseline if that's available and reliable.

---

# 26. Transaction costs

This should be a major part of the backtest.

For each trade:

[
NetReturn
=========

## GrossReturn

TC
]

with:

[
TC_i
====

Turnover_i\times Cost_i
]

At minimum test:

### Scenario 1

Low transaction costs

### Scenario 2

Base case

### Scenario 3

High transaction costs

And report:

[
Gross\ CAGR
]

versus:

[
Net\ CAGR
]

The original paper's headline improvement declines substantially after transaction costs, from roughly 5.45 percentage points to about 1.58 percentage points. 

That makes this especially important.

---

# 27. Portfolio turnover

Calculate:

[
Turnover_t
==========

\frac12
\sum_i
|w_{i,t}-w_{i,t-1}|
]

and report:

* monthly turnover
* annual turnover
* median turnover
* 90th percentile turnover

This is crucial because a strategy can have an attractive gross Sharpe but be unusable because of turnover.

---

# 28. Benchmark portfolios

We need a proper set of controls.

I'd construct:

### Benchmark 1

NSE broad-market portfolio.

### Benchmark 2

**Pure momentum**

Top momentum decile.

### Benchmark 3

**Pure skewness**

Top expected-skewness tercile/decile.

### Strategy

**Momentum + high expected skewness**

This gives:

[
\boxed{
PureMomentum
\rightarrow
Momentum+Skewness
}
]

as the central comparison.

---

# 29. The key performance question

The most important comparison isn't:

> Does Momentum + Skewness make money?

Almost any decent momentum strategy might.

The important question is:

[
\boxed{
Does\ Momentum+Skewness

>

PureMomentum?
}
]

after:

* transaction costs
* size controls
* industry controls
* liquidity controls
* realistic execution.

---

# 30. Performance statistics

For every strategy calculate:

### Returns

* annualized return
* CAGR
* monthly mean
* median monthly return

### Risk

* annualized volatility
* downside deviation
* maximum drawdown
* expected shortfall
* worst month

### Risk-adjusted

* Sharpe ratio
* Sortino ratio
* Calmar ratio

### Distribution

* skewness
* kurtosis
* percentage positive months
* percentage of return from best 5/10/20 months

That last statistic is particularly important because the paper's central hypothesis is about **extreme positive returns**.

---

# 31. Decompose the return distribution

For the momentum portfolios, I'd explicitly ask:

> Are high-skewness momentum stocks actually producing more extreme upside observations?

For each portfolio calculate:

[
P95,\ P99
]

of monthly stock-level returns.

And compare:

```text
Momentum
vs
Momentum + High Expected Skewness
```

on:

* mean return
* median return
* 90th percentile
* 95th percentile
* 99th percentile
* realized skewness

This directly tests whether the Indian data reproduce the mechanism proposed by the paper.

---

# 32. A particularly important test: winsorization

The paper's initial evidence comes from winsorizing extreme returns and examining how anomaly returns change. 

I'd reproduce this for Indian momentum.

Take stock-level daily returns and cap extreme positive observations at:

* 90th percentile
* 95th percentile
* 99th percentile

Then recompute momentum portfolio performance.

If:

[
Momentum_{HighSkew}
]

loses substantially more performance after removing extreme positive observations than ordinary momentum, that's strong evidence that we're capturing the mechanism the paper describes.

---

# 33. Important India-specific robustness: liquidity

I would run:

### Universe A

All eligible stocks.

### Universe B

Exclude bottom 20% by free-float market cap.

### Universe C

Exclude bottom 20% by turnover.

### Universe D

Exclude bottom 20% by both.

Then compare:

[
\Delta Return
]

between Momentum and Momentum+Skewness.

If the effect survives Universe D, I'd be much more confident.

---

# 34. Another important test: idiosyncratic skewness

The paper reports that using idiosyncratic skewness produces similar results to total skewness. 

For India, I'd replicate this.

Calculate:

[
r_{i,d}
=======

\alpha_i+
\beta_i r_{NIFTY,d}
+\epsilon_{i,d}
]

then:

[
IdioRS_{i,t}
============

Skewness(\epsilon_{i,d})
]

and build:

[
ExpectedIdioRS_{i,t+1}
]

Then compare:

[
Momentum+TotalSkew
]

versus:

[
Momentum+IdioSkew
]

This is a powerful robustness test.

---

# 35. Factor attribution

We should determine whether the strategy is really skewness-driven.

At minimum regress the monthly portfolio excess returns against Indian factors such as:

[
R_p-R_f
=======

\alpha+
\beta_M MKT
+\beta_S SMB
+\beta_V HML
+\beta_{MOM} MOM
+\epsilon
]

If we have appropriate Indian factor series, also test profitability/investment/quality.

The key quantity is:

[
\boxed{\alpha}
]

for:

**Momentum + skewness**

versus:

**Momentum**

---

# 36. Sector concentration

This is particularly important for India.

Every month report:

* sector weights
* maximum sector weight
* sector HHI
* number of sectors represented

Then run a version with:

[
w_{sector}\leq20%
]

or similar.

Otherwise the strategy might simply become:

> high-momentum/high-skewness Indian financials or small-cap industrials.

---

# 37. Concentration statistics

Report:

[
N_{stocks}
]

plus:

* top 5 weight
* top 10 weight
* Herfindahl index
* median stock weight
* maximum stock weight

I'd want to know whether the strategy typically owns:

**30 stocks**

or:

**150 stocks**.

That dramatically changes the interpretation.

---

# 38. Crisis/regime analysis

The paper finds particularly strong skewness-management performance during stressed conditions, including recessions and high-VIX periods. 

For India, we don't have to copy the U.S. VIX framework.

I'd divide the sample into:

* high vs low India VIX
* high vs low Nifty volatility
* bull vs bear market
* high vs low market drawdown
* high vs low credit spreads, if available

Then calculate:

[
Return_{Momentum}
]

and:

[
Return_{Momentum+Skew}
]

in each regime.

---

# 39. Statistical significance

Because we're doing many variants, standard t-statistics aren't enough.

For the main result:

[
H_0:
E[R_{MS}-R_M]=0
]

I'd report:

* Newey-West t-statistic
* block bootstrap confidence intervals
* annualized alpha
* confidence interval

And importantly, **pre-specify the primary strategy before looking at results**.

Otherwise we risk finding the best-looking combination after the fact.

---

# 40. Out-of-sample design

This is something I strongly recommend.

Don't make the entire 1995–2025 sample one giant backtest.

Use:

### Training period

For understanding/model development.

### Validation period

For choosing thresholds.

### Final out-of-sample period

Never used to choose parameters.

For example, conceptually:

```text
1995 ───────── 2010 | 2011 ───── 2017 | 2018 ───────── 2026
       development        validation         out-of-sample
```

The exact dates should depend on data availability.

The **monthly skewness regression itself can still be estimated using only information available at each historical date**. The out-of-sample framework is about preventing us from selecting the strategy after seeing its full-sample results.

---

# 41. Parameter grid I'd pre-specify

I would not optimize everything.

I'd define a small grid:

| Parameter                  | Values                                |
| -------------------------- | ------------------------------------- |
| Momentum                   | 12-2 primary; 6-1 robustness          |
| Momentum cutoff            | Top 10%, 20%, 30%                     |
| Skewness cutoff            | Top 33%, 50%                          |
| Rebalance                  | Monthly                               |
| Weight                     | FFMC primary; equal-weight robustness |
| Minimum daily observations | 15 primary; 10/20 robustness          |
| Liquidity filter           | None, 20%, 40%                        |
| Position cap               | 5% primary                            |
| Sector cap                 | 20% robustness                        |

Then select the **primary specification before examining the final performance**.

---

# 42. The exact backtest loop

This is the heart of the implementation.

For every month (t):

```text
1. Identify NSE stocks existing at t.

2. Apply eligibility/data-quality filters.

3. Calculate:
       RS(t)
       RV(t)
       monthly return(t)
       12-2 momentum(t)
       free-float market cap(t)
       industry(t)

4. Estimate cross-sectional skewness model:
       RS(t) ~ RS(t-1) + RV(t-1)
               + MOM(t-1)
               + PRIOR(t-1)
               + SIZE(t-1)
               + INDUSTRY(t-1)

5. Obtain month-t coefficients.

6. Calculate predicted skewness for t+1:
       ExpectedRS(t+1)

7. Rank stocks on momentum.

8. Select top momentum decile.

9. Within that group, rank ExpectedRS.

10. Select top expected-skewness tercile.

11. Determine portfolio weights.

12. Apply position/sector constraints.

13. Execute on next trading day.

14. Hold for one month.

15. Calculate gross return.

16. Calculate turnover.

17. Deduct transaction costs.

18. Move to next month.
```

That is the complete research engine.

---

# 43. What I would consider the primary result

I'd define the main hypothesis **before running the backtest** as:

[
\boxed{
H_1:
E[R_{Momentum+HighSkew}]

>

E[R_{Momentum}]
}
]

after transaction costs.

And the stronger hypothesis:

[
\boxed{
Sharpe_{Momentum+HighSkew}

>

Sharpe_{Momentum}
}
]

The second is arguably more important for an investor.

---

# 44. The most important diagnostic table

I'd want the final results to look something like:

| Strategy                 | CAGR | Vol | Sharpe | Max DD | Turnover | Net CAGR |
| ------------------------ | ---: | --: | -----: | -----: | -------: | -------: |
| Nifty benchmark          |      |     |        |        |          |          |
| Momentum top 10%         |      |     |        |        |          |          |
| Momentum + medium skew   |      |     |        |        |          |          |
| **Momentum + high skew** |      |     |        |        |          |          |
| High skew only           |      |     |        |        |          |          |

The key comparison is the bold row against **Momentum top 10%**.

---

# 45. Then the critical attribution table

I'd also produce:

| Specification                | Incremental return vs momentum |
| ---------------------------- | -----------------------------: |
| High skewness                |                                |
| High skewness + size control |                                |
| + liquidity control          |                                |
| + industry control           |                                |
| Idiosyncratic skewness       |                                |
| After transaction costs      |                                |
| Large-cap universe           |                                |

This tells us **what survives**.

---

# 46. My preferred research hierarchy

I'd build this in this order:

### Phase 1 — Data validation

Before any portfolio results:

* number of NSE stocks/month
* missing prices
* number of daily observations
* extreme returns
* corporate-action issues
* free-float market-cap coverage

### Phase 2 — Skewness predictor

Validate:

[
RS_t\rightarrow RS_{t+1}
]

and report:

* coefficient distributions
* average coefficients
* coefficient stability
* monthly (R^2)
* rank IC

### Phase 3 — Momentum

Build ordinary momentum.

### Phase 4 — Skewness overlay

Add expected skewness.

### Phase 5 — Costs/liquidity

Make the strategy investable.

### Phase 6 — Robustness

Industry, size, idiosyncratic skewness, regimes, factors.

---

# 47. One metric I would add that the original paper doesn't emphasize enough

### Rank Information Coefficient

Every month calculate:

[
IC_t
====

Corr(
ExpectedRS_{i,t+1},
RS_{i,t+1}
)
]

using Spearman rank correlation.

Then report:

[
Mean(IC)
]

[
ICIR=
\frac{Mean(IC)}
{SD(IC)}
]

This tells us whether the predictor actually **ranks future skewness correctly**, rather than merely producing a low (R^2) regression.

For this application, ranking is what matters.

---

# 48. Another metric: incremental rank IC

Even more interesting:

Calculate:

[
IC_{Momentum}
]

and then:

[
IC_{ExpectedSkew}
]

**within the momentum universe**.

That answers:

> Does expected skewness still contain information after we've already selected momentum winners?

That's precisely what our strategy requires.

---

# 49. The final conceptual model

I'd think of the strategy as:

[
\boxed{
\underbrace{\text{Momentum}}*{\text{Which stocks?}}
+
\underbrace{\text{Expected Skewness}}*{\text{Which momentum stocks?}}
}
]

Momentum identifies stocks with strong recent medium-term performance.

Expected skewness then asks:

> Among those winners, which ones have characteristics associated with unusually large positive future return realizations?

That is much cleaner than simply creating a composite momentum/skewness score.

---

## Recommended primary specification

If I were setting this up for an actual backtest, I'd lock in the following **before seeing the results**:

**Universe:** NSE ordinary equities
**Price:** adjusted daily price
**Frequency:** monthly
**Momentum:** 12-2
**Momentum selection:** top decile
**Skewness:** paper's raw daily-return skewness
**Skewness forecast:** monthly cross-sectional Model-7-style regression
**Forecast horizon:** 1 month
**Skewness selection:** top tercile within momentum winners
**Weighting:** free-float-market-cap weighted
**Rebalance:** monthly
**Execution:** next trading day's open
**Minimum daily observations:** 15
**Position cap:** 5%
**Primary benchmark:** same momentum portfolio without skewness selection
**Primary test:** incremental return and Sharpe **after transaction costs**
**Key robustness:** liquidity-filtered universe and idiosyncratic skewness.

This gives us a **clean test of whether the paper's skewness idea adds value to an Indian long-only momentum strategy**, rather than merely creating another optimized factor portfolio.

And importantly, the paper itself gives us a strong reason to expect the overlay to matter: its skewness-management strategy explicitly takes high expected-skewness stocks on the long side, and the expected-skewness forecast is generated from a monthly cross-sectional model rather than a long rolling time-series regression. 

If we implement this, I would **not start with optimization**. I would first reproduce this exact baseline, generate the monthly expected-skewness signal, and then test whether it adds statistically and economically significant value to ordinary Indian 12-2 momentum.

