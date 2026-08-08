I went back to the actual construction in the paper, including Table 3. The short version is:

> **They do not estimate a single time-series model of skewness using a long historical look-back. Instead, every month they run a fresh cross-sectional regression across thousands of stocks, using characteristics measured at the end of the previous month to explain the skewness realized during the current month. They then use that month's estimated coefficients and the current month's characteristics to forecast next month's skewness.**

That distinction is crucial. 

---

# 1. Start with what they mean by "future skewness"

For each stock (i), they want to predict:

[
E_t[RS_{i,t+1}]
]

where (RS_{i,t+1}) is the **realized skewness of the stock's daily returns during month (t+1)**.

So their forecasting horizon is exactly:

### **1 month ahead**

It isn't a 3-month or 12-month skewness forecast.

---

# 2. First they calculate realized monthly skewness

This is the dependent variable they are trying to predict.

For stock (i) in month (t), they have daily returns:

[
r_{i,d}, \quad d=1,\ldots,N_t
]

where (N_t) is the number of trading days in that month.

They define realized volatility as:

[
RV_{i,t}
========

\left(
\frac{1}{N_t}
\sum_{d=1}^{N_t} r_{i,d}^{2}
\right)^{1/2}
]

and realized skewness as:

[
RS_{i,t}
========

\frac{
\frac{1}{N_t}\sum_{d=1}^{N_t}r_{i,d}^{3}
}{
RV_{i,t}^{3}
}
]



### Important detail

This is **raw return skewness**, not the usual centered third central moment.

In other words, their formula is essentially:

[
\frac{E[r^3]}{(E[r^2])^{3/2}}
]

rather than:

[
\frac{E[(r-\bar r)^3]}
{\left(E[(r-\bar r)^2]\right)^{3/2}}
]

The authors explicitly describe their measure as **realized raw return skewness**. 

That's important if you want to reproduce the paper exactly.

---

# 3. What is the look-back for realized skewness?

This is perhaps the first thing I'd emphasize if you're trying to reproduce the paper.

### The look-back is the current calendar month.

For example, suppose we're forecasting skewness for **August 2020**.

They would use the daily returns occurring during **July 2020** to calculate:

[
RS_{i,\text{July 2020}}
]

and

[
RV_{i,\text{July 2020}}
]

They do **not** calculate skewness over the previous 3, 6, or 12 months.

The paper says (N(t)) is the number of trading days in month (t), and the calculation uses those daily observations. 

---

# 4. Then comes the forecasting regression

This is the key equation:

[
RS_{i,t}
========

\beta_t+
\lambda_t'X_{i,t-1}
+
\epsilon_{i,t}
]

Notice the subscripts carefully.

### Left-hand side

[
RS_{i,t}
]

is **skewness realized during month (t)**.

### Right-hand side

[
X_{i,t-1}
]

contains characteristics known at the **end of month (t-1)**.

So they have a clean information structure:

```text
End of June
    ↓
Characteristics known
    ↓
July realized skewness
    ↓
Estimate cross-sectional regression
    ↓
Use July characteristics
    ↓
Forecast August skewness
```

The paper explicitly says the regression is estimated separately each month, allowing the relationship between characteristics and skewness to change over time. 

---

# 5. This is NOT a rolling historical regression

This is the most important methodological point.

You might initially think they're doing something like:

[
RS_{i,t+1}
==========

\alpha+
\beta_1 RS_{i,t}
+\beta_2 RV_{i,t}
+\cdots
]

estimated using, say, the previous 60 months.

**They aren't.**

Instead, at every month (t), they run:

[
\boxed{
RS_{i,t}
========

\beta_t+
\lambda_t'X_{i,t-1}
+\epsilon_{i,t}
}
]

across **all stocks available in that month**.

Then they use the estimated coefficients:

[
\hat\beta_t,\hat\lambda_t
]

to calculate:

[
\boxed{
\widehat{RS}_{i,t+1}
====================

\hat\beta_t+
\hat\lambda_t'X_{i,t}
}
]



So there is **no 36-month, 60-month, or 120-month estimation window for the prediction coefficients**.

The coefficients are **re-estimated every month from that month's cross-section**.

---

# 6. What exactly goes into (X)?

The candidate variables are:

1. Lagged realized volatility
2. Lagged realized skewness
3. Momentum
4. Prior-month return
5. Turnover
6. NASDAQ dummy
7. Size dummies
8. Industry dummies
9. Book-to-market was considered but ultimately excluded



Let's go through each.

---

# 7. Predictor #1: lagged realized volatility

For the forecast made at the end of month (t), they use:

[
RV_{i,t}
]

where (RV_{i,t}) is calculated from **daily returns during month (t)**.

So:

### Look-back = approximately one month of daily returns.

For August forecast:

```text
July daily returns
       ↓
July RV
       ↓
August expected skewness
```

The average coefficient in their final model is about **2.31**. 

The authors say volatility is one of the strongest and most consistent predictors.

They also say that a one-standard-deviation increase in volatility has roughly twice the impact on future skewness as a similar increase in realized skewness. 

---

# 8. Predictor #2: lagged realized skewness

They also use:

[
RS_{i,t}
]

as a predictor of:

[
RS_{i,t+1}
]

Again, this is calculated using the **daily returns from the immediately preceding month**.

So:

```text
June daily returns → June RS
                       ↓
                  July forecast
```

The average coefficient in Model 7 is approximately:

[
0.02
]

according to Table 3. 

This is actually fairly small.

That is consistent with their statement that skewness itself is only **weakly persistent**. 

This is why they don't simply say:

> "last month's skewness predicts next month's skewness."

Instead, they use a cross-sectional model incorporating several characteristics.

---

# 9. Predictor #3: momentum

Momentum is defined very specifically:

[
MOM_{i,t}
=========

\text{return over months }t-11\text{ through }t-1
]

**excluding the most recent month.**

So this is the standard **12-2 momentum** concept.

For example, to forecast August:

```text
August forecast
       ↑
July excluded
June → previous August included
```

In other words:

### Look-back = roughly 12 months, with the most recent month skipped.

The paper explicitly defines momentum as the prior-year return omitting the most recent month. 

The average Model 7 coefficient is approximately:

[
-0.02
]



---

# 10. Predictor #4: prior-month return

This is simply:

[
PRIOR_{i,t}=R_{i,t}
]

i.e. the stock's return during the **immediately preceding month**.

So for an August forecast:

[
PRIOR = \text{July return}
]

This is distinct from momentum.

You therefore have:

| Variable     | Look-back                                   |
| ------------ | ------------------------------------------- |
| RV           | Daily returns during previous month         |
| RS           | Daily returns during previous month         |
| Prior return | Previous month                              |
| Momentum     | Previous 12 months excluding previous month |

The paper explicitly distinguishes momentum from prior-month return. 

---

## An important inconsistency in the paper

There is something worth flagging here.

The text says:

> higher prior-month returns predict higher expected skewness

but **Table 3's Model 7 coefficient on `prior` is −0.39**. 

The table itself is quite clear when you look at the actual PDF:

```text
Model 7:
rv_t-1       2.31
rs_t-1       0.02
mom         -0.02
nasdaq      -0.03
sm           0.09
med          0.08
prior       -0.39
industry     yes
R²            0.030
```

So there appears to be a **sign inconsistency between the prose and Table 3**.

For an exact replication, I would use **the actual Table 3 coefficient, −0.39**, rather than the prose statement.

That is a good example of why it's worth looking at the actual specification rather than relying only on the narrative.

---

# 11. Predictor #5: size

They include dummy variables for:

* small stocks
* medium-sized stocks

with the omitted category being the largest size group.

These are based on **size terciles**.

So the model effectively has:

[
Small_i
]

and

[
Medium_i
]

with large stocks as the reference category.

Model 7 coefficients are approximately:

[
\beta_{Small}=0.09
]

[
\beta_{Medium}=0.08
]



This is important economically because the paper finds that positively skewed stocks tend to be concentrated among smaller companies.

---

# 12. Predictor #6: NASDAQ dummy

They include:

[
NASDAQ_i =
\begin{cases}
1 & \text{NASDAQ}\
0 & \text{otherwise}
\end{cases}
]

Model 7's average coefficient is about:

[
-0.03
]



---

# 13. Predictor #7: industry fixed effects

They include **Fama-French 48 industry indicators**.

So the model isn't simply saying:

> volatility predicts skewness.

It's comparing stocks **within a month while controlling for industry differences**.

The industry dummies are included in Model 7. 

One industry category is implicitly the reference category.

---

# 14. What about turnover?

This is an interesting detail.

Turnover is among their **candidate predictors**:

[
Turnover_{i,t-1}
================

\frac{\text{sum of daily trading volume}}
{\text{average shares outstanding}}
]

with at least **15 daily observations** required for the turnover calculation. 

However:

### Turnover is NOT in their final Model 7.

Why?

Because turnover data only becomes broadly available for NASDAQ stocks after 1988, which would unnecessarily shrink their historical sample.

The authors therefore deliberately omit turnover from the final model. 

---

# 15. What about book-to-market?

They considered it.

But they **exclude book-to-market** from the final expected-skewness model.

The reason is not that they believe B/M is irrelevant.

They explicitly say B/M has been identified as a skewness predictor in previous research, but including it would unnecessarily reduce the overlap with their anomaly sample. 

This is actually a sensible design choice given that B/M is itself one of the anomalies they want to study.

---

# 16. So what exactly is Model 7?

Putting the paper together, the final specification is approximately:

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
\beta_{3,t}MOM_{i,t}
+
\beta_{4,t}NASDAQ_i
+
\beta_{5,t}Small_i
+
\beta_{6,t}Medium_i
+
\beta_{7,t}Prior_{i,t}
+
IndustryFE
+
\epsilon_{i,t}
}
]

Then:

[
\boxed{
\widehat{RS}_{i,t+1}
====================

\hat\alpha_t+
\hat\beta_{1,t}RV_{i,t}
+
\hat\beta_{2,t}RS_{i,t}
+
\hat\beta_{3,t}MOM_{i,t}
+
\hat\beta_{4,t}NASDAQ_i
+
\hat\beta_{5,t}Small_i
+
\hat\beta_{6,t}Medium_i
+
\hat\beta_{7,t}Prior_{i,t}
+
IndustryFE
}
]

This is directly based on equations (3) and (4) and their description of Model 7. 

---

# 17. The average Model 7 coefficients

The paper's Table 3 reports the **time-series average** of the monthly coefficients:

| Variable             | Model 7 average coefficient |
| -------------------- | --------------------------: |
| RV, previous month   |                    **2.31** |
| RS, previous month   |                    **0.02** |
| Momentum, 12-2       |                   **−0.02** |
| NASDAQ dummy         |                   **−0.03** |
| Small                |                   **+0.09** |
| Medium               |                   **+0.08** |
| Prior-month return   |                   **−0.39** |
| Industry FE          |                         Yes |
| Adjusted (R^2)       |                    **3.0%** |
| Average stocks/month |                   **4,139** |



**But don't use these average coefficients to reproduce their forecasts.**

This is another very important point.

---

# 18. They don't actually use the average coefficients

The numbers above are just reported summaries.

For the actual portfolio construction, they use the **coefficients estimated in each particular month**.

Suppose we're at the end of July 2020.

They estimate:

[
\hat\alpha_{July}
]

[
\hat\beta_{RV,July}
]

[
\hat\beta_{RS,July}
]

etc.

Then plug August/current characteristics into those coefficients.

So:

[
\widehat{RS}_{i,August}
=======================

\hat\alpha_{July}
+
\hat\beta_{RV,July}RV_{i,July}
+\cdots
]

The coefficients therefore **change every month**.

That's why the paper says the method allows the relationship between firm characteristics and skewness to vary over time. 

---

# 19. How many observations are in each regression?

Approximately:

### **4,139 stocks per month**

for Model 7.

The sample covers common stocks on:

* NYSE
* AMEX
* NASDAQ

and the resulting forecasts span **July 1963 through December 2024**. 

That's important because the model is not being estimated on a small panel.

Every month they're essentially doing:

[
\text{~4,000 stocks}
\times
\text{one cross-sectional regression}
]

and then repeating that every month for more than 60 years.

---

# 20. How good is the prediction?

Not very good in an absolute statistical sense.

Their average cross-sectional adjusted:

[
R^2 \approx 3.0%
]



At first glance, 3% sounds weak.

But predicting next-month **individual-stock skewness** is inherently difficult.

The authors point out that this is comparable to other short-horizon skewness prediction models.

And importantly, the signal doesn't need to predict the exact value of skewness particularly well.

Their portfolio strategy only needs it to **rank stocks reasonably well**:

> Which stocks are relatively more likely to have extreme positive returns?

That's the economic purpose of the forecast.

---

# 21. The actual forecasting process

Here's the complete procedure I'd use to reproduce their approach.

### At the end of month (t):

### Step 1 — Calculate previous-month daily statistics

For each stock:

[
RV_{i,t}
========

\sqrt{\frac{1}{N_t}\sum_d r_{i,d}^2}
]

[
RS_{i,t}
========

\frac{\frac{1}{N_t}\sum_d r_{i,d}^3}
{RV_{i,t}^3}
]

using daily returns in month (t).

### Step 2 — Calculate characteristics

For each stock:

* (RV_t)
* (RS_t)
* 12-2 momentum
* month-(t) return
* NASDAQ dummy
* size tercile
* FF48 industry

### Step 3 — Estimate the cross-sectional regression

Using the **realized skewness from month (t)** as the dependent variable and characteristics from (t-1):

[
RS_t = \alpha_t + \beta_t X_{t-1}+\epsilon_t
]

### Step 4 — Store that month's coefficients

[
\hat\alpha_t,\hat\beta_t
]

### Step 5 — Forecast month (t+1)

Use characteristics measured at the end of month (t):

[
\widehat{RS}_{t+1}
==================

\hat\alpha_t+\hat\beta_tX_t
]

### Step 6 — Rank stocks

At the beginning of month (t+1), rank stocks by:

[
\widehat{RS}_{i,t+1}
]

### Step 7 — Use the rank in the anomaly portfolio

Within each anomaly leg:

* highest expected-skewness tercile → **long**
* lowest expected-skewness tercile → **short**

The authors use this sequential sorting procedure to form the skewness-managed portfolios. 

---

# 22. The look-back structure in one table

This is probably the clearest way to think about it:

| Signal                     | Data used              | Look-back                       | Used to predict        |
| -------------------------- | ---------------------- | ------------------------------- | ---------------------- |
| Realized skewness (RS_t)   | Daily returns          | **1 month**                     | (RS_{t+1})             |
| Realized volatility (RV_t) | Daily returns          | **1 month**                     | (RS_{t+1})             |
| Prior return               | Monthly return         | **1 month**                     | (RS_{t+1})             |
| Momentum                   | Monthly returns        | **12 months, excluding latest** | (RS_{t+1})             |
| Size                       | Market cap             | Current month-end               | (RS_{t+1})             |
| NASDAQ                     | Listing status         | Current                         | (RS_{t+1})             |
| Industry                   | FF48 classification    | Current                         | (RS_{t+1})             |
| Turnover                   | Daily volume           | Previous month                  | **Not in final model** |
| B/M                        | Accounting/market data | —                               | **Not in final model** |

---

# 23. One subtle but very important issue: no long time-series look-back

This means the model is fundamentally different from something like:

[
RS_{t+1}
========

\alpha+
\beta RS_t
+\gamma RS_{t-1}
+\cdots
]

estimated over 20 years.

Instead, it's:

[
\boxed{
\text{cross-section at }t
\rightarrow
\text{forecast }t+1
}
]

The historical dimension is used to **generate many monthly cross-sectional regressions and evaluate them**, not to estimate one fixed forecasting equation.

This is why the coefficients themselves can vary through time.

---

# 24. And this explains something interesting about the paper

Their expected-skewness measure is really a **cross-sectional ranking signal**, rather than a highly accurate time-series forecast.

Imagine two stocks:

|                            | Stock A | Stock B |
| -------------------------- | ------: | ------: |
| Expected skewness          |    0.82 |   −0.15 |
| Actual next-month skewness |    0.35 |   −0.05 |

The model hasn't predicted the actual skewness particularly accurately.

But it **correctly ranked A above B**.

That's enough for their portfolio construction.

They don't need:

[
\widehat{RS}=RS
]

They need:

[
RS_A > RS_B
]

to be sufficiently likely.

---

## One thing I would investigate before reproducing this paper

There are **two details in the paper that I'd treat carefully rather than blindly coding**:

1. **The prior-return sign inconsistency:** the narrative says higher prior returns predict higher skewness, but Table 3's Model 7 coefficient is −0.39. 
2. **Exact data treatment:** the main text gives the raw-skewness formula and monthly construction, but it does **not clearly specify a minimum number of daily observations for the raw (RS/RV) calculation**. The 15-observation requirement is explicitly stated for turnover and for the *idiosyncratic-skewness* robustness test, not clearly for the baseline raw-skewness measure.  

So if your goal is **exact replication**, I wouldn't fill those gaps with assumptions.


