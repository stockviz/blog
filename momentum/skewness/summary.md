## Paper summary: *Skewness Managed Portfolios*

**Authors:** Rui Gong, John Lynch, Richard Ogden
**Date:** May 2026 

### 1. The paper in one sentence

The paper argues that **a surprisingly large part of the returns from well-known stock-market anomalies comes from a small number of stocks with extreme positive returns**, and that explicitly selecting stocks with high expected skewness for the long side—and low expected skewness for the short side—can substantially improve anomaly performance. 

---

## 2. What problem are the authors trying to solve?

Traditional factor/anomaly strategies sort stocks on characteristics such as:

* Value
* Size
* Momentum
* Profitability
* Investment
* Accruals
* Equity issuance
* ROA/ROE

But these strategies **don't explicitly consider the shape of individual stocks' return distributions**.

The authors ask:

> **Are anomaly returns disproportionately driven by a handful of extreme positive-return stocks?**

And, if so:

> **Can we exploit this by incorporating expected skewness into anomaly portfolio construction?**

They study **18 anomalies** over July 1963–December 2024. 

---

# 3. The key intuition

Think about a long-short value portfolio.

Suppose you are:

**Long:** cheap/value stocks
**Short:** expensive/growth stocks

Some value stocks are small, distressed companies that have a relatively high probability of producing a spectacular upside return.

For example:

> Most months: +2%, −3%, +1%, −2%...
> Occasionally: **+100%, +200%, +500%**

That creates **positive skewness**.

If those stocks are on your **long side**, the occasional huge winners help you.

But imagine the same type of stock is on your **short side**.

Then the occasional +200% return is disastrous.

So the authors' basic portfolio rule is:

**Long → prefer high expected skewness**

**Short → prefer low expected skewness**

That is the central idea of the paper. 

---

# 4. First piece of evidence: extreme returns matter

The authors first perform a very simple experiment.

They take the monthly stock returns and **winsorize** the extreme positive observations—essentially capping the biggest upside returns.

They use the 90th, 95th and 99th percentile thresholds.

If removing extreme positive returns materially changes an anomaly's performance, that tells us that the anomaly is dependent on those extreme observations.

They find exactly that.

The effect depends on **which side of the long-short portfolio contains the highly skewed stocks**.

### Examples

**Value, Size**

High-skewness stocks tend to be concentrated in the **long leg**.

Therefore, removing extreme positive returns hurts the strategy.

**Momentum, ROE, Profitability**

High-skewness stocks tend to be concentrated in the **short leg**.

Therefore, removing extreme positive returns actually improves the strategy because it removes the occasional explosive rebound of stocks being shorted.



This is an important result because it establishes that **skewness isn't just an abstract statistical property—it materially affects anomaly returns.**

---

# 5. How do they predict skewness?

This is probably the most interesting methodological part of the paper.

They don't use future skewness.

Instead, they try to estimate:

> **What is this stock's expected skewness next month, using information available today?**

### Step 1 — Calculate realized skewness

They calculate monthly skewness from **daily stock returns**.

### Step 2 — Predict it cross-sectionally

Every month they regress realized skewness on observable characteristics such as:

* past realized skewness
* realized volatility
* momentum
* previous-month return
* size
* industry
* exchange listing

The regression is estimated separately every month.

They then use the estimated coefficients and current characteristics to produce:

**Expected Skewness₍ᵢ,t+1₎**

The model's average cross-sectional R² is only about **3%**, which the authors note is comparable to other short-horizon skewness prediction models. 

An interesting result is that:

* **volatility**
* **prior-month return**

are particularly consistent predictors of future skewness. 

---

# 6. The actual strategy

This is a **two-stage sort**.

### Stage 1

Sort stocks according to the traditional anomaly.

For example, for Value:

**High B/M → Long**

**Low B/M → Short**

### Stage 2

Within each anomaly leg, sort stocks into three groups based on expected skewness:

* Low skewness
* Medium skewness
* High skewness

Then construct:

**Long = high-skewness stocks**

**Short = low-skewness stocks**

So instead of:

> Value = cheap stocks − expensive stocks

you get something closer to:

> **Skewness-managed Value = cheap/high-skew stocks − expensive/low-skew stocks**

The authors use 10 anomaly deciles × 3 skewness terciles, creating 30 double-sorted portfolios. 

---

# 7. The headline result

This works **across all 18 anomalies**.

Average improvement:

### **+5.45 percentage points per year**

and

### **+0.12 in Sharpe ratio**



The improvements range from:

* **+0.65%** for Net Operating Assets
* to **+9.75%** for Value/BM. 

### Particularly strong results

| Strategy                 | Approx. improvement |
| ------------------------ | ------------------: |
| Value / B/M              |          **+9.75%** |
| Investment               |          **+9.04%** |
| Profitability strategies |               Large |
| Average across 18        |          **+5.45%** |

For Value, the standard portfolio earns roughly **3.4% annually**, while the skewness-managed version earns **over 13%**. 

That's a very large effect.

---

# 8. Why does it work?

The paper finds that the location of skewness matters.

### Value / Size / Investment

High-skewness stocks tend to be concentrated on the **long side**.

So selecting them increases exposure to potential extreme winners.

### Momentum / ROE / Operating Profitability

High-skewness stocks tend to be concentrated on the **short side**.

These are often distressed/low-quality stocks that can suddenly rebound.

So the strategy avoids shorting those high-skewness stocks.

The expected-skewness signal also actually works: stocks predicted to have high skewness subsequently exhibit higher realized skewness. 

---

# 9. Is this just another way of taking more factor risk?

This is an important question.

Suppose the skewness-managed Value strategy makes more money simply because it has **more Value exposure**.

Then the result wouldn't be particularly interesting.

The authors test this.

They regress the skewness-managed portfolios against their original anomaly portfolios.

They find:

* average alpha ≈ **6.7% annually**
* betas are generally **≤ 1**
* significant alpha in **14 of 18** cases

So the managed strategy generally earns more **without simply taking proportionally more exposure to the original anomaly**. 

---

# 10. What about Fama-French and other factor models?

They test the strategies against several major models:

* Fama-French 3-factor
* Carhart 4-factor
* Fama-French 5-factor
* Hou et al. q-factor
* Stambaugh-Yuan
* Daniel-Hirshleifer-Sun

The important finding is:

> **The factor models don't explain the incremental returns from skewness management.**

For example, standard Value can be largely explained by the Fama-French 5-factor model.

But the skewness-managed Value portfolio still generates approximately **6.93% alpha** relative to that model. 

The authors therefore argue that conventional linear factor models are missing a **higher-moment dimension of returns**.

---

# 11. Is it just a generic "high skewness" factor?

They test this too.

They construct a standalone:

**High expected-skewness − Low expected-skewness**

factor.

That factor itself earns only about **2.64% per year**, and isn't statistically significant at the 5% level.

More importantly, controlling for that factor **doesn't eliminate the returns from the skewness-managed anomaly portfolios**.

So their claim is not simply:

> "High-skewness stocks outperform."

Instead, it is:

> **The interaction between skewness and the underlying anomaly characteristic creates additional returns.**

That's a much stronger claim. 

---

# 12. Robustness checks

The authors do quite a lot here.

### Transaction costs

After accounting for trading costs, average improvement falls from about **5.45% to 1.58% annually**.

The authors emphasize that the managed portfolios have higher turnover.

Size, Value and Investment remain particularly significant after costs. 

This is probably one of the biggest practical caveats.

### Idiosyncratic skewness

They repeat the analysis using **idiosyncratic skewness** rather than total return skewness.

Result:

**+5.35% average annual improvement**

Very similar to the baseline. 

### Remove conventional predictors

They remove:

* volatility
* momentum
* prior-month return

from the skewness prediction model.

The strategy still works:

* roughly **+6.0%** under one specification
* roughly **+4.7%** under another

So the result isn't simply a disguised momentum/volatility strategy. 

---

# 13. When does the strategy work best?

This is another fascinating result.

The excess return isn't evenly distributed through time.

The average improvement is approximately:

**Expansion:** +3.7%/year

**Recession:** **+20.4%/year**



The authors find particularly strong performance when:

* VIX is high
* credit spreads are wide
* market conditions are stressed
* economic activity is weak

The nonlinear real-activity factor **F13** and VIX are among the strongest predictors of the skewness-management premium. 

So this strategy appears to be particularly valuable **around crises and market rebounds**.

---

# 14. A striking cumulative result

If you take the equal-weighted average of the 18 anomalies:

### Standard anomaly portfolio

$1 → approximately **$22**

### Skewness-managed portfolio

$1 → approximately **$530**

by December 2024.

But there's an important qualification:

The skewness-managed strategy beats the standard strategy in only **54% of months**.

So the outperformance isn't a steady monthly premium.

Instead, **a relatively small number of very strong months drive a large proportion of the cumulative difference**. 

This is completely consistent with the paper's central thesis about skewness.

---

# 15. What I think is the paper's deepest insight

The paper isn't really saying:

> "Skewness is another factor."

It is saying something more subtle:

### **The payoff distribution of individual stocks interacts with traditional factors.**

Traditional factor models mostly ask:

> How much exposure does this portfolio have to Value, Size, Momentum, Profitability, etc.?

The paper asks another question:

> **Which stocks within that factor exposure have the potential to generate extreme upside outcomes?**

That distinction is important.

For a long-short strategy, **the same positively skewed stock can be extremely valuable when you own it and extremely dangerous when you short it.**

That creates an interaction between:

**factor characteristic × skewness**

which conventional factor models don't capture.

The authors explicitly interpret their results this way. 

---

# 16. How I would interpret the paper critically

There are several things I'd be cautious about.

### 1. The 5.45% headline number isn't the implementable number

After transaction costs, the average improvement drops to about **1.58%**.

That's still interesting, but materially less spectacular. 

### 2. The skewness predictor is weak statistically

The expected-skewness model has an average **R² of only 3%**.

That's not necessarily fatal—predicting tail events is difficult—but it means the signal isn't highly precise. 

### 3. The strategy is naturally exposed to small/distressed stocks

The authors find that skewness management increases exposure to:

* Size
* Market beta

and decreases exposure to:

* Profitability

This makes economic sense because small, distressed stocks tend to have more lottery-like upside potential. 

### 4. The result is partly driven by rare events

That's actually the point of the paper—but it also makes the strategy potentially sensitive to:

* sample period
* extreme observations
* market microstructure
* implementation constraints

### 5. This is an empirical result, not necessarily a causal explanation

The paper demonstrates a strong statistical relationship.

It does **not completely establish why** skewness management earns the premium.

Possible explanations include:

* compensation for risk
* behavioral preferences
* exposure to distressed/small stocks
* nonlinear payoffs
* omitted factors
* investor constraints

The authors argue that existing linear factor models don't explain it, but that doesn't by itself prove a specific economic mechanism.

---

# 17. The paper's investment recipe

If I reduce the paper to an implementable algorithm, it is roughly:

```text
For each month:

1. Calculate the normal anomaly signal.

2. Estimate each stock's expected next-month skewness
   using characteristics known today.

3. Sort stocks on the anomaly.

4. Within the long leg:
      select HIGH expected-skewness stocks.

5. Within the short leg:
      select LOW expected-skewness stocks.

6. Form the long-short portfolio.

7. Rebalance monthly.
```

That's remarkably simple relative to the magnitude of the reported effect. 

---

## 18. Bottom line

My reading of the paper is:

> **Many traditional anomalies are not homogeneous portfolios. Their returns are disproportionately influenced by a small subset of stocks with lottery-like, positively skewed payoffs. By identifying those stocks in advance and making sure that positive-skewness exposure is on the long side rather than the short side, the authors substantially improve anomaly returns.**

The empirical evidence is quite broad:

* **18 anomalies**
* **1963–2024**
* ~**+5.45% annual improvement**
* ~**+0.12 Sharpe improvement**
* significant alphas after traditional factor controls
* similar results using idiosyncratic skewness
* results survive several predictor modifications
* meaningful performance remains after transaction costs
* particularly strong performance during recessions/stress

The authors therefore conclude that **higher moments—specifically skewness—contain information that conventional linear factor models are missing.** 


