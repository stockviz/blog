# Drop Analysis: Next-Month Returns of Stocks Removed from Portfolios

**Period:** 2014-09-30 → 2026-05-29  
**Portfolios:** Momentum (top 20 by 12-mo return), Mom+Skew (top decile momentum → top tercile expected skewness → top 20)  
**Question:** When a stock is dropped from the portfolio at rebalance t, how does it perform in the *next* holding period (t → t+1)?  
**Comparison groups:** Dropped (held at t-1, removed at t) vs Kept (held at both t-1 and t) vs New (added at t, not held at t-1)  

**Regime definitions (NIFTY500 MOMENTUM 50 TR drawdown from running peak):**

- **Normal:** drawdown > -5%
- **Drawdown:** drawdown ≤ -10%
- **Recovery:** drawdown between -10% and -5%, positive monthly return

---

## 1. Summary: Mean Next-Month Return by Group

| Regime | N | Mom Drop | Mom Keep | Mom New | Skew Drop | Skew Keep | Skew New |
|--------|---|---------:|--------:|-------:|----------:|---------:|--------:|
| All | 140 | +2.12% | +2.09% | +3.13% | +2.07% | +2.09% | +2.44% |
| Normal | 53 | +2.93% | +2.93% | +3.99% | +2.25% | +2.44% | +3.98% |
| Drawdown | 50 | +2.52% | +1.37% | +2.61% | +2.45% | +2.06% | +1.77% |
| Recovery | 21 | +2.08% | +2.88% | +3.42% | +2.28% | +2.33% | +2.63% |

---

## 2. Interpretation

### 1. Are dropped stocks underperformers?

- **Momentum:** dropped stocks average **2.1%** next month vs **2.1%** for kept stocks (Δ = 0pp). Dropped stocks outperform — the rebalance may be counterproductive.
- **Mom+Skew:** dropped stocks average **2.1%** next month vs **2.1%** for kept stocks (Δ = 0pp). Dropped stocks underperform — the rebalance is adding value.

### 2. Does Mom+Skew make better drop decisions?

- Mom dropped stocks earn **2.1%** vs Skew dropped stocks earn **2.1%**. Similar drop quality.

### 3. Do new stocks outperform?

- **Momentum:** new stocks average **3.1%** next month vs **2.1%** for kept stocks (Δ = 1pp). New additions outperform — the rebalance is capturing fresh momentum.
- **Mom+Skew:** new stocks average **2.4%** next month vs **2.1%** for kept stocks (Δ = 0.3pp). New additions outperform.

### 4. Hit rates (% positive next-month returns)

| Strategy | Dropped | Kept | New |
|----------|--------:|-----:|----:|
| Momentum | 56% | 65% | 60% |
| Mom+Skew | 66% | 59% | 65% |

- Mom+Skew drops **more winners** (66% vs 56%) but its new picks also have a higher hit rate (65% vs 60%).

### 5. Regime breakdown

| Regime | Mom Drop | Mom Keep | Mom New | Skew Drop | Skew Keep | Skew New |
|--------|----------|----------|---------|-----------|-----------|----------|
| Normal | +2.9% | +2.9% | +4.0% | +2.3% | +2.4% | +4.0% |
| Drawdown | +2.5% | +1.4% | +2.6% | +2.4% | +2.1% | +1.8% |
| Recovery | +2.1% | +2.9% | +3.4% | +2.3% | +2.3% | +2.6% |

---

## 6. Bottom Line

Momentum's rebalance: drops **2.1%**, keeps **2.1%**, adds **3.1%** next month.  
Mom+Skew's rebalance: drops **2.1%**, keeps **2.1%**, adds **2.4%** next month.  

Drop quality is similar between strategies (~2.1% vs ~2.1%). New-stock quality is also similar (~3.1% vs ~2.4%). However, the skewness overlay generates **~3× more turnover** (~75% of positions churn monthly vs ~35% for Momentum), so the similar drop/add quality must overcome substantially higher transaction costs.

---

## 7. Post-2020-05-01 Subset

### Mean Next-Month Return (post-2020)

| Regime | N | Mom Drop | Mom Keep | Mom New | Skew Drop | Skew Keep | Skew New |
|--------|---|---------:|--------:|-------:|----------:|---------:|--------:|
| All | 73 | +3.00% | +3.35% | +4.01% | +2.76% | +2.94% | +3.33% |
| Normal | 29 | +4.52% | +4.58% | +5.17% | +4.09% | +4.82% | +5.57% |
| Drawdown | 28 | +1.92% | +1.36% | +3.00% | +1.11% | +1.30% | +1.80% |
| Recovery | 10 | +3.09% | +6.18% | +4.47% | +3.79% | +3.38% | +3.06% |

### Hit Rates (post-2020)

| Strategy | Dropped | Kept | New |
|----------|--------:|-----:|----:|
| Momentum | 56% | 66% | 58% |
| Mom+Skew | 67% | 57% | 63% |

**Post-2020 summary:** Mom dropped: **3%**, Skew dropped: **2.8%**. Mom new: **4%**, Skew new: **3.3%**. The drop/add quality patterns are consistent with the full period.
