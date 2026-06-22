# Changepoint Detection & Regime Classification

## What is changepoint detection?

A changepoint is a point in time where the statistical properties of a sequence
change. For financial return series this typically means the mean, variance, or
distribution of returns shifts — a stock that was trending sideways suddenly
becomes volatile, or a calm market enters a turbulent regime.

Changepoint detection is the problem of finding *where* these shifts happen.
Unlike simply eyeballing a chart, formal methods provide:

- **Statistical rigor** — they control the false-positive rate (you aren't just
  seeing patterns in noise).
- **Reproducibility** — the same data always yields the same answer.
- **Multiple-change detection** — many methods find *all* changepoints in a
  series at once, not just the single most obvious one.
- **Regime awareness** — once you know where the breaks are, you can label
  each segment (calm, volatile, trending, mean-reverting) and act accordingly.

## The regime_classify.R script

`regime_classify.R` takes an `xts` object of excess returns and runs **30
changepoint-detection methods** across four families, then combines their
output into a single **date-by-date regime label**: STABLE or UNSTABLE.

Instead of trusting any one method (which can be fragile — one method's
"changepoint" is another's noise), the script lets all 30 methods *vote* on
every date. The final regime is decided by majority rule.

## Methods used

### 1. Hidden Markov Models (HMM) — `depmixS4`
Fits Gaussian HMMs with 1 through 8 hidden states and selects the model with
the lowest BIC. Each date is assigned to the most likely state; the
higher-variance state gets the UNSTABLE label.

**Vote**: 1 method (the best-BIC HMM).

### 2. Likelihood-Ratio (LR) family — `changepoint`
Searches for changepoints in the mean, variance, or both simultaneously using
four search strategies:

| Method    | Description |
|-----------|-------------|
| AMOC      | At Most One Change — finds the single best split |
| BinSeg    | Binary Segmentation — recursively splits the series |
| PELT      | Pruned Exact Linear Time — exact, fast, finds all at once |
| SegNeigh  | Segment Neighborhood — brute-force optimal with Q max |

Target × method = 3 × 4 = **12 methods**. All use BIC as the penalty.

**Votes**: 12 methods.

### 3. Change Point Model (CPM) — `cpm`
Nonparametric tests that detect distributional changes without assuming
normality. Eight test statistics, each run in two modes:

| Test | What it detects |
|------|-----------------|
| Student | Change in mean (t-test) |
| Bartlett | Change in variance |
| GLR | Generalized likelihood ratio (mean + variance) |
| Mann-Whitney | Location shift (rank-based) |
| Mood | Scale shift (rank-based) |
| Lepage | Location or scale shift |
| Kolmogorov-Smirnov | Any distributional change |
| Cramer-von-Mises | Any distributional change |

**Offline** mode: recursive binary segmentation with α = 0.05.
**Online** mode: sequential monitoring with ARL₀ = 500, startup = 20.

8 tests × 2 modes = **16 methods**.

**Votes**: 16 methods.

### 4. Barry-Hartigan — `bcp`
Bayesian changepoint model with a product-partition prior. Changepoints are
identified where the posterior probability exceeds 0.5.

**Votes**: 1 method.

---

**Total**: 1 + 12 + 16 + 1 = **30 methods**.

## How the voting works

For any method that detects changepoints (LR, CPM, BH), the series is
partitioned into segments at those changepoint dates. Each segment is labelled:

- **UNSTABLE** if the segment's average absolute return exceeds the
  overall average absolute return.
- **STABLE** otherwise.
- If a method finds zero changepoints, the entire series is STABLE.

The HMM labels its higher-variance state as UNSTABLE directly.

All 30 votes are collected into a matrix. For each date, if a majority of
methods (≥15) vote UNSTABLE, that date is UNSTABLE.

## Usage

```r
source("regime_classify.R")
result <- classify_regime(excess_returns_xts)

# Date-level regime table
result$regime_tbl
# A tibble: N x 4
#   Date       Regime   N_Unstable N_Total
#   <date>     <chr>         <int>   <int>
# 1 2020-01-01 STABLE            3      30
# 2 2020-01-02 STABLE            5      30
# 3 2020-01-03 UNSTABLE         22      30
# ...

# Per-method changepoint counts
result$summary
# A tibble: 30 x 2
#   Method          N_ChangePoints
#   <chr>                    <int>
# 1 HMM                         3
# 2 AMOC_mean                   1
# ...

# Changepoint dates from a specific method
result$LR$PELT_meanvar

# Barry-Hartigan changepoints
result$BarryHartigan
```

## Return value

| Field | Type | Description |
|-------|------|-------------|
| `regime_tbl` | tibble | Date, Regime, N_Unstable, N_Total |
| `HMM` | list | `optimal_states` (int), `cp_dates` (Date vector) |
| `LR` | list | 12 named Date vectors of changepoints |
| `CPM` | list | 16 named Date vectors of changepoints |
| `BarryHartigan` | Date vector | Changepoint dates from `bcp` |
| `summary` | tibble | Method, N_ChangePoints for all 30 methods |

## Dependencies

- `xts`, `changepoint`, `cpm`, `depmixS4`, `bcp`, `tibble`

Install missing packages with `load_packages()` or manually via
`install.packages(...)`.
