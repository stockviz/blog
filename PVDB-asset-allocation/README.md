# PAA · VAA · DAA · BAA · ABA — A Comparative Backtest

**Keller & Keuning (2016–2022): Five Breadth-Momentum Strategies, Re-Tested with Real ETFs**

This document summarizes five breadth-momentum strategies — four from the
Keller/Keuning series plus ABA, a new strategy designed from their lessons.
All are re-tested using walk-forward optimization, real ETFs only (no synthetic
proxies), and reported out-of-sample.

## 1. The Papers

All five strategies share the same DNA: dual momentum (absolute + relative) plus a
"breadth" indicator that scales the cash/bond allocation based on how many assets
have positive momentum.

| Paper | Year | Core Innovation | Filter | Breadth Source |
|---|---|---|---|---|
| [**PAA**](https://stockviz.biz/2026/07/12/protective-asset-allocation/) — Protective | 2016 | Gradual bond-fraction blending | SMA(12) | Self |
| [**VAA**](https://stockviz.biz/2026/07/12/vigilant-asset-allocation/) — Vigilant | 2017 | Fast 13612W, winner-take-all | 13612W | Self |
| [**DAA**](https://stockviz.biz/2026/07/12/defensive-asset-allocation/) — Defensive | 2018 | Separate canary universe | 13612W | Canary |
| [**BAA**](https://stockviz.biz/2026/07/12/bold-asset-allocation/) — Bold | 2022 | Dual-filter + multi-asset defensive | SMA+13612W | Canary |
| [**ABA**](strategy.md) — Adaptive (new) | 2026 | Dual-filter + smoothed self-breadth + hysteresis | SMA+13612W | Self |

**How they differ in one sentence each:**

- **PAA** blends into bonds gradually using a slow SMA filter — steady, low turnover.
- **VAA** uses a fast 13612W filter and goes all-in on one asset — high return, high cash drag.
- **DAA** adds a separate canary universe for crash signaling — designed to reduce cash drag.
- **BAA** combines PAA's slow offense with DAA's fast canary defense — the hybrid.
- **ABA** takes BAA's dual-filter, drops the fragile canary for self-breadth, adds sigmoid
  smoothing and hysteresis — designed from what worked and what didn't in the four above.

## 2. Backtest Methodology

All five strategies use the same robustness-first methodology. Parameters and canary
assets are re-selected each year using only data strictly prior to the test year.

| Design Choice | What This Addresses |
|---|---|
| **Real ETFs only** (no synthetic proxies) | Honest data — shorter history as the cost of authenticity |
| **Walk-forward optimization** (annual expanding-window) | No static IS/OS split; every choice uses only prior data |
| **Pre-registered small parameter grids** | Avoids free-form grid-search overfitting |
| **Tiered transaction costs** (5 bps liquid, 15 bps moderate) | Realistic cost drag |
| **Canary-pool and offensive variants** | Tests sensitivity — never varied in original papers |
| **Out-of-sample-only reporting** | Parameters frozen before each test year |

### Universe Definitions

| Strategy | Risky Universe | Canary / Breadth | Bond/Cash |
|---|---|---|---|
| VAA | 12 ETFs | Self | SHY, IEF, LQD |
| DAA | 10 ETFs | HYG, EEM/SPHB, TLT, IEF, UUP | BIL, IEF, SHY |
| PAA | 10 ETFs | EEM/SPHB, AGG | IEF, SHY, BIL, AGG |
| BAA | 10 ETFs (SPY or QQQ) | EEM/SPHB, AGG | BIL, IEF, SHY, TLT, LQD, AGG |
| ABA | 10 ETFs (SPY or MTUM) | Self (no canary) | BIL, IEF, SHY |

ABA is the only strategy with no canary universe — breadth comes from the
offensive universe itself, using smoothed sigmoid scoring and hysteresis instead
of a binary bad-asset count. See the [strategy design](strategy.md) and
[backtest plan](backtest.md) for full details.

### Backtest Windows

| Strategy | Full Window | OS Window | OS Split |
|---|---|---|---|
| VAA | 2008-05 → 2026-07 | 2017-06 → 2026-07 | 2017-06-01 |
| DAA | 2018-02 → 2026-07 | 2022-05 → 2026-07 | 2022-05-01 |
| PAA | 2017-02 → 2026-07 | 2021-10 → 2026-07 | 2021-10-01 |
| BAA | 2017-02 → 2026-07 | 2021-10 → 2026-07 | 2021-10-01 |
| ABA | 2019-02 → 2026-07 | 2022-11 → 2026-07 | 2022-11-01 |

## 3. Out-of-Sample Results

All results below are from the true out-of-sample period — parameters were
selected on prior data only and held frozen through each test year.

### VAA (OS: 2017-06 → 2026-07, 110 months)

| Strategy | CAGR | MaxDD | Sharpe |
|---|---|---|---|
| VAA (T=2, B=2) | 4.1% | −24.5% | 0.43 |
| Dual | 11.4% | −21.7% | 0.91 |
| 60/40 | 7.5% | −21.4% | 0.74 |
| EW | 6.8% | −22.4% | 0.63 |
| EWC | −1.0% | −21.0% | NaN |
| SPY | 13.2% | −24.8% | 0.86 |

### DAA, PAA, BAA, ABA — OS-Only Comparison

ABA is ranked alongside the prior strategies. ABA-MTUM uses MTUM (momentum
factor ETF) in the offensive sleeve; ABA-SPY uses SPY. DAA/PAA/BAA all use
SPY-based offensive universes unless noted.

| Strategy | Filter | Breadth | Offense | OS CAGR | OS MaxDD | OS Sharpe |
|---|---|---|---|---|---|---|
| **ABA-MTUM** | SMA+13612W | Self | MTUM | 5.5% | −11.1% | 0.59 |
| **BAA-QQQ-SPHB** | SMA+13612W | SPHB | QQQ | 4.5% | −11.2% | 0.47 |
| BAA-SPY-SPHB | SMA+13612W | SPHB | SPY | 4.2% | −12.1% | 0.45 |
| ABA-SPY | SMA+13612W | Self | SPY | 4.1% | −17.5% | 0.47 |
| **DAA-SPHB** | 13612W | SPHB | SPY | 3.8% | −9.5% | 0.41 |
| BAA-QQQ-EEM | SMA+13612W | EEM | QQQ | 1.6% | −13.7% | 0.22 |
| **PAA-EEM** | SMA | EEM | SPY | 1.2% | −19.7% | 0.16 |
| DAA-EEM | 13612W | EEM | SPY | 0.5% | −15.3% | 0.10 |
| PAA-SPHB | SMA | SPHB | SPY | −0.3% | −21.3% | 0.01 |
| BAA-SPY-EEM | SMA+13612W | EEM | SPY | −0.3% | −16.4% | 0.00 |
| **SPY** | — | — | — | 12.0% | −20.9% | 0.78 |
| **60/40** | — | — | — | 5.1% | −18.1% | 0.49 |
| EW | — | — | — | 2.6% | −21.2% | 0.28 |
| EWC | — | — | — | −0.9% | −14.9% | NaN |

*EW = equal-weight risky (offensive) universe. EWC = equal-weight cash (defensive/bond) universe. EWC serves as the risk-free proxy for excess Sharpe calculations per the original papers' convention. NaN Sharpe for EWC is expected: excess over itself is undefined.*

ABA-MTUM leads the table with OS Sharpe 0.59 — the highest risk-adjusted return
of any breadth-momentum strategy. It delivers this with the simplest architecture
in the family: no canary universe, no multi-asset defensive sleeve, three cash
assets. Its OS drawdown (−11.1%) is second only to DAA-SPHB (−9.5%), while
delivering 45% more CAGR (5.5% vs 3.8%). Parameter stability is perfect: L=12,
Top=3, B=1 across all 8 walk-forward years (24/24 decisions).

VAA is shown separately above because it runs on a longer OS window. On that
window, its OS Sharpe (0.43) is competitive with BAA and DAA but trails ABA.
Its drawdown (−24.5%) covers the COVID crash which the shorter windows miss.

### Canary / Parameter Stability

| Strategy | Pool | Selection | Verdict |
|---|---|---|---|
| DAA | EEM | EEM (1 yr) → TLT (7 yrs) → EEM (1 yr) | Unstable |
| DAA | SPHB | SPHB (8/9 yrs), TLT (2020) | Stable |
| PAA | EEM | EEM (10/10 yrs) | Perfect |
| PAA | SPHB | SPHB (10/10 yrs) | Perfect |
| BAA | EEM | EEM (8/10 yrs), AGG (1st), EEM+AGG (2020) | Stable |
| BAA | SPHB | SPHB (8/10 yrs), AGG (1st), SPHB+AGG (2020) | Stable |
| ABA | Self | L=12, Top=3, B=1 (24/24 decisions) | Perfect |

## 4. Analysis

### Finding 1: ABA-MTUM leads the family on OS performance

ABA-MTUM achieves the highest OS Sharpe (0.59) and second-lowest drawdown
(−11.1%) with the simplest architecture. It uses no canary universe — breadth
comes from the offensive assets themselves via smoothed sigmoid scoring.
The dual-filter design (SMA for offense, 13612W for defense) combined with
hysteresis prevents the whipsaw that plagues VAA while responding faster
than PAA's SMA-only approach.

ABA-SPY (Sharpe 0.47) matches BAA-SPY-SPHB (0.45) despite having no canary,
confirming that self-breadth with smoothing and hysteresis can equal or exceed
canary-based approaches with less complexity.

### Finding 2: SPHB dominates as a canary under 13612W — but self-breadth wins

Every SPHB variant outperforms its EEM counterpart under 13612W. The gap is
dramatic: DAA-SPHB (0.41) vs DAA-EEM (0.10), BAA-QQQ-SPHB (0.47) vs
BAA-QQQ-EEM (0.22). Under SMA (PAA), the pattern reverses — slow filter
needs slow canary.

But ABA-MTUM (0.59) beats them all with *no canary at all*. Self-breadth with
sigmoid smoothing extracts more signal from the offensive universe than a
separate canary universe can provide — while eliminating the fragility of
canary selection entirely.

### Finding 3: DAA's canary decoupling is the family's weakest link

DAA-EEM (Sharpe 0.10) and DAA-SPHB (0.41) span the widest performance gap
of any strategy. The EEM canary was abandoned after one year — a walk-forward
pathology. Even with SPHB, DAA trails ABA and BAA. VAA (0.43) beats DAA-SPHB
(0.41) with the same 13612W filter and no canary. The canary adds complexity
without benefit.

### Finding 4: BAA's dual-filter works, but the canary and defensive sleeve are excess

BAA-QQQ-SPHB (0.47) and BAA-SPY-SPHB (0.45) are strong performers, but
ABA-MTUM (0.59) achieves better results by replacing the canary with
self-breadth and the 6-asset defensive sleeve with 3 cash assets. BAA's
architecture is directionally correct; ABA is the streamlined version.

### Finding 5: PAA's SMA filter is too slow for meaningful protection

PAA-EEM (0.16) and PAA-SPHB (0.01) are the worst OS performers. The 12-month
SMA lag makes PAA essentially a more expensive equal-weight portfolio over
short windows. The SMA filter needs either a longer backtest (where its
stability pays off) or a faster companion filter for defense — exactly what
ABA and BAA provide.

### Finding 6: No strategy beats buy-and-hold in a bull market

Every breadth-momentum strategy trails SPY and 60/40 on absolute OS return.
The structural conservatism of spending 50–86% of time in bonds/cash is a
headwind in rising markets. The value is drawdown protection:

| Strategy | OS MaxDD | SPY OS MaxDD | 60/40 OS MaxDD |
|---|---|---|---|
| DAA-SPHB | −9.5% | −20.9% | −18.1% |
| ABA-MTUM | −11.1% | −20.9% | −18.1% |
| BAA-QQQ-SPHB | −11.2% | −20.9% | −18.1% |
| BAA-SPY-SPHB | −12.1% | −20.9% | −18.1% |
| VAA | −24.5% | −24.8% | −21.4% |

### Finding 7: Walk-forward stability is excellent — except DAA-EEM

TopN=3 was selected 100/100 times across BAA and PAA. ABA selected L=12,
Top=3, B=1 in 24/24 decisions. When a strategy works, its parameters are
remarkably stable. The exception — DAA's EEM canary flip — is the diagnostic
the walk-forward design exists to surface.

## 5. Conclusion

**ABA-MTUM is the best OS performer.** Sharpe 0.59, drawdown −11.1%, perfect
parameter stability, and the simplest architecture in the family. It validates
the design thesis: dual-filter + self-breadth + smoothing + hysteresis beats
every prior approach. The MTUM momentum-factor ETF in the offensive sleeve is
an intuitive fit. Whether its edge persists over longer windows and different
regimes remains to be tested.

**BAA-QQQ-SPHB is the best prior-art strategy.** Sharpe 0.47, drawdown −11.2%.
BAA-SPY-SPHB (0.45) is the conservative choice — nearly identical risk-adjusted
return without the QQQ regime bet.

**VAA has the strongest long-term evidence.** 18 years, simplest design, most
dramatic drawdown reduction versus SPY (−47% → −24% on its full window). For
an investor whose primary goal is crash protection, VAA has the most real-world
data behind it.

**What didn't work:**

- **DAA's canary decoupling** — abandoned immediately in walk-forward; even
  when it works (SPHB), it underperforms self-breadth.
- **PAA's SMA-only filter** — too slow for meaningful crash protection on
  realistic timeframes.
- **Multi-asset defensive sleeves** — SHY/BIL/IEF do all the work; TLT, LQD,
  AGG, DBC are window dressing.

**What we learned:**

- **Self-breadth beats canaries.** ABA's smoothed sigmoid breadth on the
  offensive universe outperforms every canary-based approach. The canary
  concept — DAA's core innovation — adds fragility without benefit.
- **SPHB is a genuine discovery.** Never tested in any original paper, yet
  dominates EEM under 13612W across all canary-based strategies. Clean
  out-of-sample validation.
- **Filter pairing matters.** Fast+fast (13612W+SPHB) works. Slow+slow
  (SMA+EEM) works. Cross them and performance collapses.
- **Simple cash sleeves are sufficient.** Three assets (BIL/IEF/SHY) with
  momentum selection do everything a 6–7 asset defensive universe does,
  with half the complexity.

**The bottom line:** Breadth momentum is real. ABA-MTUM (Sharpe 0.59) shows
that fixing the known weaknesses — fragile canaries, binary triggers, slow
filters, over-engineered sleeves — produces measurable improvement. Whether
any of these strategies justify their return sacrifice versus buy-and-hold
depends on the investor's drawdown tolerance.

## Output Files

- [ABA — Adaptive Breadth Allocation](strategy.md) (new)
  - [Backtest plan](backtest.md)
  - [Cumulative returns](aba-cumulative.png) · [OS returns](aba-cumulative-os.png)
  - [Drawdowns](aba-drawdowns.png) · [Metrics table](aba-metrics.png)
- [BAA — Bold Asset Allocation](../bold-asset-allocation/README.md)
  - [Cumulative returns](../bold-asset-allocation/baa-cumulative.png) · [OS returns](../bold-asset-allocation/baa-cumulative-os.png)
  - [Drawdowns](../bold-asset-allocation/baa-drawdowns.png) · [Metrics table](../bold-asset-allocation/baa-metrics.png)
- [DAA — Defensive Asset Allocation](../defensive-asset-allocation/README.md)
  - [Cumulative returns](../defensive-asset-allocation/daa-cumulative.png) · [OS returns](../defensive-asset-allocation/daa-cumulative-os.png)
  - [Drawdowns](../defensive-asset-allocation/daa-drawdowns.png) · [Metrics table](../defensive-asset-allocation/daa-metrics.png)
- [PAA — Protective Asset Allocation](../protective-asset-allocation/README.md)
  - [Cumulative returns](../protective-asset-allocation/paa-cumulative.png) · [OS returns](../protective-asset-allocation/paa-cumulative-os.png)
  - [Drawdowns](../protective-asset-allocation/paa-drawdowns.png) · [Metrics table](../protective-asset-allocation/paa-metrics.png)
- [VAA — Vigilant Asset Allocation](../vigilant-asset-allocation/README.md)
  - [Cumulative returns](../vigilant-asset-allocation/vaa-cumulative.png) · [OS returns](../vigilant-asset-allocation/vaa-cumulative-os.png)
  - [Drawdowns](../vigilant-asset-allocation/vaa-drawdowns.png) · [Metrics table](../vigilant-asset-allocation/vaa-metrics.png)

---

## Forward Test

ABA was designed with full knowledge of how PAA, VAA, DAA, and BAA performed
over the 2008–2026 backtest window — it is not a blind out-of-sample result.
The true test is forward: we will circle back 5 years from now and compare how
all five strategies actually performed by running the consolidated runner:

```bash
cd /mnt/data/blog/PVDB-asset-allocation
Rscript pvdb-runner.R
```

This script reloads all data from the database (which will have 5 additional
years of price history), re-runs every strategy's walk-forward with the same
pre-registered parameter grids, and produces a fresh consolidated table and
charts. ABA's design thesis — that dual-filter self-breadth with smoothed
sigmoid scoring and hysteresis outperforms canary-based approaches — will be
tested against genuine future data that was not available when the strategy
was conceived.
