# RL for Finance — Knowledge Base

> Arranged easy → expert. Covers 8 sources in `../books/` (215-page Hilpisch book + 7 papers). Every idea is tagged for data frequency.

> **Execution roadmap:** see `../research-plan.md` — the phase-by-phase research plan grounded in this KB and the verified StockViz data inventory (NSE daily + 1-min index/derivative/MCX bars; note the per-table tick_stamp base-date conventions).

## Frequency tags

| Tag | Meaning |
|-----|---------|
| [CLOSE] | validated/tested on daily (or lower-frequency) close/bar data |
| [INTRADAY] | validated on intraday data (minutes / seconds / ticks, futures order-book or index) |
| [BOTH] | architecture is frequency-agnostic; works on either if you change bar size (Δt) |
| [CLOSE→INTRADAY*] | paper used close but author notes it ports to intraday with caveats |

Most RL mechanics are [BOTH] — the frequency distinction lives in **environment design, features, reward horizon, and transaction-cost modeling**, not in DQN vs PPO.

## Learning path (read in order)

```
Level 1 — Foundations (no finance)          → 01-foundations.md
Level 2 — Core RL → Deep RL                 → 02-core-rl-to-deep.md
Level 3 — Finance as MDP (states/actions/   → 03-finance-as-mdp.md
           rewards, where the analogy breaks)
Level 4 — Data, features, augmentation      → 04-data-features-augmentation.md
Level 5 — Environments & Agents (Gym API,   → 05-environments-agents.md
           Finance/Hedging/Execution envs)
Level 6 — Algorithms catalogue              → 06-algorithms.md
Level 7 — Applications (trading, hedging,   → 07-applications.md
           allocation, execution)
Level 8 — Evaluation & pitfalls             → 08-evaluation-pitfalls.md
Level 9 — Expert frontier & open problems   → 09-expert-frontier.md

Appendix — 10-frequency-matrix.md  (every idea × CLOSE/INTRADAY cross-ref)
           00-sources.md           (bibliographic inventory)
```

- **If you want the 30-min tour:** read README → 10-frequency-matrix.md → 07-applications.md.
- **If you want to build:** 01 → 05 → 06 → code templates in 05/06.
- **If you want to research:** 08 → 09 → 00-sources.md.

## Sources (see 00-sources.md for full citations)

1. LazyProgrammer (2017) — *Artificial Intelligence: RL in Python* (epub, intro) — [BOTH] basics
2. Bertoluzzo & Corazza (2012) SSRN WP 33 — FTS on daily prices — [CLOSE]
3. Hilpisch (2024) O'Reilly — *Reinforcement Learning for Finance* (215pp, code-heavy) — [BOTH], ch.6-9 finance apps
4. Théate & Ernst (2021) ESWA 173 — TDQN + Sharpe reward, artificial trajectories — [CLOSE]
5. Avramelou et al. (2024) ESWA 238 — Multi-modal DRL (price + sentiment embeddings) — [CLOSE] (daily OHLC + crypto-sentiment)
6. Si et al. (2017) IEEE ISCID — Multi-objective DRL for **intraday** stock-index futures — [INTRADAY]
7. Ponomarev et al. (2019) JCTE — A3C + LSTM on RTS futures, 60-sec decisions — [INTRADAY]
8. Hambly, Xu & Yang (2023) *Mathematical Finance* 33 — Survey *Recent advances in RL in finance* (67pp) — [BOTH] expert synthesis

## How the KB was built

- All PDFs/epub extracted with PyMuPDF; TOC, state/action/reward definitions, and experimental sections cross-checked.
- Claims are cited as [Source §page/ch]. When a source didn't specify frequency, it is tagged by its dataset (e.g., Théate's 30 stocks = daily [CLOSE]).
- Code snippets in Hilpisch chapters are summarized, not copied verbatim.

## Conventions

- **State/Obs** = what agent sees; **Action** = discrete {-1,0,1} or continuous position/size; **Reward** = PnL, log-return, Sharpe/Sortino, or hedging error.
- Prices are `p_t`, log-return `r_t = log(p_t/p_{t-1})`, transaction cost `c` per trade/volume.
- Bar size `Δt` = trading frequency constraint (Théate §2.2): max frequency = 1/Δt.
