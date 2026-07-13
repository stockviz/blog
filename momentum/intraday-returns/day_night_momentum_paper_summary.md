# Paper Summary: What Drives Momentum and Reversal? Evidence from Day and Night Signals

**Authors:** Yashar H. Barardehi (Chapman University), Vincent Bogousslavsky (Boston College), Dmitriy Muravyev (University of Illinois Urbana-Champaign)
**Status:** Conditionally accepted, *Review of Financial Studies*

---

## 1. Core Question

Does momentum come from what happens **during the trading day (intraday)** or **between the close and next open (overnight)**? The authors split the standard past-return momentum signal into its intraday and overnight components and test which piece actually predicts future returns.

---

## 2. Main Finding

- **Intraday-signal momentum works**: Stocks with high past intraday returns keep outperforming, with **no long-term reversal**.
- **Overnight-signal momentum does not work**: Past overnight returns show no significant predictive power for future returns.
- Example (7-month formation, U.S. 1963–2019): intraday-signal strategy earns **0.73%/month** (t = 3.94); overnight-signal strategy earns **0.10%/month** (t = 0.56, insignificant).
- Regular (24-hour) momentum sits in between, since it mixes both components.

This is a **novel decomposition of a well-known anomaly** — momentum isn't a single phenomenon; splitting the signal reveals it's almost entirely an intraday effect.

---

## 3. Data and Method

- **Sample**: U.S. common stocks (NYSE/AMEX/NASDAQ), 1963–2019, plus a 1926–1962 NYSE-only extension (~100 years total) and 32 international markets (1996–2023).
- **Signal construction**: Daily intraday return = close/open − 1. Overnight return is backed out so intraday × overnight compounds to the standard close-to-close return. These are compounded over formation windows (1, 7, 12, 60 months) to build monthly signals.
- **Portfolios**: Monthly decile sorts on NYSE breakpoints, value-weighted, three-factor (market/size/value) alphas, Newey-West standard errors.
- **Important design choice**: portfolios still earn the **normal holding-period return** — only the *signal* is decomposed, not the trading strategy — so trading frequency matches regular momentum exactly.

---

## 4. Key Robustness Results

- Holds within small-cap and large-cap subsamples (stronger among small stocks).
- Holds using **quote midpoints** 15 minutes after the open (1985–2015), ruling out liquidity effects at the open, bid-ask bounce, and stale overnight prices.
- Holds pre-1963 (1926–1962) and internationally (28 of 32 countries show intraday momentum beating overnight momentum).
- Not driven by differences in intraday vs. overnight volatility (double-sort on volatility ratio confirms this).
- Robust to excluding earnings announcements and major news events — so it isn't just delayed reaction to overnight news.

---

## 5. Why Intraday Returns Are Special: The Mechanism

The authors argue intraday returns reflect **private information revealed through trading**, because:
- Over 96% of trading volume happens during regular hours (after-hours trading is minor even today).
- Intraday returns are far more volatile per unit time than overnight returns — consistent with private information being impounded via trading (Kyle 1985; French & Roll 1986).
- Overnight returns, by contrast, occur with almost no trading, so they more likely reflect public news or sentiment rather than trading-based private information.

Supporting evidence:
- **Time-of-day tests**: momentum is strongest using **morning** returns (most informed order flow), weak/insignificant for mid-day and afternoon.
- **Volume-conditioned signals**: momentum is stronger when signals are built from **low-volume days** — consistent with slower price discovery when trading is thin.
- **Analyst forecasts**: analysts systematically **underreact** to past intraday returns (a 1% intraday return predicts a 0.34% future forecast error) but not to overnight returns.

---

## 6. Theoretical Interpretation

The authors test three leading behavioral theories of momentum:

| Theory | Mechanism | Consistent with results? |
|---|---|---|
| Barberis, Shleifer & Vishny (1998) | Underreaction to **public** news (conservatism bias) | No clear role for private info — less consistent |
| Daniel, Hirshleifer & Subrahmanyam (1998) | **Overreaction** via overconfidence/self-attribution | Inconsistent — predicts reversal, which isn't observed |
| **Hong & Stein (1999)** | **Underreaction to other investors' private signals**, gradual diffusion into prices | **Best fit** — momentum without reversal, driven by trading |

Because intraday-signal momentum **does not reverse** (profits persist and even continue over years 2–5), the results point away from overreaction stories and toward **underreaction to private information conveyed through trading** — investors don't fully extract the information content of other traders' activity from prices.

---

## 7. Alternative Explanations Considered and Largely Ruled Out

- **News/sentiment-driven momentum**: hard to reconcile with overnight signal's lack of predictive power, since overnight moves are usually attributed to news/sentiment.
- **Da, Gurun & Warachka (2014) "frog in the pan" (information discreteness)**: doesn't fully explain results — intraday momentum remains strong even conditioning on discrete information.
- **Investor sentiment (Baker & Wurgler)**: no significant difference in momentum alphas across high vs. low sentiment periods.
- **Institutional ownership / clientele effects (Lou, Polk & Skouras 2019)**: intraday-signal momentum holds across all institutional-ownership terciles — clientele composition doesn't explain it.
- **Short-term reversal**: a separate, distinct phenomenon from the medium-term momentum studied here.

---

## 8. Contribution

- Documents a **novel, robust stylized fact** spanning ~100 years of U.S. data and 32 countries: momentum lives in intraday returns, not overnight returns.
- Reframes momentum and long-term reversal as **potentially separate phenomena**, rather than two sides of the same coin.
- Provides some of the cleanest available evidence favoring **underreaction-to-private-information** theories (Hong & Stein 1999; Luo, Subrahmanyam & Titman 2021) over public-news or overreaction-based theories.

---

## 9. Caveats (Acknowledged by the Authors)

- The interpretation relies on the assumption that intraday returns are more informative of private signals than overnight returns — this is not directly observable and could vary over time or across markets.
- The tests cannot fully rule out all competing theories; the authors describe their evidence as suggestive of underreaction, not dispositive proof.
