# SEBI surveillance measures

**Blog**: [SEBI Surveillance Measures](https://stockviz.biz/2026/09/07/sebi-surveillance-measures/)

SEBI and Indian stock exchanges use surveillance measures to monitor securities that show unusual price or volume activity, volatility, weak fundamentals, or other surveillance concerns. The measures are preventive: they alert investors, increase the controls applied to trading, and help protect market integrity. Being placed under surveillance does not, by itself, establish fraud or wrongdoing.[1][2][3]

This README explains the measures most commonly shown as surveillance indicators by brokers. It is an educational overview, not investment advice. Exchange rules, lists, stages, and applicable restrictions can change. Check the latest notice from the relevant exchange before placing an order.

## At a glance

| Measure | What it is intended to address | Possible trading impact |
| --- | --- | --- |
| ASM | Unusual price, volume, or volatility patterns identified using objective parameters | Additional monitoring; restrictions can apply depending on the ASM category and stage |
| GSM | Prices that appear inconsistent with a security's financial health or fundamentals | Possible trade-to-trade settlement, surveillance deposit, weekly trading, or an upper price freeze |
| ESM | Abnormal price movements, particularly in smaller companies; the NSE framework covers companies with market capitalization below ₹1,000 crore | Stage-based price bands, trade-to-trade settlement, additional margin, or periodic call auction |
| Trade to trade (T2T) | Settlement discipline for securities subject to enhanced surveillance | A purchase cannot normally be sold on the same day; sale is generally available from T+1 |
| ASM-IBC | Securities connected with insolvency resolution proceedings | Additional surveillance controls and possible restrictions |
| ICA | Securities covered by an inter-creditor agreement surveillance category | Additional surveillance controls and possible restrictions |
| Promoter encumbrance | High promoter or promoter-and-non-promoter share encumbrance | A warning indicator and possible additional controls |

The exact outcome depends on the measure, stage, exchange circular, and any combination of indicators on the security.[1][4]

## How the main measures work

### Additional Surveillance Measure (ASM)

ASM is based on objective parameters such as price variation, volume variation, and volatility. It is designed to identify securities that need closer monitoring and to encourage investors to perform additional due diligence.[2]

An ASM label is not a conclusion that a company has violated a law. It signals that the security meets the exchange's surveillance criteria at the time of review. Securities can be added to or removed from the list as the exchange reviews the data.[2]

### Graded Surveillance Measure (GSM)

GSM targets securities whose market prices may not be commensurate with their financial health or fundamentals. NSE identifies factors such as earnings, book value, fixed assets, net worth, price-to-earnings multiples, and market capitalization in its framework.[3]

GSM is staged. Depending on the stage, the exchange may apply one or more of the following controls:

- Transfer to the T2T segment.
- Collection of an additional surveillance deposit.
- Trading once per week.
- Freezing the price on the upper side.
- Other measures considered necessary to maintain market integrity.

The exchange can apply these actions with short notice, and GSM operates alongside other surveillance measures.[3]

### Enhanced Surveillance Measure (ESM)

ESM is applied by SEBI and exchanges using objective parameters such as price variation and standard deviation. The NSE framework covers main-board and SME companies with market capitalization below ₹1,000 crore.[4]

ESM uses stages. The applicable restrictions can include a narrower price band, T2T settlement, additional margin, and trading through periodic call auctions. Read the current ESM circular or FAQ for the stage-specific conditions before trading.[4]

## Practical effects for an investor

### Same-day selling may be unavailable

If a security is in the T2T segment, buying it and selling it on the same day is generally not permitted. The position can generally be sold on the next trading day, T+1.[1]

Securities commonly moved to T2T include those in LT ASM Stage 4, GSM Stage 2 or above, ESM Stage 1 or 2, and ASM-IBC Stage 1 or 2. The applicable exchange circular takes precedence.[1]

### Price bands can become tighter

Securities under LT ASM Stage 4, any GSM stage, any ESM stage, or any ASM-IBC stage may be subject to a price band of 5% or lower, according to the broker's summary of exchange rules.[1]

A price band limits how far the security can move during a trading session. It can prevent an order from executing if the order price falls outside the permitted range.

### Additional margin or blocked fresh buying

GSM Stage 2 carries a 50% additional surveillance deposit, while GSM Stages 3 and 4 and ASM-IBC Stages 1 and 2 carry a 100% deposit under the referenced broker summary.[1] Brokers may handle this requirement differently. For example, a broker that does not collect the deposit may block fresh buying while allowing existing positions to be exited.[1]

### Trading may occur only once a week

GSM Stage 3 or above and ASM-IBC Stage 2 may be traded once a week under the referenced summary.[1] Confirm the trading calendar and effective date on the exchange website.

### Periodic call auction may apply

Illiquid securities that meet specified criteria can be moved to periodic call auction sessions. The broker summary gives examples including an average daily number of trades below 50 and daily trading volume below 10,000, among other conditions.[1] ESM Stage 2 securities may also be transferred to T2T and periodic call auction arrangements.[1][4]

## What to check before placing an order

1. Identify every surveillance indicator shown for the security. Multiple indicators can apply at the same time.
2. Check the current ASM, GSM, ESM, and ASM-IBC lists on the relevant exchange website.
3. Read the applicable exchange circular, including the stage, effective date, price band, settlement type, margin, and trading schedule.
4. Confirm whether the order is a fresh buy, an exit from an existing position, or a sell intended for the same day.
5. Review liquidity and execution risk. A displayed price does not guarantee that an order will execute.
6. Perform independent research on the company's financials, disclosures, and liquidity. A surveillance label is not a substitute for due diligence.

## Common surveillance indicators

Broker order screens can combine a measure with its stage. The following examples are described in the referenced NSE FAQ and broker guidance:[1][4]

| Indicator or label | Meaning |
| --- | --- |
| GSM Stage 1–6 | Graded Surveillance Measure stage |
| STASM Stage 1–2 | Short-term Additional Surveillance Measure stage |
| LTASM Stage 1–4 | Long-term Additional Surveillance Measure stage |
| ASM-IBC Stage 1–2 | Insolvency-resolution-related ASM stage |
| ASM-ICA Stage 1–2 | Inter-creditor-agreement-related ASM stage |
| ESM Stage 1–2 | Enhanced Surveillance Measure stage |
| Promoter encumbrance | High promoter or related share encumbrance category |
| Unsolicited SMS or video | Exchange information or watch-list indicator; review the source and context |

The label shown by a broker may represent a combination, such as LTASM plus GSM. Treat the combination as cumulative until the exchange confirms otherwise.[1][4]

## Findings from the StockViz event report

The accompanying `report.R` script was run against 562,240 `SEBI_SURVEILLANCE` records. It identified 2,663 first entries, 12,333 grade transitions, 507 exits, and 438 re-entries. The event file contains one row per ticker event and records the return source used for each observation.

For events with both a usable return and an original market-cap decile, the mean event-day return was -0.31% for first entries, +0.42% for grade transitions, +0.25% for exits, and +1.11% for re-entries. Coverage was 1,872 first entries, 8,538 grade transitions, 460 exits, and 396 re-entries.

The decile analysis provides evidence of differences across original market-cap deciles for first entries (Kruskal-Wallis p = 0.0023) and grade transitions (p < 0.0001). It does not provide evidence of a decile difference for exits (p = 0.7693) or re-entries (p = 0.3176). Decile 10 has only two first-entry observations with usable returns, so its plotted distribution is not informative on its own. These are unconditional event-day comparisons, not causal estimates; they do not control for sector, date, surveillance stage, liquidity, or overlapping events.

The return-source breakdown was 11,961 observations from `eod_adjusted_nse`, 632 from `RETURN_SERIES_ALL`, and 3,348 without a usable return. The report uses adjusted-close returns first and falls back to the daily return series only when an adjusted-close event return cannot be computed.

## Event-reaction backtest diagnostic

The `backtest/` subfolder contains a reproducible opportunity/risk diagnostic based on the event extract. It aggregates event-day returns into an equal-weight daily reaction series for four views: all events long, entries/transitions long, exits long, and entries/transitions contrarian. It reports cumulative wealth, drawdown, Sharpe, CAGR, maximum drawdown, 5% VaR, 5% CVaR, best/worst day, positive-day rate, event concentration, and cumulative transaction drag.

This is not a causal trading backtest. `surveillance-events.csv` contains the return from the prior close to the event-day close, while the surveillance event is only observable at or after that close. The diagnostic therefore measures the size and tail risk of the observed reaction, not the return available to an investor who trades after seeing the surveillance label. A proper causal backtest needs post-event prices, next-session execution assumptions, liquidity/price-band handling, and transaction costs. The script intentionally does not fabricate those inputs.

The diagnostic uses one available-sample window: 18 September 2020 through 7 September 2026. There is no separate full/post split because the established pre window through 31 December 2019 has no observations. A 50 bps one-way drag is charged once per equal-weight event-date transaction and is included in the daily net returns and metrics. After that drag, entries/transitions long had a -0.24% mean daily return, -57.1% CAGR, -0.82 Sharpe, -99.4% maximum drawdown, -4.94% 5% VaR, and -7.83% 5% CVaR. All-events long had a -0.27% mean daily return, -59.6% CAGR, -0.98 Sharpe, -99.6% maximum drawdown, -4.85% 5% VaR, and -7.75% 5% CVaR. These wealth figures are diagnostic compounding and must not be interpreted as investable returns.

The cumulative transaction drag was 7.43 return points for entries/transitions and 7.48 return points for all events. The event-level bootstrap means remain gross reaction estimates: entries/transitions were 0.29%, with a 95% interval of 0.16% to 0.43%; exits were 0.23%, with an interval of -0.14% to 0.60%. The largest observed daily event reaction was +64.5% gross and the worst was -39.9% gross, while the maximum event count on one date was 208. These tails and concentration measures are the main risk result: a small number of event dates can dominate any naive implementation.

Run the diagnostic from the repository root with:

    Rscript backtest/backtest.R

Outputs are written only under `backtest/`: `daily_reaction_returns.csv`, `metrics.csv`, `bootstrap_event_mean.csv`, `event_reaction_cumulative_drawdown.png`, `metrics_table.html`, and `metrics_table.png`.

### Volume findings

The report also compares event-day traded volume with the prior trading day's volume using the `v` column in `eod_adjusted_nse`. It does not substitute a volume estimate from `RETURN_SERIES_ALL`, so the volume sample is smaller when the PostgreSQL volume history is missing. There were 9,646 of 15,941 events with both event-day and prior-day volume; after requiring an original decile as well, the usable sample contained 1,761 first entries, 6,159 grade transitions, 349 exits, and 323 re-entries.

Mean volume increased by 0.29% on first-entry days, 2.45% on grade-transition days, 1.06% on exit days, and 0.78% on re-entry days. The corresponding median changes were -0.29%, -0.26%, -0.18%, and -0.18%. The gap between the means and medians is important: a small number of very large volume spikes pulls the mean upward, while the typical event had a modest volume decline. The increased-volume rates were 30.3% for first entries, 35.1% for grade transitions, 49.3% for exits, and 39.9% for re-entries.

Across original market-cap deciles, the Kruskal-Wallis test found no evidence of a volume-change difference for first entries (p = 0.584), exits (p = 0.323), or re-entries (p = 0.977). Grade transitions were different across deciles (p < 0.0001). This result should be treated cautiously because it is an unconditional distributional test, and the grade-transition sample includes repeated events for the same ticker.

The volume result is therefore more nuanced than “surveillance increases trading.” Grade transitions coincide with a positive mean volume change, but the negative median indicates that the average is driven by spikes rather than a broad increase across securities. An event-day volume spike can reflect repositioning, forced exits, speculative attention, or a change in liquidity; this report does not identify the cause.

For readability, the four event charts remove observations beyond a robust three-sigma cutoff within each measure/event-type group. The cutoff uses the group median and MAD-based sigma so extreme observations do not inflate the threshold. For volume charts, the sigma rule is applied to the log volume ratio because raw volume changes are heavily right-skewed. This trimming affects chart display only: the CSV summaries, p-values, RDS bundle, and reported findings retain the complete usable sample. The chart subtitles identify the trimmed sample size.

The decile pattern is not monotonic. For first entries, the mean return was negative in deciles 1 through 7, turned positive in decile 8, and was positive in deciles 9 and 10. The largest first-entry sample was decile 7 (267 observations, -0.84%), while decile 9 had 166 observations and a +1.54% mean. Grade-transition returns were positive in most deciles, with the largest sample in decile 9 (2,670 observations, +0.29%); decile 8 was +1.13% across 1,843 observations. These differences should not be described as a size premium because the decile is measured at the stock's first entry and the event observations are not matched by date, sector, or stage.

The results also show why sample coverage matters. There were 2,663 first-entry events, but only 1,872 had both a usable event-day return and a historical decile. There were 12,333 grade transitions, but only 8,538 had both fields. Missing returns arise when neither the adjusted-price series nor the fallback daily-return series can supply the event date; missing deciles arise when no historical membership record exists on or before the first entry date. The reported p-values therefore describe the covered sample, not every surveillance record.

### ASM and GSM split

The current `SEBI_SURVEILLANCE.STAGE` values encode more than one surveillance family. The report now decodes the plain stages `I` through `VI` as GSM, the `L`-prefixed stages as ASM combinations, and the `Stage I`, `Stage II`, `XXXVI`, and `XXXVII` values as ESM. Exit rows use the preceding active stage so that an exit remains assigned to the family that produced it. The event-level CSV and all decile summary tables include a `measure` column, and the return and volume charts use separate panels for ASM, ESM, and GSM.

The split changes the interpretation of the pooled results. For first-entry returns, the usable GSM sample was 2,037 events with a mean of -0.33%, compared with only 9 ASM events at +0.38% and 47 ESM events at +0.04%. For grade-transition returns, the GSM sample was 5,256 events at +0.42%, the ASM sample was 361 events at +1.14%, and the ESM sample was 3,965 events at +0.28%. The small ASM first-entry sample is not sufficient for a meaningful comparison.

For the decile tests, original market-cap decile was associated with return differences for ASM grade transitions (p = 0.000069) and ESM grade transitions (p = 0.000010), but not GSM grade transitions (p = 0.832). GSM first-entry returns still showed a decile difference (p = 0.0025). These are separate unconditional tests; the apparent family differences may reflect different securities, dates, stages, and sample sizes rather than a causal effect of ASM or GSM.

## Why a surveillance stock may not be tradable in the usual way

Surveillance does not mean that every order is prohibited. The restriction depends on the specific measure and stage. In practice, “cannot be traded” usually refers to one of four different situations:

### Trade-to-trade settlement blocks same-day selling

In the normal equity segment, an intraday position can be bought and sold within the same session without resulting in delivery. A T2T security is instead settled trade by trade: a purchase is intended to result in delivery of the shares, and a sale is intended to deliver shares that the seller already owns. The broker therefore rejects an attempt to sell shares bought earlier on the same day. The position can generally be sold from T+1, after the purchase has settled into the investor's holding.[1]

This is a settlement restriction, not a statement that the exchange has stopped trading the security. Orders can still be accepted during the permitted trading session, but the investor must have an eligible holding for a sale and must accept that an intraday round trip is unavailable. Securities can be moved to T2T under conditions including LT ASM Stage 4, GSM Stage 2 or above, ESM Stages 1 and 2, and ASM-IBC Stages 1 and 2.[1]

### A price band can reject an order at the chosen price

Some surveillance stages apply a 5% or narrower price band. An order outside the permitted upper or lower band is rejected or cannot execute at that price. A price band does not automatically prevent all trading; it limits the prices at which matching can occur and can make the security difficult to exit when the permitted range is far from the investor's desired price.[1]

### Additional surveillance deposits can block fresh buying

GSM Stage 2 requires a 50% additional surveillance deposit in the referenced rules. GSM Stages 3 and 4 and ASM-IBC Stages 1 and 2 require 100%.[1][3] A broker that does not collect this deposit may block new buy orders rather than accept an order it cannot margin correctly. Existing positions may still be eligible for exit, so “buying blocked” and “selling blocked” are different conditions.[1]

### Weekly trading and call auctions restrict when an order can execute

Some higher GSM stages and ASM-IBC Stage 2 securities trade only once per week.[1][3] A periodic call auction collects orders during a defined window and matches them at an auction price rather than continuously. An order submitted outside the permitted session, or an order that does not match at the auction price, may remain unexecuted even though the security is technically listed for trading.[1]

These controls are intended to slow trading, reduce disorderly price movement, and prompt additional due diligence. They do not guarantee a lower price, a future recovery, or an executable exit. The exchange circular and the broker's order notification for the specific security take precedence over this summary.[2][3][4]

## Important distinction: surveillance is not a buy or sell signal

Surveillance measures change the conditions under which a security can be traded. They do not predict that the price will rise or fall, and they do not guarantee that the security is unsafe or that a company has committed misconduct. Use the label as a reason to slow down, verify the current rules, and assess whether you can accept the liquidity, settlement, price-band, and margin risks.[1][2]

## Sources

[1] https://support.zerodha.com/category/trading-and-markets/alerts-and-nudges/nudges/articles/surveillance-indicators — Zerodha: What are surveillance measures and risks associated with them?
    > "Surveillance measures are implemented by SEBI and exchanges to proactively monitor securities across markets, ensuring the protection of investors' and traders' interests."
    > "stock is bought, and the client tries to sell the stocks on the same day, the order will be rejected. However, it can be sold on the next trading day, i.e., T+1 day."

[2] https://www.nseindia.com/static/regulations/additional-surveillance-measure — NSE: Additional Surveillance Measure (ASM)
    > "In continuation to various surveillance measures already implemented, SEBI and Exchanges, pursuant to discussions in joint surveillance meetings, have decided that along with the aforesaid measures there shall be Additional Surveillance Measures (ASM) on securities with surveillance concerns based on objective parameters viz. Price / Volume variation, Volatility etc."

[3] https://www.nseindia.com/static/regulations/graded-surveillance-measure — NSE: Graded Surveillance Measure (GSM)
    > "The main objective of these measures is to;"
    > "Requirement of depositing additional amount as Surveillance Deposit, which shall be retained for an extended period;"

[4] https://www.nseindia.com/static/regulations/enhanced-surveillance-measure-esm — NSE: Enhanced Surveillance Measure (ESM)
    > "In continuation to various surveillance measures already implemented, SEBI and Exchanges, pursuant to discussions in joint surveillance meetings, have decided that along with the aforesaid measures there shall be Enhanced Surveillance Measures (ESM) on all Companies (Main board & SME) with market capitalization less than INR 1000 crores, based on objective parameters viz. Price variation, Standard Deviation etc."
