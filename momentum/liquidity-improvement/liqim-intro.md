# A Plain-English Intro to LIQIM

## What is the Amihud Measure?

The Amihud measure is a simple way to estimate how "illiquid" a stock is — meaning how much its price moves when people trade it — using just daily price and volume data (no fancy order-book data needed).

**The intuition:** If a stock is *liquid* (easy to trade), you can buy or sell a decent chunk of it without moving the price much. If a stock is *illiquid* (hard to trade), even a small trade can send the price swinging. So illiquidity = "price impact per dollar traded."

**The formula (roughly):**

For a given stock in a given month, you look at each trading day and calculate:

```
ILLIQ = average of ( |daily return| / dollar volume traded that day )
```

Then average that ratio across all the trading days in the month.

**In plain English:** On each day, take the absolute value of how much the stock's price moved (as a %), and divide it by how many dollars' worth of the stock changed hands that day. If the price moved a lot on a day with low trading volume, that's a sign the stock is illiquid — a little trading caused a big price swing. If the price barely moved despite huge volume, that's a very liquid stock.

Then you average this ratio over the month to get one illiquidity score per stock per month. Higher ILLIQ = more illiquid/harder to trade. Lower ILLIQ = more liquid/easier to trade.

**Why it's popular:** It was introduced by Yakov Amihud in 2002, and it caught on because you only need basic data everyone has (daily returns and daily dollar trading volume) — you don't need intraday tick data or bid-ask spread data, which are much harder to get, especially for older time periods. It's also known to correlate well with more sophisticated, harder-to-obtain measures of intraday price impact.

In this paper, ILLIQ is the raw ingredient used to build LIQC (the change in liquidity) and ultimately LIQIM (the liquidity improvement factor).

---

## How is LIQIM Constructed?

The basic idea: instead of sorting stocks by how well they've done (like regular momentum), sort them by how much easier or harder they've become to trade.

**Step by step:**

1. **Measure "illiquidity"** for every stock each month, using the Amihud measure described above.

2. **Measure the *change*** in that illiquidity — did a stock get easier to trade or harder to trade over the past year compared to its more recent level? This "improvement" score is called LIQC. A positive LIQC means the stock got more liquid (better); negative means it got less liquid (worse).

3. **Sort stocks into six buckets** using a 2-by-3 grid:
   - First split by company size (big vs. small, using the NYSE median as the cutoff)
   - Then, within big and within small, split into three groups based on LIQC: most-improved (top 30%), middle, most-deteriorated (bottom 30%)

4. **Build the portfolio:** Go long (buy) the "most improved" stocks in both the big and small groups, and go short (bet against) the "most deteriorated" stocks in both groups. Average the two long groups together and the two short groups together.

So:

```
LIQIM = (average of improved-big + improved-small)
      − (average of deteriorated-big + deteriorated-small)
```

It's essentially the same size-neutral 2x3 sorting method Fama-French use for their factors — just swapping "change in liquidity" in for "book-to-market" or "past returns."

**Why it matters for the paper:** The author finds that stocks that later become momentum "winners" tend to already be improving in liquidity before they're picked, and future "losers" are already deteriorating. LIQIM is designed to capture that liquidity-reshuffling directly, and the paper argues it can explain away the momentum profit almost entirely once you control for it.
