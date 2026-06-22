# Simple Momentum

**Blog posts:**
- [Simple Momentum](https://stockviz.biz/index.php/?p=2083563)
- [Simple Momentum with Transaction costs and Taxes](https://stockviz.biz/index.php/?p=2083733)

> Replicated Michael Batnick's simple momentum strategy that switches between the S&P 500 and 5-year U.S. treasuries based on 12-month relative performance, finding the rule-based approach outperforms buy-and-hold.

> Extended the simple momentum strategy analysis by incorporating transaction costs and taxes, showing that these frictions significantly erode the gross returns and must be accounted for in realistic backtests.

## Summary

backtests a simple dual momentum strategy that switches between equity (NIFTY 50) and bonds based on trailing 12-month relative returns, with and without transaction cost/tax drag

## Scripts

### `smom.R`

backtests a simple dual momentum strategy that switches between equity (NIFTY 50) and bonds based on trailing 12-month relative returns, with and without transaction cost/tax drag

### `smom.tx.R`

backtests a simple dual momentum strategy that switches between equity (NIFTY 50) and bonds based on trailing 12-month relative returns, with and without transaction cost/tax drag

## Output

- `simple.momentum.annual.NIFTY 50.png`
- `simple.momentum.annual.NIFTY MIDCAP 100.png`
- `simple.momentum.annual.tx.NIFTY 50.png`
- `simple.momentum.annual.tx.NIFTY MIDCAP 100.png`
- `simple.momentum.cumulative.NIFTY 50.png`
- `simple.momentum.cumulative.NIFTY MIDCAP 100.png`
- `simple.momentum.cumulative.tx.NIFTY 50.png`
- `simple.momentum.cumulative.tx.NIFTY MIDCAP 100.png`
- `simple.momentum.drawdowns.NIFTY 50.png`
- `simple.momentum.drawdowns.NIFTY MIDCAP 100.png`
