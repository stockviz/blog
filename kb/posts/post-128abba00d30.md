# Volatility Lookbacks

- Difficulty level: 3
- Published: Thu, 31 Aug 2023 12:42:18 +0000
- Source: [https://stockviz.biz/2023/08/31/volatility-lookbacks/](https://stockviz.biz/2023/08/31/volatility-lookbacks/)

## Summary

The article introduces volatility signature plots as a practical analytical tool for selecting appropriate lookback periods when calculating volatility. It explains that while strategy developers typically test multiple lookbacks and choose one that appears reasonable, signature plots provide a more systematic diagnostic approach. By plotting the distribution of volatility across various lookback windows, practitioners can assess estimate stability—ideally seeking tight boxes with centered medians and short wicks. Though originating in high frequency trading, the technique applies to lower frequency time series as well. The central insight is that if the distribution of volatility itself behaves erratically across different lookbacks, the reliability of any volatility-driven strategy becomes questionable. This method quantifies the stability of volatility estimates, offering a structured way to validate lookback choices rather than relying on visual inspection alone, making it an applied technique for systematic strategy development.

## Article

Volatility is calculated over a time period – the lookback. While developing a strategy, it is typical to try a range of lookbacks and pick one that looks reasonable for the strategy being built. However, is there an “ideal” lookback period? This is where a volatility signature plot comes into the picture. It is typically used in high frequency trading but there is no reason not to use it on a lower frequency time series. If you plot the distribution of volatility over different lookbacks, this is how it looks: Ideally, you want the box to be small, the median in the middle and the wicks to be short. After all, if you are using volatility to drive a strategy, if the distribution of volatility itself is too wonky, then how do you trust the output?

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Index Constituent Correlations | https://stockviz.biz/2023/08/31/index-constituent-correlations/]
What does the pair-wise correlation of the NIFTY 50 constituents look like? Is it sticky? (Intro) What does the high-correlation regime look like? (Correlation vs. Returns & Volatility) Can correlation states be used for timing? (Correlation Timing)

[Linked: Intraday Volatility | https://stockviz.biz/2023/09/02/intraday-volatility/]
Realized Semi-variance is a measure of intraday volatility. It is nothing more than the sum of squared high-frequency positive and negative returns. It is typically used for forecasting volatility. However, can it be used for market timing? After all, volatility is said to be sticky and avoiding downside volatility is supposed to be desirable. What if, you exit the market when the current volatility is more than the historical average (based on some lookback)? Turns out, doing something like that would’ve worked on the pre-pandemic NIFTY 50. Maybe not higher returns but better Sharpe than buy & hold. However, post-pandemic returns have been disappointing. The same thing can be observed on the MIDCAP 50 index as well. We’ll add this to the growing pile of disappointing results of using volatility for directional bets.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: About Us | https://stockviz.biz/2012/11/28/about-us/]
Our Mission The StockViz mission is to make modern investment practices and systems affordable to the average Indian investor to help people trade and invest better. People Shyam Sunder has over 10 years of experience in investment management, analysis and algorithmic trading. He worked at Merrill Lynch as a trader on their ABS CDS desk in New York. Having had to build his own tools specific to the Indian market to help manage his investments better, he founded StockViz to bring those tools to a wider investor base. Follow @ShyamNation Follow @ShyamNation Col Dipanshu Sinha, SM took premature retirement from the Indian Army as a Colonel in the Assam Regiment. Initially specialising as an Army Aviator, he has subsequently served with distinction in frontline infantry roles and has substantial experience in counter-insurgency operations earning his stripes with multiple tours of duty in Kashmir and various appointments in command and staff roles. A graduate of the Defence Services Staff College, Wellington, throughout his career he felt the absence of financial and tax planning information which was accentuated by peculiar service conditions and limited exposure to money management skills training in the regular professional courses. He has been one of the driving forces behind the conceptualisation and establishment of Stockviz and is a co-founder. Dipanshu is responsible for managing day-to-day operations, strategic planning, business development and oversight and policy advice for the firm’s investment activities. Follow @dipanshusinha Follow @dipanshusinha
