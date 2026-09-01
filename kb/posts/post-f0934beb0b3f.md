# Intraday Volatility

- Difficulty level: 3
- Published: Sat, 02 Sep 2023 07:06:38 +0000
- Source: [https://stockviz.biz/2023/09/02/intraday-volatility/](https://stockviz.biz/2023/09/02/intraday-volatility/)

## Summary

To understand this article on intraday volatility and market timing, readers should first grasp the concept of realized semi-variance, which measures downside volatility using squared high-frequency returns. Familiarity with lookback periods is essential, as the strategy relies on comparing current volatility against a historical average over a specific timeframe. Additionally, readers must understand the Sharpe ratio to evaluate why avoiding downside volatility might improve risk-adjusted returns rather than absolute returns. The article also touches upon volatility signature plots, requiring a basic understanding of how volatility distributions behave across different time horizons. Finally, awareness of market timing strategies and the distinction between pre-pandemic and post-pandemic market regimes is necessary to comprehend why this applied method yielded disappointing directional results in recent years.

## Article

Realized Semi-variance is a measure of intraday volatility. It is nothing more than the sum of squared high-frequency positive and negative returns. It is typically used for forecasting volatility. However, can it be used for market timing? After all, volatility is said to be sticky and avoiding downside volatility is supposed to be desirable. What if, you exit the market when the current volatility is more than the historical average (based on some lookback)? Turns out, doing something like that would’ve worked on the pre-pandemic NIFTY 50. Maybe not higher returns but better Sharpe than buy & hold. However, post-pandemic returns have been disappointing. The same thing can be observed on the MIDCAP 50 index as well. We’ll add this to the growing pile of disappointing results of using volatility for directional bets.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Volatility Lookbacks | https://stockviz.biz/2023/08/31/volatility-lookbacks/]
Volatility is calculated over a time period – the lookback. While developing a strategy, it is typical to try a range of lookbacks and pick one that looks reasonable for the strategy being built. However, is there an “ideal” lookback period? This is where a volatility signature plot comes into the picture. It is typically used in high frequency trading but there is no reason not to use it on a lower frequency time series. If you plot the distribution of volatility over different lookbacks, this is how it looks: Ideally, you want the box to be small, the median in the middle and the wicks to be short. After all, if you are using volatility to drive a strategy, if the distribution of volatility itself is too wonky, then how do you trust the output?

[Linked: Daily Momentum | https://stockviz.biz/2023/09/09/daily-momentum/]
Daily Momentum and New Investors in an Emerging Stock Market (SSRN) describes the trading behavior of Chinese retail investors. Our study finds that daily returns, instead of monthly returns, display price momentum and attributes it to the trading behaviors of new investors using account-level transaction data. Apparently, most new entrants to the market in China take a very short-term punt on whatever worked on the day. They go on to study a bunch of DM and EM markets and its worth a read. The interesting bit is that Indian investors don’t chase daily momentum. In fact, for an equal-weighted “buy the best performing quintile and hold till tomorrow’s close” strategy, after transaction costs and taxes, there’s nothing left, on average. Also, buying the worst performers did no better either.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: About Us | https://stockviz.biz/2012/11/28/about-us/]
Our Mission The StockViz mission is to make modern investment practices and systems affordable to the average Indian investor to help people trade and invest better. People Shyam Sunder has over 10 years of experience in investment management, analysis and algorithmic trading. He worked at Merrill Lynch as a trader on their ABS CDS desk in New York. Having had to build his own tools specific to the Indian market to help manage his investments better, he founded StockViz to bring those tools to a wider investor base. Follow @ShyamNation Follow @ShyamNation Col Dipanshu Sinha, SM took premature retirement from the Indian Army as a Colonel in the Assam Regiment. Initially specialising as an Army Aviator, he has subsequently served with distinction in frontline infantry roles and has substantial experience in counter-insurgency operations earning his stripes with multiple tours of duty in Kashmir and various appointments in command and staff roles. A graduate of the Defence Services Staff College, Wellington, throughout his career he felt the absence of financial and tax planning information which was accentuated by peculiar service conditions and limited exposure to money management skills training in the regular professional courses. He has been one of the driving forces behind the conceptualisation and establishment of Stockviz and is a co-founder. Dipanshu is responsible for managing day-to-day operations, strategic planning, business development and oversight and policy advice for the firm’s investment activities. Follow @dipanshusinha Follow @dipanshusinha

[Linked: Contact Us | https://stockviz.biz/2012/11/28/contact-us/]
Message @StockViz [iframe class=”span12″ frameborder=”0″ scrolling=”no” marginheight=”0″ marginwidth=”0″ src=”https://maps.google.co.in/maps?f=q&source=s_q&hl=en&geocode=&q=stockviz&sll=12.953997,77.63094&sspn=0.815042,1.352692&t=h&ie=UTF8&hq=stockviz&hnear=&z=10&iwloc=A&cid=8020117427587487552&ll=12.916192,77.580084&output=embed”]

[Linked: Trading Account | https://stockviz.biz/the-stockviz-demat-account/]
Enjoy the benefits of hassle free trading with Composite Investments while enjoying StockViz equity trading models , daily portfolio updates and analysis – all in one convenient package. Success = Composite + StockViz Open a demat account, for Rs. 699/- and get StockViz bundled with it for free! (We expect a minimum account size of Rs. 25 lakhs to get started.) | Equity (intraday / delivery) | 0.008% / 0.05% | | Futures (equity, currency, commodities) | 0.02% | | Options (equity, currency) | Rs. 25 /lot | | Mutual Funds (through NSE) | As per AMC | Fee structure for NRI/FPI accounts | Equity (delivery) | 0.50% | | Mutual Funds (through NSE) | As per AMC | We do not offer derivative trading services for NRIs/FPIs. Nobody should be put through the torture of paperwork required. There will an additional Rs. 5,000/- account opening fee to cover legal and postage expenses. ( Why? ) *AP Details
