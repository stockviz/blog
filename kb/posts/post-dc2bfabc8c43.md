# VIX Seasonality

- Difficulty level: 3
- Published: Sat, 23 Sep 2023 15:14:45 +0000
- Source: [https://stockviz.biz/2023/09/23/vix-seasonality/](https://stockviz.biz/2023/09/23/vix-seasonality/)

## Summary

The article analyzes India VIX seasonality using time series decomposition. It reveals that while daily data is highly dispersed, decomposing the series uncovers distinct monthly patterns, specifically a pronounced spike in May that explains market nervousness. Additionally, it examines overnight volatility pricing by comparing Close-Open and Open-Close volatility, and briefly evaluates trend-following strategies on Indian and US bonds. The piece assumes the reader understands volatility indices and basic time series concepts, applying decomposition techniques to extract seasonal components from financial data. It provides code and charts to demonstrate these applied quantitative methods, bridging the gap between theoretical volatility and practical seasonal analysis.

## Article

Is India VIX seasonal? Yes. There is a huge amount of dispersion in the daily data when grouped by months. Taking averages of these may not make much sense. However, when you decompose the series, you get some interesting monthly seasonality. Zooming into the “season_year” chart: If you transform the seasonality component and plot it by month, you’ll notice why everybody gets nervous in May. Code and charts on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/blog/tree/master/volatility/vix-seasonality]
Blog post: VIX Seasonality Analyzed India VIX for seasonality by decomposing the series. Confirmed seasonal patterns exist, with the decomposed seasonality component revealing a pronounced spike that explains why markets get nervous in May. analyzes seasonal patterns in VIX levels and volatility across calendar months - INDIA-VIX.decomposition.png - INDIA-VIX.monthly.png - INDIA-VIX.seasonality.month.png - INDIA-VIX.seasonality.png

[Linked: Overnight Volatility | https://stockviz.biz/2023/09/16/overnight-volatility/]
Currently, Indian markets are open for 6.5 hours. During that time, global commodity markets are largely closed and overnight US futures markets are barely coming to life. This exposes positions carried forward to the next day to event risks. How is this risk priced? Surprisingly, Close-Open (next-day) (CO) volatility is less than Open-Close (same-day) (OC) volatility. This doesn’t quite jive with the intuition about large overnight risks. This holds even if you include pre-pandemic data. If you believe that overnight risks are larger than what the market perceives, then buying strangles at the close surprisingly doesn’t cost you much. A naïve strategy should breakeven after costs and occasionally, you might get lucky. The unknown-unknown is scarier than the known-unknown. However, it is the known-unknown that you should be worried about more.

[Linked: Trend-following Bonds | https://stockviz.biz/2023/10/01/trend-following-bonds/]
Does trend following work on bonds? According to alphaarchitect, it should. However, they use data going back to 1928 and we wanted to look at something more recent. Also, we wanted to check if it worked for Indian bonds? For Indian bonds, you are better off buying and holding. Once you consider transaction costs and taxes, there is no benefit. For US, we ran the same SMA scenarios on the TLT (20+), IEF (7-10), SHY (1-3) and AGG etfs. There is some benefit to applying a 100-day SMA filter on the first three. However, the after-cost benefits are questionable. Code and charts on github.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: About Us | https://stockviz.biz/2012/11/28/about-us/]
Our Mission The StockViz mission is to make modern investment practices and systems affordable to the average Indian investor to help people trade and invest better. People Shyam Sunder has over 10 years of experience in investment management, analysis and algorithmic trading. He worked at Merrill Lynch as a trader on their ABS CDS desk in New York. Having had to build his own tools specific to the Indian market to help manage his investments better, he founded StockViz to bring those tools to a wider investor base. Follow @ShyamNation Follow @ShyamNation Col Dipanshu Sinha, SM took premature retirement from the Indian Army as a Colonel in the Assam Regiment. Initially specialising as an Army Aviator, he has subsequently served with distinction in frontline infantry roles and has substantial experience in counter-insurgency operations earning his stripes with multiple tours of duty in Kashmir and various appointments in command and staff roles. A graduate of the Defence Services Staff College, Wellington, throughout his career he felt the absence of financial and tax planning information which was accentuated by peculiar service conditions and limited exposure to money management skills training in the regular professional courses. He has been one of the driving forces behind the conceptualisation and establishment of Stockviz and is a co-founder. Dipanshu is responsible for managing day-to-day operations, strategic planning, business development and oversight and policy advice for the firm’s investment activities. Follow @dipanshusinha Follow @dipanshusinha
