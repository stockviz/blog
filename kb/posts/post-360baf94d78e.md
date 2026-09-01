# Replication

- Difficulty level: 3
- Published: Wed, 17 Apr 2024 12:14:43 +0000
- Source: [https://stockviz.biz/2024/04/17/replication/](https://stockviz.biz/2024/04/17/replication/)

## Summary

The article explains how to replicate inaccessible indices or strategies, such as the S&P 500, using a basket of accessible Indian indices like NIFTY 50, MIDCAP SELECT, and NIFTY BANK. It demonstrates applying leverage to these replicating baskets and utilizes time-varying regression loadings to reveal how the relationship between the reference asset and the basket shifts over time. By using accessible domestic securities, investors can overcome regulatory hurdles or mandates that prevent direct investment in foreign or restricted strategies. The piece provides practical code and visual charts on GitHub to help readers implement this replication technique, making it a hands-on guide for constructing synthetic exposures.

## Article

Sometimes, you many not be able to directly buy an index or access a strategy because of regulatory hurdles, mandates etc… In these situations, replicating it using a basket of accessible securities might make sense. For example, you can replicate the S&P 500 index using Indian indices: NIFTY 50, MIDCAP SELECT and NIFTY BANK. In fact, you can do for any reference timeseries (strategies/funds) and apply leverage as desired. Also, the loadings give you and idea of the shifting relationship between the reference asset and the basket over time. Code and images are on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/blog/tree/master/replication]
Blog post: Replication Demonstrated how to replicate inaccessible indices or strategies (e.g., S&P 500) using a basket of accessible Indian indices (NIFTY 50, MIDCAP SELECT, NIFTY BANK) with time-varying regression loadings that reveal shifting relationships over time. evaluates index replication strategies and analyzes tracking error - GSPC.daily.png - GSPC.monthly-loadings.png - GSPC.monthly.png - GSPC.weekly.png

[Linked: Volatility Targeting | https://stockviz.biz/2024/04/16/volatility-targeting/]
There are a number of ways to construct low-volatility portfolios. You could either use a bottom-up approach of selecting individual stocks that have low-volatility or you could you could run them through a portfolio optimizer (Low Volatility: Stock vs. Portfolio) to get target weights. However, if you are trading a single index, then you could use its own volatility to scale your exposure up and down. The advantage here is that if you trade index futures, you can set the volatility and leverage dials to the risk that you are most comfortable with. Let’s take our own NIFTY 50, for example. Calculate the std-dev of daily returns over a sensible window. The index exposure is simply the ratio of the median std-dev vs. the current std-dev. Use a scaling factor (tvf) to further fine-tune the risk. To reduce transaction costs, rebalance once a week. A tvf of 0.25 has roughly half the returns of buy & hold but with superior risk metrics that makes it receptive to leverage. The problem with this approach is that the weights are continuous. What if you want them discrete so that it directly maps to how many lots of NIFTY you need to trade? Here, we bucket the std. dev. into quintiles and use that to set our exposures in discrete steps. At 2x leverage, you will outperform buy & hold by 5% with only half its drawdown. The same for NIFTY MIDCAP SELECT looks like this: The stats for this index looks worse than buy & hold. However, volatility sizing has resulted in lower drawdowns. Stats and charts for different indices and code are on github. Also read: Large Moves Happen Together

[Linked: Momentum Rebalance Frequency, Part II | https://stockviz.biz/2024/04/21/momentum-rebalance-frequency-part-ii/]
Previously, we looked at momentum rebalance frequencies with a monthly increment. However, if you observe the individual returns of momentum stocks (Returns under Momentum), you’ll notice that the returns of momentum stocks tail off after the first two weeks. Does switching to a weekly rebalance frequency make sense? The biggest problem with a higher frequency of rebalance is the higher transaction cost that comes with it. So, we set the drag to be 0.5% and run 1- through 4-week rebalancing scenarios. Turns out, there is an advantage to rebalancing a momentum portfolio once in two weeks rather than once a month. The transaction costs are roughly 5% (annualized) vs. 3% of the monthly rebalanced version. The main thing to watch out for is the portfolio overlap between rebalancing. The lower the overlap, higher the costs. Costs are permanent and immediate while returns are hypothetical and distant. Make of this what you will. Code and charts on github. Related: Factors

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: About Us | https://stockviz.biz/2012/11/28/about-us/]
Our Mission The StockViz mission is to make modern investment practices and systems affordable to the average Indian investor to help people trade and invest better. People Shyam Sunder has over 10 years of experience in investment management, analysis and algorithmic trading. He worked at Merrill Lynch as a trader on their ABS CDS desk in New York. Having had to build his own tools specific to the Indian market to help manage his investments better, he founded StockViz to bring those tools to a wider investor base. Follow @ShyamNation Follow @ShyamNation Col Dipanshu Sinha, SM took premature retirement from the Indian Army as a Colonel in the Assam Regiment. Initially specialising as an Army Aviator, he has subsequently served with distinction in frontline infantry roles and has substantial experience in counter-insurgency operations earning his stripes with multiple tours of duty in Kashmir and various appointments in command and staff roles. A graduate of the Defence Services Staff College, Wellington, throughout his career he felt the absence of financial and tax planning information which was accentuated by peculiar service conditions and limited exposure to money management skills training in the regular professional courses. He has been one of the driving forces behind the conceptualisation and establishment of Stockviz and is a co-founder. Dipanshu is responsible for managing day-to-day operations, strategic planning, business development and oversight and policy advice for the firm’s investment activities. Follow @dipanshusinha Follow @dipanshusinha

[Linked: Contact Us | https://stockviz.biz/2012/11/28/contact-us/]
Message @StockViz [iframe class=”span12″ frameborder=”0″ scrolling=”no” marginheight=”0″ marginwidth=”0″ src=”https://maps.google.co.in/maps?f=q&source=s_q&hl=en&geocode=&q=stockviz&sll=12.953997,77.63094&sspn=0.815042,1.352692&t=h&ie=UTF8&hq=stockviz&hnear=&z=10&iwloc=A&cid=8020117427587487552&ll=12.916192,77.580084&output=embed”]
