# Gaps and the Pre-Open Call Auction

- Difficulty level: 3
- Published: Fri, 01 May 2015 12:32:53 +0000
- Source: [https://stockviz.biz/2015/05/01/gaps-and-the-pre-open-call-auction/](https://stockviz.biz/2015/05/01/gaps-and-the-pre-open-call-auction/)

## Summary

This document discusses the impact of the pre-open call auction introduced on October 18, 2010, on opening prices and back-testing results. It explains that opening prices before and after this date should not be treated identically because the structural change affects gap analysis and statistical distributions. To make historical opening prices comparable across the entire dataset, the article suggests computing a synthetic opening price using tick-level data, similar to how closing prices are calculated. Alternatively, analysts must approach back-test results with skepticism and account for the auction mechanism in their analytical models.

## Article

tl;dr You should not treat opening prices before and after October 18, 2010 the same. Call Auction in the pre-open session If you don’t know how the pre-open session works, here’s a good explainer from BSE: When you run back-tests that use the opening price, this change will most likely trip you up. Before and after Nifty opening gaps since 2000: Notice the shift in the median before and after the auction was introduced (all figures in %): Before: After: Conclusion One way to make the opening prices comparable is to take tick-level data and compute a synthetic opening price yourself, just like how the closing price in computed. And you can use this synthetic open across your entire data set. Otherwise, you will have to take you back-test results with a healthy dose of skepticism and make sure that there is enough room in your analysis to account for this.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Long-Short Trend Following | https://stockviz.biz/2015/04/30/long-short-trend-following/]
Prior Work We had discussed the SMA On/Off Switch and its ability to escape the worst days. Based on this finding, we setup a Tactical Theme that would go long NIFTYBEES and JUNIORBEES if the CNX 100 index is trading above its 50-day SMA and move into LIQUIDBEES otherwise. What if, we could go long and short? Naive Long-Short Here’s how going long above 50-DMA and short below 50-DMA on the CNX 100 since 2001 compares: Long-Short SMA (black), Long-Only SMA (red) and Buy & Hold (green) It looks like going both long and short is not significantly better than a long-only tactical strategy. Long-Short with Volatility But what if, we add a volatility metric into the mix? The logic here is that corrections are preceded by a bout of volatility. So if you go short if either or the volatility signal or the 50-DMA indicates a negative bias and long otherwise: Long-Short SMA w/ Volatility (black), Long-Only SMA w/ Volatility (red), Long-Only SMA (green) and Buy & Hold (blue) It looks like there is significant alpha in the combination approach. Long-Short NIFTY and BANKNIFTY NIFTY returns since 2001: And the same for the BANK NIFTY since 2006: NIFTY and BANKNIFTY since 2011: NIFTY and BANKNIFTY since 2013: Long-Short Combo (black), Long-Only Combo (red), Long-Only Tactical (green) and Buy & Hold (blue) Conclusion It appears that there is long-term alpha in using a combination of volatility and 50-DMA to implement a long-short strategy. To put this to test using real-time data, we have created a theme to make it easy for you to follow along: Trend Long-Short.

[Linked: Correlation Update 02.05.2015 | https://stockviz.biz/2015/05/02/correlation-update-02-05-2015/]
Nifty one year daily return correlations Nifty one month daily return correlations Bank Nifty one year daily return correlations Bank Nifty one month daily return correlations Midcap one year daily return correlations Midcap one month daily return correlations A lot of thick blue squares mean that positive correlations are high. Red squares mean negative correlations are high. Whites are the doldrums.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: About Us | https://stockviz.biz/2012/11/28/about-us/]
Our Mission The StockViz mission is to make modern investment practices and systems affordable to the average Indian investor to help people trade and invest better. People Shyam Sunder has over 10 years of experience in investment management, analysis and algorithmic trading. He worked at Merrill Lynch as a trader on their ABS CDS desk in New York. Having had to build his own tools specific to the Indian market to help manage his investments better, he founded StockViz to bring those tools to a wider investor base. Follow @ShyamNation Follow @ShyamNation Col Dipanshu Sinha, SM took premature retirement from the Indian Army as a Colonel in the Assam Regiment. Initially specialising as an Army Aviator, he has subsequently served with distinction in frontline infantry roles and has substantial experience in counter-insurgency operations earning his stripes with multiple tours of duty in Kashmir and various appointments in command and staff roles. A graduate of the Defence Services Staff College, Wellington, throughout his career he felt the absence of financial and tax planning information which was accentuated by peculiar service conditions and limited exposure to money management skills training in the regular professional courses. He has been one of the driving forces behind the conceptualisation and establishment of Stockviz and is a co-founder. Dipanshu is responsible for managing day-to-day operations, strategic planning, business development and oversight and policy advice for the firm’s investment activities. Follow @dipanshusinha Follow @dipanshusinha

[Linked: Contact Us | https://stockviz.biz/2012/11/28/contact-us/]
Message @StockViz [iframe class=”span12″ frameborder=”0″ scrolling=”no” marginheight=”0″ marginwidth=”0″ src=”https://maps.google.co.in/maps?f=q&source=s_q&hl=en&geocode=&q=stockviz&sll=12.953997,77.63094&sspn=0.815042,1.352692&t=h&ie=UTF8&hq=stockviz&hnear=&z=10&iwloc=A&cid=8020117427587487552&ll=12.916192,77.580084&output=embed”]
