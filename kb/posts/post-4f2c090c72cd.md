# Monitoring Network Glitches

- Difficulty level: 3
- Published: Sun, 22 Jun 2025 04:54:29 +0000
- Source: [https://stockviz.biz/2025/06/22/monitoring-network-glitches/](https://stockviz.biz/2025/06/22/monitoring-network-glitches/)

## Summary

This article discusses the practical challenge of monitoring network glitches in real-time market data feeds, which are typically delivered via WebSocket streams. It highlights how even brief connection drops can cause significant data lag and system failures, and contrasts this with complete outages that can be handled by failover routers. The author shares real-world data on glitch frequencies from three Indian ISPs, emphasizing that some providers are unreliable and unresponsive to support tickets. The piece advises on building robust reconnection logic, monitoring infrastructure, and maintaining multiple backup connections. It includes links to Python and R code for glitch monitoring and charting, making it a hands-on guide for practitioners. This requires understanding of networking concepts, WebSocket protocols, and basic programming, but does not delve into advanced statistical or algorithmic analysis, placing it at an applied methods level.

## Article

More often than not, real time feeds are exposed as web socket streams. Web sockets are extremely sensitive to network glitches. Glitches – when the connection drops for a very small span of time – will cause your market data to lag badly as your client tries to reconnect/re-establish the feed, eventually leading to catastrophic failure. The problem with glitches is that it is hard to work around. A complete drop in the connection can be handled by a router that can switch over to a backup connection. Glitches are more sinister. For example, we have broadband connections from three different service providers (Hathway, Excitel and BSNL) to make sure our systems can stay connected. They all have varying degrees of stability and customer service. Here are the number of glitches per hour over the last few days: You can work around 1-2 glitches an hour by making your re-connection logic more robust. However, there is no getting around the Hathway level of glitches. If your service provider itself is not monitoring for glitches, then explaining the problem to them is impossible. Support tickets get closed because “network is connected.” Here is a history of our support tickets with Hathway – an ongoing saga with no end: As you scale, monitoring your infrastructure becomes increasingly important. Be aware that your service provider could be (willfully) blind to the specific issues you are facing – their job might depend on them not solving it. And, always have a backup and a backup for your backup. The python code to monitor for glitches and the R code to draw the charts are on github.

## Linked sources

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: Home | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: github | https://github.com/stockviz/blog/tree/master/network/monitor]
Blog post: Monitoring Network Glitches Explored the problem of network glitches disrupting WebSocket-based real-time market data feeds, comparing glitch rates across three Indian ISPs (Hathway, Excitel, BSNL). Glitches are hard to diagnose and get support for; the recommendation is to monitor infrastructure and always maintain multiple backup connections. monitors network connectivity and latency metrics for trading infrastructure - breaks.png

[Linked: Momentum Skip Month (II) | https://stockviz.biz/2025/06/17/momentum-skip-month-ii/]
Our earlier post on using a “skip” month for setting up momentum portfolios saw a slight advantage in skipping a month. However, it very will could have been because of path dependence. Going back to a 20-stock portfolio and separating out the monthly returns of stocks only in the “no-skip” portfolio (RET_NOSKIP) and those that are only in the one-month skipped portfolio (RET_SKIP) doesn’t really settle the debate in favor of skipping a month. The summary stats are similar as well. Perhaps the mean reversion that was observed prior to the early 90’s when the original paper was published is weaker now? Code on github.

[Linked: The Smirk, Part II | https://stockviz.biz/2025/07/25/the-smirk-part-ii/]
While the concept of volatility smirk is simple, the pattern itself is unstable. For example, different expiries have different shapes. And these shapes change across days as well. One way to keep track of these changes is by fitting a model through the implied volatilities. Here, we fit a parabola (y = ax2 + bx + c). a, the coefficient of strike_pct2, gives a measure of the narrowness/steepness of the smirk. By sampling the curve and tracking these coefficients, you can begin to form an opinion on what is “normal” vs. a trading opportunity. Code and charts on github.

[Linked: StockViz | https://stockviz.biz/]
Invest Without Emotions Had you invested in our {{theme.NAME_PUBLIC}} Theme, you would have made {{numeral(theme.RET).format('#,0.00%')}} in the last {{theme.RET_LB}} days.

[Linked: About Us | https://stockviz.biz/2012/11/28/about-us/]
Our Mission The StockViz mission is to make modern investment practices and systems affordable to the average Indian investor to help people trade and invest better. People Shyam Sunder has over 10 years of experience in investment management, analysis and algorithmic trading. He worked at Merrill Lynch as a trader on their ABS CDS desk in New York. Having had to build his own tools specific to the Indian market to help manage his investments better, he founded StockViz to bring those tools to a wider investor base. Follow @ShyamNation Follow @ShyamNation Col Dipanshu Sinha, SM took premature retirement from the Indian Army as a Colonel in the Assam Regiment. Initially specialising as an Army Aviator, he has subsequently served with distinction in frontline infantry roles and has substantial experience in counter-insurgency operations earning his stripes with multiple tours of duty in Kashmir and various appointments in command and staff roles. A graduate of the Defence Services Staff College, Wellington, throughout his career he felt the absence of financial and tax planning information which was accentuated by peculiar service conditions and limited exposure to money management skills training in the regular professional courses. He has been one of the driving forces behind the conceptualisation and establishment of Stockviz and is a co-founder. Dipanshu is responsible for managing day-to-day operations, strategic planning, business development and oversight and policy advice for the firm’s investment activities. Follow @dipanshusinha Follow @dipanshusinha

[Linked: Contact Us | https://stockviz.biz/2012/11/28/contact-us/]
Message @StockViz [iframe class=”span12″ frameborder=”0″ scrolling=”no” marginheight=”0″ marginwidth=”0″ src=”https://maps.google.co.in/maps?f=q&source=s_q&hl=en&geocode=&q=stockviz&sll=12.953997,77.63094&sspn=0.815042,1.352692&t=h&ie=UTF8&hq=stockviz&hnear=&z=10&iwloc=A&cid=8020117427587487552&ll=12.916192,77.580084&output=embed”]
