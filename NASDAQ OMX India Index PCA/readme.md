# Principal Components Analysis
Adventures with using pca for financial analysis

## scripts
### script01.R
Question: Is there a stark difference in loadings of PCA-1 based on the SMA 'regime' of the composite index?
Answer: No. However, some interesting observations were made. Read: [Principal Component Analysis, Part I](https://stockviz.biz/index.php/2018/09/25/principal-component-analysis-part-i/)

### script02.R
Question: The previous analysis used daily returns. What would the PCA look like if we did the analysis on the rolling 20-day returns? We lag the returns by the same number so that we don't "look back" but "look forward." 
Answer: Maximum variation in loadings are observed under SMA-50 regimes. i.e., the things that explain 20-day returns when the TR index is above 50-day SMA are mostly different from the things that explain 20-day returns when the TR index is below 50-day SMA.