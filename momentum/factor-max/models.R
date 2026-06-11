source("common.R")

modelDailyRets <- NULL
for(mid in modelIds){
  modelXts <- Common.GetEquityThemeReturns(mid, "30B18A87-D803-4476-8469-858454C2C49A", stt=0.1/100, brkg=0.05/100)
  modelDailyRets <- merge.xts(modelDailyRets, modelXts$MODEL_RET_BRK)
}
names(modelDailyRets) <- c(modelIds)
modelDailyRets <- na.trim(modelDailyRets, sides = 'left')
modelStDt <- index(xts::first(modelDailyRets))

modelMonthlyRets <- apply.monthly(modelDailyRets, Return.cumulative)

factorDates <- index(modelMonthlyRets)

#max daily return (factor MAX)

activeFactorMaxDt <- tibble()
factorMax <- NULL
for(i in 2:(length(factorDates)-1)){
  sDt <- factorDates[i-1]
  eDt <- factorDates[i] - 1 #exclude the last date
  
  fdrSubset <- modelDailyRets[paste0(sDt, '/', eDt), ]
  factorIndex <- arrayInd(which.max(fdrSubset), dim(fdrSubset))[2]
  maxFactorName <- modelIds[factorIndex]
  
  factorMax <- rbind(factorMax, xts(matrix(c(as.numeric(modelMonthlyRets[i+1, maxFactorName]), factorIndex), nrow=1), factorDates[i+1]))
  activeFactorMaxDt <- rbind(activeFactorMaxDt, c(as.character(factorDates[i+1]), maxFactorName))
}

names(activeFactorMaxDt) <- c("MONTH", "ACTIVE_FACTOR")
activeFactorMaxDt$MONTH <- as.Date(activeFactorMaxDt$MONTH)
names(factorMax) <- c("FACTOR_MAX", "FACTOR_INDEX")

factorMax$T <- ifelse(factorMax$FACTOR_INDEX == stats::lag(factorMax$FACTOR_INDEX, 1), 0, 1)
factorMax$FACTOR_RET <- ifelse(factorMax$T != 0, factorMax$FACTOR_MAX-drag, factorMax$FACTOR_MAX)

toPlot <- na.omit(merge.xts(factorMax$FACTOR_RET, benchMonthyRet))
names(toPlot) <- c("FACTOR_MAX", indexBench)
sharpe <- SharpeRatio.annualized(toPlot)
print(sharpe)

Common.PlotCumReturns(toPlot, "Model Factor MAX", 
                      sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), #NULL)
                      sprintf("%s/model-MAX.%s.png", reportPath, indexBench), NULL)


#max monthly return - (factor momentum)

activeFactorMonthlyDt <- tibble()
factorMonthly <- NULL
for(i in 2:length(factorDates)){
  sDt <- factorDates[i-1]
  eDt <- factorDates[i]
  
  factorIndex <- which.max(modelMonthlyRets[sDt, ])
  maxFactorName <- modelIds[factorIndex]
  
  factorMonthly <- rbind(factorMonthly, xts(matrix(c(as.numeric(modelMonthlyRets[eDt, maxFactorName]), factorIndex), nrow=1), eDt))
  activeFactorMonthlyDt <- rbind(activeFactorMonthlyDt, c(as.character(eDt), maxFactorName))
}

names(activeFactorMonthlyDt) <- c("MONTH", "ACTIVE_FACTOR")
activeFactorMonthlyDt$MONTH <- as.Date(activeFactorMonthlyDt$MONTH)
names(factorMonthly) <- c("FACTOR_MONTHLY", "FACTOR_INDEX")

factorMonthly$T <- ifelse(factorMonthly$FACTOR_INDEX == stats::lag(factorMonthly$FACTOR_INDEX, 1), 0, 1)
factorMonthly$FACTOR_RET <- ifelse(factorMonthly$T != 0, factorMonthly$FACTOR_MONTHLY-drag, factorMonthly$FACTOR_MONTHLY)

toPlot <- na.omit(merge.xts(factorMonthly$FACTOR_RET, benchMonthyRet))
names(toPlot) <- c("FACTOR_MONTHLY", indexBench)
sharpe <- SharpeRatio.annualized(toPlot)
print(sharpe)

Common.PlotCumReturns(toPlot, "Model Momentum", 
                      sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), #NULL)
                      sprintf("%s/model-monthly.%s.png", reportPath, indexBench), NULL)


#combined 

toPlot <- na.omit(merge.xts(factorMax$FACTOR_RET, factorMonthly$FACTOR_RET, benchMonthyRet))
names(toPlot) <- c("FACTOR_MAX", "FACTOR_MONTHLY", indexBench)
sharpe <- SharpeRatio.annualized(toPlot)
print(sharpe)

Common.PlotCumReturns(toPlot, "NIFTY 500 Factor MAX vs. Monthly Momentum", 
                      sprintf("SR: %s", paste(round(sharpe,2), collapse="/")), #NULL)
                      sprintf("%s/model-monthly-max.%s.png", reportPath, indexBench), NULL)
