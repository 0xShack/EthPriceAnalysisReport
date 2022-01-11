rm(list = ls())
setwd("~/Penn/Penn Junior Year/ECON 224/ProjectData")

library(tidyverse)
library(lubridate)

######################
## DATA COMPILATION ##
######################

## MARKET CLOSE DATA

filenames = c('BTC', 'LTC', 'SPX', 'CBOEVolatility', 'SP500Energy', 'DJRetail',
              'DJRealEstate', 'ShenghaiComp', 'CrudeOil', 'NaturalGas', 'Gold',
              'Silver', 'TY1M', 'TY1Y', 'TY10Y', 'TY30Y')

MacroData = read_csv('ETH.csv') %>%
  select(-Open, -High, -Low) %>%
  setNames(c('Date', 'ETH'))

for (name in filenames) {
  filename = paste(name, '.csv', sep = '')
  data = read_csv(filename) %>%
    select(-Open, -High, -Low) %>%
    setNames(c("Date", name))
  MacroData = inner_join(MacroData, data, by = c('Date' = 'Date'))
  name
  nrow(MacroData)
}

MacroData = MacroData %>%
  mutate(Date = mdy(Date))

## MARKET CHANGE DATA

n = nrow(MacroData)
m = ncol(MacroData)

MacroDataChange = MacroData
for (i in 1:(n-1)) {
  for (j in 2:m) {
    curr = MacroData[i, j]
    last = MacroData[i + 1, j]
    MacroDataChange[i, j] = (curr - last) / last
  }
}
MacroDataChange = slice(MacroDataChange, 1:(n-1))


## ETHEREUM DAILY SNAPSHOT DATA

ETHNetwork = read_csv('eth_snapshots_avg.csv') %>%
  mutate(timestamp = mdy(timestamp))

ETHData = read_csv('ETH.csv') %>%
  mutate(Date = mdy(Date)) %>%
  select(Date, Close) %>%
  inner_join(ETHNetwork, by = c('Date' = 'timestamp'))

## ETHEREUM DAILY SNAPSHOT CHANGE DATA

n = nrow(ETHData)
m = ncol(ETHData)

ETHDataChange = ETHData
for (i in 1:(n-1)) {
  for (j in 2:m) {
    curr = ETHData[i, j]
    last = ETHData[i + 1, j]
    ETHDataChange[i, j] = (curr - last) / last
  }
}
ETHDataChange = slice(ETHDataChange, 1:(n-1))

## FULL DATA SET

FullData = inner_join(MacroData, ETHNetwork, by = c('Date' = 'timestamp'))

n = nrow(FullData)
m = ncol(FullData)

FullDataChange = FullData
for (i in 1:(n-1)) {
  for (j in 2:m) {
    curr = FullData[i, j]
    last = FullData[i + 1, j]
    FullDataChange[i, j] = (curr - last) / last
  }
}
FullDataChange = slice(FullDataChange, 1:(n-1))

########################
## EXPLANATORY MODELS ##
########################

library(randomForest)

## Macroeconomic Model
MacroData = select(MacroData, -Date)
X = model.matrix(ETH ~ ., MacroData)[,-1]
Y = MacroData$ETH

set.seed(123)
tuneRF(X, Y, ntreeTry = 500, improve = 0.01,
       plot = TRUE, trace = TRUE, doBest = TRUE)

macroRF = randomForest(X, Y, importance = TRUE, ntree = 500, mtry = 10)
importance(macroRF)
varImpPlot(macroRF, n.var = 10)

## Macroeconomic Change Model
MacroDataChange = select(MacroDataChange, -Date)
X = model.matrix(ETH ~ ., MacroDataChange)[,-1]
Y = MacroDataChange$ETH

set.seed(123)
tuneRF(X, Y, ntreeTry = 500, improve = 0.01,
       plot = TRUE, trace = TRUE, doBest = TRUE)

macroChangeRF = randomForest(X, Y, importance = TRUE, ntree = 500, mtry = 10)
importance(macroChangeRF)
varImpPlot(macroChangeRF, n.var = 10)


## Ethereum Network Activity Model
ETHData = select(ETHData, -Date)
X = model.matrix(Close ~ ., ETHData)[,-1]
Y = ETHData$Close

set.seed(123)
tuneRF(X, Y, ntreeTry = 500, improve = 0.001,
       plot = TRUE, trace = TRUE, doBest = TRUE)

ethRF = randomForest(X, Y, importance = TRUE, ntree = 1000, mtry = 3)
importance(ethRF)
varImpPlot(ethRF, n.var = 10)

## Ethereum Network Activity Change Model
ETHDataChange = select(ETHDataChange, -Date)
X = model.matrix(Close ~ ., ETHDataChange)[,-1]
Y = ETHDataChange$Close

set.seed(123)
tuneRF(X, Y, ntreeTry = 500, improve = 0.001,
       plot = TRUE, trace = TRUE, doBest = TRUE)

ethChangeRF = randomForest(X, Y, importance = TRUE, ntree = 500, mtry = 5)
importance(ethChangeRF)
varImpPlot(ethChangeRF, n.var = 10)


## Combined Model
FullData = select(FullData, -Date)
X = model.matrix(ETH ~ ., FullData)[,-1]
Y = FullData$ETH

set.seed(123)
tuneRF(X, Y, ntreeTry = 500, improve = 0.001,
       plot = TRUE, trace = TRUE, doBest = TRUE)

fullRF = randomForest(X, Y, importance = TRUE, ntree = 1000, mtry = 5)
importance(fullRF)
varImpPlot(fullRF, n.var = 10)

## Combined Change Model
FullDataDates = FullDataChange
FullDataChange = select(FullDataChange, -Date)
X = model.matrix(ETH ~ ., FullDataChange)[,-1]
Y = FullDataChange$ETH

set.seed(123)
tuneRF(X, Y, ntreeTry = 500, improve = 0.001,
       plot = TRUE, trace = TRUE, doBest = TRUE)

fullChangeRF = randomForest(X, Y, importance = TRUE, ntree = 500, mtry = 20)
importance(fullChangeRF)
varImpPlot(fullChangeRF, n.var = 10)


#######################
## TRADING ALGORITHM ##
#######################

library(gbm)

alpha = 0.01

n = nrow(FullDataChange)
TradingData = add_column(FullDataChange, Buy = 0)
for (i in 2:n) {
  TradingData$Buy[i] = case_when(
    FullDataChange$ETH[i-1] > alpha ~ 1,
    TRUE ~ 0)
}

testData = TradingData %>%
  select(-ETH) %>%
  slice(2:28)
trainData = TradingData %>%
  select(-ETH) %>%
  slice(29:186)

# GRADIENT BOOSTING, OPTIMIZE OVER PARAMETERS
hypergrid = expand.grid(
  shrinkage = c(0.005, 0.01, 0.1),
  interaction.depth = c(1, 2, 3, 4, 5),
  optimal_trees = 0,
  min_deviance = 0
)

set.seed(1)
for (i in 1:nrow(hypergrid)) {
  gbm.tune = gbm(Buy ~ .,
                 data = trainData,
                 distribution = "bernoulli",
                 n.trees = 100,
                 interaction.depth = hypergrid$interaction.depth[i],
                 shrinkage = hypergrid$shrinkage[i],
                 cv.folds = 5,
                 n.cores = NULL,
                 verbose = FALSE)
  hypergrid$optimal_trees[i] = which.min(gbm.tune$cv.error)
  hypergrid$min_deviance[i] = min(gbm.tune$cv.error)
}

optimal_setting = which.min(hypergrid$min_deviance)
gbm.best = gbm(Buy ~ .,
               data = trainData,
               distribution = "bernoulli",
               n.trees = 100,
               interaction.depth = hypergrid$interaction.depth[optimal_setting],
               shrinkage = hypergrid$shrinkage[optimal_setting],
               cv.folds = 5,
               n.cores = NULL,
               verbose = FALSE)

min_error = which.min(gbm.best$cv.error)
gbm.best$cv.error[min_error]
gbm.perf(gbm.best, method = 'cv')

## TEST MODEL ON NEW DATA
trades = predict.gbm(gbm.best, newdata = testData, n.trees = min_error, type = "response")
trades

tradingPeriod = FullDataDates %>%
  slice(1:27) %>%
  select(Date, ETH) %>%
  add_column(hadBought = trades) %>%
  mutate(hadBought = if_else(hadBought > 0.5, 1, 0)) %>%
  mutate(dailyProfit = if_else(hadBought == 1, 100 * ETH, 0))
tradingPeriod

## Trading profits:
sum(tradingPeriod$dailyProfit)

