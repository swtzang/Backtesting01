# https://bookdown.org/kochiuyu/Technical-Analysis-with-R/blotter-package.html
# Chiu Yu Ko, Technical analysis with R
rm(list = ls())
library(quantstrat)
#
options("getSymbols.warning4.0"=FALSE)
from ="2008-01-01"
to ="2012-12-31"
symbols = c("AAPL", "IBM")
currency("USD")
getSymbols(symbols, from=from, to=to, 
           adjust=TRUE)
stock(symbols, currency="USD", multiplier=1)
initEq=10^6
# To start, we initialize account and portfolio where:
# Porfolio: stores which stocks to be traded
# Account: stores which money transactions
rm("account.buyHold", pos=.blotter)
rm("portfolio.buyHold", pos=.blotter)

initPortf("buyHold", symbol=symbols)
initAcct("buyHold", portfolios = "buyHold", initEq = initEq)
# To illustrate, we just consider buy and hold strategy:
#  Buy on the first day at closing price
# Sell on the last day at closing price
Apple.Buy.Date <- first(time(AAPL))
Apple.Buy.Price <- as.numeric(Cl(AAPL[Apple.Buy.Date,]))
Apple.Sell.Date <- last(time(AAPL))
Apple.Sell.Price <- as.numeric(Cl(AAPL[Apple.Sell.Date,]))
Apple.Qty <- trunc(initEq/(2*Apple.Buy.Price))
#
IBM.Buy.Date <- first(time(IBM))
IBM.Buy.Price <- as.numeric(Cl(IBM[IBM.Buy.Date,]))
IBM.Sell.Date <- last(time(IBM))
IBM.Sell.Price <- as.numeric(Cl(IBM[IBM.Sell.Date,]))
IBM.Qty <- trunc(initEq/(2*IBM.Buy.Price))
#
# We first add buy transactions to the system using the function addTxn:
addTxn(Portfolio = "buyHold", 
       Symbol = "AAPL", 
       TxnDate = Apple.Buy.Date, 
       TxnQty = Apple.Qty,
       TxnPrice = Apple.Buy.Price,
       TxnFees = 0)

addTxn(Portfolio = "buyHold", 
       Symbol = "IBM", 
       TxnDate = IBM.Buy.Date, 
       TxnQty = IBM.Qty,
       TxnPrice = IBM.Buy.Price,
       TxnFees = 0)
#
# Then we add the sell transactions:
  
addTxn(Portfolio = "buyHold", 
         Symbol = "AAPL", 
         TxnDate = Apple.Sell.Date, 
         TxnQty = -Apple.Qty,
         TxnPrice = Apple.Sell.Price,
         TxnFees = 0)

addTxn(Portfolio = "buyHold", 
       Symbol = "IBM", 
       TxnDate = IBM.Sell.Date, 
       TxnQty = -IBM.Qty,
       TxnPrice = IBM.Sell.Price,
       TxnFees = 0)
#
# Now we can update the account based on the added transactions:
  
updatePortf(Portfolio = "buyHold")
#
updateEndEq(Account = "buyHold")
#
chart.Posn("buyHold", Symbol = "AAPL")
chart.Posn("buyHold", Symbol = "IBM")
# We can see the trading statistics

out <- perTradeStats("buyHold", "IBM")
t(out)
#
out <- perTradeStats("buyHold", "AAPL")
t(out)
# Buy filter rule
from ="2009-01-01"
to ="2012-12-31"
symbols = c("MSFT")
currency("USD")
getSymbols(symbols, from=from, to=to, adjust=TRUE)
stock(symbols, currency="USD", multiplier=1)
initEq=10^6
# Then setup the account and portfolio:
rm("account.filter",pos=.blotter)
rm("portfolio.filter",pos=.blotter)
#
initPortf("filter", symbol=symbols)
initAcct("filter", portfolios = "filter", initEq = initEq)
# generate trading indicator
price <- Cl(MSFT)         
r <- price/Lag(price) - 1    
delta <- 0.03
signal <- c(NA)
for (i in 2: length(price)){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (r[i]< -delta){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal, Cl(MSFT))
head(Cl(MSFT))
# convert trading indicator to trading signal
trade <- Lag(signal)
trade <- na.fill(trade,0)
# Now we are ready to apply trading signal into trading action:
for (i in 1:length(price)){
    if (as.numeric(trade[i]) == 1){
        addTxn(Portfolio = "filter",
                  Symbol = "MSFT", 
                 TxnDate = time(price[i]), 
                  TxnQty = 1000,
                TxnPrice = price[i],
                 TxnFees = 0)    
    }
    if (as.numeric(trade[i]) == -1){
        addTxn(Portfolio = "filter",
                  Symbol = "MSFT", 
                 TxnDate = time(price[i]), 
                  TxnQty = -1000,
                TxnPrice = price[i],
                 TxnFees = 0)    
    }
}
# Finally, we update the account and do charting:
updatePortf(Portfolio = "filter")
updateAcct(name = "filter")
updateEndEq(Account = "filter")
chart.Posn("filter", Symbol = "MSFT")
zoom_Chart("2009-01/2009-12") 


# Simple filter rule
# Step 1: initialization----
options("getSymbols.warning4.0" = FALSE)
from ="2003-01-01"
to ="2012-12-31"
symbols = c("MSFT", "IBM")
getSymbols(symbols, from=from, to=to, 
           adjust=TRUE)
currency("USD")
stock(symbols, currency="USD", multiplier=1)
# define our strategy
strategy.st <- "filter"
portfolio.st <- "filter"
account.st <- "filter"
# Remove any old variables
rm.strat("filter")
# After naming strategy, portfolio and account, we initialize them:
initEq=100000
initDate="1990-01-01"

initPortf(name=portfolio.st, 
          symbols=symbols, 
          initDate=initDate, 
          currency='USD')
initAcct(name=account.st, 
         portfolios=portfolio.st,    
         initDate=initDate, 
         currency='USD', 
         initEq=initEq)
initOrders(portfolio=portfolio.st, 
           symbols=symbols,
           initDate=initDate)

strategy(strategy.st, store=TRUE)
# Step 2: define indicator----
filter <- function(price) {
          lagprice <- lag(price,1)
          temp <- price/lagprice - 1
          colnames(temp) <- "filter"
          return(temp)
} 
#
add.indicator(
      strategy=strategy.st,
      name = "filter", 
      arguments = list(price = quote(Cl(mktdata))), 
      label= "filter")
# To check if the indicator is defined correctly, use 
# applyindicators to see if it works. The function try() 
# is to allow the program continue to run even if there is an error.
test <-try(applyIndicators(strategy.st, 
                           mktdata=OHLC(AAPL)))
head(test, n=4)
# Step 3: trading signals----
#In quantstrat, there are three ways one can use a signal. It is refer to as name:
  
# 1. sigThreshold: more or less than a fixed value
# 2. sigCrossover: when two signals cross over
# 3. sigComparsion: compare two signals
# The column refers to the data for calculation of signal. There are five possible relationship:
# gt = greater than
# gte = greater than or equal to
# lt = less than
# lte = less than or equal to
# eq = equal to
#
# Buy Signal under simple trading rule with threshold δ = 0.05
#
# enter when filter > 1+\delta
add.signal(strategy.st, 
           name="sigThreshold",
           arguments = list(threshold = 0.05,   
                            column = "filter",
                            relationship = "gt",   
                            cross = TRUE),
           label="filter.buy")
# Sell Signal under simple trading rule with threshold δ = −0.05
# exit when filter < 1-delta
add.signal(strategy.st, 
           name="sigThreshold",
           arguments = list(threshold = -0.05, 
                            column = "filter",
                            relationship = "lt",
                            cross = TRUE),
           label = "filter.sell") 
# Step 4: trading rules----
# While trading signals tell us buy or sell, but it does not specify the execution details.
# Trading rules will specify the following seven elements:
# SigCol: Name of Signal
# SigVal: implement when there is signal (or reverse)
# Ordertype: market, stoplimit
# Orderside: long, short
# Pricemethod: market
# Replace: whether to replace other others
# Type: enter or exit the order

# Buy rule specifies that when a buy signal appears, 
# place a buy market order with quantity size.

add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="filter.buy", 
                          sigval=TRUE,  
                          orderqty=1000,
                          ordertype='market', 
                          orderside='long',
                          pricemethod='market',
                          replace=FALSE), 
         type='enter', 
         path.dep=TRUE)

# Sell rule specifies that when a sell signal appears, place a sell market order with quantity size.
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="filter.sell",
                          sigval=TRUE, 
                          orderqty=-1000,  
                          ordertype='market',  
                          orderside='long', 
                          pricemethod='market',  
                          replace=FALSE), 
         type='enter', 
         path.dep=TRUE)

# Step 5: Evaluation results
out <- try(applyStrategy(strategy=strategy.st,
                       portfolios=portfolio.st))

updatePortf(portfolio.st)
updateAcct(portfolio.st)
updateEndEq(account.st)
# visualize the trading position, profit and loss, and drawdown in graph.
for(symbol in symbols) {
  chart.Posn(Portfolio=portfolio.st,
             Symbol=symbol,
             log=TRUE)
}

tstats <- tradeStats(portfolio.st)
t(tstats) #transpose tstats

out <- perTradeStats(portfolio.st)
t(out)
# out <- perTradeStats(qs.strategy)
# write.csv(out, 'perTradeStats.csv')

# Then we can evaluate the performance using PerfomanceAnalytics package to 
# see how is the return of the trading strategy.

rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
tab <- table.Arbitrary(rets,
                       metrics=c(
                         "Return.cumulative",
                         "Return.annualized",
                         "SharpeRatio.annualized",
                         "CalmarRatio"),
                       metricsNames=c(
                         "Cumulative Return",
                         "Annualized Return",
                         "Annualized Sharpe Ratio",
                         "Calmar Ratio"))
tab
#
charts.PerformanceSummary(rets, colorset = bluefocus)

# Get some warnings in this case
# ---- Buy and hold strategy ----
getSymbols("SPY", src = 'yahoo', from=from, to=to, adjust = T)
# getSymbols('SPY', src = 'yahoo', from = '2000-01-01', to = '2018-12-31', auto.assign = F)
# Step 1: Initialization ----
rm.strat("buyHold")

#Initial Setup
initPortf("buyHold", "SPY", initDate = initDate)
initAcct("buyHold", portfolios = "buyHold",
         initDate = initDate, initEq = initEq)
# 
FirstDate <- first(time(SPY))
# Enter order on the first date
BuyDate <- FirstDate
equity = getEndEq("buyHold", FirstDate)
FirstPrice <- as.numeric(Cl(SPY[BuyDate,]))
UnitSize = as.numeric(trunc(equity/FirstPrice))
#
addTxn(Portfolio = "buyHold", 
       Symbol = "SPY", 
       TxnDate = BuyDate, 
       TxnPrice = FirstPrice,
       TxnQty = UnitSize, 
       TxnFees = 0)

# We first add the transaction to sell at the end:
LastDate <- last(time(SPY))
# Exit order on the Last Date
LastPrice <- as.numeric(Cl(SPY[LastDate,]))
addTxn("buyHold", Symbol = "SPY", 
       TxnDate = LastDate, TxnPrice = LastPrice,
       TxnQty = -UnitSize , TxnFees = 0)

updatePortf(Portfolio = "buyHold")

updateAcct(name = "buyHold")

updateEndEq(Account = "buyHold")

chart.Posn("buyHold", Symbol = "SPY")
# Compare strategy and market
rets <- PortfReturns(Account = account.st)
rets.bh <- PortfReturns(Account = "buyHold")
returns <- cbind(rets, rets.bh)
charts.PerformanceSummary(
  returns, geometric = FALSE,
  wealth.index = TRUE, 
  main = "Strategy vs. Market")
#------------------------------------------------
# SMA rule ----
# Buy every day when SMA30 SMA200
# Sell everything (all positions) when SMA30 SMA200
options("getSymbols.warning4.0"=FALSE)
from ="2012-01-01"
to ="2012-12-31"
symbols = c("IBM","MSFT")
getSymbols(symbols, from=from, to=to, adjust=TRUE)
# initialize setup
currency("USD")
strategy.st <- portfolio.st <- account.st <- "SMA"
rm.strat(strategy.st)

initPortf(portfolio.st, symbols)
initAcct(account.st, portfolios=portfolio.st, 
         initEq = initEq)
initOrders(portfolio.st)
strategy(strategy.st, store=TRUE)

# Indicator ----
add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), n=30),
              label="sma30")

add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), n=200),
              label="sma200")

# Signals
# Bull market if SMA30>SMA200
add.signal(strategy.st, 
           name="sigComparison",
           arguments=list(columns=c("sma30","sma200"),
                 relationship="gt"),
           label="buy")

# Sell market if SMA30<SMA200
add.signal(strategy.st, 
           name="sigComparison",
           arguments=list(columns=c("sma30","sma200"), 
                relationship="lt"), 
           label="sell")
# Rules ----
# Buy Rule
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="buy", 
                          sigval=TRUE,  
                          orderqty=1000, 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market', 
                          replace=FALSE), 
         type='enter', 
         path.dep=TRUE)

# Sell Rule
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="sell", 
                          sigval=TRUE,  
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market', 
                          replace=FALSE), 
         type='exit', 
         path.dep=TRUE) 
# Evaluation
out<-try(applyStrategy(strategy.st, 
                       portfolios=portfolio.st))
# update portfolio
updatePortf(portfolio.st)
updateAcct(portfolio.st)
updateEndEq(account.st)
#
for(symbol in symbols) {
  chart.Posn(Portfolio=portfolio.st,
             Symbol=symbol,log=TRUE)
}


