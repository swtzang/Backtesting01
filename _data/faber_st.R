# quantstrat tutorial: Introduction to trading systems, Guy Yollin, U of Washington
# Faber trading strategy example
rm(list = ls())
library(quantstrat)

currency("USD")

stock("SPY", currency = "USD", multiplier = 1)

ls(envir = FinancialInstrument:::.instrument)

ls(all = T)
# system settings
initDate<- '1997-12-31'
startDate<- '1999-01-01'
endDate<- '2014-06-30'
initEq<-1e6

Sys.setenv(TZ = "UTC")
getSymbols('SPY',from=startDate,to=endDate,index.class="POSIXct",adjust=T)

# SPY <-  to.monthly(SPY, from = startDate, to = endDate, index.class = "POSIXct", adjust = T)
SPY$SMA200d <- SMA(Cl(SPY), 200)
#
# inz portfolio, account
qs.strategy<-"qsFaber"
rm.strat(qs.strategy) # remove strategy etc. if this is a re-run
initPortf(qs.strategy,'SPY',initDate = initDate)
initAcct(qs.strategy, portfolios = qs.strategy, initDate = initDate, initEq = initEq)
# initialize orders container
args(initOrders)

initOrders(portfolio = qs.strategy, initDate = initDate)
# instantiate a new strategy object
args(strategy)

strategy(qs.strategy, store = TRUE)

ls(all = T)

ls(.blotter)

ls(.strategy)

args(getStrategy)

strat <- getStrategy(qs.strategy)
class(strat)

summary(strat)

# Add a 200-day simple moving average
add.indicator(strategy = qs.strategy,
              name= "SMA",
              arguments = list(x= quote(Cl(mktdata)), n=200),label = "SMA200"
              )

summary(getStrategy(qs.strategy))
# Add signal for crossing above SMA
# Add signal for crossing below SMA
add.signal(qs.strategy,
           name="sigCrossover",
           arguments= list(columns=c("Close","SMA200"),relationship="gt"),
           label="Cl.gt.SMA"
           )
#
add.signal(qs.strategy,name="sigCrossover",
           arguments= list(columns=c("Close","SMA200"),relationship="lt"),
           label="Cl.lt.SMA"
           )
#
summary(getStrategy(qs.strategy))
# Add rules
# Go LONG when close > MA
add.rule(qs.strategy,name='ruleSignal',
         arguments= list(sigcol="Cl.gt.SMA", 
                         sigval=TRUE, 
                         orderqty=900,
                         ordertype='market', 
                         orderside='long'),
         type='enter'
         )

# exit when close < MA
add.rule(qs.strategy,name='ruleSignal',
         arguments= list(sigcol="Cl.lt.SMA",
                         sigval=TRUE,
                         orderqty='all',
                         ordertype='market',
                         orderside='long'),
         type='exit'
         )

summary(getStrategy(qs.strategy))
# Apply the strategy
applyStrategy(strategy=qs.strategy ,portfolios=qs.strategy)
#
getTxns(Portfolio=qs.strategy,Symbol="SPY")
#
# mktdata is a special variable constructed during the execution of
# applyStrategy. It is a time series object which contains the historic price
# data as well as the calculated indicators, signals, and rules:
mktdata['2002']
# mktdata.df <- data.frame(date=index(mktdata), coredata(mktdata)) 
# write.csv(mktdata.df, 'mktdata.csv')
# Inspecting mktdata can be very helpful in understanding strategy
# processing and debugging
updatePortf(qs.strategy)
updateAcct(qs.strategy)
updateEndEq(qs.strategy)
# create custom theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border<- 'lightgray'
myTheme$col$up.border<- 'lightgray'
# plot performance
chart.Posn(qs.strategy,Symbol= 'SPY',
           Dates= '1998::',
           theme=myTheme,
           TA='add_SMA(n=200,col=4, on=1, lwd=2)')
#
tstats<- t(tradeStats(qs.strategy))
tstats
#
ob <- getOrderBook(qs.strategy)
class(ob)
names(ob)
names(ob$qsFaber)
names(ob$qsFaber$SPY)
#
ob$qsFaber$SPY[, 1:5]
#
ob$qsFaber$SPY[, 6:11]
#
perTradeStats(qs.strategy)
# out <- perTradeStats(qs.strategy)
# write.csv(out, 'perTradeStats.csv')

# quantstrat includes the capability to generate maximum adverse excursion
# (MAE) and maximum favorable excursion (MFE) charts.
chart.ME(Portfolio=qs.strategy, Symbol='SPY', type='MAE', scale='percent')
#
chart.ME(Portfolio=qs.strategy, Symbol='SPY', type='MFE', scale='percent')
# Retrieving the account summary
a <- getAccount(qs.strategy)
last(a$summary, 5)
#
library(lattice)
xyplot(a$summary,type="h",col=4)
#
equity<-a$summary$End.Eq

plot(equity,main="Faber Strategy Equity Curve")

ret<- Return.calculate(equity,method="log")

charts.PerformanceSummary(ret, colorset= bluefocus,
                          main="Faber Strategy Performance")



