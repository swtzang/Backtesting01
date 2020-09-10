#
# Chapter 4 Get Symbols----
# Chapter 5 Basic Strategy----
# https://timtrice.github.io/backtesting-strategies/basic-strategy.html
#
#  SMA10 and SMA30----
# If the SMA(10) indicator is greater than or equal to 
# the SMA(30) indicator we will submit a stoplimit long 
# order to open and close any short positions that may 
# be open. If the SMA(10) is less than the SMA(30) we will submit a 
# stoplimit short order to open and close any open long positions.
#--------------------
#If SMA(10) >= SMA(30):
#  BTC short, BTO long
#Else if SMA(10) < SMA(30): 
#  STC long, STO short
symbols <- basic_symbols()
getSymbols(Symbols = symbols, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)
#
stock(symbols, 
      currency = "USD", 
      multiplier = 1)
#
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"
#
rm.strat(portfolio.st)
rm.strat(account.st)
#
initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
#
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
#
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)
#
strategy(strategy.st, store = TRUE)
# Add indicators
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), 
                               n = 10),
              label = "nFast")
#
add.indicator(strategy = strategy.st, 
              name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), 
                               n = 30), 
              label = "nSlow")
#
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
#
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")
#
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = 100,
                          ordertype = "stoplimit",
                          orderside = "long", 
                          threshold = 0.0005,
                          prefer = "High", 
                          TxnFees = -10, 
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")
#
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -100,
                          ordertype = "stoplimit",
                          threshold = -0.005, 
                          orderside = "short", 
                          replace = FALSE, 
                          TxnFees = -10, 
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")
#
add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "short", 
                          sigval = TRUE, 
                          orderside = "long", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2SHORT")
#
add.rule(strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "long", 
                          sigval = TRUE, 
                          orderside = "short", 
                          ordertype = "market", 
                          orderqty = "all", 
                          TxnFees = -10, 
                          replace = TRUE), 
         type = "exit", 
         label = "Exit2LONG")
#
# Apply strategy----
#
cwd <- getwd()
# setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results <- applyStrategy(strategy.st, portfolios = portfolio.st)
  updatePortf(portfolio.st)
  updateAcct(account.st)
  updateEndEq(account.st)
  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}

setwd(cwd)













