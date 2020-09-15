#
# Chapter 6 Data Quality ----
#
library(DT)
library(data.table)
#
getSymbols("SPY", 
           src = "yahoo", 
           index.class = c("POSIXt", "POSIXct"),
           from = "2010-01-01", 
           to = "2011-01-01", 
           adjust = TRUE)

yahoo.SPY <- SPY
summary(yahoo.SPY)
#.....

# Examining Trades
rm.strat(portfolio.st)
rm.strat(account.st)
symbols <- basic_symbols()
getSymbols(Symbols = symbols, src = "yahoo", index.class = "POSIXct", 
           from = start_date, to = end_date, adjust = adjustment)
initPortf(name = portfolio.st, symbols = symbols, initDate = init_date)
initAcct(name = account.st, portfolios = portfolio.st, initDate = init_date, 
         initEq = init_equity)
initOrders(portfolio = portfolio.st, symbols = symbols, initDate = init_date)
applyStrategy(strategy.st, portfolios = portfolio.st)
checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
#
chart.Posn(portfolio.st, Symbol = "SPY", Dates="2008-01-01::2008-07-01", 
           TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")
#
le <- as.data.frame(mktdata["2008-02-25::2008-03-07", c(1:4, 7:10)])
DT::datatable(le, 
              rownames = TRUE,
              extensions = c("Scroller", "FixedColumns"), 
              options = list(pageLength = 5, 
                             autoWidth = TRUE, 
                             deferRender = TRUE, 
                             scrollX = 200, 
                             scroller = TRUE,
                             fixedColumns = TRUE), 
              caption = htmltools::tags$caption(
                "Table 6.1: mktdata object for Feb. 25, 2008 to Mar. 7, 2008"))

#
ob <- as.data.table(getOrderBook(portfolio.st)$Quantstrat$SPY)
DT::datatable(ob, 
              rownames = FALSE,
              filter = "top",
              extensions = c("Scroller", "FixedColumns"), 
              options = list(pageLength = 5, 
                             autoWidth = TRUE, 
                             deferRender = TRUE, 
                             scrollX = 200, 
                             scroller = TRUE, 
                             fixedColumns = TRUE), 
              caption = htmltools::tags$caption(
                "Table 6.2: Order book for SPY"))
#
chart.Posn(portfolio.st, Symbol = "SPY", Dates="2009-08-01::2009-12-31", 
           TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")
#
chart.Posn(portfolio.st, Symbol = "SPY", 
           TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")

# Chapter 7 Parameter optimization ----
.fastSMA <- (1:30)
.slowSMA <- (20:80)
.nsamples <- 5
#
portfolio.st <- "Port.Luxor.MA.Opt"
account.st <- "Acct.Luxor.MA.Opt"
strategy.st <- "Strat.Luxor.MA.Opt"

rm.strat(portfolio.st)
rm.strat(account.st)

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

strategy(strategy.st, store = TRUE)
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
         initDate = init_date)
#
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)

# strategy(strategy.st, store = TRUE)
# Add distribution ----
add.distribution(strategy.st,
                 paramset.label = "SMA",
                 component.type = "indicator",
                 component.label = "nFast",
                 variable = list(n = .fastSMA),
                 label = "nFAST")
#
add.distribution(strategy.st,
                 paramset.label = "SMA",
                 component.type = "indicator",
                 component.label = "nSlow",
                 variable = list(n = .slowSMA),
                 label = "nSLOW")
#
# Add distribution constraint----
add.distribution.constraint(strategy.st,
                            paramset.label = "SMA",
                            distribution.label.1 = "nFAST",
                            distribution.label.2 = "nSLOW",
                            operator = "<",
                            label = "SMA.Constraint")


# Running parallel
library(parallel)

if( Sys.info()['sysname'] == "Windows") {
  library(doParallel)
  registerDoParallel(cores=detectCores())
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}

# Apply paramset
cwd <- getwd()
setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results <- apply.paramset(strategy.st,
                            paramset.label = "SMA",
                            portfolio.st = portfolio.st,
                            account.st = account.st, 
                            nsamples = .nsamples)
  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}
setwd(cwd)

# stop loss orders
.fast <- 10
.slow <- 30
.threshold <- 0.0005
.orderqty <- 100
.txnfees <- -10
.stoploss <- 3e-3 # 0.003 or 0.3%
#
portfolio.st <- "Port.Luxor.Stop.Loss"
account.st <- "Acct.Luxor.Stop.Loss"
strategy.st <- "Strat.Luxor.Stop.Loss"

rm.strat(portfolio.st)
rm.strat(account.st)

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

# 8.2 Add indicators----
add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = .fast),
              label = "nFast")
#
add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = .slow),
              label = "nSlow")
#Add signals
add.signal(strategy.st, 
           name = "sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long"
)
add.signal(strategy.st, 
           name = "sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")

# 8.3 Add rules ----
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long" , 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long" ,
                          ordertype = "stoplimit",
                          prefer = "High",
                          threshold = .threshold,
                          TxnFees = .txnfees,
                          orderqty = +.orderqty,
                          osFUN = osMaxPos,
                          orderset = "ocolong"),
         type = "enter",
         label = "EnterLONG")
#
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          prefer = "Low",
                          threshold = .threshold,
                          TxnFees = .txnfees,
                          orderqty = -.orderqty,
                          osFUN = osMaxPos,
                          orderset = "ocoshort"),
         type = "enter",
         label = "EnterSHORT")
#
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "long" ,
                          ordertype = "market",
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "exit",
         label = "Exit2SHORT")
#
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long", 
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "exit",
         label = "Exit2LONG")
#
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "long" , 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain", 
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = FALSE)
#
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "short", 
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain", 
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled = FALSE)
#
for(symbol in symbols){
  addPosLimit(portfolio = portfolio.st,
              symbol = symbol,
              timestamp = init_date,
              maxpos = .orderqty)
}

# 8.5 Enable Rules
enable.rule(strategy.st, 
            type = "chain", 
            label = "StopLoss")

#
cwd <- getwd()
setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  load(results_file)
} else {
  results <- applyStrategy(strategy.st, portfolios = portfolio.st)
  if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save(list = "results", file = results_file)
    save.strategy(strategy.st)
  }
}
setwd(cwd)


