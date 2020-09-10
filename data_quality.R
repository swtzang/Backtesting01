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
