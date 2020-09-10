# https://timtrice.github.io/backtesting-strategies/using-quantstrat.html
rm(list = ls())
library('quantstrat')
library('quantmod')
#
Sys.setenv(TZ = "UTC")
currency('USD')
init_date <- "2007-12-31"
start_date <- "2008-01-01"
end_date <- "2009-12-31"
init_equity <- 1e4 # $10,000
adjustment <- TRUE
# get symbols
basic_symbols <- function() {
  symbols <- c(
    "IWM", # iShares Russell 2000 Index ETF
    "QQQ", # PowerShares QQQ TRust, Series 1 ETF
    "SPY" # SPDR S&P 500 ETF Trust
  )
}
#
enhanced_symbols <- function() {
  symbols <- c(
    basic_symbols(), 
    "TLT", # iShares Barclays 20+ Yr Treas. Bond ETF
    "XLB", # Materials Select Sector SPDR ETF
    "XLE", # Energy Select Sector SPDR ETF
    "XLF", # Financial Select Sector SPDR ETF
    "XLI", # Industrials Select Sector SPDR ETF
    "XLK", # Technology  Select Sector SPDR ETF
    "XLP", # Consumer Staples  Select Sector SPDR ETF
    "XLU", # Utilities  Select Sector SPDR ETF
    "XLV", # Health Care  Select Sector SPDR ETF
    "XLY" # Consumer Discretionary  Select Sector SPDR ETF
  )
}

# Guy Yollin, 2014
# http://www.r-programming.org/papers

checkBlotterUpdate <- function(port.st = portfolio.st, 
                               account.st = account.st, 
                               verbose = TRUE) {
  
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(
    sapply(
      syms, 
      FUN = function(x) eval(
        parse(
          text = paste("sum(p$symbols", 
                       x, 
                       "posPL.USD$Net.Trading.PL)", 
                       sep = "$")))))
  
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  
  if(!isTRUE(all.equal(port.tot, port.sum.tot))) {
    ok <- FALSE
    if(verbose) print("portfolio P&L doesn't match sum of symbols P&L")
  }
  
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  
  if(!isTRUE(all.equal(port.tot, endEq - initEq)) ) {
    ok <- FALSE
    if(verbose) print("portfolio P&L doesn't match account P&L")
  }
  
  if(sum(duplicated(index(p$summary)))) {
    ok <- FALSE
    if(verbose)print("duplicate timestamps in portfolio summary")
    
  }
  
  if(sum(duplicated(index(a$summary)))) {
    ok <- FALSE
    if(verbose) print("duplicate timestamps in account summary")
  }
  return(ok)
}
#
print(basic_symbols())
symbols <- basic_symbols()
#
getSymbols(Symbols = symbols, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)
#
head(IWM)
tail(IWM)
summary(IWM)
# Clear symbols
rm(list=basic_symbols())
#
getSymbols(Symbols = "DGS10", src = "FRED")
#
chartSeries(DGS10)






