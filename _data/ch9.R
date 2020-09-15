# Chapter 9 Stop loss optimization ----

.StopLoss = seq(0.05, 0.6, length.out = 48)/100

strategy.st <- "Luxor.Stop.Loss.Opt"

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)

initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date)

initOrders(portfolio = portfolio.st,
           initDate = init_date)

strategy(strategy.st, store = TRUE)

# 9.1 Add indicators ----
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
# 9.2 Add signals ----
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


# 9.3 Add rules ----







