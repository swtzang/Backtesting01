# https://lf0.com/post/xgboost-time-series-classification-trading-strategy/xgboost-time-series-quant-trading-strategy/
# Machine Learning (XGBoost) Time-Series Classification Trading Strategy

require(PerformanceAnalytics)
library(data.table)
library(dplyr)
library(tibble)
library(TTR)
library(tidyr)
library(tidyquant)
library(tsfeatures)
library(rsample)
library(purrr)
library(stringr)
library(tibbletime) # tsibble clashes with the base R index() function
library(xgboost)
library(rvest)


set.seed(1234)
###################### Pre-define functions for later ##########################

Scale_Me <- function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}                                  # Note: I don't actually use this function but I leave it in here.

#################################################################################

start_date <- "2017-01-01"
end_date <- "2020-01-01"

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
symbols <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="constituents"]') %>% 
  html_table() %>% 
  .[[1]] %>% 
  filter(!str_detect(Security, "Class A|Class B|Class C")) %>%     # Removes firms with Class A, B & C shares
  sample_n(30) %>% 
  pull(Symbol)

#> symbols
#[1] "LIN"  "NCLH" "SPG"  "CB"   "CME"  "ROL"  "DHR"  "TROW" "SLB"  "RF"   "CHTR" "CI"   "HAL"  "CBRE" "NWL"  "COG"  "VIAC" "PLD"  "KLAC" "RL"  
#[21] "FRT"  "BAX"  "ABMD" "WYNN" "DOV"  "ANTM" "GS"   "TFC"  "F"    "CNC" 


dataEnv <- new.env()
getSymbols(symbols, 
           from = start_date, 
           to = end_date, 
           #src = "yahoo", 
           #adjust = TRUE, 
           env = dataEnv)


df <- eapply(dataEnv, function(x){as.data.frame(x) %>% 
             rename_all(function(n){
                        gsub("^(\\w+)\\.", "", n, perl = TRUE)
                        }
                       ) %>%
    rownames_to_column("date")  
}) %>% 
  rbindlist(idcol = TRUE) %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(.id) %>% 
  tq_mutate(
    select = Adjusted,
    mutate_fun = periodReturn,
    period = "daily",
    type = "arithmetic"
  ) %>% 
  mutate(
    Adj_lag = lag(Adjusted),
    chng_Adj = ifelse(Adjusted > Adj_lag, 1, 0) # more simply we could have just done if ret were pos/neg
  ) %>% 
  select("date", ".id", "Adjusted", "daily.returns", "chng_Adj", "Open", "High", "Low", "Close") %>% 
  as_tibble() %>% 
  as_tbl_time(index = date) %>% 
  setNames(c("date", "ID", "prc", "ret", "chng", "open", "high", "low", "close")) %>% 
  drop_na(chng)


nested_df <- df %>%
  mutate(duplicate_ID = ID) %>% 
  nest(-ID)


# First we set the number of days we want to construct the ts features
rolled_df <- map(nested_df$data, ~ rolling_origin(.,
                                                  initial = 100,
                                                  assess = 1,
                                                  cumulative = FALSE,
                                                  skip = 0))

rolled_df[[1]]$splits[[1]]



