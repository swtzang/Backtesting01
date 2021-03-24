# https://blog.quantinsti.com/shorting-high-algo-trading-strategy-r/
# Upload the required packages

library(xlsx)
library(zoo)
library(quantmod)
library(rowr)
library(TTR)




# Set the parameters here 

pc_up_level = 6  # Set the high percentage threshold level

noDays = 2       # No of lookback days




# Read the csv file that contains the Stock tickers to be scanned

test_tickers = read.csv("F&O Stock List.csv")

symbol = as.character(test_tickers[,4])          

leverage = as.character(test_tickers[,5])        




sig_finds = data.frame(0,0,0,0,0,0)

colnames(sig_finds) = c("Ticker","Leverage","Previous Price","Current Price","Abs Change","% Change")