library(tidyverse)
library(quantmod)
library(tidyquant)
library(ggplot2)
library(dplyr)

# Download, process, and save stock data for a given set of symbols.
# The data includes weekly closing prices and returns, as well as lagged data.
# The function also generates CSV files for the data.

today <- print(Sys.Date())
class(today)


setwd("C:\\Users\\alext\\R Stuff") # I'd avoid using a folder that's hooked up to one-drive, set your own up under users!


get_stocks_weekly_close_and_lag <- function(symbols, start_date, end_date, metric, lags){

  # Download and process stock data for each symbol
  stock_data_list <- map(symbols, function(symbol) {
    stock <- getSymbols.yahoo(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    stock_weekly <- to.weekly(stock, indexAt = "last", OHLC = FALSE)
    colnames(stock_weekly) <- paste0(symbol, ".", c("Open", "High", "Low", "Close", "Volume", "Adjusted"))
    stock_weeklyClose <- stock_weekly[, paste0(symbol, ".", metric)]
    stock_weeklyClose_df <- data.frame(Close = coredata(stock_weeklyClose))
    stock_weeklyReturn_df <- data.frame(Date = index(stock_weeklyClose), Close = coredata(stock_weeklyClose))
    stock_weekly_Return <- stock_weeklyReturn_df %>%
      mutate(Weekly.Return = (stock_weeklyReturn_df[,2] - lag(stock_weeklyReturn_df[,2], n = 1)) / lag(stock_weeklyReturn_df[,2], n = 1))
    stock_weekly_Return <- arrange(stock_weekly_Return, desc(Date)) 
    stock_weekly_Return <- data.frame(stock_weekly_Return[,2:3])
    colnames(stock_weekly_Return) <- c(paste0(symbol, ".", metric), paste0(symbol, ".Weekly.Return"))
    list(stock_weekly_Return = stock_weekly_Return, stock_weeklyClose_df = stock_weeklyClose_df)
    # assign(paste0(symbol, sep = ".", "weekly.Return"), stock_weekly_Return, envir = .GlobalEnv)
  })
  
  # Extract stock data from the lists and compile them into their respective data frames
  names(stock_data_list) <- symbols
  stocks_weekly_return <- bind_cols(map(stock_data_list, "stock_weekly_Return"))
  stocks_weekly_close <- do.call(cbind, map(stock_data_list, "stock_weeklyClose_df"))
  
  
  
  
  
  # Add date columns to both dataframes
  date_data <- getSymbols.yahoo(symbols[1], src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  weekly_date <- to.weekly(date_data, indexAt = "last", OHLC = FALSE)
  colnames(weekly_date) <- paste0(symbols[1], ".", c("Open", "High", "Low", "Close", "Volume", "Adjusted"))
  date_column <- weekly_date[, paste0(symbols[1], ".Close")]
  dates_df <- data.frame(Date = index(date_column))
  dates_df$Date <- as.character(dates_df$Date)
  stocks_weekly_close <- cbind(dates_df, stocks_weekly_close)
  stocks_weekly_close <- arrange(stocks_weekly_close, desc(Date))
  dates_df <- arrange(dates_df, desc(Date)) 
  stocks_weekly_return <- cbind(dates_df, stocks_weekly_return)
  
  
  # Generate CSV in your working directory 
  write.csv(stocks_weekly_return, file = paste0(metric, ".", "weekly.Return.csv"), row.names = FALSE)
  write.csv(stocks_weekly_close, file = paste0(metric, ".", "weekly.Close.csv"), row.names = FALSE)
  

  # Prepare for lagged stock data
  stocks_weekly_close <- arrange(stocks_weekly_close, Date)
  stocks_weekly_return <- arrange(stocks_weekly_return, Date)
  
  weekly_close_lag <- map(symbols, function(symbol) {
    stock_data <- stocks_weekly_return %>% select(paste0(symbol, ".", "Adjusted"))
    stock_lagged_data <- stock_data
    
    for (i in lags){
      stock_lagged_data <- cbind(stock_lagged_data, lag(stock_data, n = i))
    }
    
    stock_lagged_data <- cbind(stocks_weekly_close$Date, stock_lagged_data)
    colnames(stock_lagged_data) <- c("date", symbol, paste0("week",lags))
  
    stock_lagged_data <- arrange(stock_lagged_data, desc(date))
    stock_lagged_data <- cbind(stock_lagged_data, stock_data_list[[symbol]]$stock_weekly_Return[2])
    stock_lagged_data <- na.omit(stock_lagged_data)

  })
  
  
  
  names(weekly_close_lag) <- symbols
  
  # Generate lagged csv data for each stock in `symbols`
  walk2(weekly_close_lag, names(weekly_close_lag), ~write.csv(.x, paste0(metric, ".", "lagged" ,.y, ".csv"), row.names = FALSE))
  
  # Gets lists in enviroment 
  # assign("Weekly_Return", stocks_weekly_return, envir = .GlobalEnv)
  # assign("Weekly_Close", stocks_weekly_close, envir = .GlobalEnv)
  # assign("Weekly_Lag", weekly_close_lag, envir = .GlobalEnv)

  

}


# This function can be called with specific symbols, dates, metric, and lags
get_stocks_weekly_close_and_lag(c("AAPL", "MSFT", "TSLA", "GOOGL"), 
                                "2019-12-01", today, "Adjusted", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))


this is a new line 


