

setwd("C:\\Users\\alext\\R Stuff\\Student Fund\\csv files")


prices.weekly <- read.csv("Adjusted.weekly.Close.csv")


print(substr(names(prices.weekly)[-1], 1, 4)) # Set stock quantities in the following order as a numeric vector. 



library(PerformanceAnalytics)

VaR.engine.Cust.with.ES <- function(Period, Significance, ...) {
  
  
  # Bind together all the extra arguments into a matrix.
  # This assumes that the extra arguments are vectors of quantities of each stock in the portfolio.
  stock.quant <- rbind(...)
  
  # Ensure that the number of stock quantities matches the number of price data points.
  # If not, print an error message and exit the function.
  if(length(stock.quant) != length(prices.weekly) - 1){
    print("you fucked up, check your stock.quant and make sure you are using the correct length")
    break
  }
  
  # Calculate the market-to-market (MtM) value of the portfolio for each time point.
  # This is done by multiplying the price matrix by the quantity matrix.
  MtM <- as.data.frame(as.matrix(prices.weekly[-1]) %*% as.matrix(stock.quant))
  
  # Add the date column to the MtM data frame and arrange it in ascending date order.
  MtM <- cbind("Date" = prices.weekly$Date, MtM)
  MtM <- arrange(MtM, Date)
  
  # Calculate the return of the portfolio for each time point.
  # This is done by taking the difference in MtM value from one time point to the next and dividing by the MtM value at the previous time point.
  MtM <- MtM %>% mutate(PF.Ret = (MtM[[2]] - lag(MtM[[2]], n = 1)) / lag(MtM[[2]], n = 1))
  
  # Remove any rows with NA values.
  MtM <- na.omit(MtM)
  
  # Rearrange the data frame in descending date order.
  MtM <- arrange(MtM, desc(Date))
  
  # Rename the columns of the data frame.
  colnames(MtM) <- c("Date", "MtM.P", "PF.Ret")
  
  # Convert the date column to Date class.
  MtM$Date <- date(MtM$Date)
  
  
  # Add a new column to the data frame containing the rolling VaR.
  MtM <- cbind(slice(MtM, as.integer(1:(dim(prices.weekly)[1] - Period))), "VaR" = rollapply(MtM$PF.Ret, Period, quantile, Significance))
  
  # Add a new column to compute the Expected Shortfall (ES)
  # MtM <- cbind(MtM, "ES" = rollapply(MtM$PF.Ret, Period, ES, p = Significance))
  # MtM <- cbind(slice(MtM, as.integer(1:(dim(MtM)[1] - Period))) , "ES" = rollapply(MtM$PF.Ret, Period, ES, p = Significance))
  
  MtM <- cbind(slice(MtM, as.integer(1:(dim(MtM)[1] - Period + 1))),
               "ES" = rollapply(MtM$PF.Ret, Period, FUN = function(x) {
                 ES(x, p = Significance, method = "historical")
               }))
  
  # Assign the MtM data frame to a variable in the global environment.
  assign("MtM", MtM, envir = .GlobalEnv)
  
  # Calculate the limits for the primary and secondary y-axes for the plot.
  ylim.prim <- c(min(MtM$MtM.P) - 0.10*min(MtM$MtM.P), max(MtM$MtM.P) + 0.10*max(MtM$MtM.P))
  ylim.sec <- c(min(MtM$VaR) + 0.10*min(MtM$VaR), max(MtM$VaR) + 0.10*max(MtM$VaR))
  
  # Calculate the parameters for transforming between the primary and secondary y-axes.
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b*ylim.sec[1]
  
  # Create a ggplot of the MtM value, VaR and ES over time.
  ggplot(MtM) + 
    geom_line(aes(x = Date, y = MtM.P), colour = "blue") + 
    geom_line(aes(x = Date, y = a + VaR*b), colour = "maroon") + 
    geom_line(aes(x = Date, y = a + ES*b), colour = "green") +  # Add the ES line
    scale_y_continuous("Market-to-Market", 
                       sec.axis = sec_axis(~ (. - a)/b, 
                                           name = "Value at Risk / Expected Shortfall")) + 
    labs(title = "Market-to-Market vs Value at Risk vs Expected Shortfall", 
         subtitle = "Blue = MtM Value, Maroon = VaR, Green = ES", 
         caption = "Source: MtM Data", 
         x = "Date", 
         y = "Market-to-Market", 
         color = "Line Color", 
         shape = "Line Shape", 
         size = "Line Size", 
         linetype = "Line Type", 
         blue = "MtM.P", 
         maroon = "VaR",
         green = "ES")  # Add the ES to the legend
}

print(substr(names(prices.weekly)[-1], 1, 4)) # Set stock quantities in the following order as a numeric vector. 

# Call the function
VaR.engine.Cust.with.ES(10, 0.01, 100, 100, 150, 200)

# 10 is the Period parameter. It specifies the window size for the rolling calculation of Value at Risk (VaR). 
# In this case, VaR is calculated over a rolling window of 10 time points.

# 0.01 is the Significance parameter. It indicates the significance level for the VaR calculation. 
# Here, VaR is calculated at the 1% significance level, meaning it is estimating the loss that will not be exceeded with 99% confidence.

# 2, 2, 4, 4 are extra arguments passed to the function call. 
# In the function definition, they are caught by the ... and bound together into a matrix using rbind(...). 
# This matrix of stock quantities is then used in the calculation of the portfolio's market-to-market value. 
# In this specific call, it seems like the portfolio consists of four stocks, with quantities 100, 100, 150, and 200 respectively.



# use VaR to get an idea of the worst-case scenario on a normal bad day, and ES to know what could happen when things get even worse

