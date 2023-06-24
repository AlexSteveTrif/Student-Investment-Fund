prices.weekly <- read.csv("Adjusted.weekly.Close.csv")


print(substr(names(prices.weekly)[-1], 1, 4))



VaR.engine.Cust <- function(Period, Significance, ...) {
  stock.quant <- rbind(...)
  if(length(stock.quant)!=length(prices.weekly)-1){
    print("you fucked up, check your stock.quant and make sure you are using the correct length")
    break
  }
  MtM <- as.data.frame(as.matrix(prices.weekly[-1]) %*% as.matrix(stock.quant))
  MtM <- cbind("Date" = prices.weekly$Date, MtM)
  MtM <- arrange(MtM, Date)
  MtM <- MtM %>% mutate(PF.Ret = (MtM[[2]] - lag(MtM[[2]], n = 1)) / lag(MtM[[2]], n = 1))
  MtM <- na.omit(MtM)
  MtM <- arrange(MtM, desc(Date))
  colnames(MtM) <- c("Date", "MtM.P", "PF.Ret")
  MtM$Date <- date(MtM$Date)
  MtM <- cbind(slice(MtM, as.integer(1:(dim(prices.weekly)[1] - Period))),
               "VaR" = rollapply(MtM$PF.Ret, Period, quantile, Significance))
  assign("MtM", MtM, envir = .GlobalEnv)
  ylim.prim <- c(min(MtM$MtM.P) - 0.10*min(MtM$MtM.P), max(MtM$MtM.P) + 0.10*max(MtM$MtM.P))
  ylim.sec <- c(min(MtM$VaR) + 0.10*min(MtM$VaR), max(MtM$VaR) + 0.10*max(MtM$VaR))
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- ylim.prim[1] - b*ylim.sec[1]
  ggplot(MtM) + 
    geom_line(aes(x = Date, y = MtM.P), colour = "blue") + 
    geom_line(aes(x = Date, y = a + VaR*b), colour = "maroon") + 
    scale_y_continuous("Market-to-Market", 
                       sec.axis = sec_axis(~ (. - a)/b, 
                                           name = "Value at Risk")) + 
    labs(title = "Market-to-Market vs Value at Risk", 
         subtitle = "Blue = MtM Value, Maroon = VaR", 
         caption = "Source: MtM Data", 
         x = "Date", 
         y = "Market-to-Market", 
         color = "Line Color", 
         shape = "Line Shape", 
         size = "Line Size", 
         linetype = "Line Type", 
         blue = "MtM.P", 
         maroon = "VaR")
}


VaR.engine.Cust(10, 0.01, 2, 2, 4, 4)


