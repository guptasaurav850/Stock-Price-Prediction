## Random Walk Model##

## time series model, used for forecasting the stock prices.
# walk means steps, how the stock performs each day
# the model says that the stock prices move in a random movement without any
# patterns, trends. the principle says that the current prices are not based on historical prices
# it says that future prices cant be determined by historical prices.


# i) SRWM (simple random walk model)
# Yt = Yt-1 + Et (where Et is white noise residual error random)

# ii) RWM with drift
# Yt= Yt-1 + mew + Et (mew is the drift)

# iii) Geometric Random Walk Model
# Yt= theta * (Yt-1 + mew + Et)

# theta is the proportion




library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)
library(dplyr)
library(zoo)
library(xts)



stock_prices <- getSymbols("ASIANPAINT.NS", from = "2020-01-01", to = "2024-10-01", auto.assign = FALSE)
stock_prices <- Cl(stock_prices)  #Close Price

# calculate simple returns
stock_returns <- Return.calculate(stock_prices, method = "discrete")
stock_returns <- na.omit(stock_returns)  # removes NA values caused by the 1st observation

set.seed(123)  # for reproducability



# calculate mean & sdv of simple returns
mu <- mean(stock_returns) #mean return (drift)
sigma <- sd(stock_returns) # volatility of returns

## generate random walk
n <- 100 #forecast horizon e.g. 100 days
random_walk <- numeric(n) #Inintialize vector for simulated prices

#random_walk <- vector("numeric",100) # alternate method

random_walk[1] <- as.numeric(last(stock_prices))  # start from the last known price data
tail(stock_prices,n=1)
class(stock_prices)
as.numeric(last(stock_prices))


for (i in 2:n) {
  random_walk[i] <- random_walk[i - 1] * (1 + mu + sigma * rnorm(1))  # geometric RWM
}
?seq()

future_dates <- seq(as.Date("2024-10-02"), by= "days", length.out= n )
class(future_dates)

random_walk_df <- data.frame(Date = future_dates, Price = random_walk)
cc <- as.data.frame(future_dates)

# cbind(future_dates,random_walk)
cc$random <- random_walk
cc[1]

ggplot(random_walk_df, aes(x= Date, y= Price)) +
  geom_line(color = "blue") + 
  ggtitle("Random Walk Model Simulation for ASIANPAINT.NS Stock Prices (Using Returns)") +
  xlab("Date") + ylab("Price")




