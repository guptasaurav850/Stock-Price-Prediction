## AutoRegressive Integrated Moving Average##

## ARIMA Model = AR + I + MA (Auto regressive + Moving Average + Integration )

library(quantmod)
library(forecast)
getSymbols("ASIANPAINT.NS", from = "2020-01-01", to = "2024-10-01", auto.assign = FALSE)
stock_prices <- Cl(ASIANPAINT.NS)  #Closing Prices

# Plot the stock prices to see the trend
plot(stock_prices, main = "ASIANPAINT.NS Stock Prices", ylab = "Price", xlab = "Date")

# Fit an ARIMA model
arima_model_prices <- auto.arima(stock_prices)
summary(arima_model_prices)

# Forecast for stock prices
forecast_prices<- forecast(arima_model_prices,h=100)  # forecast for nexxt 100 days
plot(forecast_prices, main = "ARIMA Forecast for ASIANPAINT.NS Stock Prices")

