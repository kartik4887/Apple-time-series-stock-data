packages = c('quantmod','car','forecast','tseries','FinTS', 'rugarch','utf8','ggpl
ot2')
install.packages(packages, dependencies = TRUE) 
lapply(packages, require, character.only = TRUE) 

# loading the dataset of Apple inc. from Yahoo finance

getSymbols(Symbols = 'AAPL', 
           src = 'yahoo', 
           from = as.Date('2020-01-01'), 
           to = as.Date('2023-12-31'),
           periodicity = 'daily')

apple_price = na.omit(AAPL$AAPL.Adjusted) # Adjusted Closing Price
class(apple_price) # xts (Time-Series) Object

plot(apple_price)

# Simple Moving Average 
aapl_ma4 = ma(apple_price, order = 4) 

plot(apple_price, lwd = 2)
lines(aapl_ma4, col = 'blue', lwd = 4)

# Simple Moving Average : Random Walk (with Drift) Forecast
aapl_ma8 = rwf(apple_price, h = 50, drift = TRUE) 
accuracy(aapl_ma8)

plot(aapl_ma8)

aapl_es = ses(apple_price, h = 50, alpha = 0.6)
accuracy(aapl_es)

plot(aapl_es)

tsr = decompose(apple_price) # tsr : Trend | Seasonality | Randomness
plot(tsr)

# *******************************************************************

adf_test_aapl = adf.test(apple_price); adf_test_aapl

aapl_ret = na.omit(diff(log(apple_price)))
adf_test_aapl_ret = adf.test(aapl_ret); adf_test_aapl_ret

plot(aapl_ret)
# Ljung-Box Test for Autocorrelation - Stock Data (H0: No Autocorrelation)
# ***********************************************
lb_test_aapl_ret = Box.test(aapl_ret); lb_test_aapl_ret 

acf(apple_price) # ACF of Series
pacf(apple_price) # PACF of Series

acf(aapl_ret) # ACF of returns
pacf(aapl_ret) # PACF of returns

# Auto ARIMA
arma_pq_aapl_ret = auto.arima(aapl_ret); arma_pq_aapl_ret
arma_pq_aapl = auto.arima(apple_price); arma_pq_aapl

# Ljung-Box Test for Autocorrelation - Model Residuals(H0: No Autocorrelation)
# ***************************************************
lb_test_arma_pq_aapl_ret = Box.test(arma_pq_aapl_ret$residuals); lb_test_arma_pq_aapl_ret


# ARIMA (1, 0, 0) or AR(1)
ar1 = arima(aapl_ret, order = c(1, 0, 0)); ar1

# ARIMA (2, 0, 0) or AR(2)
ar2 = arima(aapl_ret, order = c(2, 0, 0)); ar2

# ARIMA (0, 0 , 1) or MA(1)
ma1 = arima(aapl_ret, order = c(0, 0, 1)); ma1

# ARIMA (0, 0, 2) or MA(2)
ma2 = arima(aapl_ret, order = c(0, 0, 2)); ma2

# ARIMA (0, 0, 3) or MA(3)
ma3 = arima(aapl_ret, order = c(0, 0, 3)); ma3

# ARIMA (0, 0, 4) or MA(4)
ma4 = arima(aapl_ret, order = c(0, 0, 4)); ma4

# ARIMA (1, 0, 1) or ARMA(1, 1)
arma11 = arima(aapl_ret, order = c(1, 0, 1)); arma11

# ARIMA (1, 0, 2) or ARMA(1, 2)
arma12 = arima(aapl_ret, order = c(1, 0, 2)); arma12

# ARIMA (1, 0, 3) or ARMA(1, 3)
arma13 = arima(aapl_ret, order = c(1, 0, 3)); arma13

# 3.1.2. Forecasting with ARIMA Models
# ************************************

aapl_ret_fpq = forecast(arma_pq_aapl_ret, h = 40)
plot(aapl_ret_fpq)

aapl_fpq = forecast(arma_pq_aapl, h = 40)
plot(aapl_fpq)

 
plot(aapl_ret)
# Test for Volatility Clustering or Heteroskedasticity: Box Test

aapl_ret_sq = arma_pq_aapl_ret$residuals^2 # Return Variance (Since Mean Returns is approx. 0)
plot(aapl_ret_sq)

aapl_ret_sq_box_test = Box.test(aapl_ret_sq, lag = 10) # H0: Return Variance Series is Not Serially Correlated
aapl_ret_sq_box_test # Inference : Return Variance Series is Heteroskedastic (Has Volatility Clustering)

# Test for Volatility Clustering or Heteroskedasticity: ARCH Test
aapl_ret_arch_test = ArchTest(arma_pq_aapl_ret$residuals^2, lags = 10) # H0: No ARCH Effects
aapl_ret_arch_test # Inference : Return Series is Heteroskedastic (Has Volatility Clustering)


# GARCH Model
garch_model1 = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,10), include.mean = TRUE))
aapl_ret_garch1 = ugarchfit(garch_model1, data = aapl_ret); aapl_ret_garch1

garch_model2 = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,5), include.mean = FALSE))
aapl_ret_garch2 = ugarchfit(garch_model2, data = aapl_ret); aapl_ret_garch2

# GARCH Forecast
aapl_ret_garch_forecast1 = ugarchforecast(aapl_ret_garch1, n.ahead = 50); aapl_ret_garch_forecast1
aapl_ret_garch_forecast2 = ugarchforecast(aapl_ret_garch2, n.ahead = 50); aapl_ret_garch_forecast2

plot(aapl_ret_garch_forecast1)
