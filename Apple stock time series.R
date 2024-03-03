install.packages(packages, dependencies = TRUE)

packages = c('quantmod')
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

packages = c('FinTS', 'rugarch') 
install.packages(packages, dependencies = TRUE) 

lapply(packages, require, character.only = TRUE) 

# Required Packages
packages = c('forecast') 

# Install all Packages with Dependencies
install.packages(packages, dependencies = TRUE) 

# Load all Packages
lapply(packages, require, character.only = TRUE) 

# Required Packages
packages = c('tseries', 'forecast') 

# Install all Packages with Dependencies
install.packages(packages, dependencies = TRUE) 

# Load all Packages
lapply(packages, require, character.only = TRUE) 

plot(apple_price)

# Simple Moving Average 
aapl_ma4 = ma(apple_price, order = 4) 

plot(apple_price, lwd = 2)
lines(aapl_ma4, col = 'blue', lwd = 4)

# Simple Moving Average : Random Walk (with Drift) Forecast
aapl_ma8 = rwf(apple_price, h = 8, drift = TRUE) 
accuracy(aapl_ma8)

plot(aapl_ma8)

aapl_es = ses(apple_price, h = 4, alpha = 0.6)
accuracy(aapl_es)

plot(aapl_es)

tsr = decompose(apple_price) # tsr : Trend | Seasonality | Randomness
plot(tsr)

# Augmented Dickey-Fuller (ADF) Test for Stationarity with J & J Data
# *******************************************************************

adf_test_aapl = adf.test(apple_price); adf_test_aapl 

aapl_ds = diff(apple_price); plot(aapl_ds) 

adf_test_aapl_ds = adf.test(aapl_ds); adf_test_aapl_ds

# Augmented Dickey-Fuller (ADF) Test for Stationarity with Apple Data

# *******************************************************************

adf_test_aapl = adf.test(apple_price); adf_test_aapl 

aapl_ds = diff(apple_price); plot(aapl_ds) 

adf_test_aapl_ds = adf.test(aapl_ds); adf_test_aapl_ds 


lb_test_aapl_ds = Box.test(aapl_ds); lb_test_aapl_ds 

# *****************************************************************************

acf(apple_price) # ACF of Series
pacf(apple_price) # PACF of Series

acf(aapl_ds) # ACF of  Difference (Stationary) Series
pacf(aapl_ds) # PACF of  Difference (Stationary) Series

# ARIMA (1, 0, 0) or AR(1)
ar1 = arima(aapl_ds, order = c(1, 0, 0)); ar1

# ARIMA (2, 0, 0) or AR(2)
ar2 = arima(aapl_ds, order = c(2, 0, 0)); ar2

# ARIMA (0, 0 , 1) or MA(1)
ma1 = arima(aapl_ds, order = c(0, 0, 1)); ma1

# ARIMA (0, 0, 2) or MA(2)
ma2 = arima(aapl_ds, order = c(0, 0, 2)); ma2

# ARIMA (0, 0, 3) or MA(3)
ma3 = arima(aapl_ds, order = c(0, 0, 3)); ma3

# ARIMA (0, 0, 4) or MA(4)
ma4 = arima(aapl_ds, order = c(0, 0, 4)); ma4

# ARIMA (1, 0, 1) or ARMA(1, 1)
arma11 = arima(aapl_ds, order = c(1, 0, 1)); arma11

# ARIMA (1, 0, 2) or ARMA(1, 2)
arma12 = arima(aapl_ds, order = c(1, 0, 2)); arma12

# ARIMA (1, 0, 3) or ARMA(1, 3)
arma13 = arima(aapl_ds, order = c(1, 0, 3)); arma13

# Auto ARIMA
arma_pq_aapl_ds = auto.arima(aapl_ds); arma_pq_aapl_ds
arma_pq_aapl = auto.arima(apple_price); arma_pq_aapl

# ****************************************************
lb_test_arma_pq_aapl_ds = Box.test(arma_pq_aapl_ds$residuals); lb_test_arma_pq_aapl_ds


# 3.1.2. Forecasting with ARIMA Models
# ************************************
aapl_ds_f11 = predict(arma11, n.ahead = 40)
plot(aapl_ds_f11)
lines(aapl_ds_f11$pred, col = 'blue')
lines(aapl_ds_f11$pred + 2 * aapl_ds_f11$se, col = 'red')
lines(aapl_ds_f11$pred - 2 * aapl_ds_f11$se, col = 'red')

aapl_ds_fpq = forecast(arma_pq_aapl_ds, h = 40)
plot(aapl_ds_fpq)

aapl_fpq = forecast(arma_pq_aapl, h = 40)
plot(aapl_fpq)

aapl_ret = na.omit(diff(log(apple_price))) 
plot(aapl_ret)

aapl_ret_sq = aapl_ret^2 # Return Variance (Since Mean Returns is approx. 0)
plot(aapl_ret_sq)
aapl_ret_sq_box_test = Box.test(aapl_ret_sq, lag = 10) # H0: Return Variance Series is Not Serially Correlated
aapl_ret_sq_box_test # Inference : Return Variance Series is Heteroskedastic (Has Volatility Clustering)


# Test for Volatility Clustering or Heteroskedasticity: ARCH Test
aapl_ret_arch_test = ArchTest(aapl_ret, lags = 20) # H0: No ARCH Effects
aapl_ret_arch_test # Inference : Return Series is Heteroskedastic (Has Volatility Clustering)



# GARCH Model
garch_model1 = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0), include.mean = TRUE))
aapl_ret_garch1 = ugarchfit(garch_model1, data = aapl_ret); aapl_ret_garch1

garch_model2 = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,1), include.mean = FALSE))
aapl_ret_garch2 = ugarchfit(garch_model2, data = aapl_ret); aapl_ret_garch2

# GARCH Forecast
aapl_ret_garch_forecast1 = ugarchforecast(aapl_ret_garch1, n.ahead = 50); aapl_ret_garch_forecast1
aapl_ret_garch_forecast2 = ugarchforecast(aapl_ret_garch2, n.ahead = 50); aapl_ret_garch_forecast2
