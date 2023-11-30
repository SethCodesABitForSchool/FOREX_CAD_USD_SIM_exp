# FOREX_CAD_USD_SIM_exp
The ploat needs to be updated as it is combined with the differenced values of the TS and not the original values. 
> [!CAUTION]
> _This is an experiment._

> _The forecast presented in this repository is a mere excercise and comes with its own limitation as it is done by an undergard. This is completed as part of coursework for a class and should be considered likewise and limited to the domain knowledge of the author(me)._
> 
> _The forecast and the methodology employed may not reflect industry best practices. So,the results, including both the code and the forecasted values, are subject to inconsistencies._
>   
> _Therefore, the forecast may not be accurate or suitable for real-world decision-making._
>  
> _The author acknowledges the potential for errors and inaccuracies and recommends caution when interpreting or utilizing the information presented._
> 
> _This forecast should not be considered as professional advice. Users are encouraged to seek guidance from qualified professionals for any practical application AKA __Please ask your professors__._
> 
> *Thanks,*  
> *Seth*    

> [!IMPORTANT]
> __Reference:__  
> Data Source[^1]  
> Data Source[^2]


[^1]: Bank of Canada: https://www.bankofcanada.ca/rates/exchange/monthly-exchange-rates/.
[^2]: International Monetary Fund: https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/CAN?year=2023

> [!NOTE]
> __Forecasted Values, h = 2 months:__  
> November, 2023, the exchange rate is 1.643257, meaning that 1 US dollar can be exchanged for approximately __1.643257 Canadian dollars__.  
> December, 2023, the exchange rate is 1.619838, meaning that 1 US dollar can be exchanged for approximately __1.619838 Canadian dollars__.  
  
  
# The Predicted Monthly Average Exchange Rates (1 USD to CAD)


  
![image](https://github.com/SethCodesABitForSchool/FOREX_CAD_USD_SIM_exp/assets/147195203/2000002f-44a8-4b1d-8834-b151ed13454b)
<p align="center">Data Source: International Monetary funds.</p>   

  
# Appendix - Figures 
All the other figures generated for the forecast. 
<h2 align="center">Figure 1: Raw forecast</h2>

![image](https://github.com/SethCodesABitForSchool/FOREX_CAD_USD_SIM_exp/assets/147195203/005ceb67-fdb0-490b-8393-e163fcc74f07)
<p align="right">Data Source: International Monetary funds.</p>
  
  
<h2 align="center">Figure 2: ACF of Differenced TS</h2>

![image](https://github.com/SethCodesABitForSchool/FOREX_CAD_USD_SIM_exp/assets/147195203/3ae2d837-31e3-46a5-82d6-ff19fa19998d)
<p align="right">Data Source: International Monetary funds.</p>
  
  
<h2 align="center">Figure 3: PACF of Differenced TS</h2>

![image](https://github.com/SethCodesABitForSchool/FOREX_CAD_USD_SIM_exp/assets/147195203/abcc8db3-ff3f-4f50-ae86-babdc015cf67)
<p align="right">Data Source: International Monetary funds.</p> 
  
  
<h2 align="center">Figure 4: Differenced TS</h2>

![image](https://github.com/SethCodesABitForSchool/FOREX_CAD_USD_SIM_exp/assets/147195203/d94220ad-bbd6-4b19-8700-9ce68d88553e)
<p align="right">Data Source: International Monetary funds.</p>
  
  
<h2 align="center">Figure 5: Original TS</h2>

![image](https://github.com/SethCodesABitForSchool/FOREX_CAD_USD_SIM_exp/assets/147195203/af128539-57e7-4900-8b76-39007a2a78a7)
<p align="right">Data Source: International Monetary funds.</p>



<h1 align="center">The End</h1>



# Load the dataset
er = read.csv("C:/Users/kumbalas-INS/Downloads/er.csv")
er$date = as.Date(er$date)
View(er)
plot.ts(er$FXMUSDCAD)
# test for stationarity
# Install and load necessary library
install.packages("tseries")
library(tseries)
# Assuming 'ts_data' is your time series data
result_adf <- adf.test(er)
# Install and load the xts package
install.packages("xts")
install.packages("xts")
library(xts)
str(er)
# Create an xts object
ts <- ts(er$FXMUSDCAD, start = c(1, 1), frequency = 1)
# Assuming 'ts_data' is your time series data
result_adf <- adf.test(ts)
print(result_adf)
# Create an xts object
ts <- ts(er$FXMUSDCAD, start = c(1, 1), frequency = 12)
# Assuming 'ts_data' is your time series data
result_adf <- adf.test(ts)
print(result_adf)
# transform the df for stationarity - log trans
tsl <- log(ts)
plot.ts(tsl)
# check for stationarity again
result_adf <- adf.test(tsl)
print(result_adf)
# log-transformed time series decomposition
decomposition <- decompose(tsl)
residual <- decomposition$residual
View(decomposition)
str(decomposition)
plot(decomposition$seasonal)
plot(decomposition$seasonal)
lines(decomposition$x)
plot(decomposition$seasonal) + lines(decomposition$x)
# diff the ts - log
tsl_diff <- diff(tsl)
# Run ADF test on the differenced series
result_diff <- adf.test(tsl_diff)
print(result_diff)
plot.ts(tsl_diff)
summary(tsl_diff)
erd = read.csv("C:/Users/kumbalas-INS/Downloads/erd.csv")
install.packages("openxlsx")
library(openxlsx)
df1 = data.frame(tsl_diff)
cdf = cbind(erd, df1)
View(cdf)
cdf$date = as.Date(cdf$date)
plot.ts(cdf)
# Assuming 'ts_data' is your time series data
decomposition <- decompose(tsl_diff)
# Plot the components
plot(decomposition)
plot(decomposition$seasonal)
# Assuming 'ts_data' is your time series data
ts_seasonal_diff <- diff(tsl_diff, lag = 12)
plot(ts_seasonal_diff)
dec = decompose(ts_seasonal_diff)
plot(dec$seasonal)
result = adf.test(ts_seasonal_diff)
print(result)
View(result_adf)
diff2 = diff(ts_seasonal_diff, lag = 12)
plot(diff2)
result = adf.test(diff2)
print(result)
diff3 = diff(diff2, lag = 12)
plot(diff3)
result = adf.test(diff3)
print(result)
# differncing time series data
ts_seasonal_diff <- diff(tsl)
plot(ts_seasonal_diff)
dec = decompose(ts_seasonal_diff)
View(dec)
diff2 = diff(ts_seasonal_diff)
plot(diff2)
result = adf.test(diff2)
print(result)
dec = decompose(ts_seasonal_diff)
View(dec)
dec = decompose(diff2)
View(dec)
View(dec)
str(dec)
# ACF and PACF plots
acf(diff2)
pacf(diff2)
# ACF and PACF plots
auto = acf(diff2)
pauto = pacf(diff2)
str(auto)
print(auto$acf)
str(pauto)
print(pauto$acf)
# Modelling
library(forecast)
install.packages("forecast")
library(forecast)
# SARIMA model using auto.arima
sarima_model <- auto.arima(diff2, seasonal = TRUE)
summary(sarima_model)
# forecasts for the next 2 months
forecasts <- forecast(sarima_model, h = 2)
# Plot historical data, fitted values, and forecasts
plot(forecasts, main = "SARIMA Forecast")
View(forecasts)
print(forecasts)

library(ggplot2)
# Convert forecasts to a data frame
forecast_df <- data.frame(date = index(forecasts),
Point.Forecast = forecasts$mean,
Lo.80 = forecasts$lower[, "80%"],
Hi.80 = forecasts$upper[, "80%"],
Lo.95 = forecasts$lower[, "95%"],
Hi.95 = forecasts$upper[, "95%"])
# Convert forecasts to a data frame
# Convert forecasts to a data frame
forecast_df <- data.frame(date = as.Date(index(forecasts)),
Point.Forecast = as.numeric(forecasts$mean),
Lo.80 = as.numeric(forecasts$lower[, "80%"]),
Hi.80 = as.numeric(forecasts$upper[, "80%"]),
Lo.95 = as.numeric(forecasts$lower[, "95%"]),
Hi.95 = as.numeric(forecasts$upper[, "95%"]))
ggplot(forecast_df, aes(x = date, y = Point.Forecast)) +
geom_line(color = "blue", size = 1) +
geom_ribbon(aes(ymin = Lo.80, ymax = Hi.80), fill = "lightblue", alpha = 0.5) +
geom_ribbon(aes(ymin = Lo.95, ymax = Hi.95), fill = "lightblue", alpha = 0.2) +
labs(title = "SARIMA Forecast",
x = "Date",
y = "Exchange Rate") +
theme_minimal()
ggplot(forecast_df, aes(x = date, y = Point.Forecast)) +
geom_line(color = "blue", linewidth = 1) +
geom_ribbon(aes(ymin = Lo.80, ymax = Hi.80), fill = "lightblue", alpha = 0.5) +
geom_ribbon(aes(ymin = Lo.95, ymax = Hi.95), fill = "lightblue", alpha = 0.2) +
labs(title = "SARIMA Forecast",
x = "Date",
y = "Exchange Rate") +
theme_minimal()
# Assuming 'forecasts' is your forecast object
forecast_values <- as.numeric(fitted(forecasts))
# Display the forecasted values
print(forecast_values)
# Assuming 'forecasts' is your forecast object
forecast_df <- data.frame(date = index(forecasts), forecast = forecast_values)
# Set the start date to March 1, 2017
start_date <- as.Date("2017-03-01")
forecast_df$date <- seq(start_date, by = "days", length.out = nrow(forecast_df))
View(forecast_df)
forecast_df$date <- seq(start_date, by = "month", length.out = nrow(forecast_df))
# Display the updated data frame
print(forecast_df)
forecast2 = as.data.frame(forecasts)
View(forecast2)
# Assuming 'forecast_df' is your data frame with date and forecast columns
library(ggplot2)
# Convert the date column to Date type
forecast_df$date <- as.Date(forecast_df$date)
View(forecast_df)
p <- ggplot(forecast_df, aes(x = date, y = forecast)) +
geom_line(color = "blue", size = 1) +  # Line for point forecasts
geom_ribbon(aes(ymin = Lo.80, ymax = Hi.80), fill = "lightblue", alpha = 0.5) +  # 80% Confidence Interval
geom_ribbon(aes(ymin = Lo.95, ymax = Hi.95), fill = "lightblue", alpha = 0.2) +  # 95% Confidence Interval
labs(title = "Fitted Values with Confidence Intervals",
x = "Date",
y = "Fitted Values") +
theme_minimal()
# Display the plot
print(p)
rlang::last_trace()
rlang::last_trace()
rlang::last_trace(drop = FALSE)
fd3 = cbind(forecast_df, forecast2)
View(fd3)
# Create a ggplot object
p <- ggplot(fd3, aes(x = date, y = forecast)) +
geom_line(color = "blue", size = 1) +  # Line for point forecasts
geom_ribbon(aes(ymin = Lo.80, ymax = Hi.80), fill = "lightblue", alpha = 0.5) +  # 80% Confidence Interval
geom_ribbon(aes(ymin = Lo.95, ymax = Hi.95), fill = "lightblue", alpha = 0.2) +  # 95% Confidence Interval
labs(title = "Fitted Values with Confidence Intervals",
x = "Date",
y = "Fitted Values") +
theme_minimal()
# Display the plot
print(p)
rlang::last_trace()
colnames(fd3)
p <- ggplot(fd3, aes(x = date, y = forecast)) +
geom_line(color = "blue", size = 1) +
geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`), fill = "lightblue", alpha = 0.5) +
geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), fill = "lightblue", alpha = 0.2) +
labs(title = "Fitted Values with Confidence Intervals",
x = "Date",
y = "Fitted Values") +
theme_minimal()
# Display the plot
print(p)
p <- ggplot(fd3, aes(x = date, y = forecast)) +
geom_line(color = "blue", size = 1) +
geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`), fill = "lightblue", alpha = 0.5) +
geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), fill = "lightblue", alpha = 0.2) +
labs(title = "Fitted Values with Confidence Intervals",
x = "Date",
y = "Fitted Values") +
theme_minimal()
# Display the plot
print(p)
# Assuming your data frame is named df
fd3$`Lo 80`[1:78] <- NA
fd3$`Hi 80`[1:78] <- NA
fd3$`Lo 95`[1:78] <- NA
fd3$`Hi 95`[1:78] <- NA
p <- ggplot(fd3, aes(x = date, y = forecast)) +
geom_line(color = "blue", size = 1) +
geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`), fill = "lightblue", alpha = 0.5) +
geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), fill = "lightblue", alpha = 0.2) +
labs(title = "Fitted Values with Confidence Intervals",
x = "Date",
y = "Fitted Values") +
theme_minimal()
# Display the plot
print(p)
# Assuming your data frame is named df
df <- fd3[21:nrow(fd3), ]
View(df)
p <- ggplot(df, aes(x = date, y = forecast)) +
geom_line(color = "blue", size = 1) +
geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`), fill = "lightblue", alpha = 0.5) +
geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), fill = "lightblue", alpha = 0.2) +
labs(title = "Fitted Values with Confidence Intervals",
x = "Date",
y = "Fitted Values") +
theme_minimal()
# Display the plot
print(p)
df <- fd3[58:nrow(fd3), ]
p <- ggplot(df, aes(x = date, y = forecast)) +
geom_line(color = "blue", size = 1) +
geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`), fill = "lightblue", alpha = 0.5) +
geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), fill = "lightblue", alpha = 0.2) +
labs(title = "Fitted Values with Confidence Intervals",
x = "Date",
y = "Fitted Values") +
theme_minimal()
# Display the plot
print(p)
# Create a new column with the correct dates
fd3$correct_date <- as.Date(c("2023-11-01", "2023-12-01"))
View(fd3)
start_date <- as.Date("2022-02-01")
View(forecast_df)
fd3$correct_date <- seq(start_date, by = "days", length.out = nrow(forecast_df))
View(fd3)
View(forecast_df)
fd3$correct_date <- seq(start_date, by = "months", length.out = nrow(fd3))
start_date <- as.Date("2017-05-01")
fd3$correct_date <- seq(start_date, by = "months", length.out = nrow(fd3))
df <- fd3[58:nrow(fd3), ]
p <- ggplot(df, aes(x = date, y = forecast)) +
geom_line(color = "blue", size = 1) +
geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`), fill = "lightblue", alpha = 0.5) +
geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), fill = "lightblue", alpha = 0.2) +
labs(title = "Fitted Values with Confidence Intervals",
x = "Date",
y = "Fitted Values") +
theme_minimal()
# Display the plot
print(p)
View(df)
s = subset(df, select = -c("date")
View(df)
View(df)
df = subset(df, select = -c("date")
View(df)
colnames(df)
df1 = subset(df, select = c("forecast", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95", "correct_date")
View(df)
df1 = subset(df, select = c("forecast", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95", "correct_date")
df1 = subset(df, select = c("forecast", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95", "correct_date")
df1 = subset(df, select = c("forecast", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95", "correct_date")
df1 = subset(df, select = c("forecast", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95", "correct_date")
df1 = subset(df, select = c("forecast", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95", "correct_date"))
View(df1)
df1$correct_date = df1$date
View(df1)
df1 = subset(df, select = c("forecast", "Point Forecast", "Lo 80", "Hi 80", "Lo 95", "Hi 95", "correct_date"))
p <- ggplot(df1, aes(x = date, y = forecast)) +
geom_line(color = "blue", size = 1) +
geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`), fill = "lightblue", alpha = 0.5) +
geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), fill = "lightblue", alpha = 0.2) +
labs(title = "Fitted Values with Confidence Intervals",
x = "Date",
y = "Fitted Values") +
theme_minimal()
# Display the plot
print(p)
p <- ggplot(df1, aes(x = correct_date, y = forecast)) +
geom_line(color = "blue", size = 1) +
geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`), fill = "lightblue", alpha = 0.5) +
geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), fill = "lightblue", alpha = 0.2) +
labs(title = "Fitted Values with Confidence Intervals",
x = "Date",
y = "Fitted Values") +
theme_minimal()
# Display the plot
print(p)
# Reverse the second-order differencing
fd3$forecast2 = cumsum(cumsum(fd3$forecast))
# Reverse the first-order differencing
fd3$forecast1 = fd3$forecast2 + tsl_diff
# Assuming your time series is named 'your_ts'
fdd <- tsl_diff[1:(length(tsl_diff)-1)]
fd3$forecast1 = fd3$forecast2 + fdd
# Reverse the log transformation
fd3$forecast_original = exp(fd3$forecast1)
View(fd3)
View(df1)
extra = subset(fd3, select = c("forecast_original"))
View(extra)
df23 <- extra[58:nrow(extra), ]
df23 = as.data.frame(df23)
View(df)
df = cbind(df, df23)
View(df)
# Assuming your data frame is named 'your_df' and you want to remove a column named 'column_to_remove'
df <- subset(df, select = -forecast)
View(df)
df <- subset(df, select = -date)
p <- ggplot(df, aes(x = correct_date, y = df23)) +
geom_line(color = "blue", size = 1) +
geom_ribbon(aes(ymin = `Lo 80`, ymax = `Hi 80`), fill = "lightblue", alpha = 0.5) +
geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), fill = "lightblue", alpha = 0.2) +
labs(title = "Fitted Values with Confidence Intervals",
x = "Date",
y = "Fitted Values") +
theme_minimal()
# Display the plot
print(p)
View(extra)
savehistory("~/sfd.Rhistory")









