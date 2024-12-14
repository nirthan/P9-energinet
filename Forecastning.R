library(readxl)
library(readr)
library(dplyr)
library(tseries)
library(ggplot2)
library(vars)
library(scales)
library(Metrics)
library(lmtest) 

#Solar Production DK1
DK1_Production_2023 <- read_csv("/DK1_grouped_2023.csv")
DK1_Production_2024 <- read_csv("/DK1_grouped_2024.csv")
DK1_Production <- bind_rows(DK1_Production_2023, DK1_Production_2024)

#Solar Production DK2
DK2_Production_2023 <- read_csv("/DK2_grouped_2023.csv")
DK2_Production_2024 <- read_csv("/DK2_grouped_2024.csv")
DK2_Production <- bind_rows(DK2_Production_2023, DK2_Production_2024)

# Consumption DK1
DK1_Consumption_2023 <- read_csv("/DK1_grouped_forbrug_2023.csv")
DK1_Consumption_2024 <- read_csv("/2024_DK1_grouped_forbrug.csv")
DK1_Consumption <- bind_rows(DK1_Consumption_2023, DK1_Consumption_2024)

DK1_Consumption$Total_Reading[7226] <- 527112.1
DK1_Consumption$Total_Reading[15961] <- 506203.5

# Consumption DK2
DK2_Consumption_2023 <- read_csv("/DK2_grouped_forbrug_2023.csv")
DK2_Consumption_2024 <- read_csv("/2024_DK2_grouped_forbrug.csv")
DK2_Consumption <- bind_rows(DK2_Consumption_2023, DK2_Consumption_2024)

DK2_Consumption$Total_Reading[7226] <- 339772.7
DK2_Consumption$Total_Reading[15976] <- 563163.8
DK2_Consumption$Total_Reading[15961] <- 336136.7

# Bright sunshine
Bright_sunshine <- read_csv("/sun_data_all.csv")
Bright_sunshine <- head(Bright_sunshine, -1)

##### Transform the variables #####
log_DK1_Production <- log(DK1_Production$Total_Reading)
log_DK2_Production <- log(DK2_Production$Total_Reading)
log_DK1_Consumption <- log(DK1_Consumption$Total_Reading)
log_DK2_Consumption <- log(DK2_Consumption$Total_Reading)
# Replace 0 values with a small value, 1e-5
Bright_sunshine$value[Bright_sunshine$value == 0] <- 1e-5
log_Bright_sunshine <- log(Bright_sunshine$value)


std_DK1_Production <- scale(log_DK1_Production)
std_DK2_Production <- scale(log_DK2_Production)
std_DK1_Consumption <- scale(log_DK1_Consumption)
std_DK2_Consumption <- scale(log_DK2_Consumption)
std_Bright_sunshine <- scale(log_Bright_sunshine)


t <- 1:length(std_DK1_Production)
phase1 <- 0  # For yearly periodicity
phase2 <- 0  # For weekly periodicity
phase3 <- 0  # For daily periodicity
phase4 <- 0  # For half-daily periodicity

model_DK1_Production <- lm(std_DK1_Production ~ 
              t + 
              sin(t * 2 * pi / 8760 + phase1 * 2 * pi / 8760) + 
              cos(t * 2 * pi / 8760 + phase1 * 2 * pi / 8760) +
              sin(t * 2 * pi / 168 + phase2) + 
              cos(t * 2 * pi / 168 + phase2) +
              sin(t * 2 * pi / 24 + phase3 * 2 * pi / 24) + 
              cos(t * 2 * pi / 24 + phase3 * 2 * pi / 24) +
              sin(t * 2 * pi / 12 + phase4) + 
              cos(t * 2 * pi / 12 + phase4))

model_DK2_Production <- lm(std_DK2_Production ~ 
                             t + 
                             sin(t * 2 * pi / 8760 + phase1 * 2 * pi / 8760) + 
                             cos(t * 2 * pi / 8760 + phase1 * 2 * pi / 8760) +
                             sin(t * 2 * pi / 168 + phase2) + 
                             cos(t * 2 * pi / 168 + phase2) +
                             sin(t * 2 * pi / 24 + phase3 * 2 * pi / 24) + 
                             cos(t * 2 * pi / 24 + phase3 * 2 * pi / 24) +
                             sin(t * 2 * pi / 12 + phase4) + 
                             cos(t * 2 * pi / 12 + phase4))

model_DK1_Consumption <- lm(std_DK1_Consumption ~ 
                              t + 
                              sin(t * 2 * pi / 8760 + phase1 * 2 * pi / 8760) + 
                              cos(t * 2 * pi / 8760 + phase1 * 2 * pi / 8760) +
                              sin(t * 2 * pi / 168 + phase2) + 
                              cos(t * 2 * pi / 168 + phase2) +
                              sin(t * 2 * pi / 24 + phase3 * 2 * pi / 24) + 
                              cos(t * 2 * pi / 24 + phase3 * 2 * pi / 24) +
                              sin(t * 2 * pi / 12 + phase4) + 
                              cos(t * 2 * pi / 12 + phase4))

model_DK2_Consumption <- lm(std_DK2_Consumption ~ 
                              t + 
                              sin(t * 2 * pi / 8760 + phase1 * 2 * pi / 8760) + 
                              cos(t * 2 * pi / 8760 + phase1 * 2 * pi / 8760) +
                              sin(t * 2 * pi / 168 + phase2) + 
                              cos(t * 2 * pi / 168 + phase2) +
                              sin(t * 2 * pi / 24 + phase3 * 2 * pi / 24) + 
                              cos(t * 2 * pi / 24 + phase3 * 2 * pi / 24) +
                              sin(t * 2 * pi / 12 + phase4) + 
                              cos(t * 2 * pi / 12 + phase4))

model_Bright_sunshine <- lm(std_Bright_sunshine ~ 
                              t + 
                              sin(t * 2 * pi / 8760 + phase1 * 2 * pi / 8760) + 
                              cos(t * 2 * pi / 8760 + phase1 * 2 * pi / 8760) +
                              sin(t * 2 * pi / 168 + phase2) + 
                              cos(t * 2 * pi / 168 + phase2) +
                              sin(t * 2 * pi / 24 + phase3 * 2 * pi / 24) + 
                              cos(t * 2 * pi / 24 + phase3 * 2 * pi / 24) +
                              sin(t * 2 * pi / 12 + phase4) + 
                              cos(t * 2 * pi / 12 + phase4))

# Extract residuals to obtain data without seasonal patterns
trigo_DK1_Production <- resid(model_DK1_Production)
trigo_DK2_Production <- resid(model_DK2_Production)
trigo_DK1_Consumption <- resid(model_DK1_Consumption)
trigo_DK2_Consumption <- resid(model_DK2_Consumption)
trigo_Bright_sunshine <- resid(model_Bright_sunshine)



plot(t, trigo_DK1_Production, type="l", col="blue", main="Residuals", ylab="Residuals")
plot(t, trigo_DK2_Production, type="l", col="blue", main="Residuals", ylab="Residuals")
plot(t, trigo_DK1_Consumption, type="l", col="blue", main="Residuals", ylab="Residuals")
plot(t, trigo_DK2_Consumption, type="l", col="blue", main="Residuals", ylab="Residuals")
plot(t, trigo_Bright_sunshine, type="l", col="blue", main="Residuals", ylab="Residuals")


# Take the first difference
diff_log_DK1_Production <- diff(trigo_DK1_Production)
diff_log_DK2_Production <- diff(trigo_DK2_Production)
diff_log_DK1_Consumption <- diff(trigo_DK1_Consumption)
diff_log_DK2_Consumption <- diff(trigo_DK2_Consumption)
diff_log_Bright_sunshine <- diff(trigo_Bright_sunshine)

# Augmented Dickey-Fuller test - p-value = 0.01 => stationary
adf_test_DK1_Production <- adf.test(diff_log_DK1_Production)
adf_test_DK2_Production <- adf.test(diff_log_DK2_Production)
adf_test_DK1_Consumption <- adf.test(diff_log_DK1_Consumption)
adf_test_DK2_Consumption <- adf.test(diff_log_DK2_Consumption)
adf_test_Bright_sunshine <- adf.test(diff_log_Bright_sunshine)


# KPSS test - p-value = 0.1 => stationary
kpss_test_DK1_Production <- kpss.test(diff_log_DK1_Production)
kpss_test_DK2_Production <- kpss.test(diff_log_DK2_Production)
kpss_test_DK1_Consumption <- kpss.test(diff_log_DK1_Consumption)
kpss_test_DK2_Consumption <- kpss.test(diff_log_DK2_Consumption)
kpss_test_Bright_sunshine <- kpss.test(diff_log_Bright_sunshine)

# Plot for the differenced data
ggplot(data.frame(DateTime = DK1_Production$DateTime[-1], Diff_Total_Reading = diff_log_DK1_Production), aes(x = DateTime, y = Diff_Total_Reading)) +
  geom_line(color = "blue") + 
  labs(x = "Date and Time", y = "Differenced Total Reading", title = "Differenced Solar Production for DK1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

ggplot(data.frame(DateTime = DK2_Production$DateTime[-1], Diff_Total_Reading = diff_log_DK2_Production), aes(x = DateTime, y = Diff_Total_Reading)) +
  geom_line(color = "blue") + 
  labs(x = "Date and Time", y = "Differenced Total Reading", title = "Differenced Solar Production for DK2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(data.frame(DateTime = DK1_Consumption$DateTime[-1], Diff_Total_Reading = diff_log_DK1_Consumption), aes(x = DateTime, y = Diff_Total_Reading)) +
  geom_line(color = "blue") +  
  labs(x = "Date and Time", y = "Differenced Total Reading", title = "Differenced Consumption for DK1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

ggplot(data.frame(DateTime = DK2_Consumption$DateTime[-1], Diff_Total_Reading = diff_log_DK2_Consumption), aes(x = DateTime, y = Diff_Total_Reading)) +
  geom_line(color = "blue") +  
  labs(x = "Date and Time", y = "Differenced Total Reading", title = "Differenced Consumption for DK2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(data = Bright_sunshine[2:nrow(Bright_sunshine), ], aes(x = to[2:nrow(Bright_sunshine)], y = diff_log_Bright_sunshine)) +
  geom_line(color = "blue") +                
  labs(title = "Time Series Plot of Differenced Log of Bright Sunshine",
       x = "Date and Time",
       y = "Differenced Log Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
 
# Transformed data
data <- cbind(diff_log_DK1_Production,
              diff_log_DK1_Consumption,
              diff_log_Bright_sunshine)

# Remove september and october months 
data_reduced <- data[1:(nrow(data) - 1463), ]

colnames(data_reduced) <- cbind("DK1_Production", "DK1_Consumption", "Bright_Sunshine")

data_ts <- ts(data_reduced, frequency = 8760)  
plot.ts(data_ts)

# Define the lags to check
lags_to_check <- c(10, 24, 48, 168)

aic_values <- numeric(length(lags_to_check))

for (i in 1:length(lags_to_check)) {
  lag <- lags_to_check[i]
  var_model <- VAR(data, p = lag)
  aic_values[i] <- AIC(var_model)
}

lag_results <- data.frame(
  Lag = lags_to_check,
  AIC = aic_values
)

# VAR (168) model 
VAR_model_168 <- VAR(data_reduced, p = 168)  
summary(VAR_model_168) 

residuals_for_168 <- residuals(VAR_model_168)

plot(residuals_for_168[, 1], type = "l", main = "Residuals of the Production Series", ylab = "Residuals", xlab = "Time")

plot(residuals_for_168[, 2], type = "l", main = "Residuals of the Consumption Series", ylab = "Residuals", xlab = "Time")

plot(residuals_for_168[, 3], type = "l", main = "Residuals of the Brightsunshine Series", ylab = "Residuals", xlab = "Time")

# ACF plot for the residuals of the first series
acf(residuals_for_168[, 1], lag.max = 200, main = "ACF af Residualer", ylim = c(-0.1, 0.1))
# ACF plot for the residuals of the secound series
acf(residuals_for_168[, 2], lag.max = 200, main = "ACF af Residualer", ylim = c(-0.1, 0.1))
# ACF plot for the residuals of the third series
acf(residuals_for_168[, 3], lag.max = 200, main = "ACF af Residualer", ylim = c(-0.1, 0.1))

# VAR(168) model forecast
var_forecast <- predict(VAR_model_168, n.ahead = 1463)

# Extract forecasted values 
forecast_values_production <- var_forecast$fcst$DK1_Production[, "fcst"]
forecast_values_consumption <- var_forecast$fcst$DK1_Consumption[, "fcst"]
forecast_values_bright <- var_forecast$fcst$Bright_Sunshine[, "fcst"]

actual_data_production <- data[14614:(14614 + 1462), 1]
actual_data_consumption <- data[14614:(14614 + 1462), 2]
actual_data_bright <- data[14614:(14614 + 1462), 3]

forecast_time <- 14614:(14614 + 1462)

par(mfrow = c(3, 1))
# Plotting Actual vs VAR Forecast  
plot(forecast_time, actual_data_production, type = "l", col = "blue", 
     xlab = "Time", ylab = "Log Diff Production", 
     main = "Actual vs VAR(168) Forecast for Household Surplus Energy DK1_Production")
lines(forecast_time, forecast_values_production, col = "red")  
legend("topright", legend = c("Actual Data", "VAR Forecast"), col = c("blue", "red"), lty = 1)

plot(forecast_time, actual_data_consumption, type = "l", col = "blue", 
     xlab = "Time", ylab = "Log Diff Consumption", 
     main = "Actual vs VAR Forecast for Household DK1_Consumption")
lines(forecast_time, forecast_values_consumption, col = "red")  
legend("topright", legend = c("Actual Data", "VAR Forecast"), col = c("blue", "red"), lty = 1)

plot(forecast_time, actual_data_bright, type = "l", col = "blue", 
     xlab = "Time", ylab = "Log Diff Bright Sunshine", 
     main = "Actual vs VAR Forecast for Bright Sunshine")
lines(forecast_time, forecast_values_bright, col = "red")  #
legend("topright", legend = c("Actual Data", "VAR Forecast"), col = c("blue", "red"), lty = 1)
par(mfrow = c(1, 1))


# Beregn MAE og RMSE 
MAE_production <- mae(actual_data_production, forecast_values_production)
RMSE_production <- rmse(actual_data_production, forecast_values_production)

MAE_consumption <- mae(actual_data_consumption, forecast_values_consumption)
RMSE_consumption <- rmse(actual_data_consumption, forecast_values_consumption)

MAE_bright <- mae(actual_data_bright, forecast_values_bright)
RMSE_bright <- rmse(actual_data_bright, forecast_values_bright)


# Granger causality test
data_reduced_df <- data.frame(data_reduced)
granger_test_result <- grangertest(DK1_Production ~ Bright_Sunshine, order = 168, data = data_reduced_df)
print(granger_test_result)

# Naive forcast
train_data <- data_reduced  
test_data <- data[14614:16077, ]

# Yearly naive forecast: The forecast for 2024 is based on the value from the same time in 2023
naive_forecast <- train_data[5832:7295, ]  
naive_forecast_2024 <- naive_forecast[1:1463, ]

forecast_period <- 14614:(14614 + (1462))

# Beregn MAE og RMSE 
MAE_naive_production <- mae(data[forecast_period, 1], naive_forecast_2024[, 1])
RMSE_naive_production <- rmse(data[forecast_period, 1], naive_forecast_2024[, 1])

MAE_naive_consumption <- mae(data[forecast_period, 2], naive_forecast_2024[, 2])
RMSE_naive_consumption <- rmse(data[forecast_period, 2], naive_forecast_2024[, 2])

MAE_naive_bright <- mae(data[forecast_period, 3], naive_forecast_2024[, 3])
RMSE_naive_bright <- rmse(data[forecast_period, 3], naive_forecast_2024[, 3])


par(mfrow = c(3, 1))
plot(forecast_period, data[forecast_period, 1], type = "l", col = "blue", 
     xlab = "Time", ylab = "Log Diff Production", main = "Naive Forecast vs Actual for Household Surplus Energy DK1_Production")
lines(forecast_period, naive_forecast_2024[, 1], col = "red")  # Plot den naive forecast
legend("topright", legend = c("Actual Data", "Naive Forecast"), col = c("blue", "red"), lty = 1)

plot(forecast_period, data[forecast_period, 2], type = "l", col = "blue", 
     xlab = "Time", ylab = "Log Diff Consumption", main = "Naive Forecast vs Actual for Household DK1_Consumption")
lines(forecast_period, naive_forecast_2024[, 2], col = "red")  # Plot den naive forecast
legend("topright", legend = c("Actual Data", "Naive Forecast"), col = c("blue", "red"), lty = 1)

plot(forecast_period, data[forecast_period, 3], type = "l", col = "blue", 
     xlab = "Time", ylab = "Log Diff Bright Sunshine", main = "Naive Forecast vs Actual for Bright Sunshine")
lines(forecast_period, naive_forecast_2024[, 3], col = "red")  # Plot den naive forecast
legend("topright", legend = c("Actual Data", "Naive Forecast"), col = c("blue", "red"), lty = 1)

