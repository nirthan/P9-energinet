# Load required libraries
library(dplyr)
library(readxl)
library(caret)
library(fastDummies)
library(car)
library(ggplot2)
library(ResourceSelection)

# Read the Excel file 
Solcelle_final_select <- read_excel("~/Downloads/Solcelle_final_select.xlsx")
solcelle_encoded <- dummy_cols(Solcelle_final_select, remove_first_dummy = TRUE, 
                               select_columns = colnames(Solcelle_final_select)[-c(3)])

# Those before 16 are original variables - not encoded 
selected_columns_encoded <- solcelle_encoded[, 16:40]

# Response variable 
Interest_energy_sharing <- ifelse(selected_columns_encoded[, 25] == 0, 1, 0)
response_variable <- as.vector(Interest_energy_sharing)

# Explanatory variables 
selected_columns_encoded_no25 <- selected_columns_encoded[, -c(25)]
solcelle_col3 <- Solcelle_final_select[, 3, drop = FALSE] 
combined_data <- cbind(selected_columns_encoded_no25, solcelle_col3)

# Initial logistic model 
model <- glm( response_variable ~ . , data = combined_data, family = binomial)
summary(model)

# AIC,BIC
model_aic <- AIC(model)
model_bic <- BIC(model)

# Checking for multicollinarity
vif_values <- vif(model)
print(vif_values)

###################### Pearson residuals #######################
# Obtain Pearson residuals
pearson_residuals <- residuals(model, type = "pearson")
print(pearson_residuals)

plot(fitted(model), pearson_residuals, main = "Pearson Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Pearson Residuals")
abline(h = 0, col = "red", lty = 2)

# Calculate the Pearson chi-squared statistic
chi_squared_stat <- sum(pearson_residuals^2)
df <- model$df.residual
p_value <- 1 - pchisq(chi_squared_stat, df)

# Display the results
cat("Pearson Goodness-of-Fit Test\n")
cat("Chi-squared Statistic:", chi_squared_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n") # 0.3119 the model fits the data reasonably well.

######################### deviance residuals ######################
# Calculate deviance residuals
deviance_residuals <- residuals(model, type = "deviance")
print(deviance_residuals)

plot(fitted(model), deviance_residuals, 
     main = "Deviance Residuals vs Fitted Values",
     xlab = "Fitted Values", 
     ylab = "Deviance Residuals")
abline(h = 0, col = "red", lty = 2)  

######################## McFadden's Pseudo ########################
# Null model (model with intercept only)
null_model <- glm(response_variable ~ 1, data = combined_data, family = binomial)

# Log-likelihoods
logLik_model <- logLik(model)
logLik_null <- logLik(null_model)

# McFadden's Pseudo R-squared
mcfadden_r2 <- 1 - (as.numeric(logLik_model) / as.numeric(logLik_null))
mcfadden_r2

k <- length(coef(model))

# Adjusted McFadden's R-squared 
adjusted_mcfadden_r2 <- 1 - ((as.numeric(logLik_model) - k) / as.numeric(logLik_null))
adjusted_mcfadden_r2

######################## Cook's Distance ###########################
# Calculate Cook's Distance
cooks_distances <- cooks.distance(model)
print(cooks_distances)

plot(cooks_distances, 
     main = "Cook's Distance for Each Observation",
     xlab = "Observation Index",
     ylab = "Cook's Distance")
abline(h = 4 / nrow(combined_data), col = "red", lty = 2)  # Threshold line

# Highlight observations with high influence
influential_points <- which(cooks_distances > (4 / nrow(combined_data)))
points(influential_points, cooks_distances[influential_points], 
       col = "red", pch = 20)

boxplot(cooks_distances, 
        main = "Box Plot of Cook's Distances with Colored Observations",
        ylab = "Cook's Distance")  

#the top 6 outliers
top_6_indices <- order(cooks_distances, decreasing = TRUE)[1:6]
print(top_6_indices)

