# Load required libraries
library(dplyr)
library(readxl)
library(caret)
library(fastDummies)
library(car)
library(ggplot2)
library(glmnet)
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

# Define the indices of observations to remove (the outliers)
outliers_to_remove <- c(52, 166, 165, 25, 169, 15) 

# Remove the outliers from the response variable and explanatory variables
Interest_energy_sharing <- as.factor(Interest_energy_sharing[-outliers_to_remove])
explanatory_variables <- combined_data[-outliers_to_remove, ]

# LASSO model
x <- model.matrix(Interest_energy_sharing ~ ., data = explanatory_variables)[, -1]
y <- Interest_energy_sharing

set.seed(123)  
model_lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(model_lasso)

best_lambda <- model_lasso$lambda.min
print(best_lambda)

lasso_coefficients <- coef(model_lasso, s = "lambda.min")
print(lasso_coefficients)


################################# Logistic (LASSO) ###################################

explanatory_variables_reduced <- explanatory_variables[, -c(9, 11, 25)]

logistic_model <- glm( Interest_energy_sharing ~ . , data = explanatory_variables_reduced, family = binomial)
summary(logistic_model) # AIC: 131.13

###################### Pearson residuals #######################
# Obtain Pearson residuals
pearson_residuals <- residuals(logistic_model, type = "pearson")
print(pearson_residuals)

plot(fitted(logistic_model), pearson_residuals, main = "Pearson Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Pearson Residuals")
abline(h = 0, col = "red", lty = 2)

# Calculate the Pearson chi-squared statistic
chi_squared_stat <- sum(pearson_residuals^2)
df <- logistic_model$df.residual
p_value <- 1 - pchisq(chi_squared_stat, df)

# Display the results
cat("Pearson Goodness-of-Fit Test\n")
cat("Chi-squared Statistic:", chi_squared_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n") 

######################### deviance residuals ######################
# Calculate deviance residuals
deviance_residuals <- residuals(logistic_model, type = "deviance")
print(deviance_residuals)

plot(fitted(logistic_model), deviance_residuals, 
     main = "Deviance Residuals vs Fitted Values",
     xlab = "Fitted Values", 
     ylab = "Deviance Residuals")
abline(h = 0, col = "red", lty = 2)  

######################## McFadden's Pseudo ########################
# Null model (model with intercept only)
null_model <- glm(Interest_energy_sharing ~ 1, data = explanatory_variables_reduced, family = binomial)

# Log-likelihoods
logLik_model <- logLik(logistic_model)
logLik_null <- logLik(null_model)

# McFadden's Pseudo R-squared
mcfadden_r2 <- 1 - (as.numeric(logLik_model) / as.numeric(logLik_null))
mcfadden_r2

k <- length(coef(logistic_model))

# Adjusted McFadden's R-squared 
adjusted_mcfadden_r2 <- 1 - ((as.numeric(logLik_model) - k) / as.numeric(logLik_null))
adjusted_mcfadden_r2


########################### Refined logistic model ############################
# wald test
wald_test <- Anova(logistic_model, test = "Wald")
print(wald_test)

reduced_model <- step(logistic_model, direction = "backward")
summary(reduced_model)

explanatory_variables_backward <- explanatory_variables[, -c(2,9,10, 11,15,19,25)]

###################### Pearson residuals #######################
# Obtain Pearson residuals
pearson_residuals <- residuals(reduced_model, type = "pearson")
print(pearson_residuals)

plot(fitted(reduced_model), pearson_residuals, main = "Pearson Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Pearson Residuals")
abline(h = 0, col = "red", lty = 2)

# Calculate the Pearson chi-squared statistic
chi_squared_stat <- sum(pearson_residuals^2)
df <- reduced_model$df.residual
p_value <- 1 - pchisq(chi_squared_stat, df)

# Display the results
cat("Pearson Goodness-of-Fit Test\n")
cat("Chi-squared Statistic:", chi_squared_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-value:", p_value, "\n") 

######################### deviance residuals ######################
# Calculate deviance residuals
deviance_residuals <- residuals(reduced_model, type = "deviance")
print(deviance_residuals)

plot(fitted(reduced_model), deviance_residuals, 
     main = "Deviance Residuals vs Fitted Values",
     xlab = "Fitted Values", 
     ylab = "Deviance Residuals")
abline(h = 0, col = "red", lty = 2)  


######################## McFadden's Pseudo ########################
# Null model (model with intercept only)
null_model <- glm(Interest_energy_sharing ~ 1, data = explanatory_variables_backward, family = binomial)

# Log-likelihoods
logLik_model <- logLik(reduced_model)
logLik_null <- logLik(null_model)

# McFadden's Pseudo R-squared
mcfadden_r2 <- 1 - (as.numeric(logLik_model) / as.numeric(logLik_null))
mcfadden_r2

k <- length(explanatory_variables_backward)

# Adjusted McFadden's R-squared
adjusted_mcfadden_r2 <- 1 - ((as.numeric(logLik_model) - k) / as.numeric(logLik_null))
adjusted_mcfadden_r2



