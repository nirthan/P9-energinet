library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)

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

############################# Calculations ####################################
####### 2023
# Single household surplus production
DK1_Production_single_household_2023 <- (DK1_Production_2023$Total_Reading / 96166)*0.72 * 0.69
DK1_Production_single_household_2023 <- data.frame(DK1_Production_single_household_2023, DateTime = DK1_Production_2023$DateTime)

DK2_Production_single_household_2023 <- (DK2_Production_2023$Total_Reading / 38690)*0.72 * 0.69
DK2_Production_single_household_2023 <- data.frame(DK2_Production_single_household_2023, DateTime = DK2_Production_2023$DateTime)

# Single household consumption
DK1_Consumption_single_household_2023 <- (DK1_Consumption_2023$Total_Reading / 1400000)*0.882 
DK1_Consumption_single_household_2023 <- data.frame(DK1_Consumption_single_household_2023, DateTime = DK1_Consumption_2023$DateTime)

DK2_Consumption_single_household_2023 <- (DK2_Consumption_2023$Total_Reading / 1400000)*0.882 
DK2_Consumption_single_household_2023 <- data.frame(DK2_Consumption_single_household_2023, DateTime = DK2_Consumption_2023$DateTime)

####### 2024
# Single household surplus production
DK1_Production_single_household_2024 <- (DK1_Production_2024$Total_Reading / 104534)*0.72 * 0.69
DK1_Production_single_household_2024 <- data.frame(DK1_Production_single_household_2024, DateTime = DK1_Production_2024$DateTime)

DK2_Production_single_household_2024 <- (DK2_Production_2024$Total_Reading / 42137)*0.72 * 0.69
DK2_Production_single_household_2024 <- data.frame(DK2_Production_single_household_2024, DateTime = DK2_Production_2024$DateTime)

# Single household consumption
DK1_Consumption_single_household_2024 <- (DK1_Consumption_2024$Total_Reading / 1400000)*0.882 
DK1_Consumption_single_household_2024 <- data.frame(DK1_Consumption_single_household_2024, DateTime = DK1_Consumption_2024$DateTime)

DK2_Consumption_single_household_2024 <- (DK2_Consumption_2024$Total_Reading / 1400000) * 0.882
DK2_Consumption_single_household_2024 <- data.frame(DK2_Consumption_single_household_2024, DateTime = DK2_Consumption_2024$DateTime)

# Coverage for 2023 & 2024 for DK1
DK1_Coverage_single_2023 <- data.frame(Coverage_DK1 = DK1_Production_single_household_2023$DK1_Production_single_household_2023 /
                                         DK1_Consumption_single_household_2023$DK1_Consumption_single_household_2023)
DK1_Coverage_single_2024 <- data.frame(Coverage_DK1 = DK1_Production_single_household_2024$DK1_Production_single_household_2024 /
                                         DK1_Consumption_single_household_2024$DK1_Consumption_single_household_2024)

Coverage_single_DK1 <- bind_rows(DK1_Coverage_single_2023, DK1_Coverage_single_2024)

# Coverage for 2023 & 2024 for DK2
DK2_Coverage_single_2023 <- data.frame(Coverage_DK2 = DK2_Production_single_household_2023$DK2_Production_single_household_2023 / 
  DK2_Consumption_single_household_2023$DK2_Consumption_single_household_2023)
  
DK2_Coverage_single_2024 <- data.frame(Coverage_DK2 = DK2_Production_single_household_2024$DK2_Production_single_household_2024 / 
  DK2_Consumption_single_household_2024$DK2_Consumption_single_household_2024)

Coverage_single_DK2 <- bind_rows(DK2_Coverage_single_2023, DK2_Coverage_single_2024)

plot1 <- ggplot(Coverage_single_DK1, aes(x = DK1_Production$DateTime, y = Coverage_DK1)) +
  geom_line() +
  labs(x = "Date", y = "Number of households ", title = "Hourly Coverage for DK1" ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),)

plot2 <- ggplot(Coverage_single_DK2, aes(x = DK1_Production$DateTime, y = Coverage_DK2)) +
  geom_line() +
  labs(x = "Date", y = "Number of households", title = "Hourly Coverage for DK2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),)
plot1 / plot2

######### ONLY for 2023 
# Number of times the consumption is being 100 % covered by surplusenergy for DK1 
filtered_results_DK1 <- list()
for (i in 1:6) {
  lower_bound <- i
  upper_bound <- 7
  filtered_data <- DK1_Coverage_single_2023 %>%
    filter(DK1_Coverage_single_2023 >= lower_bound & DK1_Coverage_single_2023 <= upper_bound)
  filtered_results_DK1[[paste0("Range_", lower_bound, "_to_", upper_bound)]] <- filtered_data
  cat("Range", lower_bound, "to", upper_bound, "has", nrow(filtered_data), "rows\n")
}

# Number of times the consumption is being 100 % covered by surplusenergy for DK2 
filtered_results_DK2 <- list()
for (i in 1:10) {
  lower_bound <- i
  upper_bound <- 11
  filtered_data <- DK2_Coverage_single_2023 %>%
    filter(DK2_Coverage_single_2023 >= lower_bound & DK2_Coverage_single_2023 <= upper_bound)
  filtered_results_DK2[[paste0("Range_", lower_bound, "_to_", upper_bound)]] <- filtered_data
  cat("Range", lower_bound, "to", upper_bound, "has", nrow(filtered_data), "rows\n")
}

# Singe household consumption 80%  
DK1_2023_single_80 <- DK1_Consumption_single_household_2023$DK1_Consumption_single_household_2023 * 0.80
DK2_2023_single_80 <- DK2_Consumption_single_household_2023$DK2_Consumption_single_household_2023 * 0.80

DK1_Coverage_single_2023_80 <- data.frame(Coverage_DK1_80 = DK1_Production_single_household_2023$DK1_Production_single_household_2023 /
                                            DK1_2023_single_80)

DK2_Coverage_single_2023_80 <- data.frame(Coverage_DK2 = DK2_Production_single_household_2023$DK2_Production_single_household_2023 / 
                                            DK2_2023_single_80)

# Number of times the consumption is being 80 % covered by surplusenergy for DK1 
filtered_results_DK1_80 <- list()
for (i in 1:7) {
  lower_bound <- i
  upper_bound <- 8
  filtered_data <- DK1_Coverage_single_2023_80 %>%
    filter(DK1_Coverage_single_2023_80 >= lower_bound & DK1_Coverage_single_2023_80 <= upper_bound)
  filtered_results_DK1_80[[paste0("Range_", lower_bound, "_to_", upper_bound)]] <- filtered_data
  cat("Range", lower_bound, "to", upper_bound, "has", nrow(filtered_data), "rows\n")
}

# Number of times the consumption is being 80 % covered by surplusenergy for DK2
filtered_results_DK2_80 <- list()

for (i in 1:13) {
  lower_bound <- i
  upper_bound <- 14
  filtered_data <- DK2_Coverage_single_2023_80 %>%
    filter(DK2_Coverage_single_2023_80 >= lower_bound & DK2_Coverage_single_2023_80 <= upper_bound)
  filtered_results_DK2_80[[paste0("Range_", lower_bound, "_to_", upper_bound)]] <- filtered_data
  cat("Range", lower_bound, "to", upper_bound, "has", nrow(filtered_data), "rows\n")
}

# Visualization of surplus production and consumption for each month in 2023
colnames(DK1_Production_single_household_2023) <- c("Surplus_Energy", "DateTime")
colnames(DK1_Consumption_single_household_2023) <- c("Consumption", "DateTime")

combined_data <- merge(DK1_Production_single_household_2023, 
                       DK1_Consumption_single_household_2023, 
                       by = "DateTime")

combined_data$DateTime <- as.POSIXct(combined_data$DateTime)

# Plots for months 1 to 12
for (month_num in 1:12) {
  filtered_data <- combined_data %>%
    filter(month(DateTime) %in% month_num)
  
  p <- ggplot(data = filtered_data, aes(x = DateTime)) +
    geom_line(aes(y = Surplus_Energy, color = "Shared Surplus Energy per Solar Owner"), size = 1) +
    geom_line(aes(y = Consumption, color = "Consumption per Household"), size = 1) +
    labs(title = paste("Surplus Energy and Consumption (Month", month_num, ")"),
         x = "DateTime",
         y = "Energy (kWh)",
         color = "Legend") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = c(0.999, 1),
      legend.justification = c(1, 1)
    )
  print(p) 
}




