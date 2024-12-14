# Load required libraries
library(dplyr)
library(writexl)
library(readxl)
library(caret)
library(fastDummies)
library(car)
library(dplyr)
library(tidyr)

# Get the data ()
solar_final <-read_excel("~/Downloads/Final_Solcell_ ejer_svar.xlsx")


# Removes those variable which is not answered by every respondant
# 15 is also excluded from the data set (is solar panel owned or not )
solar_owner <- solar_final[ , -c(1, 15,25, 26, 27, 28, 29, 31, 32, 33, 34, 35, 36, 37, 38, 39, 42, 43, 47, 48,50)]

# removes those who said "ønsker ikke at oplyse" for income 
solar_owner[(8)][solar_owner[(8)] == "Ønsker ikke at oplyse"] <- NA

# removes those who said "ved ik" in reduced tax
solar_owner[(14)][solar_owner[(14)] == "Ved ikke"] <- NA

# removes those who said "ved ik" in size of solar panels 
solar_owner[(21)][solar_owner[(21)] == "Ved ikke"] <- NA

# remove NA (18 should be removed from original to the clean )
solcelle_clean<-na.omit(solar_owner)

############### Visualization of variables distribution ###############

# Barplot for gender 
solcelle_clean[[1]] <- factor(trimws(tolower(solcelle_clean[[1]])))
levels(solcelle_clean[[1]]) <- c("Female", "Male")

barplot(table(solcelle_clean[[1]]), 
        main = "Gender", 
        ylab = "Frequency", 
        col = "lightblue", 
        border = "black")


# Bar plot for employment status
solcelle_clean[[4]] <- factor(trimws(tolower(solcelle_clean[[4]])))
levels(solcelle_clean[[4]]) <- c("Part time", "Full time", "Unemployed","Retired","Self-employed")

barplot(table(solcelle_clean[[4]]), 
        main = "Employment status",
        xlab = "Categories",
        ylab = "Frequency",
        col = "lightblue", 
        border = "black")


# Bar plot for Type of Housing
solcelle_clean[[6]] <- factor(trimws(tolower(solcelle_clean[[6]])))
levels(solcelle_clean[[6]]) <- c("Cooperative housing ", "Own home", "Rental flat","Townhouse","Other")

barplot(table(solcelle_clean[[6]]), 
        main = "Type of Housing",
        xlab = "Categories",
        ylab = "Frequency",
        col = "lightblue", 
        border = "black")


# Bar plot for Receive Money for Surplus Energy?
solcelle_clean[[15]] <- factor(trimws(tolower(solcelle_clean[[15]])))
levels(solcelle_clean[[15]]) <- c("Yes ", "No")

barplot(table(solcelle_clean[[15]]), 
        main = "Payment for Surplus Energy",
        xlab = "Categories",
        ylab = "Frequency",
        col = "lightblue", 
        border = "black")


# Bar plot for reason you installed solar panels
solcelle_clean[[17]] <- factor(trimws(tolower(solcelle_clean[[17]])))

levels(solcelle_clean[[17]]) <- c("Energy independence  ", "Green transition", "Economic benefit")

barplot(table(solcelle_clean[[17]]), 
        main = "Reason for installing solar panels",
        xlab = "Categories",
        ylab = "Frequency",
        col = "lightblue", 
        border = "black")

# Bar plot for maintenance or repair
solcelle_clean[[18]] <- factor(trimws(tolower(solcelle_clean[[18]])))
levels(solcelle_clean[[18]]) <- c("Never", "Once a year ", "Several times a year","Have not needed it yet")

barplot(table(solcelle_clean[[18]]), 
        main = "Maintenance or repairs",
        xlab = "Categories",
        ylab = "Frequency",
        col = "lightblue", 
        border = "black")


# Barplot of challenge for solar panel owners having  solar panels 
values_to_keep <- c("Manglende energilagringsmuligheder", 
                    "Startomkostninger", 
                    "Teknisk vedligeholdelse", 
                    "Ingen")
# Replace all the other rows with NA 
solcelle_clean[[19]] <- ifelse(solcelle_clean[[19]] %in% values_to_keep, 
                               solcelle_clean[[19]], 
                               NA)

# Change NA values to other value like "other " 
solcelle_clean[[19]][is.na(solcelle_clean[[19]])] <- "Andet"

solcelle_clean[[19]] <- factor(trimws(tolower(solcelle_clean[[19]])))
levels(solcelle_clean[[19]]) <- c("Initial costs", "Technical maintenance", "Lack of energy storage ", "None ", "Other")

barplot(table(solcelle_clean[[19]]), 
        main = "Biggest challenge for solar panel owners",
        xlab = "Categories",
        ylab = "Frequency",
        col = "lightblue", 
        border = "black")


# Bar plot of renewable energy  
barplot(table(solcelle_clean[[24]]), 
        main = "View on renewable energy",
        xlab = "Categories",
        ylab = "Frequency",
        col = "lightblue", 
        border = "black")

# Bar plot of participated in sharing economic before
solcelle_clean[[25]] <- factor(trimws(tolower(solcelle_clean[[25]])))
levels(solcelle_clean[[25]]) <- c("Yes", "No")

barplot(table(solcelle_clean[[25]]), 
        main = "Participation in sharing economy",
        xlab = "Categories",
        ylab = "Frequency",
        col = "lightblue", 
        border = "black")



# Bar plot of factors in investing in new tech
expanded_data <- solcelle_clean %>%
  select(26) %>%
  mutate(response = trimws(tolower(as.character(solcelle_clean[[26]])))) %>%
  separate_rows(response, sep = ",\\s*") %>%
  # Replace answers with desired responses using case_when
  mutate(response = case_when(
    response == "miljømæssige fordele" ~ "Environmental benefits",
    response == "økonomisk gevinst eller besparelser" ~ "Financial gain",
    response == "tilskud eller finansielle incitamenter" ~ "Support from finance",
    response == "anbefalinger fra eksperter eller bekendte" ~ "Expert help",
    response == "teknologiens brugervenlighed" ~ "User-friendliness",
    TRUE ~ "Other"  # Default category
  )) 
# Count occurrences of each response
response_counts <- table(expanded_data$response)

# Create the bar plot for factors in investing in new tech
barplot(response_counts, 
        main = "Investing in New Technology",
        xlab = "Categories",
        ylab = "Frequency",
        col = "lightblue", 
        border = "black")

solar_removed_final<-solcelle_clean[ , -c(1,4,6,15,17,18,19,24,25,26,27)]

write_xlsx(solar_removed_final, "solar_removed_final.xlsx")
Solar_final_imported <- read_excel("solar_removed_final.xlsx")
getwd()
