# Load required libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(writexl)
library(readxl)

#solcelleproduktion_2023 <- read_excel("~/Desktop/P9-Karsten_Data/Produktion/solcelleproduktion_2023.xls")
solcelleproduktion_2023 <-read_excel("2023_production.xlsx")

# Filtrér for rækker med DK1 i Budzone
DK1_2023 <- solcelleproduktion_2023 %>%
  filter(...1 == "DK1")
head(DK1_2023)


# Gruppér efter Report 1 (måned), dag og time og summer reading values
DK1_grouped <- DK1_2023 %>%
  group_by(`Report 1`, ...4, ...5) %>%  # Gruppér efter Report 1 (måned), dag og time
  summarise(Total_Reading = sum(as.numeric(...8), na.rm = TRUE)) %>%  # Summér reading values
  arrange(as.numeric(`Report 1`), as.numeric(...4), as.numeric(...5))  # Sortér efter måned, dag og time

# Vis det grupperede og sorterede resultat
print(DK1_grouped)


# Opret en datetime kolonne ved at kombinere måned, dag og time
DK1_grouped <- DK1_grouped %>%
  mutate(DateTime = as.POSIXct(paste(2023, `Report 1`, ...4, ...5, sep = "-"), 
                               format = "%Y-%m-%d-%H"))

################################################### write excel #########################################

DK1_grouped_cleaned <- DK1_grouped %>%
  filter(format(DateTime, "%Y") >= "2023") %>%
  select(DateTime, everything())  # Ensure correct column order
write_xlsx(DK1_grouped_cleaned, "DK1_grouped_cleaned.xlsx")

DK1_grouped_cleaned <- read_excel("DK1_grouped_cleaned.xlsx")

getwd()

View(DK1_grouped_cleaned)

# Plot en tidsserie
ggplot(DK1_grouped, aes(x = DateTime, y = Total_Reading)) +
  geom_line(color = "blue") +  # Linjeplot for Total_Reading over tid
  labs(x = "DateTime", y = "Total Reading", title = "Total Solar Reading Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotér x-aksen for bedre læsbarhed

###############################################################################

# Filtrér for rækker med DK2 i Budzone
DK2_2023 <- solcelleproduktion_2023 %>%
  filter(...1 == "DK2")

# Gruppér efter Report 1 (måned), dag og time og summer reading values
DK2_grouped <- DK2_2023 %>%
  group_by(`Report 1`, ...4, ...5) %>%  # Gruppér efter Report 1 (måned), dag og time
  summarise(Total_Reading = sum(as.numeric(...8), na.rm = TRUE)) %>%  # Summér reading values
  arrange(as.numeric(`Report 1`), as.numeric(...4), as.numeric(...5))  # Sortér efter måned, dag og time

# Vis det grupperede og sorterede resultat
print(DK2_grouped)


# Opret en datetime kolonne ved at kombinere måned, dag og time
DK2_grouped <- DK2_grouped %>%
  mutate(DateTime = as.POSIXct(paste(2023, `Report 1`, ...4, ...5, sep = "-"), 
                               format = "%Y-%m-%d-%H"))


################################################### write excel #########################################
#excel fil for DK2
write_xlsx(DK2_grouped, "DK2_grouped.xlsx")
DK2_grouped_imported <- read_excel("DK2_grouped.xlsx")
getwd()


# Plot en tidsserie
ggplot(DK2_grouped, aes(x = DateTime, y = Total_Reading)) +
  geom_line(color = "blue") +  # Linjeplot for Total_Reading over tid
  labs(x = "DateTime", y = "Total Reading", title = "Total Solar Reading Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotér x-aksen for bedre læsbarhed
