# Load necessary libraries
library(dplyr)
library(tidyr)

# Load the datasets
hiv_data <- read.csv("Data/HIV data 2000-2023.csv", encoding = "ISO-8859-1")
poverty_data <- read.csv("Data/multidimensional_poverty.csv", encoding = "ISO-8859-1")


clean_hiv_value <- function(value) {
  value <- trimws(value)
  
  # Handle missing data cases
  if (value %in% c("Nodata", "<100") || value == "") {
    return(NA)
  }
  
  # If the value starts with a number before any brackets
  if (grepl("^\\d", value)) {
    # Extract the first numeric part before any brackets
    clean_value <- gsub("[^0-9]", "", sub("\\s*\\[.*\\]", "", value))
    return(as.numeric(clean_value))
  }
  
  # If it only contains brackets
  if (grepl("\\[.*\\]", value)) {
    range_str <- sub(".*\\[(\\d+[\\d,]*)\\s*-\\s*(\\d+[\\d,]*)\\].*", "\\1,\\2", value)
    range_values <- as.numeric(gsub("[^0-9]", "", strsplit(range_str, ",")[[1]]))
    if (length(range_values) == 2 && !any(is.na(range_values))) {
      return(mean(range_values, na.rm = TRUE))
    } else {
      return(NA)
    }
  }
  
  return(NA)
}


hiv_data$CleanedValue <- sapply(hiv_data$Value, clean_hiv_value)
head(hiv_data[, c("Indicator", "Location", "Period", "Value", "CleanedValue")])



# Calculate the total HIV cases globally
total_hiv_cases <- sum(hiv_data$CleanedValue, na.rm = TRUE)

# Calculate the cumulative sum and identify countries contributing to 75% of the global burden
hiv_data_country_sum <- hiv_data %>%
  group_by(Location) %>%
  summarise(total_hiv = sum(CleanedValue, na.rm = TRUE)) %>%
  arrange(desc(total_hiv)) %>%
  mutate(cumulative_sum = cumsum(total_hiv), cumulative_percentage = 100 * cumulative_sum / total_hiv_cases)

# Filter the countries contributing to 75% of the global burden
countries_75_percent <- hiv_data_country_sum %>%
  filter(cumulative_percentage <= 75) %>%
  pull(Location)

# Filter the merged dataset to include only the selected countries
hiv_data_75_percent <- hiv_data %>%
  filter(Location %in% countries_75_percent)




# Load necessary visualization libraries
library(ggplot2)

# Plot the trend of HIV cases over time for countries contributing to 75% of the global burden
ggplot(hiv_data_75_percent, aes(x = Period, y = CleanedValue, color = Location, group = Location)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Trend of HIV Cases in Countries Contributing to 75% of the Global Burden (2000-2023)",
    x = "Year",
    y = "HIV Cases (Estimated)",
    color = "Countries"    # <- set legend title here
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "right")




library(dplyr)
library(ggplot2)

hiv_data_top75_clean <- hiv_data_top75 %>% 
  filter(!is.na(CleanedValue))  # Keep only rows where CleanedValue is not NA


# Step 1: Calculate cumulative contribution within each WHO region
hiv_top75 <- hiv_data %>%
  group_by(ParentLocationCode, Location) %>%
  summarise(total_cases = sum(CleanedValue, na.rm = TRUE)) %>%
  arrange(ParentLocationCode, desc(total_cases)) %>%
  group_by(ParentLocationCode) %>%
  mutate(cum_percent = cumsum(total_cases) / sum(total_cases)) %>%
  filter(cum_percent <= 0.75)

# Step 2: Filter original dataset to only keep top 75% countries
hiv_data_top75 <- hiv_data %>%
  filter(Location %in% hiv_top75$Location)

# Step 3: Plot the trend
ggplot(hiv_data_top75_clean, aes(x = Period, y = CleanedValue, color = Location, group = Location)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ParentLocationCode) +  # one plot per WHO region
  theme_minimal() +
  labs(
    title = "Trend of HIV Cases (Top 75% Countries by WHO Region)",
    x = "Year",
    y = "HIV Cases (Estimated)",
    color = "Country"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")





colnames(hiv_data)
colnames(poverty_data)

# First, prepare both datasets (optional but neat)
hiv_data_clean <- hiv_data %>% 
  select(Location, Period, CleanedValue)  # keep what we need

poverty_data_clean <- poverty_data %>% 
  select(Economy, Reporting.Year, Poverty.Headcount.Ratio, 
         Educational.Attainment, Educational.Enrollment, 
         Electricity, Sanitation, Drinking.Water)

# Replace all NA values with 0 for the entire poverty dataset
poverty_data_clean1 <- poverty_data_clean %>%
  replace_na(list(
    Poverty.Headcount.Ratio = 0,
    Educational.Attainment = 0,
    Educational.Enrollment = 0,
    Electricity = 0,
    Sanitation = 0,
    Drinking.Water = 0
  ))


# Rename 'Location' to 'Country' in the HIV data
hiv_data_clean <- hiv_data_clean %>%
  rename(Country = Location)

# Rename 'Economy' to 'Country' in the Poverty data
poverty_data_clean1 <- poverty_data_clean1 %>%
  rename(Country = Economy)

# Perform the left join using the renamed 'Country' column and 'Period'
hiv_poverty_merged1 <- hiv_data_clean %>%
  left_join(poverty_data_clean1, by = c("Country"))

# Check the result
head(hiv_poverty_merged1)

colnames(hiv_poverty_merged1)


library(lme4)
library(Matrix)

model <- lmer(CleanedValue ~ Poverty.Headcount.Ratio + Educational.Attainment + 
                Educational.Enrollment + Electricity + Sanitation + Drinking.Water + 
                (1 | Country) + (1 | Period), 
              data = hiv_poverty_merged1)

# Summarize the model
summary(model)


library(nlme)

# Remove rows with missing values in the relevant columns
hiv_poverty_merged_clean <- hiv_poverty_merged1 %>%
  filter(!is.na(CleanedValue) & !is.na(Poverty.Headcount.Ratio) & 
           !is.na(Educational.Attainment) & !is.na(Educational.Enrollment) &
           !is.na(Electricity) & !is.na(Sanitation) & !is.na(Drinking.Water))

# Fit the model with the cleaned data
library(nlme)

model <- lme(CleanedValue ~ Poverty.Headcount.Ratio + Educational.Attainment + 
               Educational.Enrollment + Electricity + Sanitation + Drinking.Water,
             random = ~ 1 | Country/Period, 
             data = hiv_poverty_merged_clean)

# Summarize the model
summary(model)








