# Load required libraries
library(sf)
library(ggplot2)
library(dplyr)

# Load shapefiles for East African countries
# Make sure the shapefiles are downloaded from GADM and provide the path to the shapefile

shapefile <- st_read("EA Shape Files/ESA_admin1_region.shp")

# Filter the shapefile for the East African Community (EAC) countries
#eac_countries <- c("Burundi", "Kenya", "Rwanda", "South Sudan", "Tanzania", 
                   #"Uganda", "Democratic Republic of the Congo", "Somalia")
shapefile_data <- shapefile %>%
  mutate(COUNTRY = ifelse(COUNTRY == "Congo DRC", "Democratic Republic of the Congo", COUNTRY))

eac_countries <- c("Burundi", "Kenya", "Rwanda", "South Sudan",
                   "Tanzania", "Uganda","Somalia","Democratic Republic of the Congo")

# Filter shapefile for only EAC countries
eac_shapefile <- shapefile_data %>%
  filter(COUNTRY %in% eac_countries)

# Check the result
plot(st_geometry(eac_shapefile))


# Rename "Congo DRC" to "Democratic Republic of the Congo" in the 'Country' column
#shapefile_data <- shapefile %>%
  #mutate(COUNTRY = ifelse(COUNTRY == "Democratic Republic of the Congo", "Congo DRC", COUNTRY))



#unique(shapefile_data$COUNTRY)

# Load the filtered mortality data
# Read the dataset saved as CSV
mortality_data1 <- read.csv("Data/dataset_datascience.csv")

colnames(mortality_data1)

# Rename "United Republic of Tanzania" to "Tanzania" in the 'Country' column
mortality_data <- mortality_data1 %>%
  mutate(Geographic.area = ifelse(Geographic.area == "United Republic of Tanzania", "Tanzania", Geographic.area))

unique(mortality_data$Geographic.area)

#shapefile_eac <- shapefile[shapefile$COUNTRY %in% eac_countries, ]

library(dplyr)

mortality_data_cleaned <- mortality_data %>%
  filter(!is.na(Geographic.area),
         !is.na(Indicator),
         !is.na(Series.Year),
         !is.na(Observation.Value),
         grepl("^\\d+$", Series.Year)) %>%  # keep only fully numeric years
  mutate(Series.Year = as.numeric(Series.Year))



View(shapefile_eac)

# Filter the data for EAC countries and select relevant columns
mortality_data_filtered <- mortality_data_cleaned %>%
  filter(Geographic.area %in% eac_countries) %>%
  select(Geographic.area, Indicator, Series.Year, Observation.Value)

# Merge the mortality data with the shapefile (based on the country names)
merged_data <- merge(eac_shapefile, mortality_data_filtered, by.x = "COUNTRY", by.y = "Geographic.area")

View(merged_data)

unique(merged_data$COUNTRY)

# Create maps for the latest mortality rate by country
latest_data_neonatal <- merged_data %>% 
  filter(Indicator == "Neonatal mortality rate" & Series.Year == max(Series.Year))

latest_data_under5 <- merged_data %>% 
  filter(Indicator == "Under-five mortality rate" & Series.Year == max(Series.Year))

# Calculate average mortality per country over years
avg_trends <- mortality_data_filtered %>%
  group_by(Geographic.area, Indicator, Series.Year) %>%
  summarize(Average_Value = mean(Observation.Value, na.rm = TRUE), .groups = "drop")

avg_trends


# Plot: Neonatal Mortality Trend
ggplot(avg_trends, aes(x = Series.Year, y = Average_Value, color = Indicator)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Average Mortality Rate Trends in East Africa",
       x = "Year", y = "Average Mortality Rate (per 1,000)") +
  theme_minimal()


# Plot maps
ggplot() +
  geom_sf(data = eac_shapefile, fill = "white", color = "black") +
  geom_sf(data = latest_data_neonatal, aes(fill = Observation.Value), color = "black") +
  scale_fill_viridis_c() +
  labs(title = "Latest Neonatal Mortality Rate (per 1,000 live births)") +
  theme_minimal()

ggplot() +
  geom_sf(data = eac_shapefile, fill = "white", color = "black") +
  geom_sf(data = latest_data_under5, aes(fill = Observation.Value), color = "black") +
  scale_fill_viridis_c() +
  labs(title = "Latest Under-five Mortality Rate (per 1,000 live births)") +
  theme_minimal()

# Trend plots: Plot average trends over time for each country
avg_trends <- mortality_data_filtered %>%
  group_by(Geographic.area, Indicator, Series.Year) %>%
  summarize(Average_Value = mean(Observation.Value, na.rm = TRUE))

ggplot(avg_trends %>% filter(Indicator == "Neonatal mortality rate"), aes(x = Series.Year, y = Average_Value, color = Geographic.area)) +
  geom_line() +
  labs(title = "Neonatal Mortality Rate Trends Over Time") +
  theme_minimal()

ggplot(avg_trends %>% filter(Indicator == "Under-five mortality rate"), aes(x = Series.Year, y = Average_Value, color = Geographic.area)) +
  geom_line() +
  labs(title = "Under-five Mortality Rate Trends Over Time") +
  theme_minimal()


