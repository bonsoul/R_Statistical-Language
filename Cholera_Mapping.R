library(ggplot2)   # For visualization
library(sf)        # For handling spatial data
library(dplyr)     # For data manipulation
library(leaflet)
library(readr)
library(knitr)
library(kableExtra)
library(leaflet)
library(htmltools)
library(shiny)



#read data(cholera)
data <- read_csv("D:/Downloads/cleaned_cholera_data.csv")
View(data)


#reading shape files
file.exists("D:\\Downloads\\County_shapefile\\County.shp")

sub_county <- st_read("D:\\Downloads\\County_shapefile\\County.shp")

print(sub_county)

colnames(sub_county)
#eda

#unique entries in Data

unique(data$`Sub County`)

#checking duplicates

duplicates <- data[duplicated(data),]

print(duplicates) #No duplicates
 

#missing values
missing_per_column <- colSums(is.na(data)) 
total_missing <- sum(missing_per_column) 
print(total_missing)
print(missing_per_column)

columns_with_missing <- names(missing_per_column[missing_per_column > 0]) 
print(columns_with_missing)


# Drop duplicate columns

#data <- data %>% select(`County`, `Sub County`)


#calculate the number of cases per county

cholera_case_county <- data %>%
  group_by(County) %>%
  summarise(Cholera_Cases = n()) %>%
  arrange(desc(Cholera_Cases)) # descending order

kable(cholera_case_county, caption = "Cholera Cases per County")

kable(cholera_case_county, caption = "Cholera Cases Per County") %>%
  kable_styling(bootstrap_options = c("striped", "hover","condensed","responsive"), full_width = FALSE)



# Function to plot cholera cases map
plotCholeraMap <- function(cholera_map) {
  
  # Define bins for classification
  bins = c(0, 10, 50, 100, 200, 500, 1000, max(cholera_map$Cholera_Cases, na.rm = TRUE))
  
  # Define color palette
  pal = colorBin("YlOrRd", domain = cholera_map$Cholera_Cases, bins = bins)
  
  # Define labels for hover text
  labels = sprintf(
    "<strong>%s</strong><br/>Cholera Cases: %g",
    cholera_map$COUNTY, cholera_map$Cholera_Cases
  ) %>% lapply(htmltools::HTML)
  
  # Create the leaflet map
  leaflet(data = cholera_map) %>%
    setView(lat = -0.0, lng = 36.681660, zoom = 6) %>%
    addPolygons(
      fillColor = ~pal(Cholera_Cases),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.8,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal, values = ~Cholera_Cases, opacity = 0.7, title = "Cholera Cases",
      position = "bottomleft"
    )
}

# Function to process data and plot cholera cases
plotCholeraMapForDS <- function(dataset) {
  # Read shapefile
  cholera_map <- st_read("D:/Downloads/extracted/County.shp")
  
  # Convert COUNTY column to character
  cholera_map$COUNTY <- as.character(cholera_map$COUNTY)
  
  # Aggregate cholera cases per county
  cholera_cases_by_county <- dataset %>%
    group_by(County) %>%
    summarise(Cholera_Cases = n())
  
  # Join with shapefile
  cholera_map <- left_join(cholera_map, cholera_cases_by_county, by = c("COUNTY" = "County"))
  
  # Replace NA cases with 0
  cholera_map$Cholera_Cases[is.na(cholera_map$Cholera_Cases)] <- 0
  
  # Plot the map
  plotCholeraMap(cholera_map)
}

# Call the function with cholera dataset
plotCholeraMapForDS(data)


#sub county

colnames(data)
# Convert column names for consistency
data$`Sub County` <- as.character(data$`Sub County`)



cholera_case_subcounty <- data %>% 
  group_by(County,Sub County) %>%
  summarise(Cholera_Case = n(), .groups = "drop")

data <- data %>%
  rename(Sub_County = `Sub County`)

cholera_case_subcounty <- data %>% 
  group_by(County, Sub_County)

  
cholera_case_subcounty <- data %>%
  group_by(County, Sub_County) %>%
  summarise(Cholera_Cases = n()) %>%
  arrange(desc(Cholera_Cases)) # Sorting in descending order

# Display as a table
kable(cholera_case_subcounty, caption = "Cholera Cases per Sub County") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)


  
