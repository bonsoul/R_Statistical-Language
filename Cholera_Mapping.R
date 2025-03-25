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
library(lubridate)
library(tidyr)



#read data(cholera)
data <- read_csv("D:/Downloads/1chorela_cases_dataset.csv")
colnames(data)


#reading shape files
file.exists("D:\\Downloads\\County_shapefile\\County.shp")

sub_county <- st_read("D:\\Downloads\\County_shapefile\\County.shp")

print(sub_county)

colnames(sub_county)
#eda


data$Age <- -as.numeric(data$Age)


# Remove negative or NA values
data <- data %>%
  filter(!is.na(Age) & Age >= 0) %>%
  mutate(Age_Group = cut(Age, 
                         breaks = seq(0, 100, by = 10), 
                         labels = paste0(seq(0, 90, by = 10), "-", seq(9, 99, by = 10)), 
                         right = FALSE))

# Print age group counts
print(table(data$Age_Group))


# Count the number of cases per age group

age_distribution <- data %>%
  group_by(Age_Group) %>%
  summarise(Count = n()) %>%
  arrange(Age_Group)

# Print the results
print(age_distribution)
  
  # Plot histogram with grouped age ranges
  ggplot(data, aes(x = Age_Group)) +
    geom_bar(fill = "green") +
    labs(title = "Age Distribution", x = "Age Range", y = "Frequency") +
    theme_minimal()
})





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

# Ensure Date.Of.Onset is in Date format
data <- data %>%
  mutate(`Date Of Onset` = as.Date(`Date Of Onset`, format="%d/%m/%Y"),
         Year = format(`Date Of Onset`, "%Y"))


# Aggregate cases per year
yearly_cases <- data %>%
  group_by(Year) %>%
  summarise(Cases = n()) %>%
  mutate(Year = as.numeric(Year)) %>%  # Convert Year to numeric
  filter(Year >= 2008 & Year <= 2024)  # Filter years from 2008 to 2024

# Plot the trend of cases over the years

ggplot(yearly_cases, aes(x = Year, y = Cases)) +
  geom_line(color = "blue", size = 1) +  # Line graph
  geom_point(color = "red", size = 2) +  # Add points for each year
  labs(title = "Trend of Cases (2008 - 2024)",
       x = "Year",
       y = "Number of Cases") +
  theme_minimal()



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


#rename the column
df1 <- data %>% rename(SubCounty = 'Sub-County')

colnames(df1)

cholera_case_subcounty <- df1 %>% 
  group_by(County,SubCounty) %>%
  summarise(Cholera_Case = n(), .groups = "drop")
  
# Display as a table
kable(cholera_case_subcounty, caption = "Cholera Cases per Sub County") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)


  


# Function to plot cholera cases for selected county
plotCholeraMapBySubCounty <- function(selected_county, dataset, shapefile_path) {
  
  # Read the shapefile for sub-counties
  subcounty_map <- st_read(sub_county)
  
  # Convert columns to character and trim whitespace
  subcounty_map <- subcounty_map %>%
    mutate(SUB_COUNTY = trimws(as.character(SUB_COUNTY)),
           COUNTY = trimws(as.character(COUNTY)))
  
  dataset <- dataset %>%
    mutate(Sub_County = trimws(as.character(Sub_County)),
           County = trimws(as.character(County)))
  
  # Check for unmatched sub-county names
  unmatched_subcounties <- setdiff(unique(dataset$Sub_County), unique(subcounty_map$SUB_COUNTY))
  
  if (length(unmatched_subcounties) > 0) {
    print("Unmatched Sub-Counties:")
    print(unmatched_subcounties)
  }
  
  # Filter dataset for the selected county
  cholera_cases_by_subcounty <- dataset %>%
    filter(County == selected_county) %>%
    group_by(SubCounty) %>%
    summarise(Cholera_Cases = n(), .groups = "drop")
  
  # Perform the left join
  subcounty_map <- left_join(subcounty_map, cholera_cases_by_subcounty, by = c("SUB_COUNTY" = "Sub_County"))
  
  # Replace NA cases with 0 (areas without cases)
  subcounty_map$Cholera_Cases[is.na(subcounty_map$Cholera_Cases)] <- 0
  
  # Define bins for color classification
  bins <- c(0, 10, 50, 100, 200, 500, 1000, max(subcounty_map$Cholera_Cases, na.rm = TRUE))
  
  # Define color palette
  pal <- colorBin("YlOrRd", domain = subcounty_map$Cholera_Cases, bins = bins)
  
  # Define labels for hover text
  labels <- sprintf(
    "<strong>%s</strong><br/>Cholera Cases: %g",
    subcounty_map$SUB_COUNTY, subcounty_map$Cholera_Cases
  ) %>% lapply(htmltools::HTML)
  
  # Create the leaflet map
  leaflet(data = subcounty_map) %>%
    setView(lat = -0.0, lng = 36.681660, zoom = 7) %>%
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

# Create a Shiny app for interactive county selection
shinyApp(
  ui = fluidPage(
    titlePanel("Cholera Cases by Sub-County"),
    sidebarLayout(
      sidebarPanel(
        selectInput("selected_county", "Select County:", choices = unique(data$County))
      ),
      mainPanel(
        leafletOutput("cholera_map")
      )
    )
  ),
  server = function(input, output) {
    output$cholera_map <- renderLeaflet({
      plotCholeraMapBySubCounty(input$selected_county, data, "D:/Downloads/extracted/SubCounty.shp")
    })
  }
)
