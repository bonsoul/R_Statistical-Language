library(tidyverse)
library(lubridate)


library(stringr)

library(leaflet)
library(rgdal)
library(sf)
library(htmlwidgets)


library(knitr)
library(DT)

library(caret)
library(forecast)
library(prophet)
library(dplyr)


#read thed data
rm(list=ls())

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"
fillColorLightCoral = "#F08080"

overall_poverty_est <- read_csv("D:/Downloads/overall_poverty_est.xls")
hardcore_poverty_est <- read_csv("D:/Downloads/hardcore_poverty_est.xls")
food_poverty_est <- read_csv("D:/Downloads/food_poverty_est.xls")
kiva_loans <- read_csv("D:/Downloads/kiva_loans.csv.zip")
kiva_mpi_region <- read_csv("D:/Downloads/kiva_mpi_region_locations.csv")
loan_theme_ids <- read_csv("D:/Downloads/loan_theme_ids.csv.zip")
loan_themes_region_ <- read_csv("D:/Downloads/loan_themes_by_region.csv.zip")


conflictsdata <- read_csv("D:/Downloads/african_conflicts.csv.zip", col_types = cols(
  .default = col_character(),
  FATALITIES = col_integer(),
  GEO_PRECISION = col_integer(),
  GWNO = col_integer(),
  INTER1 = col_integer(),
  INTER2 = col_integer(),
  INTERACTION = col_integer(),
  LATITUDE = col_character(),
  LONGITUDE = col_character(),
  TIME_PRECISION = col_integer(),
  YEAR = col_integer()
))
conflictsdata$LATITUDE <- gsub("[^0-9.-]", "", conflictsdata$LATITUDE)
conflictsdata$LONGITUDE[!grepl("^[0-9.]+$", conflictsdata$LONGITUDE)] <- NA
conflictsdata$LONGITUDE <- gsub("[^0-9.-]", "", conflictsdata$LONGITUDE)  # Remove bad characters
conflictsdata$LONGITUDE <- as.numeric(conflictsdata$LONGITUDE)  # Convert to numeric
conflictsdata$LONGITUDE[!grepl("^-?[0-9.]+$", conflictsdata$LONGITUDE)] <- NA  # Replace bad values

colnames(conflictsdata)  # Print all column names

# Summary of the datasets
glimpse(overall_poverty_est)

list.files("C:/Users/pc/Desktop/Counties Shape File/County.shp")
p_regions_map <- st_read("C:/Users/pc/Desktop/Counties Shape File/County.shp")
print(p_regions_map)


# preprocessing
preprocessDataset <- function(dataset) {
  dataset$residence_county = as.character(dataset$residence_county)
  dataset$residence_county = str_replace(dataset$residence_county,"Taita/Taveta","Taita Taveta")
  dataset$residence_county = str_replace(dataset$residence_county,"Tharaka-Nithi","Tharaka")
  dataset$residence_county = str_replace(dataset$residence_county,"Muranga","Murang'a")
  
  dataset <- dataset %>%
    rename(PovertyGap = `Poverty Gap (%)`)
  
  dataset$residence_county = as.character(dataset$residence_county)
  
  return(dataset)
}


overall_poverty_est <- preprocessDataset(overall_poverty_est)
food_poverty_est <- preprocessDataset(food_poverty_est)
hardcore_poverty_est <- preprocessDataset(hardcore_poverty_est)


overall_poverty_est %>%
  arrange(desc(PovertyGap)) %>%
  head(10) %>%
  select(residence_county,PovertyGap) %>%
  kable()


plotPovertyMap <- function(p_regions_map) {
  
  bins = c(0,2,6,8,11,13,25,40,100)
  
  pal = colorBin("YlOrRd", domain = p_regions_map$PovertyGap, bins = bins)
  
  
  labels = sprintf(
    "<strong>%s</strong><br/>%g",
    p_regions_map$COUNTY, p_regions_map$PovertyGap
  ) %>% lapply(htmltools::HTML)
  
  
  leaflet(data = p_regions_map) %>%  
    setView(lat = -0.0, lng = 36.681660, 6) %>%
    addPolygons(
      fillColor = ~pal(PovertyGap),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 9,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    
    
    
    addLegend(pal = pal, values = ~PovertyGap, opacity = 0.7, title = "Poverty Gap",
              position = "bottomleft")
}



plotPovertyMapForDS <- function(dataset) {
  # Read shapefile using sf
  p_regions_map <- st_read("C:/Users/pc/Desktop/Counties Shape File/County.shp")
  
  # Convert COUNTY column to character
  p_regions_map$COUNTY <- as.character(p_regions_map$COUNTY)
  
  # Join dataset with shapefile data
  p_regions_map <- left_join(p_regions_map, dataset, by = c("COUNTY" = "residence_county"))
  
  # Plot the map
  plotPovertyMap(p_regions_map)
}

plotPovertyMapForDS(overall_poverty_est)

png("poverty_map.png", width = 1200, height = 800, res = 150)
plotPovertyMapForDS(overall_poverty_est)
dev.off()


# food poverty Estimate
food_poverty_est %>%
  arrange(desc(PovertyGap)) %>%
  head(20) %>%
  select(residence_county,PovertyGap) %>%
  kable()
#food poverty Estimate in a Map
plotPovertyMapForDS(food_poverty_est)

png("food_poverty_map.png", width = 1200, height = 800, res = 150)
plotPovertyMapForDS(overall_poverty_est)
dev.off()

#hardcore poverty estimate
food_poverty_est %>%
  arrange(desc(PovertyGap)) %>%
  head(10) %>%
  select(residence_county,PovertyGap) %>%
  kable()

plotPovertyMapForDS(food_poverty_est)

png("hardcore_poverty_map.png", width = 1200, height = 800, res = 150)
plotPovertyMapForDS(overall_poverty_est)
dev.off()


#Kenya
#Loan in Kenya

# Check for rows with missing lat or lon values
invalid_coords <- country_loans[is.na(country_loans$lat) | is.na(country_loans$lon), ]
print(invalid_coords)

country_loans_clean <- country_loans[!is.na(country_loans$lat) & !is.na(country_loans$lon), ]
country_loans$lat <- as.numeric(country_loans$lat)
country_loans$lon <- as.numeric(country_loans$lon)

country_loans = loan_themes_region_ %>% 
  filter(country == "Kenya") %>%
  rename (themeType = `Loan Theme Type`) 


center_lon = median(country_loans$lon,na.rm = TRUE)
center_lat = median(country_loans$lat,na.rm = TRUE)


m <- leaflet(country_loans_clean) %>% addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat,radius = ~(amount/100) ,
             color = ~c("magenta"))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 5) 
m
# Save the map to an HTML file
saveWidget(m,file = "my_kenya_loans.html", selfcontained = TRUE)


#most dominat field partner
country_loans %>%
  rename(FieldPartnerName =`Field Partner Name`) %>%
  group_by(FieldPartnerName) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(FieldPartnerName = reorder(FieldPartnerName,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = FieldPartnerName,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = FieldPartnerName, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Field Partner Name', 
       y = 'Count', 
       title = 'Field Partner Name and Count') +
  coord_flip() +
  theme_bw()

#most popular sector
plotLoansAndSectorByCountry <- function(kiva_loans, countryName,fillColor2) {
  kiva_loans %>%
    filter(country == countryName) %>%
    group_by(sector) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(sector = reorder(sector,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = sector,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor2) +
    geom_text(aes(x = sector, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Sector', 
         y = 'Count', 
         title = 'Sector and Count') +
    coord_flip() +
    theme_bw()
}


png("most popular sector.png", width = 800, height = 600)

plotLoansAndSectorByCountry(kiva_loans,"Kenya",fillColor)
png("most popular sector2.png", width = 800, height = 600)


plotLoansAndSectorByCountry <- function(country_loans_clean, countryName,fillColor2) {
  country_loans_clean %>%
    filter(country == countryName) %>%
    group_by(sector) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(sector = reorder(sector,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = sector,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor2) +
    geom_text(aes(x = sector, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Sector', 
         y = 'Count', 
         title = 'Sector and Count') +
    coord_flip() +
    theme_bw()
}

plotLoansAndSectorByCountry(country_loans_clean,"Kenya",fillColor)


plotLoansAndActivityByCountry <- function(country_loans_clean, countryName,fillColor2) {
  country_loans_clean %>%
    filter(country == countryName) %>%
    group_by(activity) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(activity = reorder(activity,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = activity,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor2) +
    geom_text(aes(x = activity, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Activity', 
         y = 'Count', 
         title = 'Activity and Count') +
    coord_flip() +
    theme_bw()
}

plotLoansAndActivityByCountry(country_loans_clean,"Kenya",fillColor2)
