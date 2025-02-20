library(tidyverse)
library(lubridate)

library(stringr)

install.packages("rgdal")

install.packages("caret")
install.packages("prophet")
install.packages("forecast")
install.packages("lattice")

library(leaflet)
library(rgdal)

library(knitr)
library(DT)

library(caret)
library(forecast)
library(prophet)


library(knitr)
library(sf)


overall_poverty_est
glimpse(overall_poverty_est)
summary(overall_poverty_est)


rm(list=ls())


fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

fillColorLightCoral = "#F08080"


library(readr)
hardcore_poverty_est <- read_csv("D:/Downloads/hardcore_poverty_est.xls")
View(hardcore_poverty_est)


install.packages("stringr") # Install if not already installed
library(stringr)         # Load the package
library(dplyr)


preprocessDataset <- function(dataset) {
  dataset$residence_county = as.character(dataset$residence_county)
  dataset$residence_county = str_replace(dataset$residence_county, "Taita/Taveta", "Taita Taveta")
  dataset$residence_county = str_replace(dataset$residence_county, "Tharaka-Nithi", "Tharaka")
  dataset$residence_county = str_replace(dataset$residence_county, "Muranga", "Murang'a")
  
  #dataset <- dataset %>%
    #rename(PovertyGap = 'Poverty Gap (%')
  
  dataset$residence_county = as.character(dataset$residence_county)
  
  
  return(dataset)
}



overall_poverty_est <- preprocessDataset(overall_poverty_est)
food_poverty_est <- preprocessDataset(food_poverty_est)
hardcore_poverty_est <- preprocessDataset(hardcore_poverty_est)

#overall poverty estimate
overall_poverty_est %>%
  arrange(desc(`Poverty Gap (%)`)) %>%
  head(10)  %>%
  select(residence_county, `Poverty Gap (%)`) %>%
  kable()

names(overall_poverty_est)


plotPovertyMap <- function(p_regions_map) {
  
  bins = c(0,2,6,8,11,13,25,40,100)
  
  pal = colorBin("YlOrRd", domain = p_regions_map@data$PovertyGap, bins = bins)
  
  
  labels = sprintf(
    "<strong>%s</strong><br/>%g",
    p_regions_map@data$COUNTY, p_regions_map@data$PovertyGap
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
  p_regions_map <- readOGR(dsn = "../input/kenya-counties-shapefile/County.shp")
  p_regions_map@data$COUNTY = as.character(p_regions_map@data$COUNTY)
  p_regions_map@data = left_join(p_regions_map@data, dataset, by = c("COUNTY" = "residence_county"))
  plotPovertyMap(p_regions_map)
}

plotPovertyMapForDS(overall_poverty_est)

overall_poverty_est_sf <- st_as_sf(overall_poverty_est, coords = c("lon", "lat"), crs = 4326)


overall_poverty_est <- as.data.frame(overall_poverty_est)
class(overall_poverty_est)
problems(overall_poverty_est)
st_geometry(overall_poverty_est)

library(sf)

overall_poverty_sf <- st_as_sf(overall_poverty_est, coords = c("longitude", "latitude"), crs = 4326)
plotPovertymapDS(overall_poverty_sf)


#overall poverty estimate in a map
plotPovertymap <- function(p_regions_map) {
  bins = c(0,2,6,8,11,13,25,40,100)
  
  pal = colorBin("Y10rRd", domain = p_regions_map@data@PovertyGad, bins = bins)
  
  labels = spintf(
    "<strong>%s</strong><br/>%g",
    p_regions_map@data$COUNTY, p_regions_map@data$PovertyGap) %>% lapply(htmltools::HTML)
  
  leaflet(data = p_regions_map) %>%
    setView(lat = -0.0, lng = 36.681660, 6) %>%
    addPolygons(fillColor = ~pal(PovertyGap),
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
    
    addLegend(pal = pal, values = ~PovertyGap, opacity = 0.7, title = "PovertyGap",
              position = "bottomleft")
}

plotPovertymapDS <- function(dataset) {
  p_regions_map <- read_csv("D:/Downloads/County.shp.zip")
  p_regions_map@data$COUNTY = as.character(p_regions_map@data$COUNTY)
  p_regions_map@data = left_join(p_regions_map@data, dataset, by = c("COUNTY" = "residence_county"))
  plotPovertymap(p_regions_map)
}

plotPovertymapDS(overall_poverty_est)


problems(data)


plotPovertymap <- function(p_regions_map){
  bins = c(0,2,6,8,11,13,25,40,100)
  
  pal = colorBin("YlorRD", domain = p_regions_map@data$PovertyGap, bins = bins)
  
  
  labels = sprintf(
    "<strong>%s</strong><br/>%g",
    p_regions_map@data$COUNTY, pd_regions_map@data$PovertyGap) %>% apply(htmltools::HTML
  )
  
  leaflet(data = p_regions_map) %>% 
    setView(lat = -0.0, lng = 36.681,660,6) %>%
    addPolygons(
      fillColor = ~pal(PovertyGap),
      weight =2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 9,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = 0.7,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal",
                       padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
        
        addLegend(pal = pal, values =  ~PovertyGap, opacity=0.7,title="Poverty Gap",
                  position = "bottomleft")
      
    )
}


plotPovertyMapForDS <- function(dataset){
  p_regions_map <- readOGR(dsn = ("D:/Downloads/County.shp.zip")
                           p_regions_map@data$COUNTY = as.character(p_regions_map@data$COUNTY)
          p_regions_map@data =  left_join(p_regions_map@data, dataset, by = c("COUNTY" = "residency_county"))
                           
          plotPovertymap(p_regions_map)
}


summary(country_loans$lon)
summary(country_loans$lat)
problematic_rows <- country_loans[is.na(country_loans$lon) | is.na(country_loans$lat), ]
head(problematic_rows)

valid_country_loans <- country_loans %>% 
  filter(!is.na(lon) & !is.na(lat))


leaflet(valid_country_loans) %>% 
  addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat, radius = ~(amount/100),
             color = ~"blue") %>%
  setView(lng = center_lon, lat = center_lat, zoom = 5)


install.packages("mapview")
library(mapview)
mapshot(m, file = "leaflet_plot.png")


plotPovertyMapForDS(overall_poverty_est)
themes_region <-  read_csv("C:/Users/pc/Desktop/loan_themes_by_region.csv")


country_loans = themes_region %>% 
  filter(country == "Kenya") %>%
  rename (themeType = `Loan Theme Type`) 


center_lon = median(country_loans$lon,na.rm = TRUE)
center_lat = median(country_loans$lat,na.rm = TRUE)


leaflet(country_loans) %>% addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat,radius = ~(amount/100) ,
             color = ~c("blue"))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 5) 
