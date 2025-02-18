library(tidyverse)
library(lubridate)

library(stringr)


library(leaflet)
library(rgdal)

library(knitr)
library(DT)

library(caret)
library(forecast)
library(prophet)

install.packages("rgdal")

install.packages("caret")
install.packages("prophet")
install.packages("forecast")
install.packages("lattice")

library(knitr)
library(sf)

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
library(stringr)          # Load the package
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


overall_poverty_est <- as.data.frame(overall_poverty_est)
class(overall_poverty_est)
problems(overall_poverty_est)
st_geometry(overall_poverty_est)

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
