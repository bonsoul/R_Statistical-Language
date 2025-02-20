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


library(htmlwidgets)
saveWidget(m, file = "leaflet_plot.html", selfcontained = TRUE)

library(leaflet)


m <- leaflet(valid_country_loans) %>%
  addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat, radius = ~(amount/100), color = "blue") %>%
  setView(lng = center_lon, lat = center_lat, zoom = 5)

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

library(dplyr)       # or
library(tidyverse)   # which loads dplyr, ggplot2, and more


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


loans <- read_csv("D:/Downloads/kiva_loans.csv.zip")

plotLoansAndSectorByCountry <- function(loans, countryName,fillColor2) {
  loans %>%
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


plotLoansAndSectorByCountry(loans,"Kenya",fillColor)

plotLoansAndActivityByCountry <- function(loans, countryName,fillColor2) {
  loans %>%
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


plotLoansAndActivityByCountry(loans,"Kenya",fillColor2)



plotLoansAndUseByCountry <- function(loans, countryName,fillColor2) {
  loans %>%
    filter(country == countryName) %>%
    filter(!is.na(use)) %>%
    group_by(use) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(use = reorder(use,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = use,y = Count)) +
    geom_bar(stat='identity',colour="white", fill = fillColor2) +
    geom_text(aes(x = use, y = 1, label = paste0("(",Count,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Use of Loans', 
         y = 'Count', 
         title = 'Use of Loans and Count') +
    coord_flip() +
    theme_bw() 
}


plotLoansAndUseByCountry(loans,"Kenya",fillColorLightCoral)


fundedLoanAmountDistribution <- function(loans)
{
  loans %>%
    ggplot(aes(x = funded_amount) )+
    scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) + 
    geom_histogram(fill = fillColor2,bins=50) +
    labs(x = 'Funded Loan Amount' ,y = 'Count', title = paste("Distribution of", "Funded Loan Amount")) +
    theme_bw()
}


country_loans = loans %>%
  filter(country == "Kenya")

fundedLoanAmountDistribution(country_loans)


summary(country_loans$funded_amount)


loansData = loans %>%
  filter(country == "Kenya") %>%
  filter(!is.na(funded_time)) %>%
  mutate(year = year(ymd_hms(funded_time))) %>%
  mutate(month = month(ymd_hms(funded_time))) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(month)) %>%
  group_by(year,month) %>%
  summarise(Count = n()) %>%
  mutate(YearMonth = make_date(year=year,month=month) ) 

loansData %>%
  ggplot(aes(x=YearMonth,y=Count,group = 1)) +
  geom_line(size=1, color="red")+
  geom_point(size=3, color="red") +
  labs(x = 'Time', y = 'Count',title = 'Trend of loans') +
  theme_bw() 


datatable(loansData, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


install.packages("DT")
library(DT)

library(dplyr)

install.packages("forecast")  # Install if you haven't already
library(forecast) 

library(ggplot2)
loansData2 =loansData[1:nrow(loansData)-1,] %>% select(Count)
loansData2 <- loansData %>%
  ungroup() %>%  # Remove grouping
  slice(1:(n() - 1)) %>%  # Alternatively, use slice to avoid indexing issues
  select(Count)

tsLoans = ts(loansData2$Count)

fit <- auto.arima(tsLoans)

preds = forecast(fit, h = 5)

preds %>% autoplot(include=42) +theme_bw()

predictions = round(as.numeric(preds$mean))

cat("The predictions are ","\n")

predictions


fit <- ets(tsLoans)

preds = forecast(fit, h = 5)

preds %>% autoplot(include=42) +theme_bw()
