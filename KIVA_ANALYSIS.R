file.exists("KIVA_ANALYSIS.R")

while (sink.number() > 0) {
  sink(NULL)
}


rmarkdown::render("KIVA_ANALYSIS.R", output_format = "html_document")

rmarkdown::render("KIVA_ANALYSIS.R", output_format = "html_document", quiet = FALSE)




library(tidyverse)
library(lubridate)
library(rmarkdown)



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



colnames(kiva_loans)



# Summary of the datasets
glimpse(overall_poverty_est)



unzip("D:/Downloads/archive(5).zip", list = TRUE)

unzip("D:/Downloads/archive(5).zip", exdir = "D:/Downloads/extracted/")


list.files("D:/Downloads/extracted/")

p_regions_map <- st_read("D:/Downloads/extracted/County.shp")
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
  p_regions_map <- st_read("D:/Downloads/extracted/County.shp")
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

kiva_loans  %>%
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


#mostpopular sector

kiva_loans %>%
  group_by(activity) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(activity = reorder(activity,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = activity,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = activity, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Activity', 
       y = 'Count', 
       title = 'Activity and Count') +
  coord_flip() +
  theme_bw()


#popular use of loans
kiva_loans %>%
  mutate(use = trimws(use)) %>%
  filter(!is.na(use)) %>%
  group_by(use) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(use = reorder(use,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = use,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColorLightCoral) +
  geom_text(aes(x = use, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Use of Loans', 
       y = 'Count', 
       title = 'Use of Loans and Count') +
  coord_flip() +
  theme_bw() 



#distribution of funded loans

country_loans = kiva_loans %>%
  filter(country == "Kenya")

fundedLoanAmountDistribution(country_loans)


#trends of loans
loansData = kiva_loans %>%
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


#arima model
loansData2 = loansData[1:nrow(loansData)-1,] %>% select(Count)

tsLoans = ts(loansData2$Count)

fit <- auto.arima(tsLoans)

preds = forecast(fit, h = 5)

preds %>% autoplot(include=42) + theme_bw()

fit <- ets(tsLoans)

preds = forecast(fit, h = 5)

preds %>% autoplot(include=42) +theme_bw()

autoplot(preds, include=42) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma, limits = c(0, max(preds$upper, na.rm = TRUE))) +
  ggtitle("ETS Forecast of Loan Counts") +
  ylab("Number of Loans") +
  xlab("Time")


loansData2 =loansData[1:nrow(loansData)-1,] %>% 
  ungroup %>%
  select(YearMonth,Count)

colnames(loansData2) = c("ds","y")

m <- prophet(loansData2,changepoint.prior.scale = 0.1)

future <- make_future_dataframe(m, periods = 5,freq = "month")

forecast <- predict(m, future)

predictions = tail(round(forecast$yhat),5)

plot(m, forecast)


cat("The predictions are ","\n")

predictions
