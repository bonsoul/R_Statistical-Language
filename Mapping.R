# #Load packages ---------------------------------------------------library(tidyverse)

install.packages(c("sf", "sp", "rgdal", "tmap", "ggplot2"))


library(sf)
library(tmap)
library(ggplot2)
library(plotly)


install.packages("plotly")

World1 <- map_data("world")


ggplot(World1, aes(x=long, y=lat, group=group))+
  geom_polygon(fill="green",color = "black")


ggplot(World1, aes(x=long, y=lat,group=group,)) +
  geom_polygon(fill="blue", color="black") +
  coord_map(xlim=c(33,42), ylim=c(-5,5.5)) + 
  xlab("") +
  ylab("")


library(rgdal) #reading shapefiles
library(plotly) # visualization

library(readr)
cholera_data1<- read_csv("D:/Downloads/cleaned_cholera_data.csv")
colnames(cholera_data1)


library(dbplyr)
cholera_data1 <- cholera_data1 %>%
  select(-County,-'Sub County')
colnames(cholera_data1
)

#seting names
cholera_county <- setNames(cholera_data1, c('county','sub_county','ward'))


#loading the shapefiles

library(readr)
kenya <- read_csv("D:/Downloads/KEN_adm4.csv")
View(kenya)


kenyashape <- fortify(kenya)
colnames(kenyashape)

popdf <- merge(kenyashape, cholera_county, by.x='id',by.y = 'PID')
