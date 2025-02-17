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
Kenya_Counties <- read_csv("D:/Downloads/Kenya-Counties.csv")
View(Kenya_Counties)

kenyashape <- fortify(Kenya_Counties)





popdf <- merge(kenyashape, cholera_county)


#visualization

Map <- ggplot(popdf, aes(long, lat, group = group, label = county)) +
  geom_polygon() + 
  coord_equal() + 
  labs(fill = 'date_of_onset_of_diseases.1') +
  theme_minimal(base_size = 12) + 
  theme(text=element_text(size=15),
        plot.caption = element_text(size=10, hjust=0.5,
                                    face='italic',color='blue')) +
  labs(caption = "Data Source ")


Map <- Map + scale_fill_gradient(low='gray',high = 'red')
Map






