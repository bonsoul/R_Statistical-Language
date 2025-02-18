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

glimpse(overall_poverty_est)
summary(overall_poverty_est)


rm(list=ls())


fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

fillColorLightCoral = "#F08080"


