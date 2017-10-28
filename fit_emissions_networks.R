
rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks")

library(data.table)
library(geosphere)
library(lubridate)
library(tidyr)
library(mgcv)
source("./functions_emissions_networks.R")

##-----------------------------------------------------------------------##
##            Create Emissions Networks                                  ##
##-----------------------------------------------------------------------##

load(file = "../Data/daily_emissions_facility_temperature.RData")
setkey(M_locations, ID)
setkey(PP_locations, ID)

#time period of interest
years <- 2005:2006
start.day <- "06-01" #MM-DD
end.day <- "08-31"   #MM-DD

#Other parameters
max.distance = 1000 #max distance between edges
percent.of.powerplants = 10 #percent of power plants to use
k1 = 3
wind.speed = 13 #kph


edges <- lapply(years, fitDailyPMmodels, emissions, PM, PP_locations, M_locations,
                       start.day, end.day, percent.of.powerplants = percent.of.powerplants, 
                       k1 = k1, wind.speed = wind.speed, 
                       max.distance = max.distance)
names(edges) <- years


#save(list = "edges", file = "./edges2003_2016_1000max2.RData")
