rm(list=ls())

library(data.table)
library(geosphere)
library(lubridate)
library(tidyr)
library(mgcv)
source("functions_emissions_networks.R")
##-----------------------------------------------------------------------##
##            Create Emissions Networks                                  ##
##-----------------------------------------------------------------------##

#time period of interest
start.day <- "06-01"
end.day <- "08-31"


#Other parameters
max.distance = 1000 #max distance between edges
percent.of.powerplants = 100 #percent of power plants to use
k1 = 5
wind.speed = 13 #kph

PMdecomposed <- fread(file = "PMdecomposed2005.csv")
monitors <- PMdecomposed$V1
PMdecomposed <- as.matrix(PMdecomposed[ ,-1])
rownames(PMdecomposed) <- monitors


edges <- fitDailyPMmodels(2005, emissions, PMdecomposed, PP_locations, M_locations, 
                          start.day, end.day, percent.of.powerplants = percent.of.powerplants, 
                          k1 = k1, wind.speed = wind.speed, max.distance = max.distance)

write.csv(edges, file = "edges2005summer_PMdecomposed.csv")
