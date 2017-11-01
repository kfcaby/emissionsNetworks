rm(list=ls())

library(data.table)
library(geosphere)
library(lubridate)
library(tidyr)
library(mgcv)
library(maps)
library(maptools)
source("functions_emissions_networks.R")
load(file = "PMimputed.RData")
load(file = "../decomposePM/PMimputedlowfreq.RData")
##-----------------------------------------------------------------------##
##            Create Emissions Networks                                  ##
##-----------------------------------------------------------------------##

#time period of interest
start.day <- "06-01" #MM-DD
end.day <- "08-31"   #MM-DD

#Other parameters
max.distance = 1000 #max distance between edges
percent.of.powerplants = 100 #percent of power plants to use
k1 = 5
wind.speed = 13 #kph

edges <- fitDailyPMmodels(2005, emissions, PM, PP_locations, M_locations, 
                          start.day, end.day, percent.of.powerplants = percent.of.powerplants, 
                          k1 = k1, wind.speed = wind.speed, max.distance = max.distance)

edges_imputed <- fitDailyPMmodels(2005, emissions, PM.imputed, PP_locations, M_locations, 
                          start.day, end.day, percent.of.powerplants = percent.of.powerplants, 
                          k1 = k1, wind.speed = wind.speed, max.distance = max.distance)

edges_lowfreq <- fitDailyPMmodels(2005, emissions, PM.imputed.lowfreq3, PP_locations, M_locations, 
                          start.day, end.day, percent.of.powerplants = percent.of.powerplants, 
                          k1 = k1, wind.speed = wind.speed, max.distance = max.distance)

save(list = c("edges","edges_imputed","edges_lowfreq"), file = "edges2005_imputed_decomposed.RData")
