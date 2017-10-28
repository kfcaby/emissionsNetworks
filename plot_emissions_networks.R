rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks")

library(data.table)
library(lubridate)
library(geosphere)
library(maps)
library(maptools)

source("./functions_emissions_networks.R")

load(file = "../Data/daily_emissions_facility_temperature.RData")
load(file = "./edges2003_2016_1000max2.RData")
setkey(M_locations, ID)
setkey(PP_locations, ID)

##-----------------------------------------------------------------------##
##            Plot Emissions Networks                                    ##
##-----------------------------------------------------------------------##

cutoff.perc = 0.70

mapply(function(x,i) {
  plotEmissionsNetwork(edges = x, emissions, 
                       PM, PP_locations, M_locations,
                       main = paste("Network Plot, Summer",i, sep = " "),
                       plot.diagnostics = TRUE,
                       plot.percent.of.powerplants = 100)
}, edges, names(edges))




#pdf(file = "./SRmapping2.pdf")
par(mfrow = c(2,1), mar = c(1,1,1,1))
mapply(function(x,i) {
  layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))
  plotEmissionsNetwork(edges = x, emissions, 
                       PM, PP_locations, M_locations,
                       main = paste("Network Plot, Summer",i, sep = " "),
                       plot.diagnostics = TRUE,
                       plot.percent.of.powerplants = 100)
  par(mfrow = c(1,1))
  plotHighLowMap(x, PP_locations, M_locations, cutoff.perc = cutoff.perc,
                 main = paste("High/Low Plot, Summer",i,sep = " "))
  }, edges, names(edges))
par(mfrow = c(1,1))
#dev.off()
