rm(list = ls())

library(data.table)
library(lubridate)
library(arepa)

source(file = "../functions_emissions_networks.R")

PM.zipcode <- fread("../data/PM.daily.zipcode2005.csv")
zips <- PM.zipcode$V1
PM.zipcode[ , V1 := NULL]
PM.zipcode <- as.matrix(PM.zipcode)
rownames(PM.zipcode) <- zips

zipcode_locations <- fread("../data/zipcode.locations.csv")[ ,V1:=NULL]
setkey(zipcode_locations,ID)
zipcode_locations <- zipcode_locations[rownames(PM.zipcode),]

#time period of interest
start.day <- "06-01"
end.day <- "08-31"
include.west <- FALSE

#Other parameters
max.distance = 1000 #max distance between edges
percent.of.powerplants = 100 #percent of power plants to use
k1 = 5
wind.speed = 13 #kph
num.processes = 100 #number of cores to run on

#load decomposed PM
# PMdecomposed <- fread(file = "PMdecomposed2005.csv")
# monitors <- PMdecomposed$V1
# PMdecomposed <- as.matrix(PMdecomposed[ ,-1])
# rownames(PMdecomposed) <- monitors

#Remove observations in the west
if(include.west == FALSE){
  PM.zipcode <- PM.zipcode[zipcode_locations$Longitude > -100,]
  zipcode_locations <- zipcode_locations[rownames(PM.zipcode),]
}

#determine which monitors to fit during this process
process <- as.integer(as.character(commandArgs(trailingOnly = TRUE))) 
#process <- 0
monitors.per.process <- ceiling(nrow(PM.zipcode)/num.processes)
breaks <- seq(from = 1,by = monitors.per.process, length.out = num.processes + 1)
process.assignments <- findInterval(1:nrow(PM.zipcode), breaks) - 1
monitors.this.process <- which(process.assignments == process)

if(length(monitors.this.process) > 0){
  
  edges <- fitDailyPMmodels(2005, emissions, PM.zipcode[monitors.this.process,], PP_locations, zipcode_locations, 
                            start.day, end.day, percent.of.powerplants = percent.of.powerplants, 
                            k1 = k1, wind.speed = wind.speed, max.distance = max.distance)
  
  write.csv(edges, file = paste("output/edges2005summer_PMzipcode",process,".csv", sep = ""))
}

