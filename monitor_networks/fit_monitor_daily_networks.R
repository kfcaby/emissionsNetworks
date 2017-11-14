rm(list = ls())

library(data.table)
library(lubridate)
library(arepa)

source(file = "../functions_emissions_networks.R")
source(file = "../fitDailyPMmodels.R")
load(file = "../data/daily_emissions_facility_temperature.RData")

#Parameters

#time period of interest
start.date <- "2005-09-01"
end.date <- "2005-11-30"
season <- "fall"
PM.type <- "decomposed" #c("raw","decomposed")

#Other parameters
max.distance = 1000 #max distance between edges
k1 = 5
wind.speed = 13 #kph
include.west <- FALSE
num.processes = 100 #number of cores to run on

#Load PM
s
PM.file = paste("../data/PM.daily.monitor2005_",PM.type, ".csv", sep = "") 
PM.zipcode <- fread(PM.file) 
zips <- PM.zipcode$V1
PM.zipcode[ , V1 := NULL]
PM.zipcode <- as.matrix(PM.zipcode)
rownames(PM.zipcode) <- zips

zipcode_locations <- fread("../data/zipcode.locations.csv")[ ,V1:=NULL]
setkey(zipcode_locations,ID)
zipcode_locations <- zipcode_locations[rownames(PM.zipcode),]

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
  
  edges <- fitDailyPMmodels(emissions, PM.zipcode[monitors.this.process,], PP_locations, zipcode_locations, 
                            start.date, end.date, k1 = k1, wind.speed = wind.speed, max.distance = max.distance)
  
  this.file <- paste("output_",season,"_",PM.type,"/edges2005_zipcode",process,".csv", sep="")
  write.csv(edges, file = this.file)
}

