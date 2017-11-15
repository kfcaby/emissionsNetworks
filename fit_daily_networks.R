rm(list = ls())

library(data.table)
library(lubridate)
library(arepa)

#source(file = "R/functions_emissions_networks.R")
source(file = "R/fitDailyPMmodels.R")
#Parameters
#----------------------------------------------------------------------------------------
#time period of interest
start.date <- "2005-06-01"
end.date <- "2005-08-31"
season <- "summer"

# raw PM or decomposed PM
PM.type <- "decomposed43" #c("raw","decomposedXX")

unit.type <- "monitor" #c("zipcode","monitor") #perform a monitor or zipcode analysis

#Other parameters
max.distance = 1000 #max distance between edges
k1 = 5
wind.speed = 13 #kph
include.west <- FALSE
num.processes = 10 #number of cores to run on
#----------------------------------------------------------------------------------------

#Load PM
PM.file = paste("data/PM.daily.",unit.type,"2005_",PM.type, ".csv", sep = "") 
PM <- fread(PM.file) 
receptors <- PM$V1
PM[ , V1 := NULL]
PM <- as.matrix(PM)
rownames(PM) <- receptors

#Load receptor locations
receptor.locations.file <- paste("data/",unit.type,".locations.csv",sep ="")
receptor_locations <- fread(receptor.locations.file)[ ,V1:=NULL]
setkey(receptor_locations,ID)
receptor_locations <- receptor_locations[rownames(PM),]

#Load emissions
emissions <- fread("data/daily.emissions.csv")
power.plants <- emissions$V1
emissions[ , V1 := NULL]
emissions <- as.matrix(emissions)
rownames(emissions) <- power.plants

#Load power plant locations
powerplant_locations <- fread("data/powerplant.locations.csv")[ , V1 := NULL]
setkey(powerplant_locations, ID)
powerplant_locations <- powerplant_locations[rownames(emissions),]

#Remove observations in the west
if(include.west == FALSE){
  PM <- PM[receptor_locations$Longitude > -100,]
  receptor_locations <- receptor_locations[rownames(PM),]
}


#determine which monitors to fit during this process
process <- as.integer(as.character(commandArgs(trailingOnly = TRUE))) 
#process <- 0
monitors.per.process <- ceiling(nrow(PM)/num.processes)
breaks <- seq(from = 1,by = monitors.per.process, length.out = num.processes + 1)
process.assignments <- findInterval(1:nrow(PM), breaks) - 1
monitors.this.process <- which(process.assignments == process)

if(length(monitors.this.process) > 0){
  
  edges <- fitDailyPMmodels(emissions, PM[monitors.this.process,], powerplant_locations, receptor_locations, 
                            start.date, end.date, k1 = k1, wind.speed = wind.speed, max.distance = max.distance)
  
  this.file <- paste(unit.type,"_networks/output_",season,"_",PM.type,"/edges",process,".csv", sep="")
  write.csv(edges, file = this.file)
}

