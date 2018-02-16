rm(list = ls())

library(data.table)
library(lubridate)
library(arepa)

#source(file = "R/functions_emissions_networks.R")
source(file = "R/fitDailyPMmodels.R")
#Parameters
#----------------------------------------------------------------------------------------
#time period of interest
start.date <- "2005-01-01"
end.date <- "2005-12-31"
season <- "year_max5000_temperature"

# raw PM or decomposed PM
PM.type <- "decomposed75" #c("raw","decomposedXX")

unit.type <- "monitor" #c("zipcode","monitor") #perform a monitor or zipcode analysis

#Other parameters
max.distance = 5000 #max distance between edges in km
k1 = 16 
num.processes = 50 #number of cores to run on
receptor.regions <- c("IndustrialMidwest","Northeast", "Southeast", 
                      "Northwest", "SouthernCalifornia","Southwest",
                      "UpperMidwest")

adjust.temp = TRUE

#convert NAs in the dataset to zeros
NAtoZero = TRUE
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

#Remove observations outside region of interest
PM <- PM[receptor_locations$receptor.region %in% receptor.regions,] #uncomment to remove some regions
receptor_locations <- receptor_locations[rownames(PM),]


#Load emissions
emissions <- fread("data/daily.emissions.csv")
power.plants <- emissions$V1
emissions[ , V1 := NULL]
emissions <- as.matrix(emissions)
rownames(emissions) <- power.plants

if(NAtoZero == TRUE){
  emissions[is.na(emissions)] <- 0
}

#Load power plant locations
powerplant_locations <- fread("data/powerplant.locations.csv")[ , V1 := NULL]
setkey(powerplant_locations, ID)
powerplant_locations <- powerplant_locations[rownames(emissions),]

#Load temperature
temperature <- fread("data/temperature_complete.csv") 
receptors <- temperature$V1
temperature[ , V1 := NULL]
temperature <- as.matrix(temperature)
rownames(temperature) <- receptors
temperature <- temperature[rownames(PM),]
colnames(temperature) <- colnames(PM)

#determine which monitors to fit during this process
process <- as.integer(as.character(commandArgs(trailingOnly = TRUE))) 
#process <- 0
monitors.per.process <- ceiling(nrow(PM)/num.processes)
breaks <- seq(from = 1,by = monitors.per.process, length.out = num.processes + 1)
process.assignments <- findInterval(1:nrow(PM), breaks) - 1
monitors.this.process <- which(process.assignments == process)

if(length(monitors.this.process) > 0){
  
  edges <- fitDailyPMmodels(emissions, PM[monitors.this.process,], powerplant_locations, receptor_locations,temperature, 
                            start.date, end.date, k1 = k1, max.distance = max.distance, receptor.regions = receptor.regions,
                            adjust.temp = adjust.temp)
  
  print(monitors.this.process)
  this.file <- paste(unit.type,"_networks/output_",season,"_",PM.type,"/edges",process,".csv", sep="")
  write.csv(edges, file = this.file)
}

