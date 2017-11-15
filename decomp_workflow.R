rm(list=ls())

library(data.table)
library(geosphere)
library(lubridate)

source(file = "R/decomposePM.R")

#Parameters
#-----------------------------------------------------------------------------
unit.type = "monitor"
numLevels = 4
lowest.level = 3
#-----------------------------------------------------------------------------

#Load PM
PM.file = paste("data/PM.daily.",unit.type,"2005_raw.csv", sep = "")
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

PMdecomposed <- decomposePM(PM, receptor_locations, numLevels = numLevels, lowest.level = lowest.level)

output.file = paste("data/PM.daily.",unit.type,"2005_decomposed",numLevels,lowest.level, sep ="")
write.csv(PMdecomposed, file = output.file)
