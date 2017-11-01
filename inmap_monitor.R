#This code takes the zipcode level output from rinmap and obtains
#monitor level inmap estimates by finding the nearest
#zipcode centriod to each monitor.

rm(list=ls())

library(data.table)
library(geosphere)
library(lubridate)
library(tidyr)
library(mgcv)
library(maps)
library(maptools)
library(arepa)
library(ggmap)
source("./functions_emissions_networks.R")

##-----------------------------------------------------------------------##
##            Create Emissions Networks                                  ##
##-----------------------------------------------------------------------##

load(file = "daily_emissions_facility_temperature.RData")
setkey(M_locations, ID)
setkey(PP_locations, ID)
get_zip_codes()
zipcode <- data.table(zipcode)
setkey(zipcode, "zip")

#format InMAP matrix a little differently
inmap_temp <- read.csv("inmap_by_facility.csv")
inmap <- as.matrix(inmap_temp[,7:ncol(inmap_temp)])
rownames(inmap) <- convertZip(inmap_temp$ZIP)
colnames(inmap) <- paste("PP",gsub('\\D','',colnames(inmap)),sep = "")

#link each monitor to the nearest zipcode
zipcode <- zipcode[complete.cases(zipcode),]
zip_monitor_linkage <- spatial_link_index(
                   zipcode, "latitude", "longitude", "zip",
                   M_locations, "Latitude", "Longitude", "ID",
                   closest = TRUE)
inmap <- data.table(inmap, keep.rownames = TRUE)
colnames(inmap)[1] <- "zip"
setkey(inmap, "zip")
inmap_monitor <- inmap[as.character(zip_monitor_linkage$zip),]
inmap_monitor[, zip := NULL]
inmap_monitor <- as.matrix(inmap_monitor)
rownames(inmap_monitor) <- zip_monitor_linkage$ID

inmap_long <- data.table(melt(inmap_monitor))
colnames(inmap_long) <- c("Monitor","PP", "inmapPM")
head(inmap_long)

write.csv(inmap_long, file = "inmapPM.csv")

#FINISH THIS TOMORROW

