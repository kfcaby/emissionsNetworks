
library(data.table)
library(lubridate)
library(arepa)

source(file = "functions_emissions_networks.R")

start.date <- as.Date("2005-06-01")
end.date <- as.Date("2005-08-31")
PM.zipcode <- readRDS("/Users/kfcummiskey/Dropbox/ARP/Projects/Source Receptor Modeling/Cummiskey/Data/PREDICTIONGeneral2_PM25_zipcode_SUBSET_2000_2012.rds")
PM.zipcode <- subset(PM.zipcode, as.Date(rownames(PM.zipcode)) >= start.date & as.Date(rownames(PM.zipcode)) <= end.date) 
PM.zipcode <- PM.zipcode[ ,-which(colSums(is.na(PM.zipcode)) == nrow(PM.zipcode)) ]
PM.zipcode <- t(PM.zipcode)
rownames(PM.zipcode) <- paste("Z",rownames(PM.zipcode), sep="")


zipcode_locations <- get_zip_codes()
zipcode_locations <- zipcode_locations[ , .(zip, Longitude.zip,Latitude.zip, City.zip, State.zip)]
colnames(zipcode_locations) <- c("ID", "Longitude", "Latitude", "zip.city", "zip.state")
zipcode_locations$ID <- paste("Z",zipcode_locations$ID, sep = "")
setkey(zipcode_locations, ID)

#edges <- fitDailyPMmodels(2005,emissions, PM.zipcode, PP_locations, zipcode_locations, start.day = "06-01", end.day = "08-31")


