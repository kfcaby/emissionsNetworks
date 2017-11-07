rm(list=ls())

library(data.table)
library(geosphere)
library(lubridate)

source(file = "functions_emissions_networks.R")

start.date <- "2005-01-01"
end.date <- "2005-12-31"
start.index <- which(colnames(PM) == start.date)
end.index <- which(colnames(PM) == end.date)

PMimputed <- t(apply(PM[ ,start.index:end.index],1, imputePM))
colnames(PMimputed) <- colnames(PM[ ,start.index:end.index])

PMdecomposed <- decomposePM(PMimputed, M_locations, numLevels = 4, lowest.level =3)

write.csv(PMimputed, file = "PMimputed2005.csv")
write.csv(PMdecomposed, file = "PMdecomposed2005.csv")