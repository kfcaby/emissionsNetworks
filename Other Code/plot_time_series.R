rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks")

library(data.table)
library(lubridate)
load(file = "data/daily_emissions_facility_temperature.RData")
source(file = "R/functions_emissions_networks.R")
source(file = "R/fitDailyPMmodels.R")
source(file = "Other Code/imputePM.R")

PM.impute <- t(apply(PM,1,imputePM)) 
colnames(PM.impute) <- colnames(PM)

sink(file = "Other Plots/model_output.txt")
pdf("Other Plots/timeseries.pdf", height = 7, width = 5)
model <- get_gams_model("PP1733", "M26163-0016", emissions, PM.impute, M_locations, PP_locations, 
                        start.day = "06-01", end.day = "08-31", year = 2005, return.summary = TRUE, 
                        return.plots = TRUE, return.map = FALSE, wind.speed = 13, k1 = 5) 
dev.off()
sink()

#pdf("Other Plots/example_map.pdf", height = 9, width = 22)
model <- get_gams_model("PP1733", "M26163-0016", emissions, PM.impute, M_locations, PP_locations, 
                        start.day = "06-01", end.day = "08-31", year = 2005, return.summary = TRUE, 
                        return.plots = FALSE, return.map = TRUE, wind.speed = 13, k1 = 5) 
#dev.off()