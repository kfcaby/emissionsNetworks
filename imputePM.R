rm(list=ls())

library(data.table)
library(geosphere)
library(lubridate)
library(tidyr)
library(mgcv)
library(maps)
library(maptools)
library(imputeTS)

source("./functions_emissions_networks.R")


load(file = "daily_emissions_facility_temperature.RData")
setkey(M_locations, ID)
setkey(PP_locations, ID)

imputePM <- function(monitorPM, plot.imputed = FALSE){
  monitorPM <- ts(monitorPM)
  first_nonNA <- min(which(!is.na(monitorPM)))
  first_nonNA <- ifelse(first_nonNA == 3, 1, first_nonNA)
  last_nonNA <-max(which(!is.na(monitorPM)))
  imp <- tryCatch({
    na.kalman(monitorPM[first_nonNA:last_nonNA])
    }, error = function(err) monitorPM[first_nonNA:last_nonNA]
  )
  imp <- c(rep(NA, first_nonNA-1), imp, rep(NA,length(monitorPM) - last_nonNA))
  if(plot.imputed == TRUE){
    plotNA.imputations(monitorPM,imp)
  }
  return(imp)
}

PM.imputed <- t(apply(PM, 1, function(x) imputePM(monitorPM = x)))

#save(list = "PM.imputed", file = "PM.imputed.RData")

