# Basic functions for manipulating emissions/PM time series and creating an emissions network

source(file = "fitDailyPMmodels.R")
source(file = "plotEmissionsNetwork.R")
load(file = "daily_emissions_facility_temperature.RData")
setkey(M_locations, ID)
setkey(PP_locations, ID)


#convert a ZIP code from 3-digit to 5-digit format
convertZip <- function(zip){
  temp_zip<-formatC(zip, width = 5, format = "d", flag = "0")
  zipcode<-unlist(lapply(temp_zip, function(x) as.numeric(paste(strsplit(as.character(x),"")[[1]],collapse=""))))
  zipcode<-formatC(zipcode, width = 5, format = "d", flag = "0")
  return(zipcode)
}

logNA <- function(x){
  return(ifelse(x > 0, log(x), 0))
}

#function takes a vector of monitor IDs and powerplant IDs and returns
#the gams model for each along with summary plots
get_gams_model <- Vectorize(function(powerplant, monitor, emissions, PM, M_locations, PP_locations,
                                     start.day = "06-01", end.day = "08-31", year = 2005, return.summary = TRUE,
                                     return.plots = TRUE, wind.speed = 13, k1 = 5){
  
  lag.breaks <- (1:20)*24*wind.speed
  
  start.date <- as.Date(paste(year,"-",start.day, sep = ""))
  end.date <- as.Date(paste(year,"-",end.day,sep = ""))
  
  #Subset emissions and PM by start and end date
  emissions <- emissions[ ,as.Date(colnames(emissions)) >= start.date & as.Date(colnames(emissions)) <= end.date]
  PM <- PM[ ,as.Date(colnames(PM)) >= start.date & as.Date(colnames(PM)) <= end.date]
  
  M_locations <- M_locations[rownames(PM), ]
  PP_locations <- PP_locations[rownames(emissions),]
  
  distance <- distm(M_locations[monitor, c(2:3)],PP_locations[powerplant, c(2:3)])/1000
  lag <- findInterval(distance,lag.breaks)
  dataset <- make_dataset(emissions, PM, monitor, powerplant, lag)
  model <- gam(log(y) ~ s(time, bs = "cr", k = k1) + x + weekdays(as.Date(date)), 
               data = dataset, family = gaussian, na.action = na.omit)
  if(return.summary == TRUE){
    print(summary(model))
  }
  if(return.plots == TRUE){
    
    #US map
    US <- map("state",fill=TRUE, plot=FALSE)
    US.names <- US$names
    US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
    US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
    plot(US_poly_sp, xlim = c(-125,-68), ylim = c(26,50), main = "")
    points(PP_locations[powerplant ,2:3], pch = 24, bg = "yellow", col = "black", lwd = 0.50, cex = 1)
    points(M_locations[monitor ,2:3], pch = 21, bg = "green", col = "black", lwd = 0.50, cex = 1)
    
    par(mfrow = c(3,1))
    
    #emissions time-series
    plot(emissions[powerplant,], type = 'o', main = powerplant, ylab = "SO2", 
         xlab = NA, ylim = c(0,max(emissions[powerplant,], na.rm = TRUE)))
    #PM time-series
    plot(PM[monitor, ], type = 'o', main = monitor, ylab = "PM2.5", 
         xlab = NA, ylim = c(0, max(PM[monitor,], na.rm = TRUE)))
    #model
    plot(model, residuals = TRUE, cex = 3)
    par(mfrow = c(1,1))
  }
  #print(paste(powerplant, monitor, round(distance,2), lag, round(summary(model)$p.coeff[2],6), round(summary(model)$p.pv[2],6), sep = " "))
  return(model)
}, vectorize.args = c("powerplant","monitor"))



createAdjacencyMatrix <- function(edges){
  require(reshape2)
  network <- dcast(edges, PP ~ Monitor, value.var = "edge")
  PP <- network$PP
  network <- network[, -1]
  network <- as.matrix(network)
  network[is.na(network)] <- 0
  rownames(network) <- as.character(PP)
  return(network)
}

#This function returns some basic information about the output from the 
#fitDailyPMmodels function
edge_analysis <- function(edges){
  network <- createAdjacencyMatrix(edges)
  max.distance <- edges$max.distance[1]
  print(paste("Number of edges in the network:", sum(edges$edge, na.rm = TRUE) , sep = " "))
  print(paste("The edge density is:", round(sum(edges$edge, na.rm = TRUE)/sum(!is.na(edges$edge)),2), sep = " "))
  print(paste("Percent of GAMS models failing:", 
              round(100*(length(is.na(edges$edge)) - length(edges$distance > max.distance))/length(edges$distance < max.distance),2),
              "%",sep = " "))
  print("")
  print("Power Plants")
  print(paste("Total:", length(unique(edges$PP)), sep = " "))
  print(paste("With at least one linked monitor:", length(unique(subset(edges, edge == 1)$PP)), sep = " "))
  print(paste("Median number of linked monitors:", median(rowSums(network, na.rm = TRUE)) ,sep = " "))
  print("")
  print("Monitors")
  print(paste("Total:", length(unique(edges$Monitor)), sep = " "))
  print(paste("With at least one linked powerplant:", length(unique(subset(edges, edge == 1)$Monitor)), sep = " "))
  print(paste("Median number of linked powerplants:", median(colSums(network, na.rm = TRUE)) ,sep = " "))
}
  
#function to plot basic missingness information
missingness_analysis <- function(edges, emissions, PM, M_locations, PP_locations){
  
  #Subset emissions and PM by start and end date
  emissions <- emissions[ ,as.Date(colnames(emissions)) >= start.date & as.Date(colnames(emissions)) <= end.date]
  PM <- PM[ ,as.Date(colnames(PM)) >= start.date & as.Date(colnames(PM)) <= end.date]
  
  #Remove powerplants with no emissions during the time period
  emissions <- emissions[rowSums(emissions, na.rm = TRUE) > 0, ]
  PM <- PM[rowSums(PM, na.rm = TRUE) > 0, ]
  
  
  hist(rowSums(is.na(emissions)), main = "Number of missing emissions observations per power plant",
       xlab = "Number of days missing", breaks = 50)
  hist(rowSums(is.na(PM)), main = "Number of missing PM observations per monitor",
       xlab = "Number of days missing", breaks = 50)
  
  #power plants with at least one linked monitor
  PP_linked <- as.character(unique(subset(edges, edge == 1)$PP))
  hist(rowSums(is.na(emissions[PP_linked,])), 
       main = "Number of missing emissions observations per power plant \n with at least one linked monitor",
       xlab = "Number of days missing", breaks = 10)
  
  #monitors with at least one linked powerplant
  Monitors_linked <- as.character(unique(subset(edges, edge == 1)$Monitor))
  hist(rowSums(is.na(PM[Monitors_linked,])), 
       main = "Number of missing PM observations per monitor \n with at least one linked powerplant",
       xlab = "Number of days missing", breaks = 10)
}

