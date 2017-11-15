# Basic functions for manipulating emissions/PM time series and creating an emissions network

getRegion <- function(states){
  Northeast = c("ME", "NH", "VT", "NY", "PA", "DE", "NJ", "MD", "DC", "VA", "MA", "CT", "RI")
  IndustrialMidwest = c("WV", "OH", "KY", "IN", "IL", "WI", "MI")
  Southeast = c("FL", "GA", "SC", "NC", "TN", "AL", "MS", "AR","LA")
  UpperMidwest = c("MN", "IA", "MO", "KS", "NE", "SD", "ND")
  Southwest = c("TX", "OK", "NM", "AZ")
  SouthernCalifornia = c("CA")
  Northwest = c("NV", "UT", "CO", "WY", "MT", "ID", "OR", "WA")
  regions <- ifelse(states %in% Northeast,"Northeast",
                    ifelse(states %in% IndustrialMidwest, "IndustrialMidwest",
                           ifelse(states %in% Southeast, "Southeast",
                                  ifelse(states %in% UpperMidwest, "UpperMidwest",
                                         ifelse(states %in% Southwest, "Southwest",
                                                ifelse(states %in% SouthernCalifornia, "SouthernCalifornia",
                                                       ifelse(states %in% Northwest,"Northwest", NA)))))))
  return(regions)
}


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
  require(geosphere)
  require(mgcv)
  require(maps)
  require(maptools)
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
    plot(model, residuals = TRUE, cex = 3, xlim = c(0, end.date - start.date))
    par(mfrow = c(1,1))
  }
  #print(paste(powerplant, monitor, round(distance,2), lag, round(summary(model)$p.coeff[2],6), round(summary(model)$p.pv[2],6), sep = " "))
  return(model)
}, vectorize.args = c("powerplant","monitor"))



#This function returns some basic information about the output from the 
#fitDailyPMmodels function

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

