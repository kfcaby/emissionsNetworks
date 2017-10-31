# Basic functions for manipulating emissions/PM time series and creating an emissions network

#convert a ZIP code from 3-digit to 5-digit format
convertZip <- function(zip){
  temp_zip<-formatC(zip, width = 5, format = "d", flag = "0")
  zipcode<-unlist(lapply(temp_zip, function(x) as.numeric(paste(strsplit(as.character(x),"")[[1]],collapse=""))))
  zipcode<-formatC(zipcode, width = 5, format = "d", flag = "0")
  return(zipcode)
}

make_dataset <- function(emissions, PM, monitor, powerplant, lag){
  y <- emissions[powerplant, ]
  x <- PM[monitor, ]
  time <- 1:(length(x) - lag)
  date <- colnames(emissions)
  if(lag > 0){
    y <- y[-1*((length(y)-lag + 1):length(y))]
    x <- x[-1*(1:lag)]
    date <- date[-1*((length(date)-lag + 1):length(date))]
  }
  dataset <- data.frame(time,y,x,date)
  return(dataset)
}


# Using MSBVAR function
granger_causality <- function(x, y, lag = 3){
  result <- granger.test(cbind(x,y), p = lag)
  return(c(result[2,2],result[1,2]))
}

gams.test <- function(dataset, k1 = 3){
  tryCatch({
    model <- gam(log(y) ~ s(time, bs = "cr", k = k1) + x + weekdays(as.Date(date)), 
                   data = dataset, family = gaussian, na.action = na.omit)
    coeff <- summary(model)$p.coeff[2]
    p.value <- summary(model)$p.pv[2]
    return(c(coeff,p.value))
  }, error = function(err) return(NA)
  )
}

# 

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
  print(paste(powerplant, monitor, round(distance,2), lag, round(summary(model)$p.coeff[2],6), round(summary(model)$p.pv[2],6), sep = " "))
  return(model)
}, vectorize.args = c("powerplant","monitor"))


fitDailyPMmodels <- function(year, emissions, PM, PP_locations, M_locations ,start.day, end.day, 
                                 percent.of.powerplants = 100, alpha = 0.05, p.adjust.method = "BH",
                                 lag = "distance_dependent", k1 = 3, plot.pvalues = FALSE, wind.speed = 13,
                                 max.distance = 2000, include.west = FALSE){
  # start.day <- "06-01"
  # end.day <- "08-31"
  # year <- 2005
  # percent.of.powerplants = 20
  # lag = "distance_dependent"
  # p.adjust.method = "BH"
  # alpha = 0.05
  # k1 = 3
  # plot.pvalues = FALSE
  # wind.speed = 13
  # max.distance = 1000
  # include.west = FALSE
  require(mgcv)
  
  start.date <- as.Date(paste(year,"-",start.day, sep = ""))
  end.date <- as.Date(paste(year,"-",end.day,sep = ""))
  

  lag.breaks <- (1:20)*24*wind.speed

  print(year(start.date))
  
  #Subset emissions and PM by start and end date
  emissions <- emissions[ ,as.Date(colnames(emissions)) >= start.date & as.Date(colnames(emissions)) <= end.date]
  PM <- PM[ ,as.Date(colnames(PM)) >= start.date & as.Date(colnames(PM)) <= end.date]
  
  #Take only biggest emitters
  emissions <- emissions[rowSums(emissions, na.rm = TRUE) > quantile(rowSums(emissions, na.rm = TRUE), 1 - percent.of.powerplants/100),]
  
  # Remove plants with no emissions during this period
  emissions <- subset(emissions, rowSums(is.na(emissions)) < ncol(emissions))
  PP_locations <- subset(PP_locations, ID %in% rownames(emissions))
  
  M_locations <- M_locations[rownames(PM),]
  
  #Remove observations in the west
  if(include.west == FALSE){
    PM <- PM[M_locations$Longitude > -100,]
    M_locations <- M_locations[rownames(PM),]
  }
 
  
  pairs <- spDists(as.matrix(M_locations[,c(2,3)]),as.matrix(PP_locations[,c(2,3)]), longlat = TRUE)
  colnames(pairs) <- rownames(emissions)
  rownames(pairs) <- rownames(PM)
  pairs <- melt(t(pairs), value.name = "distance")
  colnames(pairs)[1:2] <- c("PP","Monitor")
  pairs$PP <- as.character(pairs$PP)
  pairs$Monitor <- as.character(pairs$Monitor)
  
  #compute the lag 
  pairs$lag <- ifelse(pairs$distance < max.distance, findInterval(pairs$distance,lag.breaks), NA)
  pairs <- data.table(pairs)
  
  start.time <- Sys.time()
 
  temp <- pairs[!is.na(lag),]
  
  gams.results <- mapply(function(emissions, PM, y, x, z){
    dataset <- make_dataset(emissions, PM, powerplant = y, monitor = x, lag = z)
    return(gams.test(dataset,k1))
    }, x = temp$Monitor, y = temp$PP, z = temp$lag,
                         MoreArgs = list(emissions = emissions, PM = PM))
    
  
  
  coeff <- rep(NA, nrow(pairs))
  p.value <- rep(NA, nrow(pairs))
  if(is.list(gams.results)){
    gams.results <- do.call(cbind,gams.results)
  } 
  
  coeff[!is.na(pairs$lag)] <- gams.results[1,]
  p.value[!is.na(pairs$lag)] <- gams.results[2,]
  
  pairs$coeff <- coeff
  pairs$p.value <- p.value
  
  print("Model fitting complete:")
  print(Sys.time() - start.time)
  
  adj <- rep(NA, nrow(pairs))
  adj[!is.na(pairs$lag)] <- p.adjust(gams.results[2,], method = p.adjust.method)
  
  #adjust for multiple comparisons
  pairs$p.value_adj <- adj 
  
  #edges in network
  pairs$edge <- ifelse(pairs$p.value_adj <= alpha & pairs$coeff > 0, 1, 0)
  
  pairs$max.distance <- max.distance
  
  pairs$start.day <- start.day
  pairs$end.day <- end.day
  pairs$year <- year
  pairs$wind.speed <- wind.speed
  pairs$knots <- k1
  
  pairs$PP.longitude <- PP_locations[pairs$PP,]$Longitude
  pairs$PP.latitude <- PP_locations[pairs$PP,]$Latitude
  pairs$M.longitude <- M_locations[pairs$Monitor,]$Longitude
  pairs$M.latitude <- M_locations[pairs$Monitor,]$Latitude
  
  print(paste("The number of edges is:",sum(pairs$edge,na.rm = TRUE) ,sep = " "))
  print(paste("Edge density is:",round(sum(pairs$edge,na.rm = TRUE)/sum(!is.na(pairs$edge)),2), sep = ""))

  return(pairs)
  
}

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

  ##-----------------------------------------------------------------------##
  ##            Plot an Emissions Network                                  ##
  ##-----------------------------------------------------------------------##
  
plotEmissionsNetwork <- function(edges, exposure.type = NA, exposure.var = NULL, exposure.binary.cutoff = 0.80, num.colors = 10, plot.edges = TRUE,
                                      main = " ", plot.diagnostics = TRUE){
  require(RColorBrewer)
  
  US <- map("state",fill=TRUE, plot=FALSE)
  US.names <- US$names
  US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
  US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
  plot(US_poly_sp, xlim = c(-125,-68), ylim = c(26,50), main = "")
  
  #determine colors of monitors
  if(is.na(exposure.type) || !exposure.type %in% c("binary","continuous")){
    bg.monitor <- "green"
    col.monitor <- "black"
  } else{
    if(exposure.type == "binary"){
      
      #number of links
      monitor_degree <- edges[, list(degree = sum(edge,na.rm = TRUE),
                               possible = sum(distance < max.distance, na.rm = TRUE)),
                               by = "Monitor"]
      setkey(monitor_degree, Monitor)
      monitor_degree[ , percent := degree/possible]
      monitor_degree[ , percent := ifelse(is.na(percent), 0, percent)]
      monitor_degree[ , High := ifelse(percent > quantile(percent, exposure.binary.cutoff, na.rm = TRUE),1,0)]
      
      
      print(paste("The cutoff between high/low is:", quantile(monitor_degree$percent, exposure.binary.cutoff, na.rm = TRUE), sep = " "))
      print(paste("The cutoff in terms of number of links is:", quantile(monitor_degree$degree, exposure.binary.cutoff, na.rm = TRUE), sep = " "))
      
      bg.monitor <- ifelse(monitor_degree$High == 1, "red","green")
      col.monitor <- "black"
    }
    
    if(exposure.type == "continuous"){
      setkey(edges, Monitor)
      rbPal <- colorRampPalette(c('green','red'))
      exposure <- edges[J(unique(Monitor)), get(exposure.var), mult = "first"]
      
      bg.monitor = rbPal(num.colors)[as.numeric(cut(exposure, breaks = num.colors))]
      col.monitor <- "black"
    }
  }
  
  #Plot the monitors and the power plants
  setkey(edges, Monitor)
  points(edges[J(unique(Monitor)), c("M.longitude","M.latitude"), mult = "first"],
         pch = 21, bg = bg.monitor, col = col.monitor, lwd = 0.50, cex = 1) 
  setkey(edges, PP)
  points(edges[J(unique(PP)), c("PP.longitude","PP.latitude"), mult = "first"],
         pch = 24, bg = "yellow", col = "black", lwd = 0.50, cex = 1) 
  
  #plot the edges
  if(plot.edges == TRUE & sum(edges$edge, na.rm = TRUE) > 0){
    #assign colors based on lag
    colors <- brewer.pal(n = 4, name = "RdYlBu")
    color.index <- 4 - subset(edges,edge == 1)$lag
    color.index <- ifelse(color.index < 1, 1, color.index)
    
    segments(subset(edges, edge == 1)$M.longitude,
             subset(edges, edge == 1)$M.latitude,
             subset(edges, edge == 1)$PP.longitude,
             subset(edges, edge == 1)$PP.latitude,
             col = colors[color.index],
             lwd = 0.4)
  }

  if(plot.diagnostics == TRUE){
    breaks <- seq(0,edges$max.distance[1], by = 250)
    edges$dist.cat <- findInterval(edges$distance, breaks)
    probs <- edges[ , list(sum(edge == 1,na.rm = TRUE)/sum(!is.na(edge))), by = "dist.cat"]
    barplot(height = probs$V1, space = 0, main = "Edge Probability by Distance Category")
    axis(1, at = 0:(length(breaks)-1), labels = breaks)
  }
}

