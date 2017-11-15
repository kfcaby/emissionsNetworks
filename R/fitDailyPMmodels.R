# start.date <- "2005-06-01"
# end.date <- "2005-08-31"
# percent.of.powerplants = 20
# lag = "distance_dependent"
# p.adjust.method = "BH"
# alpha = 0.05
# k1 = 3
# plot.pvalues = FALSE
# wind.speed = 13
# max.distance = 1000
# include.west = FALSE

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

gams.test <- function(dataset, k1 = 3){
  tryCatch({
    model <- gam(log(y) ~ s(time, bs = "cr", k = k1) + x + weekdays(as.Date(date)), 
                 data = dataset, family = gaussian, na.action = na.omit)
    coeff <- summary(model)$p.coeff[2]
    p.value <- summary(model)$p.pv[2]
    return(c(coeff,p.value))
  }, error = function(err) return(rep(NA,2))
  )
}

fitDailyPMmodels <- function(emissions, PM, PP_locations, M_locations ,start.date, end.date, 
                             percent.of.powerplants = 100, alpha = 0.05, p.adjust.method = "BH",
                             lag = "distance_dependent", k1 = 3, plot.pvalues = FALSE, wind.speed = 13,
                             max.distance = 2000, 
                             receptor.regions = c("IndustrialMidwest","Northeast","Southeast")){

  require(mgcv)
  require(geosphere)
  require(lubridate)
  
  start.date <- as.Date(start.date)
  end.date <- as.Date(end.date)
  
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
  
  #Remove observations outside region of interest
  PM <- PM[M_locations$receptor.region %in% receptor.regions,]
  M_locations <- M_locations[rownames(PM),]
  
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
  
  pairs$gams.coeff <- coeff
  pairs$p.value <- p.value
  
  print("Model fitting complete:")
  print(Sys.time() - start.time)
  
  
  pairs$max.distance <- max.distance
  
  pairs$start.date <- start.date
  pairs$end.date <- end.date
  pairs$wind.speed <- wind.speed
  pairs$knots <- k1
  
  pairs$PP.longitude <- PP_locations[pairs$PP,]$Longitude
  pairs$PP.latitude <- PP_locations[pairs$PP,]$Latitude
  pairs$receptor.longitude <- M_locations[pairs$Monitor,]$Longitude
  pairs$receptor.latitude <- M_locations[pairs$Monitor,]$Latitude
  
  pairs$PP.county <- PP_locations[pairs$PP,]$PP.county
  pairs$PP.state <- PP_locations[pairs$PP,]$PP.state
  pairs$PP.region <- PP_locations[pairs$PP,]$PP.region
  
  pairs$receptor.city <- M_locations[pairs$Monitor,]$receptor.city
  pairs$receptor.state <- M_locations[pairs$Monitor,]$receptor.state
  pairs$receptor.region <- M_locations[pairs$Monitor,]$receptor.region
  
  pairs[ , avgPM := rowMeans(PM[Monitor,], na.rm = TRUE)]
  pairs[ , avgemissions := rowMeans(emissions[PP ,], na.rm = TRUE)]
  pairs[ , PM.NAdays := rowSums(is.na(PM[Monitor,]))]
  pairs[ , emissions.NAdays := rowSums(is.na(emissions[PP,]))]
  pairs[ , total.days := as.numeric(end.date - start.date)]
  
  setkey(pairs, Monitor, PP)
  
  pairs$bearing <- bearing(cbind(pairs$PP.longitude,pairs$PP.latitude),
                           cbind(pairs$receptor.longitude,pairs$receptor.latitude))
  pairs$bearing <- ifelse(pairs$bearing < 0, pairs$bearing + 360, pairs$bearing)
  
  #add direction to edge data
  angle_breaks <- seq(22.5,337.5, by = 45)
  direction_ints <- data.table(PP = pairs$PP, Monitor = pairs$Monitor, 
                               interval = findInterval(pairs$bearing, angle_breaks), key = "interval")
  direction.table <- data.table(interval = 0:8, direction = c("N","NE","E","SE", "S","SW","W","NW","N"), key = "interval")
  directions <- direction_ints[direction.table]
  setkey(directions, PP, Monitor)
  setkey(pairs, PP, Monitor)
  pairs <- merge(pairs,directions[ ,.(PP,Monitor,direction)])
  
  print(paste("The number of edges is:",sum(pairs$edge,na.rm = TRUE) ,sep = " "))
  print(paste("Edge density is:",round(sum(pairs$edge,na.rm = TRUE)/sum(!is.na(pairs$edge)),2), sep = ""))
  
  return(pairs)
  
}
