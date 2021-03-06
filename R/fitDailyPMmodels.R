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

#use when specifying one lag
make_dataset <- function(emissions, PM, monitor, powerplant, lag){
  x <- emissions[powerplant, ]
  y <- PM[monitor, ]
  time <- 1:(length(x) - lag)
  date <- colnames(emissions)
  if(lag > 0){
    x <- x[-1*((length(x)-lag + 1):length(x))]
    y <- y[-1*(1:lag)]
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

make_dataset_distLag <- function(emissions, PM, temperature, monitor, powerplant, adjust.temp = FALSE){
  x0 <- emissions[powerplant, ]
  x1 <- c(NA,x0[-length(x0)])
  x2 <- c(NA,NA,x0[-1*(length(x0)-1):length(x0)])
  x3 <- c(NA,NA,NA,x0[-1*(length(x0)-2):length(x0)])
  y <- PM[monitor, ]
  time <- 1:length(x0)
  date <- colnames(emissions)
  if(adjust.temp == TRUE){
    temp_monitor <- temperature[monitor,]
    dataset <- data.frame(time,y,x0,x1,x2,x3,date,temp_monitor)
  }
  if(adjust.temp == FALSE){
    dataset <- data.frame(time,y,x0,x1,x2,x3,date)
  }
  return(dataset)
}

gams.test_distLag <- function(dataset, k1 = 3, adjust.temp = FALSE){
  tryCatch({
    if(adjust.temp == TRUE){
      model <- gam(log(y) ~ s(time, bs = "cr", k = k1) + x0 + x1 + x2 + x3 + weekdays(as.Date(date)) + s(temp_monitor, bs = "cr", k = 10), 
                 data = dataset, family = gaussian, na.action = na.omit)
    }
    if(adjust.temp == FALSE){
      model <- gam(log(y) ~ s(time, bs = "cr", k = k1) + x0 + x1 + x2 + x3 + weekdays(as.Date(date)), 
                   data = dataset, family = gaussian, na.action = na.omit)
    }
    sum.coeff <- sum(summary(model)$p.coeff[2:5])
    cov.mat <- vcov(model)[2:5,2:5]    
    var.sum <- sum(diag(cov.mat)) + sum(2*cov.mat[lower.tri(cov.mat)])
    p.value <- pnorm(abs(sum.coeff/sqrt(var.sum)), lower.tail = FALSE)
    return(c(sum.coeff,p.value))
  }, error = function(err) return(rep(NA,2))
  )
}


fitDailyPMmodels <- function(emissions, PM, PP_locations, M_locations, temperature = NULL, start.date, end.date, 
                             percent.of.powerplants = 100, alpha = 0.05, p.adjust.method = "BH",
                             lag = "distance_dependent", k1 = 3, plot.pvalues = FALSE, wind.speed = 13,
                             max.distance = 2000, 
                             receptor.regions = c("IndustrialMidwest","Northeast","Southeast"),
                             adjust.temp = FALSE){

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
  if(adjust.temp == TRUE){
    temperature <- temperature[ ,as.Date(colnames(temperature)) >= start.date & as.Date(colnames(temperature)) <= end.date]
  }
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
  
  #Remove this line when temperature is not adjusted for
  if(adjust.temp == TRUE){
    pairs <- subset(pairs, Monitor %in% rownames(temperature))
  }
  
  #compute the lag 
  pairs$lag <- ifelse(pairs$distance < max.distance, findInterval(pairs$distance,lag.breaks), NA)
  pairs <- data.table(pairs)
  
  start.time <- Sys.time()
  
  temp <- pairs[!is.na(lag),]
  
#   gams.results <- mapply(function(emissions, PM, y, x, z){
#     dataset <- make_dataset(emissions, PM, powerplant = y, monitor = x, lag = z)
#     return(gams.test(dataset,k1))
#   }, x = temp$Monitor, y = temp$PP, z = temp$lag,
#   MoreArgs = list(emissions = emissions, PM = PM))
  
  #use with distributed lag
  gams.results <- mapply(function(emissions, PM, temperature, adjust.temp, y, x){
    dataset <- make_dataset_distLag(emissions, PM, temperature, adjust.temp, powerplant = y, monitor = x)
    return(gams.test_distLag(dataset,k1, adjust.temp))
  }, x = temp$Monitor, y = temp$PP,
  MoreArgs = list(emissions = emissions, PM = PM, temperature = temperature, adjust.temp = adjust.temp))
  
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
  pairs[ , sdPM := apply(PM[Monitor,], 1, sd, na.rm = TRUE)]
  pairs[ , avgemissions := rowMeans(emissions[PP ,], na.rm = TRUE)]
  pairs[ , sdemissions := apply(emissions[PP,], 1, sd, na.rm = TRUE)] 
  pairs[ , PM.NAdays := rowSums(is.na(PM[Monitor,]))]
  pairs[ , emissions.NAdays := rowSums(is.na(emissions[PP,]))]
  pairs[ , emissions.ZeroDays := rowSums(emissions[PP,] == 0, na.rm = TRUE)]
  pairs[ , total.days := as.numeric(end.date - start.date)]
  

  pairs[ , distance_cat := ifelse(distance <= 250, 1, 
                                ifelse(distance <= 500, 2,
                                       ifelse(distance <= 750, 3,
                                              ifelse(distance <= 1000, 4, NA))))]

  pairs[ , powerplant_cat := ifelse(avgemissions >= quantile(avgemissions, 0.80),2,1 )]


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
