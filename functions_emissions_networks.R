# Basic functions for manipulating emissions/PM time series and creating an emissions network

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
    par(mfrow = c(3,1))
    plot(emissions[powerplant,], type = 'o', main = powerplant, ylab = "SO2", 
         xlab = NA, ylim = c(0,max(emissions[powerplant,], na.rm = TRUE)))
    plot(PM[monitor, ], type = 'o', main = monitor, ylab = "PM2.5", 
         xlab = NA, ylim = c(0, max(PM[monitor,], na.rm = TRUE)))
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
  
  #Remove observations with mostly missing obs
  n = dim(PM)[2]
  PM <- PM[rowSums(!is.na(PM)) > 0.67*n, ]
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
  
  print(paste("The number of edges is:",sum(pairs$edge,na.rm = TRUE) ,sep = " "))
  print(paste("Edge density is:",round(sum(pairs$edge,na.rm = TRUE)/sum(!is.na(pairs$edge)),2), sep = ""))

  return(pairs)
  
}

createAdjacencyMatrix <- function(edges){
  require(reshape2)
  network <- dcast(edges, PP ~ Monitor, value.var = "edge")
  rownames(network) <- network$PP
  network <- network[, -1]
  network <- as.matrix(network)
  network[is.na(network)] <- 0
  return(network)
}


  
  ##-----------------------------------------------------------------------##
  ##            Plot an Emissions Network                                  ##
  ##-----------------------------------------------------------------------##
  

plotEmissionsNetwork <- function(edges, emissions, PM, PP_locations, M_locations, 
                              plot.type = "highest_degree", plot.percent.of.powerplants = 100, 
                              plot.diagnostics = TRUE,
                              plot.edges = TRUE, plot.legend = FALSE, 
                              max.distance = 1000, main = ""){
 
  require(RColorBrewer)
  
  
#   edges <- edges_copy[[6]]
#   plot.type = "highest_degree"
#   plot.percent.of.powerplants = 100
#   plot.diagnostics = TRUE
#   plot.edges = TRUE
#   plot.legend = FALSE
  
  network <- createAdjacencyMatrix(edges)
  max.distance <- edges$max.distance[1]
  
  wind.speed <- 13 #kph
  lag.breaks <- (1:20)*24*wind.speed
  
  if(!plot.type %in% c("highest_emitters", 
                       "highest_degree")){
    plot.type = "highest_emitters"
    print("Invalid plot.type. Plotting highest_degree power plants")
  }
  
  #make sure location data.table matches network adjacency matrix
  PP_locations <- PP_locations[rownames(network),]
  M_locations <- M_locations[colnames(network),]
  emissions <- emissions[rownames(network),]
  PM <- PM[colnames(network),]
  
  #calculate distances for data analysis
  dist <- spDists(as.matrix(M_locations[,c(2,3)]),as.matrix(PP_locations[,c(2,3)]), longlat = TRUE)
  colnames(dist) <- rownames(network)
  rownames(dist) <- colnames(network)
  dist_list <- melt(t(dist))
  dist_list <- dist_list[order(dist_list$Var1,dist_list$Var2),]
  
  #convert network to long form and add lat/long data for plotting
  diad_list <- melt(network)
  diad_list <- merge(diad_list, PP_locations, by.x = "Var1", by.y = "ID")
  diad_list <- merge(diad_list, M_locations, by.x = "Var2", by.y = "ID")
  diad_list <- diad_list[order(diad_list$Var1,diad_list$Var2),]
  diad_list$Distance <- dist_list$value
  
  
  #find direction for data analysis purposes
  diad_list$bearing <- bearing(cbind(diad_list$Longitude.x,diad_list$Latitude.x), cbind(diad_list$Longitude.y,diad_list$Latitude.y))
  diad_list$bearing <- ifelse(diad_list$bearing < 0, diad_list$bearing + 360, diad_list$bearing)
  angle_breaks <- seq(22.5,337.5, by = 45)
  direction_ints <- findInterval(diad_list$bearing, angle_breaks)
  diad_list$direction <- ifelse(direction_ints == 1, "NE",
                                ifelse(direction_ints == 2, "E",
                                       ifelse(direction_ints == 3, "SE",
                                              ifelse(direction_ints == 4, "S",
                                                     ifelse(direction_ints == 5, "SW",
                                                            ifelse(direction_ints == 6, "W",
                                                                   ifelse(direction_ints == 7, "NW","N")))))))
  
  # take only edges (as opposed to diads)
  edge_list <- subset(diad_list, value == 1)
  #indicator that distance from power plant to monitor is probably too long for there to be an edge
  edge_list$lag <- findInterval(edge_list$Distance, lag.breaks)
  
  #distance plot for data analysis
  #breaks <- c(50,100,200,300,400,seq(500,4500, by = 250))
  breaks <- seq(0,max.distance, by = 250)
  edge_prob_dist <- rep(0, length(breaks)-1)
  for(b in 1:(length(breaks)-1)){
    #number of edges
    edge_sum <- sum(subset(diad_list, Distance > breaks[b] & Distance <= breaks[b+1])$value)
    #possible edges
    edge_total <- length(subset(diad_list, Distance > breaks[b] & Distance <= breaks[b+1])$value)
    edge_prob_dist[b] <- edge_sum/edge_total
  }
  #plot function call is below
  
  #azimuth plot of data analysis
  edge_prob_angle <- rep(0, 8)
  directions <- c("N","NE", "E", "SE","S","SW","W","NW")
  for(b in 1:8){
    #number of edges
    edge_sum <- sum(subset(diad_list, direction == directions[b])$value)
    #possible edges
    edge_total <- length(subset(diad_list, direction == directions[b])$value)
    edge_prob_angle[b] <- edge_sum/edge_total
  }
  
  #plot US Map
  US <- map("state",fill=TRUE, plot=FALSE)
  US.names <- US$names
  US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
  US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
  plot(US_poly_sp, xlim = c(-125,-68), ylim = c(26,50), main = main)
  
  
  
  points(PP_locations[ ,2:3], pch = 24, bg = "yellow", col = "black", lwd = 0.50, cex = 0.50)
  points(M_locations[ ,2:3], pch = 21, bg = "green", col = "black", lwd = 0.50, cex = 1)

  if(plot.edges == TRUE & sum(network, na.rm = TRUE) > 0){
      #assign colors based on lag
      colors <- brewer.pal(n = 4, name = "RdYlBu")
      edge_list$color.index <- 4 - edge_list$lag
      edge_list$color.index <- ifelse(edge_list$color.index < 1, 1, edge_list$color.index)
      
      segments(edge_list$Longitude.x,
               edge_list$Latitude.x,
               edge_list$Longitude.y,
               edge_list$Latitude.y,
               col = colors[edge_list$color.index],
               lwd = 0.4)
      if(plot.legend == TRUE){
        legend(x = -79, y = 33, legend = c("0 day lag","1 day lag", "2 day lag", ">2 day lag"),
               lty = 1, col = colors[length(colors):1], cex = 0.5)
      }
  }
  
  
  #plots
  
  if(plot.diagnostics == TRUE & sum(network, na.rm = TRUE) > 0){
    #Plot 1 - observed edge probability vs distance
    barplot(height = edge_prob_dist, space = 0, main = "Edge Prob vs Distance")
    axis(1, at = 0:(length(breaks)-1), labels = breaks)
    
    
    #Plot 2 - observed edge probability by direction from PP to monitor
#     barplot(edge_prob_angle, names.arg = directions, main = "Edge Prob by Edge Direction")
    
    #Plot 3 - observed degree of powerplant by powerplant size category
#     degree <- rowSums(network, na.rm = TRUE)
#     plot(logNA(rowSums(emissions, na.rm = TRUE))[degree > 1], log(degree)[degree > 1],
#          xlab = "log(Total SO2 emissions)", ylab = "log(number of monitors linked)",
#          main = "Emissions vs. Number of Edges")
#     abline(lm(logNA(degree)[degree > 1] ~ logNA(rowSums(emissions, na.rm = TRUE))[degree > 1]),
#            col = "blue", lty = 2)
    #boxplot(logNA(degree) ~ pp.size, data = PP_locations, xlab = "power size category", ylab = "degree")
   # print(summary(degree ~ pp.size))
  }
  return()
}


plotHighLowMap <- function(edges, PP_locations, M_locations, 
                           cutoff.perc = 0.80, main = ""){
  network <- createAdjacencyMatrix(edges)
  M_locations <- M_locations[colnames(network),]
  PP_locations <- PP_locations[rownames(network),]
  
  max.distance <- edges$max.distance[1]
  
  numLinks <- colSums(network)
  
  distance <- spDists(as.matrix(PP_locations[,c(2,3)]),as.matrix(M_locations[,c(2,3)]), longlat = TRUE)
  numPossibleLinks <- colSums(distance < max.distance)
  
  
  High <- ifelse(numLinks/numPossibleLinks > quantile(numLinks/numPossibleLinks, cutoff.perc),1,0)
  
  print(paste("The cutoff between high/low is:", quantile(numLinks/numPossibleLinks, cutoff.perc), sep = " "))
  print(paste("The cutoff in terms of number of links is:", quantile(numLinks, cutoff.perc), sep = " "))
  
  US <- map("state",fill=TRUE, plot=FALSE)
  US.names <- US$names
  US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
  US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
  plot(US_poly_sp, xlim = c(-125,-68), ylim = c(26,50), main = main)
  points(PP_locations[ ,2:3], pch = 24, bg = "yellow", col = "black", lwd = 0.50, cex = 0.25)
  points(M_locations[ ,2:3], pch = 21, bg = ifelse(High == 1, "red","green"), col = "black", lwd = 0.50, cex = 1)
}







###OTHER CODE FROM BEFORE

##-----------------------------------------------------------------------##
##            Plot time-series for specific monitors/power plants        ##
##-----------------------------------------------------------------------##

# indices <- which(colnames(emissions) >= start.dates & colnames(emissions) <= end.dates)
# 
# plot(as.Date(colnames(emissions)[indices]),emissions["PP709", indices], 
#      ylab = NA, xlab = NA, ylim = c(0,265), type = "o", cex.axis = 1.5,
#      main = NA)
# title("SO2", line = 0.5, cex.main = 2)
# 
# 
# indices <- which(colnames(PM) >= start.dates & colnames(PM) <= end.dates)
# 
# plot(as.Date(colnames(PM)[indices]),PM["M13067-0003", indices], 
#      ylab = NA, xlab = NA, type = "o", cex.axis = 1.5,
#      main = NA, ylim = c(0,20))
# title("PM", line = 0.5, cex.main = 2)


##-----------------------------------------------------------------------##
##            Clustering by Fitting Stochastic Block Model               ##
##-----------------------------------------------------------------------##

# emissions <- emissions.changes
# PM <- PM.changes
# 
# #Fit a stochastic block model - bipartite network 
# adjacency <- matrix(0, nrow = nrow(emissions) + nrow(PM), ncol = nrow(emissions) + nrow(PM))
# diag(adjacency) <- 1
# rownames(adjacency) <- c(rownames(emissions), rownames(PM))
# colnames(adjacency) <- c(rownames(emissions), rownames(PM))
# 
# adjacency[(nrow(emissions)+1):nrow(adjacency),1:nrow(emissions)] <- network
# adjacency[1:nrow(emissions),(nrow(emissions)+1):nrow(adjacency)] <- network
# 
# 
# #remove isolates
# adjacency <- adjacency[rowSums(adjacency) > 0, ]
# adjacency <- adjacency[ ,colSums(adjacency) > 0]
# 
# start.time <- Sys.time()
# blkmodel <- BM_bernoulli(membership_type = "SBM", adj = adjacency, verbosity = 0)
# blkmodel$estimate()
# Sys.time() -start.time
# 
# membership_probs <- blkmodel$memberships[[which.max(blkmodel$ICL)]]$Z
# 
# membership <- apply(membership_probs, 1, which.max)
# 
# #Make a map of the results with group memberships
# #pdf("blockmodel_groups.pdf")
# US <- map("state",fill=TRUE, plot=FALSE)
# US.names <- US$names
# US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
# US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
# plot(US_poly_sp, xlim = c(-125,-68), ylim = c(26,50), main = NA)
# 
# points(PP_locations[,2:3], pch = 24, 
#        bg = membership[1:nrow(emissions)], col = "black", cex = 2)
# points(M_locations[,2:3], pch = 21, 
#        bg = membership[(nrow(emissions)+1):(nrow(PM)+ nrow(emissions))], col = "black")
# 
# 
# 
# #Fit a stochastic block model - projection to one-mode network
# 
# network_proj <- t(network) %*% network
# 
# blkmodel_uni <- BM_poisson(membership_type = "SBM", adj = network_proj)
# blkmodel_uni$estimate()
# 
# membership_uni_probs <- blkmodel_uni$memberships[[which.max(blkmodel_uni$ICL)]]$Z
# #membership_uni_probs <- blkmodel_uni$memberships[[10]]$Z
# 
# membership_uni <- apply(membership_uni_probs, 1, which.max)
# 
# US <- map("state",fill=TRUE, plot=FALSE)
# US.names <- US$names
# US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
# US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
# plot(US_poly_sp, xlim = c(-125,-68), ylim = c(26,50), main = NA)
# points(M_locations[,2:3], pch = 19, col = as.integer(membership_uni))
# text(M_locations[,2:3], labels = as.character(membership_uni),
#      pos = 3, cex = 0.75)
# 
# 
# ##-----------------------------------------------------------------------##
# ##           Plot Specific Power plants/monitors                         ##
# ##-----------------------------------------------------------------------##
# 
# PP_list <- c("PP3140_3")
# M_list <-  c("M42043-0401")
# 
# US <- map("state",fill=TRUE, plot=FALSE)
# US.names <- US$names
# US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
# US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
# plot(US_poly_sp, xlim = c(-125,-68), ylim = c(26,50), main = NA)
# points(PP_locations[ID %in% PP_list,2:3], pch = 24, bg = "yellow", col = "black", cex = 1.5)
# points(M_locations[ID %in% M_list,2:3], pch = 21, bg = "green", col = "black")
# 
# start.time <- Sys.time()
# granger.test(cbind(emissions[1,],PM[1,]), p =1)
# Sys.time() - start.time
# 
# ##-----------------------------------------------------------------------##
# ##            Plot a Time-Series decomp                                  ##
# ##-----------------------------------------------------------------------##
# 
# #Plot a PM decomp
# x <- PM.impute[8,]
# x <- as.numeric(x)
# x <- ts(x, frequency = 365)
# fit <- stl(x, t.window = 4*365, s.window = "periodic",robust = TRUE)
# plot(fit)
# 
# #Plot an emissions decomp
# x <- emissions.impute[60,]
# x <- as.numeric(x)
# #x <- na.kalman(x)
# x <- ts(x, frequency = 365)
# fit <- stl(x, t.window = 6*30, s.window = "periodic",robust = TRUE)
# plot(fit)
# 
# ##-----------------------------------------------------------------------##
# ##            Plot Specific Monitors                                     ##
# ##-----------------------------------------------------------------------##
# 
# par(mfrow = c(1,1))
# monitorIDs <- c("M53061-1007","M49005-0004", "M16079-0017","M51059-0030", "M06037-4004")
# 
# US <- map("state",fill=TRUE, plot=FALSE)
# US.names <- US$names
# US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
# US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
# plot(US_poly_sp, xlim = c(-125,-68), ylim = c(26,50), main = NA)
# 
# points(M_locations[monitorIDs,2:3], pch = 21, bg = "green", col = "black")
# 
# ##-----------------------------------------------------------------------##
# ##            Clustering Monitors Using Correlations                      ##
# ##-----------------------------------------------------------------------##
# 
# # Try clustering based on correlations in PM of monitors
# num_clusters = 5 #set to same as above analysis usually
# 
# 
# # Convert to ts object and cluster
# PM.ts <- t(apply(PM.impute,1,ts, frequency = 365))
# PM.dis <- diss(PM.ts, "COR")
# PM.hclust <- cutree(hclust(PM.dis), k = num_clusters)
# 
# #cluster.evaluation(PM.hclust,membership_uni)
# 
# US <- map("state",fill=TRUE, plot=FALSE)
# US.names <- US$names
# US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
# US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
# plot(US_poly_sp, xlim = c(-125,-68), ylim = c(26,50), main = NA)
# points(M_locations[,2:3], pch = 19, col = as.integer(PM.hclust))
# text(M_locations[,2:3], labels = as.character(PM.hclust),
#      pos = 3, cex = 0.75)
# 
# ##-----------------------------------------------------------------------##
# ##            Clustering Monitors/Power Plants Using Correlations        ##
# ##-----------------------------------------------------------------------##
# 
# # Try clustering based on correlations in PM of monitors
# num_clusters = 5 #set to same as above analysis usually
# 
# data.impute <- rbind(PM.impute, emissions.impute)
# 
# # Convert to ts object and cluster
# data.ts <- t(apply(data.impute,1,ts, frequency = 365))
# data.dis <- diss(data.ts, "COR")
# data.hclust <- cutree(hclust(data.dis), k = num_clusters)
# 
# PM.hclust <- data.hclust[1:nrow(PM)]
# emissions.hclust <- data.hclust[(nrow(PM)+1):(nrow(PM)+ nrow(emissions))]
# 
# #cluster.evaluation(PM.hclust,membership_uni)
# 
# US <- map("state",fill=TRUE, plot=FALSE)
# US.names <- US$names
# US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
# US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
# plot(US_poly_sp, xlim = c(-125,-68), ylim = c(26,50), main = NA)
# points(M_locations[,2:3], pch = 19, col = as.integer(PM.hclust))
# text(M_locations[,2:3], labels = as.character(PM.hclust),
#      pos = 3, cex = 0.75)
# points(PP_locations[,2:3], pch = 17, col = as.integer(emissions.hclust))
# text(PP_locations[,2:3], labels = as.character(emissions.hclust),
#      pos = 3, cex = 0.75)
# 
# 
# ##-----------------------------------------------------------------------##
# ##            Time-series plots of emissions and PM                      ##
# ##-----------------------------------------------------------------------##
# 
# #Plot a sample of the power plants
# plot_samp <- sample(1:nrow(emissions),min(20,nrow(emissions)))
# 
# #print time-series plots - different scale
# par(mfrow = c(2,1))
# for(i in 1:nrow(emissions)){
#   plot(as.Date(colnames(emissions)),emissions[i,], type = "o",
#        axes = TRUE, main = rownames(emissions)[i],
#        xlab = NA, ylab = NA)
# }
# par(mfrow = c(1,1))
# 
# #Plot a sample of the monitors
# plot_samp <- sample(1:nrow(PM),min(20,nrow(PM)))
# 
# par(mfrow = c(2,1))
# for(i in 1:nrow(PM)){
#   plot(as.Date(colnames(PM)),PM[i,], type = "o",
#        axes = TRUE, main = rownames(PM)[i],
#        xlab = NA, ylab = NA)
# }
# par(mfrow = c(1,1))
# 
# 
# #M_list = paste("M",M_list,sep = "")
# 
# # for(i in M_list){
# #   plot(as.Date(colnames(PM_all)),PM_all[i,], type = "o",
# #        axes = TRUE, main = i,
# #        xlab = NA, ylab = NA)
# # }
# 
# 
# ##-----------------------------------------------------------------------##
# ##            Time-series decomp of Changes in PM and Emissions          ##
# ##-----------------------------------------------------------------------##
# 
# # emissions.changes.rem <- t(apply(emissions.changes, 1, ts_remainder, t.window = 31,
# #                                s.window = "periodic", robust = TRUE))
# # 
# # PM.changes.rem<- t(apply(PM.changes, 1, ts_remainder, t.window = 31,
# #                         s.window = "periodic", robust = TRUE))
# 
# 
# ##-----------------------------------------------------------------------##
# ##            Plot US Map of rawdata                                     ##
# ##-----------------------------------------------------------------------##
# 
# par(mfrow = c(1,1))
# US <- map("state",fill=TRUE, plot=FALSE)
# US.names <- US$names
# US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
# US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
# plot(US_poly_sp, xlim = c(-125,-68), ylim = c(26,50), main = NA)
# 
# points(PP_locations[ ,2:3], pch = 24, bg = "yellow", col = "black", cex = 1.5)
# points(M_locations[ ,2:3], pch = 21, bg = "green", col = "black")
# 
