#functions used in performing the 2d wavelet decomposition
# (Antonelli et al) for daily PM data

waveletDecomposition <- function(data, numLevels, lowest.level){
  require(Irregular2dWavelets)
  Decomp = Irregular2dWavelet(data$Longitude, data$Latitude, data$PM25, numLevels=numLevels)
  
  threshold.betalow <- threshold2d(Decomp$beta, numLevels, remove.x=(lowest.level+1):numLevels, 
                                   remove.y=(lowest.level+1):numLevels)
  lowfreq <- Decomp$Zxy%*%threshold.betalow
  
  return(lowfreq)
}

decomposePM <- function(PM, M_locations, numLevels, lowest.level, include.west = TRUE){
  M_locations <- M_locations[rownames(PM), ]
  
  if(include.west == FALSE){
    PM <- PM[M_locations$Longitude > -100,]
    M_locations <- M_locations[rownames(PM),]
  }
   
  start.time <- Sys.time()
  PM.lowfreq <- apply(PM, 2, function(x, M_locations, numLevels, lowest.level){
    lowfreq <- rep(NA, length(x))
    data <- data.frame(PM25 = x[!is.na(x)], Longitude = M_locations[!is.na(x)]$Longitude, Latitude = M_locations[!is.na(x)]$Latitude)
    lowfreq[!is.na(x)] <- waveletDecomposition(data,numLevels,lowest.level)
    return(lowfreq)
  }, M_locations = M_locations, numLevels = numLevels, lowest.level = lowest.level)
  
  rownames(PM.lowfreq) <- rownames(PM)
  print("Wavelet decomposition complete:")
  print(Sys.time() - start.time)
  
  return(PM.lowfreq)
}