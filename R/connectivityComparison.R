
connectivityComparison <- function(edges, var1, var2, softPower = 10, regions = c("Northeast","Southeast","IndustrialMidwest")){
  require(reshape2)
  require(WGCNA)
    
  edges <- subset(edges, receptor.region %in% regions)
  
  
  edges[ , gams.metric := ifelse(edge == 1, gams.coeff, 0)]
  if(var1 == "gams.coeff") var1 = "gams.metric"
  if(var2 == "gams.coeff") var2 = "gams.metric"
  
  var1_matrix <- acast(edges, Monitor ~ PP, value.var = var1)
  var1_matrix[is.na(var1_matrix)] <- 0
  var2_matrix <- acast(edges, Monitor ~ PP, value.var = var2)
  var2_matrix[is.na(var2_matrix)] <- 0
  
  softConn.var1 <- softConnectivity(t(var1_matrix),type="signed",power=softPower)
  softConn.var2 <- softConnectivity(t(var2_matrix),type="signed",power=softPower)
  
  rankConn.var1 = rank(softConn.var1)
  rankConn.var2 = rank(softConn.var2)
  
  include <- 1:length(rankConn.var1)
  
  if(var1 == "gams.coeff") include = which(softConn.var1 != -1)
  if(var2 == "gams.coeff") include = which(softConn.var2 != -1)
  
  correlation <- cor(rankConn.var1[include], rankConn.var2[include])
  plot(rankConn.var1,rankConn.var2, 
       xlab= paste("Ranked Connectivity ",var1, sep = ""),
       ylab = paste("Ranked Connectivity ",var2, sep = ""), 
       main = paste(paste(regions, collapse = ", "),"correlation =",round(correlation,2), sep = " ")) 
}

