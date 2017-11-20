
rankComparison <- function(edges, var1, var2, regions = c("Northeast","Southeast","IndustrialMidwest")){
  
  edges <- subset(edges, receptor.region %in% regions)
  
  setkey(edges, Monitor)
  if(var1 == "gams.coeff"){
    var1.monitor <- edges[ , sum(get(var1)*edge, na.rm = TRUE), by = "Monitor"]$V1
  } else {
    var1.monitor <- edges[ , sum(get(var1), na.rm = TRUE), by = "Monitor"]$V1
  }
  
  if(var2 == "gams.coeff"){
    var2.monitor <- edges[ , sum(get(var2)*edge, na.rm = TRUE), by = "Monitor"]$V1
  } else {
    var2.monitor <- edges[ , sum(get(var2), na.rm = TRUE), by = "Monitor"]$V1
  }
  correlation <- cor(rank(var1.monitor), rank(var2.monitor))
  plot(rank(var1.monitor),rank(var2.monitor), 
       xlab= paste("Ranked Exposure ",var1, sep = ""),
       ylab = paste("Ranked Exposure ",var2, sep = ""), 
       main = paste(paste(regions, collapse = ", "),"correlation =",round(correlation,2), sep = " ")) 
}