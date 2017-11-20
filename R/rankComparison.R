
rankComparison <- function(edges, var1, var2, regions = c("Northeast","Southeast","IndustrialMidwest")){
  
  edges <- subset(edges, receptor.region %in% regions)
  include <- 1:length(unique(edges$Monitor)) 
  
  setkey(edges, Monitor)
  if(var1 == "gams.coeff"){
    var1.monitor <- edges[ , sum(get(var1)*edge, na.rm = TRUE), by = "Monitor"]$V1
    include <- which(var1.monitor > 0)
  } 
  if(var1 == "avgPM") { 
    var1.monitor <- edges[ , unique(get(var1)), by = "Monitor"]$V1
  }
  if(var1 == "inmapPM") { 
    var1.monitor <- edges[ , sum(get(var1), na.rm = TRUE), by = "Monitor"]$V1
  }
  
  if(var2 == "gams.coeff"){
    var2.monitor <- edges[ , sum(get(var2)*edge, na.rm = TRUE), by = "Monitor"]$V1
    include <- which(var1.monitor > 0)
  } 
  if(var2 == "avgPM") { 
    var2.monitor <- edges[ , unique(get(var2)), by = "Monitor"]$V1
  }
  if(var2 == "inmapPM") { 
    var2.monitor <- edges[ , sum(get(var2), na.rm = TRUE), by = "Monitor"]$V1
  }
  
  
  
  correlation <- cor(rank(var1.monitor[include]), rank(var2.monitor[include]))
  plot(rank(var1.monitor),rank(var2.monitor), 
       xlab= paste("Ranked Exposure ",var1, sep = ""),
       ylab = paste("Ranked Exposure ",var2, sep = ""), 
       main = paste(paste(regions, collapse = ", "),"correlation =",round(correlation,2), sep = " ")) 
}