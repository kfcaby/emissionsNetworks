
rankComparison <- function(edges, var1, var2, regions = c("Northeast","Southeast","IndustrialMidwest")){
  require(ggplot2)
  require(viridis)
  
  edges <- subset(edges, receptor.region %in% regions)
  include <- 1:length(unique(edges$Monitor)) 
  
  setkey(edges, Monitor)
  if(var1 == "gams.coeff"){
    var1.monitor <- edges[ , list(V1 = sum(get(var1)*edge*avgemissions, na.rm = TRUE),
                                  receptor.state = unique(receptor.state)),
                          by = "Monitor"]
    include <- which(var1.monitor$V1 > 0)
  } 
  if(var1 == "avgPM") { 
    var1.monitor <- edges[ , list(V1 = unique(get(var1)),
                                  receptor.state = unique(receptor.state)), 
                          by = "Monitor"]
  }
  if(var1 == "inmapPM") { 
    var1.monitor <- edges[ , list(V1 = sum(get(var1), na.rm = TRUE),
                                  receptor.state = unique(receptor.state)),
                          by = "Monitor"]
  }
  if(var1 == "num_edges"){
    var1.monitor <- edges[ , list(V1 = sum(edge, na.rm = TRUE),
                                  receptor.state = unique(receptor.state)),
                          by = "Monitor"]
    include <- which(var1.monitor$V1 > 0)
  }
  if(var1 == "avgemissions"){
    var1.monitor <- edges[ , list(V1 = sum(log(avgemissions)*edge, na.rm = TRUE),
                                  receptor.state = unique(receptor.state)),
                          by = "Monitor"]
    include <- which(var1.monitor$V1 > 0)
  }
  if(var1 == "InMAPedge"){
    var1.monitor <- edges[ , list(V1 = sum(inmapPM*edge, na.rm = TRUE),
                                  receptor.state = unique(receptor.state)),
                          by = "Monitor"]
    include <- which(var1.monitor$V1 > 0)
  }
  if(var1 == "dist_emissions"){
    var1.monitor <- edges[ , list(V1 = sum(avgemissions*(1/log(distance))*edge, na.rm = TRUE),
                                  receptor.state = unique(receptor.state)),
                          by = "Monitor"]
    include <- which(var1.monitor$V1 > 0)
  }
  
  
  
  if(var2 == "gams.coeff"){
    var2.monitor <- edges[ , sum(get(var2)*edge*avgemissions, na.rm = TRUE), by = "Monitor"]$V1
    include <- which(var2.monitor$V1 > 0)
  } 
  if(var2 == "avgPM") { 
    var2.monitor <- edges[ , list(V1 = unique(get(var2)),
                                  receptor.state = unique(receptor.state)), 
                          by = "Monitor"]
  }
  if(var2 == "inmapPM") { 
    var2.monitor <- edges[ , list(V1 = sum(get(var2), na.rm = TRUE),
                                  receptor.state = unique(receptor.state)),
                          by = "Monitor"]
  }
  if(var2 == "num_edges"){
    var2.monitor <- edges[ , list(V1 = sum(edge, na.rm = TRUE),
                                  receptor.state = unique(receptor.state)),
                          by = "Monitor"]
  }
  if(var2 == "avgemissions"){
    var2.monitor <- edges[ , list(V1 = sum(avgemissions*edge, na.rm = TRUE),
                          receptor.state = unique(receptor.state)),
  by = "Monitor"]
  }
  if(var2 == "InMAPedge"){
    var2.monitor <- edges[ , list(V1 = sum(inmapPM*edge, na.rm = TRUE),
                                  receptor.state = unique(receptor.state)),
                          by = "Monitor"]
  }

  correlation <- cor(rank(var1.monitor[include,V1]), rank(var2.monitor[include,V1]))
#   plot(rank(var1.monitor$V1),rank(var2.monitor$V1), 
#        xlab= paste("Ranked Exposure ",var1, sep = ""),
#        ylab = paste("Ranked Exposure ",var2, sep = ""), 
#        main = paste(paste(regions, collapse = ", "),"correlation =",round(correlation,2), sep = " "),
#        cex.lab = 2, cex = 2, cex.main = 2, cex.axis = 2) 
  
  var1.monitor[ , receptor.state := ifelse(receptor.state %in% c("RI","CT","MA","ME","VT","NH"),
                                           "N.Eng", receptor.state)]
  var1.monitor[ , receptor.state := ifelse(receptor.state %in% c("NJ","MD","DE","DC"),
                                           "MidAtl", receptor.state)]
  var1.monitor[ , receptor.state := ifelse(receptor.state %in% c("GA","SC"),
                                          "SC/GA", receptor.state)]
  var1.monitor[ , receptor.state := ifelse(receptor.state %in% c("MS","AL","LA"),
                                         "AL/MS/LA", receptor.state)]
  
  #include <- which(!is.na(var2.monitor$V1))
  comparison <- data.frame(var1.rank = rank(var1.monitor[include, V1]), 
                           var2.rank = rank(var2.monitor[include, V1]),
                           receptor.state = factor(var1.monitor[include,receptor.state],
                                                   levels = rev(c("N.Eng","NY",
                                                              "MidAtl","VA", "PA",
                                                              "WI","IL","MI","IN","KY","OH","WV",
                                                              "FL","LA","SC/GA","AL/MS/LA",
                                                              "AR","NC","TN"))))
                          
  

  txt.size = 16
  p.comparison <- ggplot(comparison, aes(x = var1.rank, y = var2.rank, color = receptor.state)) + geom_point(size = 8) + 
    scale_color_viridis(discrete = TRUE, begin = 0, end = 1, name = "Monitor State") +
    theme(legend.position = "right",
        axis.title = element_text(size = txt.size),
        axis.text = element_text(size = txt.size),
        legend.text = element_text(size = txt.size),
        legend.title = element_text(size = txt.size),
        legend.justification = "top",
      #  legend.key.size = unit(2, "line" ),
        plot.title = element_text(size = txt.size + 2, hjust = 0.5)) +
    labs(x = paste("Ranked Exposure ",var1, sep = ""), y = paste("Ranked Exposure ",var2, sep = ""),
         title =  paste(paste(regions, collapse = ", ")," Monitors \n (correlation = ",round(correlation,2),")", sep = ""))

  return(p.comparison)
  
  
  
}