 
# exposure.type can be NA, continuous, binary
# exposure.var can be avgPM, inmapPM, gams.coeff, edge
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
      rbPal <- colorRampPalette(c('white','black'))
      if(exposure.var == "avgPM"){
        exposure <- edges[J(unique(Monitor)), get(exposure.var), mult = "first"]
      }
      if(exposure.var %in% c("inmapPM", "gams.coeff", "edge")){
        exposure <- edges[ , sum(get(exposure.var), na.rm = TRUE), by = "Monitor"]$V1
      }
      #exposure <- logNA(exposure)
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

