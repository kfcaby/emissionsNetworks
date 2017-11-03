 
# exposure.type can be NA, continuous, binary
# exposure.var can be avgPM, inmapPM, gams.coeff, num_edges
plotEmissionsNetwork <- function(edges, exposure.type = NA, exposure.var = "avgPM", exposure.binary.cutoff = 0.80, num.colors = 10, plot.edges = TRUE,
                                 main = " ", plot.diagnostics = TRUE){
  require(RColorBrewer)
  require(maps)
  require(maptools)
  dft <- par("mar")
  par(mar = c(0,0,0,0))
  US <- map("state",fill=TRUE, plot=FALSE)
  US.names <- US$names
  US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
  US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
  plot(US_poly_sp)
  title(main, line = -3)
  
  
  #determine colors of monitors
  if(is.na(exposure.type) || !exposure.type %in% c("binary","continuous")){
    bg.monitor <- "green"
    col.monitor <- "black"
    setkey(edges, Monitor)
    pch.monitor <- ifelse(is.na(edges[J(unique(Monitor)), "avgPM", mult = "first"]),4,21)
  } else{
    setkey(edges, Monitor)
    rbPal <- colorRampPalette(c('white','black'))
    
    if(exposure.var == "avgPM"){
      exposure <- edges[J(unique(Monitor)), get(exposure.var), mult = "first"]
    }
    
    if(exposure.var %in% c("inmapPM", "gams.coeff")){
      exposure <- edges[ , sum(get(exposure.var), na.rm = TRUE), by = "Monitor"]$V1
    }
    
    if(exposure.var == "num_edges"){
      monitor_degree <- edges[, list(degree = sum(edge,na.rm = TRUE),
                                     possible = sum(distance < max.distance, na.rm = TRUE)),
                              by = "Monitor"]
      setkey(monitor_degree, Monitor)
      exposure <- monitor_degree[ , percent := degree/possible]$percent
    }
    
    if(exposure.type == "continuous"){
      #exposure <- logNA(exposure)
      bg.monitor = rbPal(num.colors)[as.numeric(cut(exposure, breaks = num.colors))]
      col.monitor <- "black"
      pch.monitor <- ifelse(is.na(exposure), 4, 21)
      if(exposure.var %in% c("num_edges","gams.coeff")){
        setkey(edges, Monitor)
        pch.monitor <- ifelse(is.na(edges[J(unique(Monitor)), "avgPM", mult = "first"]),4,21)
      }
    }
    if(exposure.type == "binary"){
      
      High <- ifelse(exposure > quantile(exposure, exposure.binary.cutoff, na.rm = TRUE), 1, 0)
      
      bg.monitor <- ifelse(High == 1 | is.na(High), "red","green")
      col.monitor <- "black"
      pch.monitor <- ifelse(is.na(High), 4, 21)
      if(exposure.var %in% c("num_edges","gams.coeff")){
        setkey(edges, Monitor)
        pch.monitor <- ifelse(is.na(edges[J(unique(Monitor)), "avgPM", mult = "first"]),4,21)
      }
    }
  }
  
  #Plot the monitors and the power plants
  setkey(edges, PP)
  points(edges[J(unique(PP)), c("PP.longitude","PP.latitude"), mult = "first"],
         pch = 24, bg = "yellow", col = "black", lwd = 0.50, cex = 0.5) 
  setkey(edges, Monitor)
  points(edges[J(unique(Monitor)), c("M.longitude","M.latitude"), mult = "first"],
         pch = pch.monitor, bg = bg.monitor, col = col.monitor, lwd = 0.50, cex = 1) 
  
  
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
  par(mar = dft)
}

