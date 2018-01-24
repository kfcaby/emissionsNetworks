
# exposure.type can be NA, continuous, binary
# exposure.var can be avgPM, inmapPM, gams.coeff, num_edges
plotEmissionsNetwork <- function(edges, exposure.type = NA, exposure.var = "avgPM",
                                 exposure.binary.cutoff = 0.70, num.colors = 10, plot.edges = c(0,1000),
                                 main = " ", plot.diagnostics = FALSE,
                                 receptor.regions = c("Northeast","IndustrialMidwest","Southeast"),
                                 plot.legend = TRUE, plot.close.powerplants = TRUE,
                                 xlim = NULL, ylim = NULL){
  
  
  edges <- subset(edges, receptor.region %in% receptor.regions)
  
  #only plot powerplants that could possibly connect to a monitor
  if(plot.close.powerplants == TRUE){
    PP.close <- edges[ , sum(!is.na(lag)), by = "PP"]
    PP.close <- subset(PP.close, V1 > 0)
    edges <- subset(edges, PP %in% PP.close$PP)
  }
  
  
  
  require(maps)
  require(maptools)
  require(viridis)
  
  dft <- par("mar")
  par(mar = c(0,0,0,0))
  US <- map("state",fill=TRUE, plot=FALSE)
  US.names <- US$names
  US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
  US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
  
  if(is.null(ylim)){
    plot(US_poly_sp)
  } else {
    plot(US_poly_sp, ylim = ylim, xlim = xlim)
  }
    
  title(main, line = -3, adj = 0.75, cex.main = 2)
   
  #determine colors of monitors
  if(is.na(exposure.type) || !exposure.type %in% c("binary","continuous")){
    bg.monitor <- "black"
    col.monitor <- "black"
    setkey(edges, Monitor)
    #determine how to plot cases when no PM was observed
    pch.monitor <- ifelse(is.na(edges[J(unique(Monitor)), "avgPM", mult = "first"]),4,21)
    cex.monitor <- ifelse(is.na(edges[J(unique(Monitor)), "avgPM", mult = "first"]),0,3)
  } else{
    setkey(edges, Monitor)
    rbPal <- colorRampPalette(c('white','black'))
    
    if(exposure.var == "avgPM"){
      exposure <- edges[J(unique(Monitor)), get(exposure.var), mult = "first"]
    }
    
    if(exposure.var == "inmapPM"){
      exposure <- edges[ , sum(get(exposure.var), na.rm = TRUE), by = "Monitor"]$V1
    }
    
    if(exposure.var == "gams.coeff"){
      exposure <- edges[ , sum(get(exposure.var)*edge*avgemissions, na.rm = TRUE), by = "Monitor"]$V1
    }
    
    if(exposure.var == "num_edges"){
      exposure <- edges[ , sum(edge, na.rm = TRUE) , by = "Monitor"]$V1
    }
    
    if(exposure.var == "dist_emissions"){
      exposure <- edges[ , sum(avgemissions*(1/log(distance))*edge, na.rm = TRUE) , by = "Monitor"]$V1
    }
    
    if(exposure.type == "continuous"){
      #exposure <- logNA(exposure)
      bg.monitor = rev(viridis(num.colors))[as.numeric(cut(exposure, breaks = num.colors))]
      col.monitor <- bg.monitor
      pch.monitor <- ifelse(is.na(exposure), 4, 21)
      #plot NAs really small
      cex.monitor <- ifelse(is.na(exposure), 0, 3)
      if(exposure.var %in% c("num_edges","gams.coeff")){
        setkey(edges, Monitor)
        pch.monitor <- ifelse(is.na(edges[J(unique(Monitor)), "avgPM", mult = "first"]),4,21)
        cex.monitor <- ifelse(is.na(edges[J(unique(Monitor)), "avgPM", mult = "first"]),0,3)
      }
      
    }
    if(exposure.type == "binary"){
      
      High <- ifelse(exposure > quantile(exposure, exposure.binary.cutoff, na.rm = TRUE), 1, 0)
      
      binary.colors <- viridis(2)
      bg.monitor <- ifelse(High == 1 | is.na(High), binary.colors[1], binary.colors[2])
      col.monitor <- bg.monitor
      pch.monitor <- ifelse(is.na(High), 4, 21)
      cex.monitor <- ifelse(is.na(exposure), 0, 3)
      if(exposure.var %in% c("num_edges","gams.coeff")){
        setkey(edges, Monitor)
        pch.monitor <- ifelse(is.na(edges[J(unique(Monitor)), "avgPM", mult = "first"]),4,21)
        cex.monitor <- ifelse(is.na(edges[J(unique(Monitor)), "avgPM", mult = "first"]),0,3)
      }
      
    }
  }
  
  
  
  setkey(edges, Monitor)
  points(edges[J(unique(Monitor)), c("receptor.longitude","receptor.latitude"), mult = "first"],
         pch = pch.monitor, bg = bg.monitor, col = col.monitor, cex = cex.monitor) 
  
  setkey(edges, PP)
  #scale size
  emissions <- edges[J(unique(PP)), "avgemissions", mult = "first"]$avgemissions
  pp.cex <- ifelse(emissions < quantile(emissions, 0.8), 0.75, 1.25)
  points(edges[J(unique(PP)), c("PP.longitude","PP.latitude"), mult = "first"],
         pch = 24, bg = viridis(2)[2], col = "black", lwd = 2, cex = pp.cex) 
  
  
  par(mar = dft)
  #plot the edges
  plotted.edges = FALSE #used for legend plotting
  if(sum(edges$edge, na.rm = TRUE) > 0 & !any(is.na(plot.edges))){
    #assign colors based on lag
    colors <- viridis(4)
  
    edges.to.plot <- subset(edges, edge == 1 & distance > plot.edges[1] & distance < plot.edges[2])
    
    color.index <- 4 - edges.to.plot$lag
    color.index <- ifelse(color.index < 1, 1, color.index)
    
    segments(edges.to.plot$receptor.longitude,
             edges.to.plot$receptor.latitude,
             edges.to.plot$PP.longitude,
             edges.to.plot$PP.latitude,
             col = colors[color.index],
             lwd = 0.4)
    if(nrow(edges.to.plot) > 0) plotted.edges = TRUE
  }
  if(plot.legend == TRUE){
    if(!is.na(exposure.type)){
      if(exposure.type == "binary"){
        legend(x = -78.8, y = 32.4, 
               legend = c("Large coal power plant","Coal power plant",
                          "High-exposed location", "Low-exposed location"),
               pch = c(24,24,21, 21),
               pt.cex = c(1.25,0.75,3,3),
               pt.bg = c(viridis(2)[2],viridis(2)[2], binary.colors[1],binary.colors[2]),
               cex = 1.5)
      } 
      if(exposure.type != "binary"){
        legend(x = -78.8, y = 32.4, 
               legend = c("Large coal power plant","Coal power plant","AQS monitor"),
               pch = c(24,24,21),
               pt.cex = c(1.25,0.75,3),
               pt.bg = c(viridis(2)[2],viridis(2)[2],"black"),
               cex = 1.5)
      }
    }
    if(is.na(exposure.type)){
      legend(x = -78.8, y = 32.4, 
             legend = c("Large coal power plant","Coal power plant","AQS monitor"),
             pch = c(24,24,21),
             pt.cex = c(1.25,0.75,3),
             pt.bg = c(viridis(2)[2],viridis(2)[2],"black"),
             cex = 1.5)
    }
    
    
    #plot edges legend when edges were plotted 
    if(plotted.edges == TRUE){
      legend(x = -74.06, y = 38.94, 
             legend = c("lag 0 edge","lag 1 edge","lag 2 edge", "lag 3 edge"),
             col = rev(viridis(4)),
             lty = 1, lwd = 2, cex = 1.5)
    }
  }

  
  
}

