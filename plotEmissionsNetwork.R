 
plotDirections <- function(edges, region){
  potential.edges <- subset(edges, !(is.na(edge)) & PP.region == region) 
  probs <- potential.edges[ , list(sum(edge == 1,na.rm = TRUE)/sum(!is.na(edge))), by = "direction"]
  probs <- probs[complete.cases(probs),]
  probs[ , direction := factor(direction, levels = c("N","NE","E","SE","S","SW","W","NW"))]
  setkey(probs,direction)
  barplot(height = probs$V1, space = 0, main = paste("Edge Probability by Direction (",region," Power Plants)", sep = ""))
  axis(1, labels = probs$direction, at = 1:(nrow(probs)) - 0.5)
}

plotRadial <- function(edges, region, samps){
  potential.edges <- subset(edges, !(is.na(edge)) & PP.region == region) 
  potential.edges <- potential.edges[sample(1:nrow(potential.edges),samps),]
  lines.lty <- ifelse(potential.edges$edge == 1, 1, 1)
  lines.lwd <- ifelse(potential.edges$edge == 1, 4, 0.25)
  lines.color <- ifelse(potential.edges$edge == 1, "green", "red")
  polar.plot(potential.edges$distance,potential.edges$bearing,main= paste(region,"Links", sep = " "),start = 90,
             lwd=lines.lwd,line.col= lines.color, clockwise = TRUE, 
             labels = c("N","NE","E","SE", "S","SW","W","NW"),
             label.pos = seq(0,315, by = 45))
  points(0,0, cex = 3, pch = 24, bg = "yellow")
}

# exposure.type can be NA, continuous, binary
# exposure.var can be avgPM, inmapPM, gams.coeff, num_edges
plotEmissionsNetwork <- function(edges, exposure.type = NA, exposure.var = "avgPM", exposure.binary.cutoff = 0.80, num.colors = 10, plot.edges = c(0,1000),
                                 main = " ", plot.diagnostics = TRUE){
  
  
  require(RColorBrewer)
  require(maps)
  require(maptools)
  #require(plotrix)
  
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
  
  par(mar = dft)
  #plot the edges
  if(sum(edges$edge, na.rm = TRUE) > 0 & !any(is.na(plot.edges))){
    #assign colors based on lag
    colors <- brewer.pal(n = 4, name = "RdYlBu")
  
    edges.to.plot <- subset(edges, edge == 1 & distance > plot.edges[1] & distance < plot.edges[2])
    
    color.index <- 4 - edges.to.plot$lag
    color.index <- ifelse(color.index < 1, 1, color.index)
    
    segments(edges.to.plot$M.longitude,
             edges.to.plot$M.latitude,
             edges.to.plot$PP.longitude,
             edges.to.plot$PP.latitude,
             col = colors[color.index],
             lwd = 0.4)
  }
  
  if(plot.diagnostics == TRUE){
    probs <- edges[ , list(sum(edge == 1,na.rm = TRUE)/sum(!is.na(edge))), by = "lag"]
    probs <- probs[complete.cases(probs),]
    setkey(probs,lag)
    barplot(height = probs$V1, space = 0, main = "Edge Probability by Lag")
    axis(1, labels = 0:nrow(probs), at = 1:(nrow(probs) + 1) - 0.5)
    
    
    #edge angle
    samps = 20000
    plotDirections(edges, region = "Northeast")
    #plotRadial(edges, region = "Northeast", samps)
    plotDirections(edges, region = "IndustrialMidwest")
    #plotRadial(edges, region = "IndustrialMidwest", samps)
    plotDirections(edges, region = "Southeast")
    #plotRadial(edges, region = "Southeast", samps)
    
  }
  
}

