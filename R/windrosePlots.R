require(ggplot2)
require(viridis)
require(gridExtra)
require(grid)
require(cowplot)

createWindroseData <- function(edges, regions, distlim = c(0,1000), center = "monitors"){
  edges_subset <- edges[!is.na(edge), ]
  if(center == "monitors"){
    edges_subset <- edges_subset[receptor.region %in% regions, ]
  }
  if(center == "powerplants"){
    edges_subset <- edges_subset[PP.region %in% regions, ]
  }
  edges_subset <- edges_subset[distance >= distlim[1] & distance <= distlim[2], ]
  
  distance <- edges_subset$distance
  edge <- edges_subset$edge
  
  # calculate back azimuth (input is currently direction from PP to source)
  # we want the reverse direction
  if(center == "monitors"){
    dir <- ifelse(edges_subset$bearing < 180, edges_subset$bearing + 180, edges_subset$bearing -180)
  }
  if(center == "powerplants"){
    dir <- edges_subset$bearing
  }
  
  dir <- ifelse(dir > 345, dir - 360, dir)
  
  if (is.numeric(distance) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(distance = distance,
                       dir = dir, edge = edge)
    distance = "distance"
    dir = "dir"
    edge = "edge"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  data$distance.binned <- cut(x = data[[distance]],
                              breaks = c(0,250,500,750,1000),
                              ordered_result = TRUE)
  data$distance.binned = with(data, factor(distance.binned, 
                                           levels = rev(levels(distance.binned))))
  
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = seq(-15,345,30),
                    ordered_result = TRUE)
  #levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  txt.size = 14
  
  data <- data.table(data)
  setkey(data, dir.binned)
  dir.counts <- data[ , list(pairs.dir = length(edge)),
                      by = "dir.binned"]
  setkey(dir.counts, dir.binned)
  data <- data[dir.counts]
  data[ , prob := edge/pairs.dir]
  
  return(data)
}

plotPairCounts <- function(edges, regions, title = "", center = "monitors"){
  data <- createWindroseData(edges, regions = regions, center = center)
  txt.size = 12
  if(center == "monitors") {legend.title <- "monitor to power plant distance (km)"}
  if(center == "powerplants") {legend.title <- "power plant to monitor distance (km)"}
  plot <- ggplot(data = data,aes(x = dir.binned, fill = distance.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = c("N","","","E","","","S","","","W","","")) + 
    coord_polar(start = -((30/2)/360) * 2*pi) +
    scale_fill_manual(name = legend.title,
                      labels = c("750-1000","500-750","250-500","0-250"),
                      values = viridis(4),
                      drop = FALSE) +
    theme(axis.title.x = element_blank(),
          panel.grid.major = element_line(colour="grey65"),
          legend.position = "bottom",
          axis.title = element_text(size = txt.size),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.line = element_blank(),
          #axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          legend.text = element_text(size = txt.size),
          legend.title = element_text(size = txt.size),
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.margin = unit(rep(-0.3,4), "cm")
    ) + 
    #  ylim(ylim[1],ylim[2]) +
    ggtitle(title) + guides(fill = guide_legend(reverse = T))
  return(plot)
}

plotEdgeProbs <- function(edges, regions, center = "monitors"){
  data <- createWindroseData(edges, regions = regions, center = center)
  data_summary <- data[ , list(prob = sum(prob)),
                        by = c("distance.binned","dir.binned")]
  txt.size = 12
  title = ""
  legend.position = "none"
  plot <- ggplot(data = data_summary, aes(x = dir.binned, y = prob, fill = distance.binned)) +
    geom_col() + scale_x_discrete(drop = FALSE,
                                  labels = c("N","","","E","","","S","","","W","","")) +
    coord_polar(start = -((30/2)/360) * 2*pi) +
    scale_fill_manual(name = "Distance to Source (km)",
                      labels = c("750-1000","500-750","250-500","0-250"),
                      values = viridis(4),
                      drop = FALSE) +
    theme(axis.title.x = element_blank(),
          panel.grid.major = element_line(colour="grey65"),
          legend.position = legend.position,
          axis.title = element_blank(),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.line = element_blank(),
          #axis.ticks.y = element_blank(),
          legend.text = element_text(size = txt.size),
          legend.title = element_text(size = txt.size),
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.margin = unit(rep(-0.3,4), "cm")
    ) + 
    #  ylim(ylim[1],ylim[2]) + 
    ggtitle(title)
  
  return(plot)
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}