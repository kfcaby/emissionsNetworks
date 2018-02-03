#adapted from https://rpubs.com/mariner610/windrose

plot.windrose <- function(edges,regions, title){
  require(viridis)
  require(ggplot2)
  
  edges_subset <- subset(edges, edge == 1 & (receptor.region %in% regions))
  distance <- edges_subset$distance
  
  # calculate back azimuth (input is currently direction from PP to source)
  # we want the reverse direction
  dir <- ifelse(edges_subset$bearing < 180, edges_subset$bearing + 180, edges_subset$bearing -180)
  dir <- ifelse(dir > 345, dir - 360, dir)
  
  if (is.numeric(distance) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(distance = distance,
                       dir = dir)
    distance = "distance"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  

  data$distance.binned <- cut(x = data[[distance]],
                         breaks = c(0,250,500,750,1000),
                         ordered_result = TRUE)
  data$distance.binned = with(data, factor(distance.binned, levels = rev(levels(distance.binned))))
  
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = seq(-15,345,30),
                    ordered_result = TRUE)
  #levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned

  txt.size = 14
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = distance.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = c("N","","","E","","","S","","","W","","")) +
    
    coord_polar(start = -((30/2)/360) * 2*pi) +
    
    scale_fill_manual(name = "Distance to Source (km)",
                      labels = c("750-1000","500-750","250-500","0-250"),
                      values = viridis(4),
                      drop = FALSE) +
    #theme_bw() +
    theme(axis.title.x = element_blank(),
          panel.grid.major = element_line(colour="grey65"),
          legend.position = "bottom",
          axis.title = element_text(size = txt.size),
          axis.text = element_text(size = txt.size),
          legend.text = element_text(size = txt.size),
          legend.title = element_text(size = txt.size),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
    
    #ylim(0,600) +
    
    ggtitle(paste("Edge counts by distance/direction to source \n",title," receptors", sep =""))
    
  # print the plot
  #print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}
