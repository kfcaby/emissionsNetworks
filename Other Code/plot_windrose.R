#adapted from https://rpubs.com/mariner610/windrose
#stat = c("count","prob")

plot.windrose <- function(edges,regions, title, distlim = c(0,1000),
                          stat = "count", plot.legend = TRUE){
  require(viridis)
  require(ggplot2)
  
  legend.position <- ifelse(plot.legend == TRUE, "bottom", "none")
  
  # edges_subset <- subset(edges, edge == 1 & (receptor.region %in% regions))
  edges_subset <- edges[!is.na(edge), ]
  edges_subset <- edges_subset[receptor.region %in% regions, ]
  edges_subset <- edges_subset[distance >= distlim[1] & distance <= distlim[2], ]
  
  distance <- edges_subset$distance
  edge <- edges_subset$edge
  
  # calculate back azimuth (input is currently direction from PP to source)
  # we want the reverse direction
  dir <- ifelse(edges_subset$bearing < 180, edges_subset$bearing + 180, edges_subset$bearing -180)
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
  
  
  # create the plot ----
  if(stat == "count"){
    p.windrose <- ggplot(data = subset(data, edge == 1),
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
            legend.position = legend.position,
            axis.title = element_text(size = txt.size),
            axis.text = element_text(size = txt.size),
            legend.text = element_text(size = txt.size),
            legend.title = element_text(size = txt.size),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
      
      ylim(0,3000) +
      
      ggtitle(title)
  }  
  if(stat != "count"){
    data_summary <- data[ , list(prob = sum(prob)),
                         by = c("distance.binned","dir.binned")]
    p.windrose <- ggplot(data = data_summary,
                         aes(x = dir.binned, y = prob, fill = distance.binned)) +
      geom_col() + 
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
            legend.position = legend.position,
            axis.title = element_text(size = txt.size),
            axis.text = element_text(size = txt.size),
            legend.text = element_text(size = txt.size),
            legend.title = element_text(size = txt.size),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
      
      #ylim(0,600) +
      
      ggtitle(title)
  }
    
  
  # return the handle to the wind rose
  return(p.windrose)
}
