createWindroseData <- function(edges, regions, distlim = c(0,1000)){
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
  
  return(data)
}

plotEdgeCounts <- function(edges, regions){
  data <- createWindroseData(edges, regions = regions)
  txt.size = 12
  legend.position = "none"
  title = ""
  plot <- ggplot(data = subset(data, edge == 1),aes(x = dir.binned, fill = distance.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = c("N","","","E","","","S","","","W","","")) + 
    coord_polar(start = -((30/2)/360) * 2*pi) +
    scale_fill_manual(name = "Distance to Source (km)",
                      labels = c("750-1000","500-750","250-500","0-250"),
                      values = viridis(4),
                      drop = FALSE) +
    theme(axis.title.x = element_blank(),
          panel.grid.major = element_line(colour="grey65"),
          legend.position = legend.position,
          axis.title = element_text(size = txt.size),
          axis.text.x = element_text(size = txt.size),
          axis.text.y = element_text(size = 8),
          #axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          legend.text = element_text(size = txt.size),
          legend.title = element_text(size = txt.size),
          plot.title = element_text(size = 12, hjust = 0.5)) + 
    #  ylim(ylim[1],ylim[2]) +
    ggtitle(title)
  return(plot)
}

plotEdgeProbs <- function(edges, regions){
  data <- createWindroseData(edges, regions = regions)
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
          axis.text.x = element_text(size = txt.size),
          axis.text.y = element_text(size = 8),
          #axis.ticks.y = element_blank(),
          legend.text = element_text(size = txt.size),
          legend.title = element_text(size = txt.size),
          plot.title = element_text(size = 12, hjust = 0.5)) + 
  #  ylim(ylim[1],ylim[2]) + 
    ggtitle(title)
  
  return(plot)
}


#Edge Counts Plots
pdf(file = "results/windrose_edgecount.pdf", width = 6.5, height = 2)
p1 <- plotEdgeCounts(edges, regions = "IndustrialMidwest")
p2 <- plotEdgeCounts(edges, regions = "Northeast")
p3 <- plotEdgeCounts(edges, regions = "Southeast")
grid.arrange(p1,p2,p3, ncol = 3)
dev.off()

#Finish implementing this code for the other plots

#Edge Probability Plots
pdf(file = "results/windrose_edgeprob.pdf", width = 6.5, height = 2)
p1 <- plotEdgeProbs(edges, regions = "IndustrialMidwest")
p2 <- plotEdgeProbs(edges, regions = "Northeast")
p3 <- plotEdgeProbs(edges, regions = "Southeast")
grid.arrange(p1,p2,p3, ncol = 3)
dev.off()

#Edge Probability Plots (<250km)
pdf(file = "results/windrose_edgeprob250.pdf", width = 6.5, height = 2)
p1 <- plotEdgeProbs(edges[distance_cat == 1, ], regions = "IndustrialMidwest")
p2 <- plotEdgeProbs(edges[distance_cat == 1, ], regions = "Northeast")
p3 <- plotEdgeProbs(edges[distance_cat == 1, ], regions = "Southeast")
grid.arrange(p1,p2,p3, ncol = 3)
dev.off()

pdf(file = "results/windrose_edgeprob250.pdf", width = 6.5, height = 2)
p1 <- plotEdgeProbs(edges[distance_cat == 1, ], regions = "IndustrialMidwest")
p2 <- plotEdgeProbs(edges[distance_cat == 1, ], regions = "Northeast")
p3 <- plotEdgeProbs(edges[distance_cat == 1, ], regions = "Southeast")
grid.arrange(p1,p2,p3, ncol = 3)
dev.off()



#plot 1
pdf(file = "results/windrose_edgeprob.pdf", width = 6.5, height = 2)
txt.size = 12
legend.position = "none"
title = ""
ylim = c(0,0.4)
data <- createWindroseData(edges, regions = "IndustrialMidwest")
data_summary <- data[ , list(prob = sum(prob)),
                     by = c("distance.binned","dir.binned")]
p1 <- ggplot(data = data_summary, aes(x = dir.binned, y = prob, fill = distance.binned)) +
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
        axis.text.x = element_text(size = txt.size),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = txt.size),
        legend.title = element_text(size = txt.size),
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  ylim(ylim[1],ylim[2]) + 
  ggtitle(title)
#Plot 2
data <- createWindroseData(edges, regions = "Northeast")
data_summary <- data[ , list(prob = sum(prob)),
                     by = c("distance.binned","dir.binned")]
p2 <- ggplot(data = data_summary, aes(x = dir.binned, y = prob, fill = distance.binned)) +
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
        axis.text.x = element_text(size = txt.size),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = txt.size),
        legend.title = element_text(size = txt.size),
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  ylim(ylim[1],ylim[2]) + 
  ggtitle(title)
#Plot 3
data <- createWindroseData(edges, regions = "Southeast")
data_summary <- data[ , list(prob = sum(prob)),
                     by = c("distance.binned","dir.binned")]
p3 <- ggplot(data = data_summary, aes(x = dir.binned, y = prob, fill = distance.binned)) +
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
        axis.text.x = element_text(size = txt.size),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = txt.size),
        legend.title = element_text(size = txt.size),
        plot.title = element_text(size = 12, hjust = 0.5)) + 
  ylim(ylim[1],ylim[2]) + 
  ggtitle(title)
grid.arrange(p1,p2,p3, ncol = 3)
dev.off()

