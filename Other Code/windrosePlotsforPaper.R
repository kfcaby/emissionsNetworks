#Note: must import edges first using edge_analysis_workflow

require(ggplot2)
require(viridis)
require(gridExtra)
require(grid)
require(cowplot)

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

plotEdgeCounts <- function(edges, regions, title = ""){
  data <- createWindroseData(edges, regions = regions)
  txt.size = 12
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

plotDistanceLegend <- function(edges,regions){
  data <- createWindroseData(edges, regions = regions)
  txt.size = 12
  legend.position = "bottom"
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
  legend <- g_legend(plot)
  return(legend)
}

#FIX SPACE BETWEEN PLOTS

p13 <- plotEdgeCounts(edges, regions = "IndustrialMidwest")
p14 <- plotEdgeCounts(edges, regions = "Northeast")
p15 <- plotEdgeCounts(edges, regions = "Southeast")
p16 <- plotEdgeProbs(edges, regions = "IndustrialMidwest")
p17 <- plotEdgeProbs(edges, regions = "Northeast")
p18 <- plotEdgeProbs(edges, regions = "Southeast")
p1 <- plotEdgeProbs(edges[distance_cat == 1, ], regions = "IndustrialMidwest")
p2 <- plotEdgeProbs(edges[distance_cat == 1, ], regions = "Northeast")
p3 <- plotEdgeProbs(edges[distance_cat == 1, ], regions = "Southeast")
p4 <- plotEdgeProbs(edges[distance_cat == 2, ], regions = "IndustrialMidwest")
p5 <- plotEdgeProbs(edges[distance_cat == 2, ], regions = "Northeast")
p6 <- plotEdgeProbs(edges[distance_cat == 2, ], regions = "Southeast")
p7 <- plotEdgeProbs(edges[distance_cat == 3, ], regions = "IndustrialMidwest")
p8 <- plotEdgeProbs(edges[distance_cat == 3, ], regions = "Northeast")
p9 <- plotEdgeProbs(edges[distance_cat == 3, ], regions = "Southeast")
p10 <- plotEdgeProbs(edges[distance_cat == 4, ], regions = "IndustrialMidwest")
p11 <- plotEdgeProbs(edges[distance_cat == 4, ], regions = "Northeast")
p12 <- plotEdgeProbs(edges[distance_cat == 4, ], regions = "Southeast")
legend <- g_legend(p13)
blank <- rectGrob(gp = gpar(col = "white"))


pdf(file = "results/windrose_plots.pdf", width = 5.5, height = 7)
grid.arrange(arrangeGrob(p13+theme(legend.position = "none"),p16,p1,p4,p7,p10, ncol = 1, 
                         top = textGrob("IndustrialMidwest",gp = gpar(fontsize = 12))), 
             arrangeGrob(p14+theme(legend.position = "none"),p17,p2,p5,p8,p11, ncol = 1,
                         top = textGrob("Northeast", gp = gpar(fontsize = 12))), 
             arrangeGrob(p15+theme(legend.position = "none"),p18,p3,p6,p9,p12, ncol = 1,
                         top = textGrob("Southeast", gp = gpar(fontsize = 12))),
             legend,
             layout_matrix = rbind(c(1,2,3),c(4,4,4)),
             heights = c(0.95,0.05),
             ncol = 3)
dev.off()


