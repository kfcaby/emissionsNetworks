#Note: must import edges first using edge_analysis_workflow

source(file = "R/windrosePlots.R")


edges.subset[ , distance_cat := ifelse(distance <= 250, 1, 
                                ifelse(distance <= 500, 2,
                                       ifelse(distance <= 750, 3,
                                              ifelse(distance <= 1000, 4, NA))))]


#Monitor Center

p13 <- plotPairCounts(edges.subset[distance < 1000, ], regions = "IndustrialMidwest", ylim = c(0,8000))
p14 <- plotPairCounts(edges.subset[distance < 1000, ], regions = "Northeast")
p15 <- plotPairCounts(edges.subset[distance < 1000, ], regions = "Southeast")
p16 <- plotPairCounts(edges.subset[edge == 1, ], regions = "IndustrialMidwest")
p17 <- plotPairCounts(edges.subset[edge == 1, ], regions = "Northeast")
p18 <- plotPairCounts(edges.subset[edge == 1, ], regions = "Southeast")
p1 <- plotEdgeProbs(edges.subset[distance_cat == 1, ], regions = "IndustrialMidwest")
p2 <- plotEdgeProbs(edges.subset[distance_cat == 1, ], regions = "Northeast")
p3 <- plotEdgeProbs(edges.subset[distance_cat == 1, ], regions = "Southeast")
p4 <- plotEdgeProbs(edges.subset[distance_cat == 2, ], regions = "IndustrialMidwest")
p5 <- plotEdgeProbs(edges.subset[distance_cat == 2, ], regions = "Northeast")
p6 <- plotEdgeProbs(edges.subset[distance_cat == 2, ], regions = "Southeast")
p7 <- plotEdgeProbs(edges.subset[distance_cat == 3, ], regions = "IndustrialMidwest")
p8 <- plotEdgeProbs(edges.subset[distance_cat == 3, ], regions = "Northeast")
p9 <- plotEdgeProbs(edges.subset[distance_cat == 3, ], regions = "Southeast")
p10 <- plotEdgeProbs(edges.subset[distance_cat == 4, ], regions = "IndustrialMidwest")
p11 <- plotEdgeProbs(edges.subset[distance_cat == 4, ], regions = "Northeast")
p12 <- plotEdgeProbs(edges.subset[distance_cat == 4, ], regions = "Southeast")
legend <- g_legend(p13)
blank <- rectGrob(gp = gpar(col = "white"))

#pdf(file = "results/windrose_plots_negative.pdf", width = 6.5, height = 8)
grid.arrange(arrangeGrob(p13+theme(legend.position = "none"),
                         p16+theme(legend.position = "none"),
                         p1,p4,p7,p10, ncol = 1, 
                         top = textGrob("IndustrialMidwest",gp = gpar(fontsize = 12))), 
             arrangeGrob(p14+theme(legend.position = "none"),
                         p17+theme(legend.position = "none"),
                         p2,p5,p8,p11, ncol = 1,
                         top = textGrob("Northeast", gp = gpar(fontsize = 12))), 
             arrangeGrob(p15+theme(legend.position = "none"),
                         p18+theme(legend.position = "none"),
                         p3,p6,p9,p12, ncol = 1,
                         top = textGrob("Southeast", gp = gpar(fontsize = 12))),
             legend,
             layout_matrix = rbind(c(1,2,3),c(4,4,4)),
             heights = c(0.95,0.05),
             ncol = 3)
#dev.off()


#Power plant center


 
p13 <- plotPairCounts(edges.subset[distance < 1000, ], regions = "IndustrialMidwest", center = "powerplants", ylim = c(0,9000))
p14 <- plotPairCounts(edges.subset[distance < 1000, ], regions = "Northeast", center = "powerplants", ylim = c(0,9000))
p15 <- plotPairCounts(edges.subset[distance < 1000, ], regions = "Southeast", center = "powerplants", ylim = c(0,9000))
p16 <- plotPairCounts(edges.subset[edge == 1, ], regions = "IndustrialMidwest", center = "powerplants",ylim = c(0,3000))
p17 <- plotPairCounts(edges.subset[edge == 1, ], regions = "Northeast", center = "powerplants",ylim = c(0,3000))
p18 <- plotPairCounts(edges.subset[edge == 1, ], regions = "Southeast", center = "powerplants",ylim = c(0,3000))
p1 <- plotEdgeProbs(edges.subset[distance_cat == 1, ], regions = "IndustrialMidwest", center = "powerplants")
p2 <- plotEdgeProbs(edges.subset[distance_cat == 1, ], regions = "Northeast", center = "powerplants")
p3 <- plotEdgeProbs(edges.subset[distance_cat == 1, ], regions = "Southeast", center = "powerplants")
p4 <- plotEdgeProbs(edges.subset[distance_cat == 2, ], regions = "IndustrialMidwest", center = "powerplants")
p5 <- plotEdgeProbs(edges.subset[distance_cat == 2, ], regions = "Northeast", center = "powerplants")
p6 <- plotEdgeProbs(edges.subset[distance_cat == 2, ], regions = "Southeast", center = "powerplants")
p7 <- plotEdgeProbs(edges.subset[distance_cat == 3, ], regions = "IndustrialMidwest", center = "powerplants")
p8 <- plotEdgeProbs(edges.subset[distance_cat == 3, ], regions = "Northeast", center = "powerplants")
p9 <- plotEdgeProbs(edges.subset[distance_cat == 3, ], regions = "Southeast", center = "powerplants")
p10 <- plotEdgeProbs(edges.subset[distance_cat == 4, ], regions = "IndustrialMidwest", center = "powerplants")
p11 <- plotEdgeProbs(edges.subset[distance_cat == 4, ], regions = "Northeast", center = "powerplants")
p12 <- plotEdgeProbs(edges.subset[distance_cat == 4, ], regions = "Southeast", center = "powerplants")
legend <- g_legend(p13)
blank <- rectGrob(gp = gpar(col = "white"))

#pdf(file = "results/windrose_powerplants.pdf", width = 6.5, height = 8)
grid.arrange(arrangeGrob(p13+theme(legend.position = "none"),
                         p16+theme(legend.position = "none"),
                         p1,p4,p7,p10, ncol = 1, 
                         top = textGrob("IndustrialMidwest",gp = gpar(fontsize = 12))), 
             arrangeGrob(p14+theme(legend.position = "none"),
                         p17+theme(legend.position = "none"),
                         p2,p5,p8,p11, ncol = 1,
                         top = textGrob("Northeast", gp = gpar(fontsize = 12))), 
             arrangeGrob(p15+theme(legend.position = "none"),
                         p18+theme(legend.position = "none"),
                         p3,p6,p9,p12, ncol = 1,
                         top = textGrob("Southeast", gp = gpar(fontsize = 12))),
             legend,
             layout_matrix = rbind(c(1,2,3),c(4,4,4)),
             heights = c(0.95,0.05),
             ncol = 3)
#dev.off()



