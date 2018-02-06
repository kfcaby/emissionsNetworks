rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks")

library(data.table)
library(geosphere)
library(gridExtra)
library(grid)

#source(file = "../functions_emissions_networks.R")
source(file = "R/plotEmissionsNetwork.R")
source(file = "R/import_edges.R")
source(file = "R/edge_analysis.R")
source(file = "R/rankComparison.R")
source(file = "R/connectivityComparison.R")
source(file = "R/plotLagProbs.R")
source(file = "Other Code/plot_windrose.R")

#Parameters
unit.type <- "monitor" # either "zipcode" or "monitor"
season <- "year_noNA_distLag"
PM.type <- "decomposed75" #c("raw", "decomposedXX")

set.seed(1000)

#imports output from fitDailyPMmodels
edges <- import_edges(unit.type,season, PM.type)

# edges <- subset(edges, PP == "PP1010")
# pdf(file = "results/january2005.pdf", height = 9, width = 22)
# plotEmissionsNetwork(edges, plot.diagnostics = FALSE, plot.legend = FALSE, main = "January 2005")
# dev.off()



#START HERE
plot.file = paste(unit.type,"_networks/plots/",unit.type,"_",season,"_",PM.type,".pdf", sep = "")
pdf(plot.file, height = 9, width = 22)

#blank map
#pdf(file = "blankmap.pdf, height = 9, width = 22")
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, main = "",
                     plot.edges = c(0,0), plot.close.powerplants = FALSE)
#dev.off()

#average PM exposure
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "continuous", 
                     exposure.var = "avgPM",main = paste("avgPM_",season,"_",PM.type, sep ="")
                     , plot.edges = c(0,0))


#edges by region

plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste("IndustrialMidwest - ",season," 2005", sep = ""), receptor.regions = "IndustrialMidwest")
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste("Northeast - ",season," 2005", sep = ""), receptor.regions = "Northeast")
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste("Southeast - ",season," 2005", sep = ""), receptor.regions = "Southeast")


#edge plots by lag 
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = "Edges 0-250km", 
                     plot.edges = c(0, 250))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = "Edges 250-500km", 
                     plot.edges = c(250,500))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = "Edges 500-750km", 
                     plot.edges = c(500, 750))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = "Edges 750-1000km", 
                     plot.edges = c(750, 1000))
par(mfrow = c(1,2))


#Plot Ohio Moniotors for fun
monitors <- edges[ , list(edges = sum(edge, na.rm = TRUE),
                          receptor.state = unique(receptor.state)),
                  by = "Monitor"]
monitors <- subset(monitors, edges > 0)
setkey(monitors, Monitor)

monitors.Ohio <- subset(monitors, receptor.state == "OH" & edges > 0)[1:8,]
par(mfrow = c(2,4),
    mar = c(1,1,1,1))
lapply(monitors.Ohio$Monitor,
       function(x){
         edges_monitor <- subset(edges, Monitor == x)
         plotEmissionsNetwork(edges_monitor, plot.diagnostics = FALSE, main = "", 
                              xlim = c(-90.7,-78.3), ylim = c(36.2,43.8),
                              plot.legend = FALSE)
         box(which = "plot", lwd = 3)
})

monitors.sample <- monitors[ , .SD[sample(.N,min(.N,8))], by = "receptor.state"]
lapply(monitors.sample$Monitor,
       function(x){
         edges_monitor <- subset(edges, Monitor == x)
         lat <- unique(edges_monitor$receptor.latitude)
         long <- unique(edges_monitor$receptor.longitude)
         plotEmissionsNetwork(edges_monitor, plot.diagnostics = FALSE, main = "", 
                              xlim = c(long-6.2,long + 6.2), ylim = c(lat - 3.8,lat + 3.8),
                              plot.legend = FALSE)
         box(which = "plot", lwd = 3)
})

#diagnostics
par(mfrow = c(1,3))
plotLagProbs(edges, region = "IndustrialMidwest")
plotLagProbs(edges, region = "Northeast")
plotLagProbs(edges, region = "Southeast")
par(mfrow = c(1,1))

p1 <- plot.windrose(edges, regions = "Northeast", 
                    title = "edge counts - by direction \nfrom NE monitor to power plants")
p2 <- plot.windrose(edges, regions = "IndustrialMidwest", title = "Industrial Midwest")
p3 <- plot.windrose(edges, regions = "Southeast", title = "Southeast")
grid.arrange(p2,p1,p3, ncol = 3)

plot.windrose(edges, regions = "Southeast", stat = "prob", distlim = c(0,1000),
              title = "edge counts - by direction \nfrom SE monitor to power plants")

# p1 <- plot.windrose(subset(edges, receptor.state == "IL"), regions = "IndustrialMidwest", title = "Illinois")
# p2 <- plot.windrose(subset(edges, receptor.state == "IN"), regions = "IndustrialMidwest", title = "Indiana")
# p3 <- plot.windrose(subset(edges, receptor.state == "OH"), regions = "IndustrialMidwest", title = "Ohio")
# grid.arrange(p1,p2,p3, ncol = 3)
# 
# p1 <- plot.windrose(subset(edges, receptor.state == "MI"), regions = "IndustrialMidwest", title = "Michigan")
# p2 <- plot.windrose(subset(edges, receptor.state == "KY"), regions = "IndustrialMidwest", title = "Kentucky")
# p3 <- plot.windrose(subset(edges, receptor.state == "WV"), regions = "IndustrialMidwest", title = "West Virginia")
# grid.arrange(p1,p2,p3, ncol = 3)
# 
# p1 <- plot.windrose(subset(edges, receptor.state == "PA"), regions = "Northeast", title = "Pennsylvania")
# p2 <- plot.windrose(subset(edges, receptor.state == "VA"), regions = "Northeast", title = "Virginia")
# p3 <- plot.windrose(subset(edges, receptor.state == "CT"), regions = "Northeast", title = "Connecticut")
# grid.arrange(p1,p2,p3, ncol = 3)
# 
# p1 <- plot.windrose(subset(edges, receptor.state == "TN"), regions = "Southeast", title = "Tennessee")
# p2 <- plot.windrose(subset(edges, receptor.state == "AR"), regions = "Southeast", title = "Arkansas")
# p3 <- plot.windrose(subset(edges, receptor.state == "GA"), regions = "Southeast", title = "Georgia")
# grid.arrange(p1,p2,p3, ncol = 3)

source(file = "Other Code/emissions_analysis.R")

par(mfrow = c(1,2))


#1/distance*avgemissions vs PM plots
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "continuous",
                     exposure.var = "dist_emissions", plot.edges = c(0,0),
                     main = paste("Monitor exposure: \n sum of avgemissions*(1/log(distance)), ", season,"2005", sep = " "),
                     plot.legend = FALSE)
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "continuous",
                     exposure.var = "avgPM", plot.edges = c(0,0),
                     main = paste("Monitor exposure: \n avgPM, ",PM.type," ", season, " 2005", sep = ""),
                     plot.legend = FALSE)

p1 <- rankComparison(edges, var1 = "dist_emissions", var2 = "avgPM", regions = "IndustrialMidwest")
p2 <-rankComparison(edges, var1 = "dist_emissions", var2 = "avgPM", regions = "Northeast")
p3 <- rankComparison(edges, var1 = "dist_emissions", var2 = "avgPM", regions = "Southeast")
blank <- rectGrob(gp = gpar(col = "white"))
grid.arrange(blank, p1,p2,p3, ncol = 3, heights = c(0.05,0.90,0.05),
             layout_matrix = rbind(c(1,1,1),c(2,3,4),c(1,1,1)),
             top = textGrob("Comparison of coal emissions exposure \n (sum of (1/log(distance))*avgemissions vs. low freq PM)", 
                            gp = gpar(fontsize = 30)))




#num_edges vs PM plot
par(mfrow = c(1,2))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "continuous",
                     exposure.var = "num_edges", plot.edges = c(0,0),
                     main = paste("Monitor exposure: num_edges, ", season,"2005", sep = " "),
                     plot.legend = FALSE)
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "continuous",
                     exposure.var = "avgPM", plot.edges = c(0,0),
                     main = paste("Monitor exposure: avgPM, ",PM.type," ", season, " 2005", sep = ""),
                     plot.legend = FALSE)
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "binary",
                     exposure.var = "num_edges", plot.edges = c(0,0),
                     main = paste("Highest exposed: num_edges, ", season,"2005", sep = " "),
                     plot.legend = FALSE)
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "binary",
                     exposure.var = "avgPM", plot.edges = c(0,0),
                     main = paste("Highest exposed: avgPM, ",PM.type," ", season, " 2005", sep = ""),
                     plot.legend = FALSE)

p1 <- rankComparison(edges, var1 = "num_edges", var2 = "avgPM", regions = "IndustrialMidwest")
p2 <-rankComparison(edges, var1 = "num_edges", var2 = "avgPM", regions = "Northeast")
p3 <- rankComparison(edges, var1 = "num_edges", var2 = "avgPM", regions = "Southeast")
blank <- rectGrob(gp = gpar(col = "white"))
grid.arrange(blank, p1,p2,p3, ncol = 3, heights = c(0.05,0.90,0.05),
             layout_matrix = rbind(c(1,1,1),c(2,3,4),c(1,1,1)),
             top = textGrob("Comparison of coal emissions exposure (num_edges vs. low freq PM)", 
                            gp = gpar(fontsize = 30)))

p1 <- rankComparison(edges, var1 = "avgemissions", var2 = "avgPM", regions = "IndustrialMidwest")
p2 <-rankComparison(edges, var1 = "avgemissions", var2 = "avgPM", regions = "Northeast")
p3 <- rankComparison(edges, var1 = "avgemissions", var2 = "avgPM", regions = "Southeast")
blank <- rectGrob(gp = gpar(col = "white"))
grid.arrange(blank, p1,p2,p3, ncol = 3, heights = c(0.05,0.90,0.05),
             layout_matrix = rbind(c(1,1,1),c(2,3,4),c(1,1,1)),
             top = textGrob("Comparison of coal emissions exposure (avgemissions vs. low freq PM)", 
                            gp = gpar(fontsize = 30)))


par(mfrow = c(1,1))
dev.off()
sink.file = paste(unit.type,"_networks/plots/edge_analysis_",unit.type,"_",season,"_",PM.type,".txt", sep = "")
sink(file = sink.file)
print(edge_analysis(edges))
sink()



