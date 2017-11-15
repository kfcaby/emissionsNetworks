rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks/zipcode_networks")

library(data.table)
library(geosphere)

source(file = "../functions_emissions_networks.R")
source(file = "../plotEmissionsNetwork.R")
source(file = "../import_edges.R")

#Parameters
season <- "fall"
PM.type <- "decomposed" #c("raw", "decomposed")

#imports output from fitDailyPMmodels
edges <- import_edges(season, PM.type)



#START HERE
plot.file = paste("plots/zipcode_",season,"_",PM.type,".pdf", sep = "")
pdf(plot.file, height = 9, width = 22)
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = "", plot.edges = c(0,0))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste("Northeast - ",season," 2005", sep = ""), regions = "Northeast")
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste("IndustrialMidwest - ",season," 2005", sep = ""), regions = "IndustrialMidwest")
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste("Southeast - ",season," 2005", sep = ""), regions = "Southeast")
plotEmissionsNetwork(edges, plot.diagnostics = TRUE)
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste(season, " 2005 Lag 0 edges", sep = ""), 
                     plot.edges = c(0, 13*24))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste(season, " 2005 Lag 1 edges", sep = ""), 
                     plot.edges = c(13*24, 2*13*24))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste(season, " 2005 Lag 2 edges", sep = ""), 
                     plot.edges = c(2*13*24, 3*13*24))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste(season, " 2005 Lag 3 edges", sep = ""), 
                     plot.edges = c(3*13*24, 4*13*24))
par(mfrow = c(1,2))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "continuous",
                     exposure.var = "gams.coeff", plot.edges = c(0,0),
                     main = paste("sum of gams.coeff", season,"2005", sep = " "))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "continuous",
                     exposure.var = "avgPM", plot.edges = c(0,0),
                     main = paste("avgPM", season, "2005", sep = " "))

plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "binary",
                      exposure.var = "gams.coeff", plot.edges = c(0,0),
                     main = paste("sum of gams.coeff", season,"2005", sep = " "))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "binary",
                     exposure.var = "avgPM", plot.edges = c(0,0),
                     main = paste("avgPM", season,"2005", sep = " "))
par(mfrow = c(1,1))
dev.off()
sink.file = paste("plots/edge_analysis_zipcode_",season,"_",PM.type,".txt", sep = "")
sink(file = sink.file)
print(edge_analysis(edges))
sink()

# 
# plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "binary",
#                      exposure.var = "avgPM", plot.edges = c(0,0))
# 
# #Why are there so few connections in the Southeast?
# edges.subset <- edges[ distance <= 1000 & PP.region %in% c("IndustrialMidwest", "Southeast", "Northeast"), ]
# setkey(edges.subset, PP.region)
# region.analysis <- edges.subset[ , list(
#               num.possible = sum(!is.na(edge)),
#               edges = sum(edge, na.rm = TRUE),
#               edge.prob = round(sum(edge, na.rm = TRUE)/sum(!is.na(edge)),3),
#               avg.monitor.distance = round(mean(distance, na.rm = TRUE),1),
#               lag0.perc = round(sum(lag == 0, na.rm = TRUE)/sum(!is.na(lag)),2),
#               lag1.perc = round(sum(lag == 1, na.rm = TRUE)/sum(!is.na(lag)),2),
#               lag2.perc = round(sum(lag == 2, na.rm = TRUE)/sum(!is.na(lag)),2),
#               lag3.perc = round(sum(lag == 3, na.rm = TRUE)/sum(!is.na(lag)),2),
#               median.PM.NAdays = as.double(median(PM.NAdays, na.rm = TRUE)),
#               median.emissions.NAdays = as.double(median(emissions.NAdays, na.rm = TRUE))),
#        by = PP.region]
# region.analysis
# 
# # Southeast monitors are much farther away from power plants.
# par(mfrow = c(1,2))
# hist(subset(edges,distance < 1000 & PP.region == "Northeast")$distance, 
#      xlab = "distance",main = "Northeast - Power Plant to Monitor distance")
# hist(subset(edges,distance < 1000 & PP.region == "Southeast")$distance, 
#      xlab = "distance",main = "Southeast - Power Plant to Monitor distance")
# par(mfrow = c(1,1))
# 
# plotEmissionsNetwork(edges, plot.edge = c(0,13*24), plot.diagnostics = FALSE)
# 
# 
# plotEmissionsNetwork(edges, exposure.type = "binary", exposure.var = "num_edges",
#                      plot.edges = FALSE, plot.diagnostics = FALSE, main = "Exposure: Number of Edges")
# 
# plotEmissionsNetwork(edges, exposure.type = "binary", exposure.var = "gams.coeff",
#                      plot.edges = FALSE, plot.diagnostics = FALSE, main = "Exposure: GAMS Coeff")
# 
# 
# setkey(edges, Monitor)
# gams.monitor <- edges[ , sum(gams.coeff, na.rm = TRUE), by = "Monitor"]$V1
# inmap.monitor <- edges[ , sum(inmapPM, na.rm = TRUE), by = "Monitor"]$V1
# verboseScatterplot(rank(gams.monitor),rank(inmap.monitor), xlab="Ranked Exposure GAMS Coeff", 
#                    ylab="Ranked Exposure InMAP")
