rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks")

library(data.table)
library(geosphere)

#source(file = "../functions_emissions_networks.R")
source(file = "R/plotEmissionsNetwork.R")
source(file = "R/import_edges.R")
source(file = "R/edge_analysis.R")

#Parameters
unit.type <- "monitor" # either "zipcode" or "monitor"
season <- "summer"
PM.type <- "decomposed74" #c("raw", "decomposedXX")

#imports output from fitDailyPMmodels
edges <- import_edges(unit.type,season, PM.type)

#TO DO
#1 - make monitor markers bigger
#2 - try on 73 decomposed PM


#START HERE
plot.file = paste(unit.type,"_networks/plots/",unit.type,"_",season,"_",PM.type,".pdf", sep = "")
pdf(plot.file, height = 9, width = 22)

#blank map
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, main = "US map of Power Plants and AQS monitors",
                     plot.edges = c(0,0), plot.close.powerplants = FALSE)

#average PM exposure
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "continuous", 
                     exposure.var = "avgPM",main = paste("avgPM_",season,"_",PM.type, sep ="")
                     , plot.edges = c(0,0))


#edges by region
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste("Northeast - ",season," 2005", sep = ""), receptor.regions = "Northeast")
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste("IndustrialMidwest - ",season," 2005", sep = ""), receptor.regions = "IndustrialMidwest")
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = paste("Southeast - ",season," 2005", sep = ""), receptor.regions = "Southeast")


#diagnostics
plotEmissionsNetwork(edges, plot.diagnostics = TRUE)

#edge plots by lag 
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

#continuous gams vs PM plots
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "continuous",
                     exposure.var = "gams.coeff", plot.edges = c(0,0),
                     main = paste("sum of gams.coeff", season,"2005", sep = " "))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "continuous",
                     exposure.var = "avgPM", plot.edges = c(0,0),
                     main = paste("avgPM", season, "2005", sep = " "))

#binary gams vs PM plot
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "binary",
                      exposure.var = "gams.coeff", plot.edges = c(0,0),
                     main = paste("sum of gams.coeff", season,"2005", sep = " "))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "binary",
                     exposure.var = "avgPM", plot.edges = c(0,0),
                     main = paste("avgPM", season,"2005", sep = " "))
#numedges plots
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "continuous",
                     exposure.var = "num_edges", plot.edges = c(0,0),
                     main = paste("num_edges", season,"2005", sep = " "))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, exposure.type = "binary",
                     exposure.var = "num_edges", plot.edges = c(0,0),
                     main = paste("num_edges", season,"2005", sep = " "))
par(mfrow = c(1,1))
dev.off()
sink.file = paste(unit.type,"_networks/plots/edge_analysis_",unit.type,"_",season,"_",PM.type,".txt", sep = "")
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
