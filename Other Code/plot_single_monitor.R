rm(list=ls())
setwd("/nfs/nsaph_ci3/users/ci3_kcummiskey/emissionsNetworks")

library(data.table)
library(geosphere)


#source(file = "../functions_emissions_networks.R")
source(file = "R/plotEmissionsNetwork.R")
source(file = "R/import_edges.R")
source(file = "R/edge_analysis.R")
source(file = "R/rankComparison.R")
source(file = "R/connectivityComparison.R")

#Parameters
unit.type <- "monitor" # either "zipcode" or "monitor"
season <- "summer"
PM.type <- "decomposed75" #c("raw", "decomposedXX")

#imports output from fitDailyPMmodels
edges <- import_edges(unit.type,season, PM.type)


monitors <- edges[ , list(edges = sum(edge, na.rm = TRUE),
                          receptor.state = unique(receptor.state)),
                  by = "Monitor"]
monitors.Ohio <- subset(monitors, receptor.state == "OH")
par(mfrow = c(2,3),
    mar = c(1,1,1,1))
lapply(monitors.Ohio$Monitor,
    function(x){
      edges_monitor <- subset(edges, Monitor == x)
      plotEmissionsNetwork(edges_monitor, plot.diagnostics = FALSE, main = "", 
                           xlim = c(-90.7,-78.3), ylim = c(36.2,43.8),
                           plot.legend = FALSE)
      box(which = "plot", lwd = 3)
    })


plotEmissionsNetwork(edges_one_monitor, plot.diagnostics = FALSE, main = "", 
                     xlim = c(-90.7,-78.3), ylim = c(36.2,43.8))





