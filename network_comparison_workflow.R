rm(list=ls())

library(data.table)
library(reshape2)
library(WGCNA)
load(file = "edges2005.RData")
source(file = "functions_emissions_networks.R")
colnames(edges_imputed)[5] <- "gams.coeff"

inmap <- fread(file = "inmapPM.csv")[ , V1 := NULL]
setkey(inmap, Monitor, PP)
edges_imputed[ , inmapPM := inmap[.(edges_imputed$Monitor,edges_imputed$PP),"inmapPM"]]


#Compare ranks for average total exposure by each metric
setkey(edges_imputed, Monitor)
gams.monitor <- edges_imputed[ , sum(gams.coeff, na.rm = TRUE), by = "Monitor"]$V1
inmap.monitor <- edges_imputed[ , sum(inmapPM, na.rm = TRUE), by = "Monitor"]$V1
verboseScatterplot(rank(gams.monitor),rank(inmap.monitor), xlab="Ranked Exposure GAMS ", 
                   ylab="Ranked Exposure InMAP")


gams_matrix <- acast(edges_imputed, Monitor ~ PP, value.var = "gams.coeff")
inmap_matrix <- acast(edges_imputed, Monitor ~ PP, value.var = "inmapPM")

softPower = 10
rankConn.gams = rank(softConnectivity(t(gams_matrix),type="signed",power=softPower))
rankConn.inmap = rank(softConnectivity(t(inmap_matrix),type="signed",power=softPower))

verboseScatterplot(rankConn.gams,rankConn.inmap, xlab="Ranked Connectivity GAMS", 
                   ylab="Ranked Connectivity InMAP")


adj_inmap <- softConnectivity(t(inmap_matrix),type="signed",power=softPower)
