rm(list=ls())

library(data.table)
library(imputeTS)
library(WGCNA)
library(geosphere)

source(file = "functions_emissions_networks.R")

getRegion <- function(states){
  Northeast = c("ME", "NH", "VT", "NY", "PA", "DE", "NJ", "MD", "DC", "VA", "MA", "CT", "RI")
  IndustrialMidwest = c("WV", "OH", "KY", "IN", "IL", "WI", "MI")
  Southeast = c("FL", "GA", "SC", "NC", "TN", "AL", "MS", "AR","LA")
  UpperMidwest = c("MN", "IA", "MO", "KS", "NE", "SD", "ND")
  Southwest = c("TX", "OK", "NM", "AZ")
  SouthernCalifornia = c("CA")
  Northwest = c("NV", "UT", "CO", "WY", "MT", "ID", "OR", "WA")
  regions <- ifelse(states %in% Northeast,"Northeast",
                    ifelse(states %in% IndustrialMidwest, "IndustrialMidwest",
                           ifelse(states %in% Southeast, "Southeast",
                                  ifelse(states %in% UpperMidwest, "UpperMidwest",
                                         ifelse(states %in% Southwest, "Southwest",
                                                ifelse(states %in% SouthernCalifornia, "SouthernCalifornia",
                                                       ifelse(states %in% Northwest,"Northwest", NA)))))))
  return(regions)
}


## list all output files in the output directory
output_files <- list.files("output",
                           pattern = "^edges2005summer_PMdecomposed[0-9]+\\.csv$",
                           full.names=TRUE)

## read each file and append them
edges <- do.call(rbind, lapply(output_files, fread))[ ,V1 := NULL]
edges$p.value_adj <- p.adjust(edges$p.value, method = "BH")
edges$edge <- ifelse(edges$p.value_adj <= 0.05 & edges$gams.coeff > 0, 1, 0)


facility.state <- unique(fread("/Users/kfcummiskey/Dropbox/ARP/Projects/Source Receptor Modeling/Cummiskey/Data/unitlevel.csv",
               select = c("Facility.ID..ORISPL.","State.x")))
colnames(facility.state) <- c("PP","State")
facility.state$PP <- paste("PP",facility.state$PP,sep = "")
setkey(facility.state, PP)
setkey(edges,PP,Monitor)
edges$PP.state <- facility.state[edges$PP,]$State
edges$PP.region <- getRegion(edges$PP.state)

#add bearing to edge data
edges$bearing <- bearing(cbind(edges$PP.longitude,edges$PP.latitude), cbind(edges$M.longitude,edges$M.latitude))
edges$bearing <- ifelse(edges$bearing < 0, edges$bearing + 360, edges$bearing)

#add direction to edge data
angle_breaks <- seq(22.5,337.5, by = 45)
direction_ints <- data.table(PP = edges$PP, Monitor = edges$Monitor, interval = findInterval(edges$bearing, angle_breaks), key = "interval")
direction.table <- data.table(interval = 0:8, direction = c("N","NE","E","SE", "S","SW","W","NW","N"), key = "interval")
directions <- direction_ints[direction.table]
setkey(directions, PP, Monitor)
setkey(edges, PP, Monitor)
edges <- merge(edges,directions[ ,.(PP,Monitor,direction)])

#START HERE
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = "Network Plot (Summer 2005) - PM decomposed")
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = "Network Plot (Summer 2005) - PM decomposed \n Lag 0 edges", 
                     plot.edges = c(0, 13*24))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = "Network Plot (Summer 2005) - PM decomposed \n Lag 1 edges", 
                     plot.edges = c(13*24, 2*13*24))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = "Network Plot (Summer 2005) - PM decomposed \n Lag 2 edges", 
                     plot.edges = c(2*13*24, 3*13*24))
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, 
                     main = "Network Plot (Summer 2005) - PM decomposed \n Lag 3 edges", 
                     plot.edges = c(3*13*24, 4*13*24))
edge_analysis(edges)

set.seed(1000)
plotEmissionsNetwork(edges, plot.diagnostics = TRUE, main = "Network Plot (Summer 2005) - PM decomposed")

#Why are there so few connections in the Southeast?
edges.subset <- edges[ distance <= 1000 & PP.region %in% c("IndustrialMidwest", "Southeast", "Northeast"), ]
setkey(edges.subset, PP.region)
region.analysis <- edges.subset[ , list(
              num.possible = sum(!is.na(edge)),
              edges = sum(edge, na.rm = TRUE),
              edge.prob = round(sum(edge, na.rm = TRUE)/sum(!is.na(edge)),3),
              avg.monitor.distance = round(mean(distance, na.rm = TRUE),1),
              lag0.perc = round(sum(lag == 0, na.rm = TRUE)/sum(!is.na(lag)),2),
              lag1.perc = round(sum(lag == 1, na.rm = TRUE)/sum(!is.na(lag)),2),
              lag2.perc = round(sum(lag == 2, na.rm = TRUE)/sum(!is.na(lag)),2),
              lag3.perc = round(sum(lag == 3, na.rm = TRUE)/sum(!is.na(lag)),2),
              median.PM.NAdays = as.double(median(PM.NAdays, na.rm = TRUE)),
              median.emissions.NAdays = as.double(median(emissions.NAdays, na.rm = TRUE))),
       by = PP.region]
region.analysis

# Southeast monitors are much farther away from power plants.
par(mfrow = c(1,2))
hist(subset(edges,distance < 1000 & PP.region == "Northeast")$distance, 
     xlab = "distance",main = "Northeast - Power Plant to Monitor distance")
hist(subset(edges,distance < 1000 & PP.region == "Southeast")$distance, 
     xlab = "distance",main = "Southeast - Power Plant to Monitor distance")
par(mfrow = c(1,1))

plotEmissionsNetwork(edges, plot.edge = c(0,13*24), plot.diagnostics = FALSE)


plotEmissionsNetwork(edges, exposure.type = "binary", exposure.var = "num_edges",
                     plot.edges = FALSE, plot.diagnostics = FALSE, main = "Exposure: Number of Edges")

plotEmissionsNetwork(edges, exposure.type = "binary", exposure.var = "gams.coeff",
                     plot.edges = FALSE, plot.diagnostics = FALSE, main = "Exposure: GAMS Coeff")


setkey(edges, Monitor)
gams.monitor <- edges[ , sum(gams.coeff, na.rm = TRUE), by = "Monitor"]$V1
inmap.monitor <- edges[ , sum(inmapPM, na.rm = TRUE), by = "Monitor"]$V1
verboseScatterplot(rank(gams.monitor),rank(inmap.monitor), xlab="Ranked Exposure GAMS Coeff", 
                   ylab="Ranked Exposure InMAP")
