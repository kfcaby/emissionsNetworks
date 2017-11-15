createAdjacencyMatrix <- function(edges){
  require(reshape2)
  network <- dcast(edges, PP ~ Monitor, value.var = "edge")
  PP <- network$PP
  network <- network[, -1]
  network <- as.matrix(network)
  network[is.na(network)] <- 0
  rownames(network) <- as.character(PP)
  return(network)
}

edge_analysis <- function(edges, regions = c("Northeast","IndustrialMidwest","Southeast")){
  
  if("Monitor.region" %in% colnames(edges)){
    edges <- subset(edges, Monitor.region %in% regions)
  }
  
  network <- createAdjacencyMatrix(edges)
  max.distance <- edges$max.distance[1]
  print(paste("Number of edges in the network:", sum(edges$edge, na.rm = TRUE) , sep = " "))
  print(paste("Number of possible edges in the network:", nrow(edges) , sep = " "))
  print(paste("The edge density is:", round(sum(edges$edge, na.rm = TRUE)/sum(!is.na(edges$edge)),3), sep = " "))
  print(paste("Percent of GAMS models failing:", 
              round(100*(length(is.na(edges$edge)) - length(edges$distance > max.distance))/length(edges$distance < max.distance),2),
              "%",sep = " "))
  print("")
  setkey(edges,PP)
  print("Power Plants")
  print(paste("Total:", length(unique(edges$PP)), sep = " "))
  print(paste("With at least some emissions:", sum(!is.na(edges[J(unique(PP)), "avgemissions", mult = "first"])), sep = " "))
  print(paste("With at least one linked monitor:", length(unique(subset(edges, edge == 1)$PP)), sep = " "))
  print(paste("Median number of linked monitors:", median(rowSums(network, na.rm = TRUE)) ,sep = " "))
  
  #by power plant size
  PP.cutoff.perc = 0.80
  emissions.cutoff <- quantile(edges[J(unique(PP)), "avgemissions", mult = "first"]$avgemissions,
                               PP.cutoff.perc)
  edges[ , PP.cat := ifelse(avgemissions <= emissions.cutoff, 0,1)]
  size.summary <- edges[ , round(sum(edge, na.rm = TRUE)/sum(!is.na(edge)),3), by = c("PP.cat","PP.region")]
  size.summary <- dcast(size.summary,  PP.region ~ PP.cat, value.var = "V1")
  print(paste("The probability of an edge occurring from a power plant"))
  print(paste("by size and region (1 = ",PP.cutoff.perc*100,"th percentile)", sep = ""))
  print(size.summary)
  print("The number of power plants by region")
  power.plants <- edges[J(unique(PP)), c("PP","PP.cat", "PP.region"), mult = "first"]
  power.plants <- power.plants[ , .N, by = c("PP.cat","PP.region")]
  print(dcast(power.plants, PP.region ~ PP.cat, value.var = "N"))
  print("")
  setkey(edges,Monitor)
  print("Monitors")
  print(paste("Total:", length(unique(edges$Monitor)), sep = " "))
  print(paste("With at least one PM measurement:", sum(!is.na(edges[J(unique(Monitor)), "avgPM", mult = "first"])), sep = " "))
  print(paste("With at least one linked powerplant:", length(unique(subset(edges, edge == 1)$Monitor)), sep = " "))
  print(paste("Median number of linked powerplants:", median(colSums(network, na.rm = TRUE)) ,sep = " "))
}
