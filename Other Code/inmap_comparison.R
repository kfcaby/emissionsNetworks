edges_possible <- subset(edges, !is.na(edge))

inmapPM <- dcast(edges_possible, Monitor ~ PP, value.var = "inmapPM")
setkey(inmapPM, Monitor)

#median inmap percentiles of the edges by monitor
edge_inmap_perc <- lapply(inmapPM$Monitor, function(x){
  test <- subset(edges_possible, edge == 1 & Monitor == x)
  edge_inmap <- test$inmapPM[!is.na(test$inmapPM)]
  monitor_inmap <-as.numeric(inmapPM[x])
  monitor_inmap <- monitor_inmap[!is.na(monitor_inmap)]
  median_perc <- ifelse(sum(!is.na(monitor_inmap)) > 0 ,median(ecdf(monitor_inmap)(edge_inmap)) , NA)
  return(median_perc)
})
edge_inmap_perc <- unlist(edge_inmap_perc)

#number of edges per monitor
num_edges <- edges_possible[ , list(sum(edge, na.rm = TRUE)), by = "Monitor"]$V1

p1 <- qplot(edge_inmap_perc, geom = "histogram", xlim = c(0,1), main = "Median InMAP percentile of edges - by Monitor") +
  geom_vline(xintercept = 0.5)
p1
median(edge_inmap_perc, na.rm = TRUE)
#by power plant

inmapPM <- dcast(edges_possible, PP ~ Monitor, value.var = "inmapPM")
setkey(inmapPM, PP)

#median inmap percentiles of the edges by powerplant
edge_inmap_perc_byPP <- lapply(inmapPM$PP, function(x){
  test <- subset(edges_possible, edge == 1 & PP == x)
  edge_inmap <- test$inmapPM[!is.na(test$inmapPM)]
  PP_inmap <-as.numeric(inmapPM[x])
  PP_inmap <- PP_inmap[!is.na(PP_inmap)]
  median_perc <- ifelse(sum(!is.na(PP_inmap)) > 0 ,median(ecdf(PP_inmap)(edge_inmap)) , NA)
  return(median_perc)
})
edge_inmap_perc_byPP <- unlist(edge_inmap_perc)


p2 <- qplot(edge_inmap_perc_byPP, geom = "histogram", xlim = c(0,1), main = "Median InMAP percentile of edges - by Powerplant") +
  geom_vline(xintercept = 0.5)
p2
median(edge_inmap_perc_byPP, na.rm = TRUE)

# inmapPM_strong <- subset(edges_possible, inmapPM > 0.05 & !is.na(edge))
# sum(inmapPM_strong$edge)/nrow(inmapPM_strong)
# 
# inmapPM_weak <- subset(edges_possible, inmapPM < 0.000001 & !is.na(edge))
# sum(inmapPM_weak$edge)/nrow(inmapPM_weak)
