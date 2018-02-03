library(xtable)
library(ggplot2)
library(directlabels)
library(viridis)
library(extrafont)

region_abbr <- Vectorize(function(region){
  if(region == "IndustrialMidwest") abbr <- "IMW"
  if(region == "Northeast") abbr <- "NE"
  if(region == "Southeast") abbr <- "SE"
  return(abbr)
}, vectorize.args = "region")

dist_labels <- Vectorize(function(distance_cat){
  if(distance_cat == 1) label <- "0-250km"
  if(distance_cat == 2) label <- "250-500km"
  if(distance_cat == 3) label <- "500-750km"
  if(distance_cat == 4) label <- "750-1000km"
  return(label)
}, vectorize.args = "distance_cat")

edges[ , distance_cat := ifelse(distance <= 250, 1, 
                                ifelse(distance <= 500, 2,
                                       ifelse(distance <= 750, 3,
                                              ifelse(distance <= 1000, 4, NA))))]

edges[ , powerplant_cat := ifelse(avgemissions >= quantile(avgemissions, 0.80),2,1 )]



#Monitors
monitors <- edges[ , list(Monitor = unique(Monitor), avgPM = unique(avgPM),
                          sdPM = unique(sdPM), linkedPP = sum(edge, na.rm = TRUE),
                          receptor.region = unique(receptor.region)),
                  by = "Monitor"]
monitor_summary_temp <- monitors[ , list(n = round(length(Monitor),0),
                                    avgPM = round(mean(avgPM, na.rm = TRUE),1),
                                    sdPM = round(mean(sdPM, na.rm = TRUE),1),
                                    linkedPP = median(linkedPP, na.rm = TRUE)),
                            by = "receptor.region"]
step1 <- melt(monitor_summary_temp, id.vars = "receptor.region")
monitor_summary <- dcast(step1, variable ~ receptor.region, value.var = "value")


monitors[ , list(hasPP_perc = sum(linkedPP > 0)/length(linkedPP),
                 linkedPP = median(linkedPP))
         , by = "receptor.region"]

#Power plants
powerplants <- edges[PP.region %in% c("IndustrialMidwest","Northeast","Southeast"),
                     list(PP = unique(PP), avgemissions = unique(avgemissions),
                          sdemissions = unique(sdemissions), emissions.NAdays = unique(emissions.NAdays),
                          PP.region = unique(PP.region),linkedMonitors = sum(edge,na.rm = TRUE),
                          powerplant_cat = unique(powerplant_cat)),
                     by = "PP"]
powerplant_summary_temp <- powerplants[ , list(m = round(length(PP), 0),
                                          avgemissions = round(mean(avgemissions, na.rm = TRUE), 1),
                                          sdemissions = round(mean(sdemissions, na.rm = TRUE),1),
                                          missing.days = round(median(emissions.NAdays, na.rm = TRUE),1),
                                          linkedMonitors = round(median(linkedMonitors, na.rm = TRUE),0)),
                                  by = "PP.region"]
step1 <- melt(powerplant_summary_temp, id.vars = "PP.region")
powerplant_summary <- dcast(step1, variable ~ PP.region, value.var = "value") 


summary <- rbind(monitor_summary, powerplant_summary)
summary$variable <- c("number of monitors", "average daily PM2.5 per monitor", "average stdev in daily PM2.5",
                      "median number of linked power plants", "number of power plants", "average daily SO2 emissions",
                      "average stdev in daily emissions", "median number of missing days", "median number of linked monitors")

summary <- xtable(summary, digits = 1, align = c("l","l","l","l", "l"))
sink(file = "results/tableone.tex")
print(summary, include.rownames = FALSE, hline.after = c(-1,0,4, nrow(summary)))
sink()


#edge analysis
dcast(edges[!is.na(edge) & PP.region %in% c("IndustrialMidwest","Northeast","Southeast") ,], 
      PP.region ~ receptor.region, 
      fun = function(x) round(sum(edge)/length(edge),2),
      value.var = "edge")

#edge percent by distance
edges[!is.na(edge), list(perc = sum(edge)/length(edge)), by = "distance_cat"]

edges[!is.na(edge), list(perc = sum(edge)/length(edge),
                         sdemissions = mean(avgemissions/sdemissions),
                         missing = median(emissions.NAdays)), by = "powerplant_cat"]

#edge percent by power plant size
PP_size_summary <-edges[!is.na(edge), list(perc = sum(edge)/length(edge),
                                           median_distance = median(distance)), by = c("powerplant_cat","distance_cat")]
setkey(PP_size_summary, distance_cat)
PP_size_summary

#median distance summary
median_dist <- edges[!is.na(edge), list(median_distance = median(distance)), by = c("powerplant_cat","distance_cat")]
setkey(median_dist)
median_dist


#edge summary by power plant and receptor region
edge_summary <- melt(dcast(edges[!is.na(edge) & PP.region %in% c("IndustrialMidwest","Northeast","Southeast") ,], 
      PP.region + distance_cat ~ receptor.region, 
      fun = function(x) round(sum(edge)/length(edge),2),
      value.var = "edge"),id.vars = c("PP.region","distance_cat"), 
     variable.name = "receptor.region", value.name = "edge.percent")


edge_summary$direction <- paste(region_abbr(edge_summary$PP.region),"-",
                                region_abbr(edge_summary$receptor.region),
                                sep = "")
edge_summary$distance_label <- dist_labels(edge_summary$distance_cat)

# Edge percents by orientation and distance plot for paper
#pdf(file = "Other Plots/edges_by_dist.pdf", width = 5.5, height = 4)
ggplot(edge_summary, aes(x = distance_label, y = edge.percent, color = PP.region)) + 
  geom_line(aes(group = direction)) +
  geom_dl(aes(label = direction),  method = list("last.points", cex = 0.5)) +
  scale_color_viridis(discrete = TRUE, name = "Power plant region") +
  theme_bw() +
  theme(
      text = element_text(size = 12),
      legend.position = "bottom"
    ) + ylim(0,0.35) +
  labs(x = "Distance from power plant to monitor", y = "Percent of power plant/monitor pairs")
#dev.off()

plotEmissionsNetwork(subset(edges, PP.region == "IndustrialMidwest" & receptor.region == "Northeast" & distance_cat == 4))

NA_summary <- powerplants[ , list(emissions.NAdays = median(emissions.NAdays)), by = c("PP.region","powerplant_cat")]
setkey(NA_summary, PP.region,powerplant_cat)
NA_summary

#Exposure map for paper
#pdf(file = "results/exposure_map.pdf", width = 22, height = 9)
plotEmissionsNetwork(edges, exposure.type = "continuous", exposure.var = "dist_emissions", plot.edges = c(0,0))
#dev.off()

#Inmap Comparison plot for paper
pdf(file = "results/inmap_comparison.pdf", width = 9, height = 22)
p1 <- rankComparison(edges, var1 = "dist_emissions", var2 = "inmapPM", regions = "IndustrialMidwest")
p2 <-rankComparison(edges, var1 = "dist_emissions", var2 = "inmapPM", regions = "Northeast")
p3 <- rankComparison(edges, var1 = "dist_emissions", var2 = "inmapPM", regions = "Southeast")
blank <- rectGrob(gp = gpar(col = "white"))
grid.arrange(p1,p2,p3, ncol = 1)
dev.off()
           
#avgPM Comparison plot for paper
pdf(file = "results/avgPM_comparison.pdf", width = 9, height = 22)
p1 <- rankComparison(edges, var1 = "dist_emissions", var2 = "avgPM", regions = "IndustrialMidwest")
p2 <-rankComparison(edges, var1 = "dist_emissions", var2 = "avgPM", regions = "Northeast")
p3 <- rankComparison(edges, var1 = "dist_emissions", var2 = "avgPM", regions = "Southeast")
blank <- rectGrob(gp = gpar(col = "white"))
grid.arrange(p1,p2,p3, ncol = 1)
dev.off()

# for(region in c("IndustrialMidwest","Northeast", "Southeast")){
#   pdf(file = paste("results/monitor_edges_",region,".pdf", sep = ""), width = 22, height = 9)
#   plotEmissionsNetwork(edges, plot.diagnostics = FALSE, main = "", receptor.regions = region, plot.legend = FALSE)
#   dev.off()
# }

# for(region in c("IndustrialMidwest","Northeast", "Southeast")){
#   pdf(file = paste("results/powerplant_edges_",region,".pdf", sep = ""), width = 22, height = 9)
#   plotEmissionsNetwork(subset(edges, PP.region == region), plot.diagnostics = FALSE, main = "", plot.legend = FALSE)
#   dev.off()
# }
