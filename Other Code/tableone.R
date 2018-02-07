#tables and figures for paper
#Note: need to import edges using edge_analysis_workflow before running this code.

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

## ------------------------------------------------------------------------------------ ##
##   Table for Monitor/Power Plant Summary                                              ##
## ------------------------------------------------------------------------------------ ##

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

## ------------------------------------------------------------------------------------ ##
##   Old plot Cory doesn't like                                                         ##
## ------------------------------------------------------------------------------------ ##

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
#Plot Cory doesn't like
#pdf(file = "Other Plots/edges_by_dist.pdf", width = 5.5, height = 4)
ggplot(edge_summary, aes(x = distance_label, y = edge.percent, color = PP.region)) + 
  geom_line(aes(group = direction)) +
  geom_dl(aes(label = direction),  method = list("last.points", cex = 0.5)) +
  scale_color_viridis(discrete = TRUE, name = "Power plant region") +
  theme_bw() +
  theme(
      text = element_text(size = 12),
      legend.position = "bottom"
    ) + ylim(0,0.5) +
  labs(x = "Distance from power plant to monitor", y = "Percent of power plant/monitor pairs")
#dev.off()

## ------------------------------------------------------------------------------------ ##
##   Blank Map                                                                          ##
## ------------------------------------------------------------------------------------ ##

#blank map for paper
pdf(file = "results/blankmap.pdf", height = 4)
par(mar = c(0,0,0,0))
US <- map("state",fill=FALSE, plot=FALSE)
US.names <- US$names
US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
IndustrialMidwest <- c("west virginia","ohio", "indiana", "illinois", "michigan",
                       "wisconsin", "kentucky")
Northeast <- c("rhode island", "massachusetts", "connecticut", "maine",
               "new hampshire", "vermont", "new york","pennsylvania",
               "new jersey", "delaware", "maryland", "virginia")
Southeast <- c("north carolina", "south carolina", "tennessee", "georgia",
               "alabama", "mississippi", "louisiana", "florida", "arkansas")
col.region <- rep("white",length(US.IDs))
col.region[US.IDs %in% IndustrialMidwest] <- viridis(4, alpha = 0.5)[1]
col.region[US.IDs %in% Northeast] <- viridis(4, alpha = 0.5)[2]
col.region[US.IDs %in% Southeast] <- viridis(4, alpha = 0.5)[3]
map("state", fill = TRUE, col = col.region, plot = TRUE)
#US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
#plot(US_poly_sp)
setkey(edges, Monitor)
points(edges[J(unique(Monitor)), c("receptor.longitude","receptor.latitude"), mult = "first"],
       pch = 21, bg = "black")
setkey(edges,PP)
emissions <- edges[J(unique(PP)), "avgemissions", mult = "first"]$avgemissions
pp.cex <- ifelse(emissions < quantile(emissions, 0.8), 0.6, 1.25)
points(edges[J(unique(PP)), c("PP.longitude","PP.latitude"), mult = "first"],
       pch = 24, bg = viridis(2)[2], col = "black", lwd = 1, cex = pp.cex)
legend(x = -79, y = 33.5, 
       legend = c("coal power\nplant","AQS monitor"),
       pch = c(24,21),
       pt.cex = c(1.25,1),
       pt.bg = c(viridis(2)[2],"black"),
       cex = 1, bty = "n")
dev.off()

## ------------------------------------------------------------------------------------ ##
##   side-by-side maps for monitor exposure                                             ##
## ------------------------------------------------------------------------------------ ##
pdf(file = "results/exposure_map.pdf", height = 4)
map("state", fill = FALSE, plot = TRUE)
setkey(edges, Monitor)
monitors <- edges[ , list(longitude = unique(receptor.longitude),
                          latitude = unique(receptor.latitude),
                          receptor.state = unique(receptor.state),
                          exposure = sum(avgemissions*(1/distance)*edge, na.rm = TRUE),
                          exposure_inmap = sum(inmapPM, na.rm = TRUE)),
                   by = "Monitor"]
monitors[ , exposure_std := (exposure - mean(exposure))/sd(exposure)]

breaks = c(seq(min(monitors$exposure_std),3, by = 0.1),10000)
monitors[ , exposure_interval :=  as.numeric(cut(exposure_std, breaks = breaks))]
#monitors[is.na(exposure_interval), ]$exposure_interval <- 1
num.colors = length(unique(monitors$exposure_interval))
monitors[ , bg.monitor := rev(viridis(num.colors))[exposure_interval]]
points(monitors[ , .(longitude,latitude)] ,pch = 21, bg = monitors$bg.monitor)
dev.off()

pdf(file = "results/inmap_map.pdf", height = 4)
map("state", fill = FALSE, plot = TRUE)
monitors[ , inmap_std := (exposure_inmap - mean(exposure_inmap))/sd(exposure_inmap)]
breaks = seq(-1.5,3.1, by = 0.1)
monitors[ , inmap_interval :=  as.numeric(cut(inmap_std, breaks = breaks))]
monitors[ , bg.monitor.inmap := rev(viridis(length(breaks)-1))[inmap_interval]]
points(monitors[ , .(longitude,latitude)] ,pch = 21, bg = monitors$bg.monitor.inmap, cex = 1.5)
dev.off()

hist(monitors$inmap_std, xlim = c(-3,3))
hist(monitors$exposure_std, xlim = c(-3,3))

#Exposure map for paper
#pdf(file = "results/exposure_map.pdf", width = 22, height = 9)
plotEmissionsNetwork(edges, exposure.type = "continuous", exposure.var = "dist_emissions", plot.edges = c(0,0))
plotEmissionsNetwork(edges, exposure.type = "continuous", exposure.var = "inmapPM", plot.edges = c(0,0))
#dev.off()

plotEmissionsNetwork(edges[receptor.state == "NH",])

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
