#tables and figures for paper
#Note: need to import edges using edge_analysis_workflow before running this code.

library(xtable)
library(ggplot2)
library(directlabels)
library(viridis)
library(extrafont)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
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
edges[ , PP.type := ifelse(emissions.ZeroDays > 90, "seasonal", "all.year")]
## ------------------------------------------------------------------------------------ ##
##   Table for Monitor/Power Plant Summary                                              ##
## ------------------------------------------------------------------------------------ ##

#Monitors
monitors <- edges[ , list(Monitor = unique(Monitor), avgPM = unique(avgPM), 
                          sdPM = unique(sdPM), linkedPP = sum(edge, na.rm = TRUE),
                          receptor.region = unique(receptor.region),
                          monitor.freq = unique(monitor.freq)),
                  by = "Monitor"]
monitor_summary_temp <- monitors[ , list(n = round(length(Monitor),0),
                                    avgPM = round(mean(avgPM, na.rm = TRUE),1),
                                    sdPM = round(mean(sdPM, na.rm = TRUE),1),
                                    linkedPP = median(linkedPP, na.rm = TRUE)),
                            by = "receptor.region"]
step1 <- melt(monitor_summary_temp, id.vars = "receptor.region")
monitor_summary <- dcast(step1, variable ~ receptor.region, value.var = "value")


monitor_freq_summary <- monitors[ , list(hasPP_perc = sum(linkedPP > 0)/length(linkedPP),
                 median_linkedPP = median(linkedPP), n = length(Monitor))
         , by = c( "monitor.freq")]
setkey(monitor_freq_summary,  monitor.freq)
monitor_freq_summary

par(mfrow = c(1,2))
hist(monitors[monitor.freq == "daily",]$linkedPP, xlim = c(0,100))
hist(monitors[monitor.freq != "daily",]$linkedPP, xlim = c(0,100))
par(mfrow = c(1,1))
#Power plants
powerplants <- edges[PP.region %in% c("IndustrialMidwest","Northeast","Southeast"),
                     list(avgemissions = unique(avgemissions),
                          sdemissions = unique(sdemissions), emissions.NAdays = unique(emissions.NAdays),
                          PP.region = unique(PP.region), PP.state = unique(PP.state),
                          PP.latitude = unique(PP.latitude), PP.longitude = unique(PP.longitude),
                          linkedMonitors = sum(edge,na.rm = TRUE),
                          linkedMonitorsPerc = sum(edge,na.rm = TRUE)/sum(edge ==1 | edge == 0, na.rm = TRUE),
                          powerplant_cat = unique(powerplant_cat),
                          emissions.ZeroDays = unique(emissions.ZeroDays)),
                     by = "PP"]
powerplants[ , type := ifelse(emissions.ZeroDays > 90, "seasonal", "all.year")]
powerplants[ , link.cat := ifelse(linkedMonitorsPerc == 0, "none", ifelse(linkedMonitorsPerc > 0.3, "high", "normal"))]
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
#sink(file = "results/tableone.tex")
#print(summary, include.rownames = FALSE, hline.after = c(-1,0,4, nrow(summary)))
#sink()

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
#pdf(file = "results/exposure_map.pdf", height = 4)
map("state", fill = FALSE, plot = TRUE)
setkey(edges, Monitor)
monitors <- edges[ , list(longitude = unique(receptor.longitude),
                          latitude = unique(receptor.latitude),
                          receptor.state = unique(receptor.state),
                          receptor.region = unique(receptor.region),
                          exposure = sum(avgemissions*(1/distance)*edge, na.rm = TRUE),
                          exposure_inmap = sum(inmapPM, na.rm = TRUE)),
                   by = "Monitor"]
#partition into 4 colors
bg.monitor <- rep(NA, nrow(monitors))
#bg.monitor[monitors$exposure > 0] <- as.numeric(cut(ecdf(monitors[exposure > 0,]$exposure)(monitors[exposure > 0,]$exposure), 
#                  breaks = c(0 ,0.25, 0.5, 0.75, 1)))
bg.monitor[monitors$exposure > 0] <- rank(monitors[exposure > 0,]$exposure)
bg.monitor[monitors$exposure > 0] <- rev(plasma(nrow(monitors[exposure > 0,])))[bg.monitor[monitors$exposure > 0]]
points(monitors[ , .(longitude,latitude)] ,pch = 21, bg = bg.monitor, cex = 1.25)
#dev.off()

#legend
#pdf(file = "results/exposure_legend.pdf", height = 4)
plot(0,0)
legend(x = 0.5, y = 0.5, title = "Exposure\nQuartile", title.adj = 0,
       legend = c("1st", "2nd", "3rd", "4th"),
       pch = 21,
       pt.cex = 2,
       pt.bg = rev(plasma(4)),
       cex = 1, bty = "n")
#dev.off()


#pdf(file = "results/inmap_map.pdf", height = 4)
map("state", fill = FALSE, plot = TRUE)
bg.monitor.inmap <- rep(NA, nrow(monitors))
# bg.monitor[monitors$exposure_inmap > 0] <- as.numeric(cut(ecdf(monitors[exposure_inmap > 0,]$exposure_inmap)(monitors[exposure_inmap > 0,]$exposure_inmap), 
#                                                     breaks = c(0 ,0.25, 0.5, 0.75, 1)))
# bg.monitor[monitors$exposure_inmap > 0] <- rev(plasma(4))[bg.monitor[monitors$exposure_inmap > 0]]

bg.monitor.inmap[monitors$exposure_inmap > 0] <- rank(monitors[exposure_inmap > 0,]$exposure_inmap)
bg.monitor.inmap[monitors$exposure_inmap > 0] <- rev(plasma(nrow(monitors[exposure_inmap > 0,])))[bg.monitor.inmap[monitors$exposure_inmap > 0]]
points(monitors[ , .(longitude,latitude)] ,pch = 21, bg = bg.monitor.inmap, cex = 1.25)
#dev.off()

hist(monitors$exposure, breaks = 50)
hist(monitors$exposure_inmap, breaks = 50)


## ------------------------------------------------------------------------------------ ##
##   rank comparisons for monitor exposure                                             ##
## ------------------------------------------------------------------------------------ ##
rankComp <- function(monitors, regions){
  data <- monitors[(receptor.region %in% regions) & (exposure > 0), ]
  
  data[ , receptor.subregion := receptor.state]
  data[ , receptor.subregion := ifelse(receptor.state %in% c("RI","CT","MA","ME","VT","NH"),
                                           "N.Eng", receptor.subregion)]
  data[ , receptor.subregion := ifelse(receptor.state %in% c("NJ","MD","DE","DC"),
                                           "MidAtl", receptor.subregion)]
  data[ , receptor.subregion := ifelse(receptor.state %in% c("GA","SC"),
                                           "SC/GA", receptor.subregion)]
  data[ , receptor.subregion := ifelse(receptor.state %in% c("MS","AL","LA"),
                                           "AL/MS/LA", receptor.subregion)]
  data[ , exposure_rank := rank(exposure)]
  data[ , inmap_rank := rank(exposure_inmap)]
  data$receptor.subregion = factor(data$receptor.subregion,
                                       levels = rev(c("N.Eng","NY",
                                                      "MidAtl","VA", "PA",
                                                      "WI","IL","MI","IN","KY","OH","WV",
                                                      "FL","LA","SC/GA","AL/MS/LA",
                                                      "AR","NC","TN")))
  print(data$receptor.subregion)
  txt.size = 10
  plot <- ggplot(data, aes(x = exposure_rank, y = inmap_rank, color = receptor.subregion)) + geom_point() + 
    scale_color_viridis(discrete = TRUE, begin = 0, end = 1, name = "state") +
    theme(legend.position = "right",
          axis.title = element_text(size = txt.size),
          axis.text = element_text(size = txt.size),
          legend.text = element_text(size = txt.size),
          legend.title = element_text(size = txt.size),
          legend.justification = "top",
          #  legend.key.size = unit(2, "line" ),
          plot.title = element_text(size = txt.size + 2, hjust = 0.5)) +
    labs(x = "emissions network", y = "InMAP", title = "")
  
  print(cor(data$exposure_rank, data$inmap_rank))
  print(cor(data$exposure, data$exposure_inmap, method = "spearman"))
  
  return(plot)
}

p1 <- rankComp(monitors, regions = "IndustrialMidwest")
pdf(file = "results/comparisonIMW.pdf", height = 2.5, width = 3.1)
p1
dev.off()

#without Michigan and Ohio
rankComp(monitors[!(receptor.state %in% c("MI","OH"))  ,], regions = "IndustrialMidwest")

p2 <- rankComp(monitors, regions = "Northeast")
pdf(file = "results/comparisonNE.pdf", height = 2.5, width = 3.35)
p2
dev.off()

#excluding western Penn.
rankComp(monitors[!(receptor.state %in% c("PA") & longitude < -77.7),], regions = "Northeast")

p3 <- rankComp(monitors, regions = "Southeast")
pdf(file = "results/comparisonSE.pdf", height = 2.5, width = 3.65)
p3
dev.off()

## ------------------------------------------------------------------------------------ ##
##   ROC curve for edges and InMAP PM                                                   ##
## ------------------------------------------------------------------------------------ ##
setkey(edges, PP)
getROC <- function(edges, p){
  #function return false positive rate and true positive rate for different cutoffs
  roc <- Vectorize(function(edges, p){
    complete_edges <- edges[!is.na(edge) & !is.na(inmapPM),]
    
    #TRUE
    network_edges <- complete_edges$edge
    #PREDICTED
    inmap_edges <- ifelse(complete_edges$inmapPM >= quantile(complete_edges$inmapPM,p),1,0)
    
    #false positives
    false_pos <- sum(ifelse(inmap_edges == 1 & network_edges == 0, 1, 0))
    # true negatives
    true_neg <- sum(ifelse(inmap_edges == 0 & network_edges == 0, 1, 0))
    
    #false positive rate
    FPR <- false_pos/(false_pos + true_neg)
    
    
    #true positives
    true_pos <- sum(ifelse(inmap_edges == 1 & network_edges == 1, 1, 0))
    #false negatives
    false_neg <- sum(ifelse(inmap_edges == 0 & network_edges == 1, 1, 0))
    
    #true positive rate
    TPR <- true_pos/(true_pos + false_neg)
    
    return(c(FPR,TPR))
  }, vectorize.args = "p")
  
  rates <- roc(edges,p)
  #format output
  rates <- data.frame(`InMAP cutoff` = p,FPR = rates[1 ,], TPR = rates[2, ])
  return(rates)
}
plotROC <- function(edges, p = seq(0.02,0.98,0.02), title = ""){
  require(viridis)
  rates <- getROC(edges, p)
  p1 <- ggplot(rates, aes(x = FPR, y = TPR, color = InMAP.cutoff)) + geom_point() +
    theme_bw() + geom_abline(slope = 1, intercept = 0) + scale_color_viridis(direction = -1) +
    theme(legend.position = "none", 
#          axis.text = element_blank(),
#          axis.title = element_blank(),
           legend.direction = "horizontal"
          ) + 
    labs(title = title)
  return(p1)
}

getROC_variation <- function(edges, p){
  #function return false positive rate and true positive rate for different cutoffs
  roc <- Vectorize(function(edges, p){
    complete_edges <- edges[!is.na(edge) & !is.na(inmapPM),]
    setkey(complete_edges, Monitor)
    #TRUE
    network_edges <- complete_edges$edge
    #PREDICTED - percentile cutoffs are different for each power plant
    cutoffs <- complete_edges[ , list(inmapPM.cutoff = quantile(inmapPM, p)), by = "Monitor"]
    setkey(cutoffs, Monitor)
    complete_edges <- complete_edges[cutoffs]
    inmap_edges <- ifelse(complete_edges$inmapPM >= complete_edges$inmapPM.cutoff,1,0)
    
    #false positives
    false_pos <- sum(ifelse(inmap_edges == 1 & network_edges == 0, 1, 0))
    # true negatives
    true_neg <- sum(ifelse(inmap_edges == 0 & network_edges == 0, 1, 0))
    
    #false positive rate
    FPR <- false_pos/(false_pos + true_neg)
    
    
    #true positives
    true_pos <- sum(ifelse(inmap_edges == 1 & network_edges == 1, 1, 0))
    #false negatives
    false_neg <- sum(ifelse(inmap_edges == 0 & network_edges == 1, 1, 0))
    
    #true positive rate
    TPR <- true_pos/(true_pos + false_neg)
    
    return(c(FPR,TPR))
  }, vectorize.args = "p")
  
  rates <- roc(edges,p)
  #format output
  rates <- data.frame(`InMAP cutoff` = p,FPR = rates[1 ,], TPR = rates[2, ])
  return(rates)
}

PP.high <- powerplants[link.cat == "high",]$PP
PP.normal <- powerplants[link.cat == "normal",]$PP
PP.none <- powerplants[link.cat == "none"]$PP
PP.seasonal <- powerplants[type == "seasonal"]$PP
PP.allyear <- powerplants[type == "all.year"]$PP

PP.IMW <- powerplants[PP.region == "IndustrialMidwest",]
PP.NE <- powerplants[PP.region == "Northeast",]
PP.SE <- powerplants[PP.region == "Southeast",]

plotROC(edges[PP.IMW], p)

regions <- c("IndustrialMidwest", "Northeast", "Southeast")
powerplant_cat <- c(1,2)
type = c("all.year","seasonal")

cases <- expand.grid(regions,powerplant_cat, type)
cases <- data.table(cases)
names(cases) <- c("PP.region", "powerplant_cat", "PP.type")
setkey(cases, PP.region, powerplant_cat, PP.type)

plots <- as.list(rep(0,12))
for(i in 1:nrow(cases)){
  plots[[i]] <- plotROC(edges[PP.region == cases[i,]$PP.region & powerplant_cat == cases[i,]$powerplant_cat & PP.type == cases[i,]$PP.type,
                              ], p)
}

# for(i in 1:12){
#   pdf(file = paste("results/roc",i,".pdf", sep = ""), height = 0.75, width = 0.75)
#   print(plots[[i]])
#   dev.off()
# }

pdf(file = "results/roc_all.pdf", height = 3, width = 2.5)
plotROC(edges, title = "All regions")
dev.off()

pdf(file = "results/roc_byPPregion.pdf", height = 3, width = 6.5)
p1 <- plotROC(edges[PP.region == "IndustrialMidwest",], title = "Industrial Midwest")
p2 <- plotROC(edges[PP.region == "Northeast",], title = "Northeast")
p3 <- plotROC(edges[PP.region == "Southeast",], title = "Southeast")
grid.arrange(p1,p2,p3, ncol = 3)
dev.off()

pdf(file = "results/roc_byMonitorregion.pdf", height = 3, width = 6.5)
p1 <- plotROC(edges[receptor.region == "IndustrialMidwest",], title = "Industrial Midwest")
p2 <- plotROC(edges[receptor.region == "Northeast",], title = "Northeast")
p3 <- plotROC(edges[receptor.region == "Southeast",], title = "Southeast")
grid.arrange(p1,p2,p3, ncol = 3)
dev.off()

pdf(file = "results/roc_byDistcat.pdf", height = 2.5, width = 7)
p1 <- plotROC(edges[receptor.region == "IndustrialMidwest" & distance_cat == 1,], title = "0-250km")
p2 <- plotROC(edges[receptor.region == "IndustrialMidwest" & distance_cat == 2,], title = "250-500km")
p3 <- plotROC(edges[receptor.region == "IndustrialMidwest" & distance_cat == 3,], title = "500-750km")
p4 <- plotROC(edges[receptor.region == "IndustrialMidwest" & distance_cat == 4,], title = "750-1000km")
grid.arrange(p1,p2,p3, p4, ncol = 4)
dev.off()

pdf(file = "results/roc_bySizecat.pdf", height = 3, width = 4)
p1 <- plotROC(edges[PP.region == "Northeast" & powerplant_cat == 1,], title = "Cat 1 Plants")
p2 <- plotROC(edges[PP.region == "Northeast" & powerplant_cat == 2,], title = "Cat 2 Plants")
grid.arrange(p1,p2, ncol = 2)
dev.off()

plotROC(edges[PP.region == "IndustrialMidwest" & direction == "E",])
pdf(file = "results/roc_example.pdf", height = 6, width = 5)
p1 <- plotROC(edges[PP.region == "Northeast",], title = "power plants (NE) to all monitors")
p2 <- plotROC(edges[PP.region == "Northeast" & distance_cat == 1 ,], title = "power plants (NE) to close monitors")
p3 <- plotROC(edges[PP.region == "Northeast" & distance_cat == 1 & direction %in% c("NE","E", "SE"),],
        title = "power plants (NE) to close monitors to the east")
grid.arrange(p1,p2,p3,ncol = 1)
dev.off()


## ------------------------------------------------------------------------------------ ##
##  Other network and InMAP comparisons                                                 ##
## ------------------------------------------------------------------------------------ ##

zero_sum <- powerplants[ , list(median.ZeroDays = median(emissions.ZeroDays)), by = c("PP.region","powerplant_cat")]
setkey(zero_sum, PP.region)
zero_sum

type_summary <- powerplants[ , list(n = length(PP), 
                                    largePerc = signif(sum(powerplant_cat==2)/length(PP),2),
                                    median.linked.perc = median(linkedMonitorsPerc)),
                                    by = c("PP.region", "type")]
setkey(type_summary, PP.region)
type_summary

link_summary <- powerplants[ , list(n = length(PP),
                                    largePerc = signif(sum(powerplant_cat==2)/length(PP),2),
                                    seasonalPerc = signif(sum(type == "seasonal")/length(PP),2)),
                             by = c("link.cat")]
setkey(link_summary,link.cat)
link_summary

#InMAP table from paper

InMAPsummary <- edges[!is.na(edge) & PP.region %in% c("IndustrialMidwest", "Northeast", "Southeast"), 
                      list(medianInMAP = signif(median(inmapPM, na.rm = TRUE)*1000,2),
                           meandist = signif(mean(distance),2),
                           n = length(unique(PP))), 
                      by = c("PP.region", "powerplant_cat", "PP.type", "edge")]
setkey(InMAPsummary, PP.region, powerplant_cat, PP.type, edge)
InMAPsummary
InMAPsummary$powerplant_cat = as.integer(InMAPsummary$powerplant_cat)
InMAPsummary$edge = as.integer(InMAPsummary$edge)
print(xtable(InMAPsummary), include.rownames = FALSE)

edgePerc_summary <- edges[!is.na(edge) & PP.region %in% c("IndustrialMidwest", "Northeast", "Southeast"),
             list(edgePerc = round(sum(edge)/length(edge),2)),
      by = c("PP.region", "powerplant_cat", "PP.type")]
setkey(edgePerc_summary, PP.region, powerplant_cat,PP.type)
edgePerc_summary

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

pp.cex <- ifelse(powerplants$powerplant_cat == 1, 0.6, 1.25)
pp.bg <- ifelse(powerplants$linkedMonitorsPerc > 0.5, "red","yellow")
points(powerplants$PP.longitude, powerplants$PP.latitude,
       pch = 24, bg = pp.bg, col = "black", lwd = 1, cex = pp.cex)
legend(x = -79, y = 33.5, 
       legend = c("coal power\nplant","AQS monitor"),
       pch = c(24,21),
       pt.cex = c(1.25,1),
       pt.bg = c(viridis(2)[2],"black"),
       cex = 1, bty = "n")



