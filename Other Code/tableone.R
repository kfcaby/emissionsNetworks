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
  if(region == "Northwest") abbr <- "NW"
  if(region == "Southwest") abbr <- "SW"
  if(region == "UpperMidwest") abbr <- "UMW"
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
edges[ , edge.neg := ifelse(gams.coeff < 0 & p.value_adj < 0.05,1,0)]

edges <- edges[receptor.2005NAs < 365, ]

## ------------------------------------------------------------------------------------ ##
##   Table for Monitor/Power Plant Summary                                              ##
## ------------------------------------------------------------------------------------ ##

#Monitors
monitors <- edges[ , list(NAdays = unique(receptor.2005NAs),
                          avgPM = unique(avgPM), 
                          sdPM = unique(sdPM), linkedPP = sum(edge, na.rm = TRUE),
                          linkedPP.neg = sum(edge.neg, na.rm = TRUE),
                          receptor.region = unique(receptor.region),
                          monitor.freq = unique(monitor.freq),
                          receptor.latitude = unique(receptor.latitude),
                          receptor.longitude = unique(receptor.longitude)),
                  by = "Monitor"]
monitors[ , degree.zero := ifelse(linkedPP == 0, 1, 0)]

#Power plants
powerplants <- edges[ ,
                     list(avgemissions = unique(avgemissions),
                          sdemissions = unique(sdemissions), emissions.NAdays = unique(emissions.NAdays),
                          PP.region = unique(PP.region), PP.state = unique(PP.state),
                          PP.latitude = unique(PP.latitude), PP.longitude = unique(PP.longitude),
                          linkedMonitors = sum(edge,na.rm = TRUE),
                          linkedMonitorsPerc = round(sum(edge,na.rm = TRUE)/sum(edge ==1 | edge == 0, na.rm = TRUE),2),
                          linkedMonitors.neg = sum(edge.neg,na.rm = TRUE),
                          linkedMonitors.negPerc = round(sum(edge.neg,na.rm = TRUE)/sum(edge ==1 | edge == 0, na.rm = TRUE),2),
                          powerplant_cat = unique(powerplant_cat),
                          emissions.ZeroDays = unique(emissions.ZeroDays)),
                     by = "PP"]
powerplants[ , type := ifelse(emissions.ZeroDays > 90, "seasonal", "all.year")]
powerplants[ , link.cat := ifelse(linkedMonitors == 0, "unlinked", "linked")]
powerplants[ , link.cat := factor(link.cat, levels = c("linked", "unlinked"))]
powerplants[ , degree.zero := ifelse(linkedMonitors == 0, 1, 0)]
powerplants[ , PP.region.abbr := region_abbr(PP.region)]

## ------------------------------------------------------------------------------------ ##
##   summary table                                                                      ##
## ------------------------------------------------------------------------------------ ##

pp1 <- powerplants[ , list(n = .N,
                           avgemissions = round(mean(avgemissions, na.rm = TRUE),2),
                           avgsdemissions = round(mean(sdemissions, na.rm = TRUE),2),
                           linkedMonitors = median(linkedMonitors, na.rm = TRUE),
                           unlinked = sum(linkedMonitors == 0, na.rm = TRUE)),
                    by = "PP.region"]
pp2 <- melt(pp1, id.vars = "PP.region")
xtable(dcast(pp2, variable ~ PP.region, value.var = "value"))

m1 <- monitors[ , list(n = .N,
                       avgPM = round(mean(avgPM, na.rm = TRUE),2),
                       avgsdPM = round(mean(sdPM, na.rm = TRUE),2),
                       linkedPP = median(linkedPP, na.rm = TRUE),
                       unlinked = sum(linkedPP == 0, na.rm = TRUE)),
                by = "receptor.region"]
m2 <- melt(m1, id.vars = "receptor.region")
xtable(dcast(m2, variable ~ receptor.region, value.var = "value"))

## ------------------------------------------------------------------------------------ ##
##   remove pairs from hyper connectors and zero connectors                             ##
## ------------------------------------------------------------------------------------ ##
monitors.subset <- monitors[linkedPP > 0, ]
powerplants.subset <- powerplants

edges <- edges[Monitor %in% monitors.subset$Monitor & PP %in% powerplants.subset$PP, ]



#recalculate power plant and monitor stats
#Monitors
monitors.subset <- edges[ , list(Monitor = unique(Monitor), 
                                 avgPM = unique(avgPM), 
                                 sdPM = unique(sdPM), 
                                 linkedPP = sum(edge, na.rm = TRUE),
                                 linkedPP.neg = sum(edge.neg, na.rm = TRUE),
                                 exposure = sum(avgemissions*(1/distance)*edge, na.rm = TRUE),
                                 exposure_inmap = sum(inmapPM, na.rm = TRUE),
                                 receptor.region = unique(receptor.region),
                                 receptor.state = unique(receptor.state),
                                 monitor.freq = unique(monitor.freq),
                                 receptor.latitude = unique(receptor.latitude),
                                 receptor.longitude = unique(receptor.longitude)),
                          by = "Monitor"]


#Power plants
powerplants.subset <- edges[ PP.region %in% c("IndustrialMidwest", "Northeast", "Southeast"),
                             list(avgemissions = unique(avgemissions),
                                  sdemissions = unique(sdemissions), emissions.NAdays = unique(emissions.NAdays),
                                  PP.region = unique(PP.region), PP.state = unique(PP.state),
                                  PP.latitude = unique(PP.latitude), PP.longitude = unique(PP.longitude),
                                  linkedMonitors = sum(edge,na.rm = TRUE),
                                  linkedMonitorsPerc = sum(edge,na.rm = TRUE)/sum(edge ==1 | edge == 0, na.rm = TRUE),
                                  linkedMonitors.neg = sum(edge.neg,na.rm = TRUE),
                                  powerplant_cat = unique(powerplant_cat),
                                  emissions.ZeroDays = unique(emissions.ZeroDays)),
                             by = "PP"]






## ------------------------------------------------------------------------------------ ##
##   Blank Map                                                                          ##
## ------------------------------------------------------------------------------------ ##

#blank map for paper
#pdf(file = "results/blankmap.pdf", height = 4)
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
points(monitors$receptor.longitude, monitors$receptor.latitude, pch = 21, bg = "black")
pp.cex <- ifelse(powerplants$powerplant_cat == 1, 0.6, 1.25)
points(powerplants$PP.longitude, powerplants$PP.latitude,
       pch = 24, bg = viridis(2)[2], col = "black", lwd = 1, cex = pp.cex)
legend(x = -79, y = 33.5, 
       legend = c("coal power\nplant","AQS monitor"),
       pch = c(24,21),
       pt.cex = c(1.25,1),
       pt.bg = c(viridis(2)[2],"black"),
       cex = 1, bty = "n")
#dev.off()

## ------------------------------------------------------------------------------------ ##
##   edge percents                                                                      ##
## ------------------------------------------------------------------------------------ ##

edge.perc <- edges[ , list(edgePerc = sum(edge, na.rm = TRUE)/sum(!is.na(edge)),
                           edgePerc.neg = sum(edge.neg, na.rm = TRUE)/sum(!is.na(edge.neg))), by = "distance_cat"]
setkey(edge.perc, distance_cat)
edge.perc

nrow(edges[!is.na(edge),])
nrow(edges[edge == 1,])

nrow(edges[edge == 1,])/nrow(edges[!is.na(edge),])
nrow(edges[edge.neg == 1,])/nrow(edges[!is.na(edge),])

## ------------------------------------------------------------------------------------ ##
##   Degree Histograms                                                                  ##
## ------------------------------------------------------------------------------------ ##

pdf(file = "results/monitor_degree.pdf", height = 3, width = 3)
hist(monitors$linkedPP, main = NA, xlab = NA, breaks = 50, cex = 0.75, ylab = "power plants (#)")
dev.off()

pdf(file = "results/powerplant_degree.pdf", height = 3, width = 3)
hist(powerplants$linkedMonitors, main = NA, xlab = NA, breaks = 50, cex = 0.75, ylab = "monitors (#)")
dev.off()

median(monitors.subset$linkedPP)
sum(monitors.subset$linkedPP == 0)
nrow(monitors.subset)
sum(monitors.subset$linkedPP == 0)/nrow(monitors.subset)

median(powerplants.subset$linkedMonitors)
sum(powerplants.subset$linkedMonitors == 0)
nrow(powerplants.subset)
sum(powerplants.subset$linkedMonitors == 0)/nrow(powerplants.subset)

## ------------------------------------------------------------------------------------ ##
##   degree zero                                                                       ##
## ------------------------------------------------------------------------------------ ##

powerplants[ , list(emissions.ZeroDays = median(emissions.ZeroDays)), by = "degree.zero"]

monitors[degree.zero == 1, list(365 - NAdays)]

## ------------------------------------------------------------------------------------ ##
##   side-by-side maps for monitor exposure                                             ##
## ------------------------------------------------------------------------------------ ##
pdf(file = "results/exposure_map.pdf", height = 4)
map("state", fill = FALSE, plot = TRUE)
bg.monitor <- rep(NA, nrow(monitors.subset))
bg.monitor[monitors.subset$exposure > 0] <- rank(monitors.subset[exposure > 0,]$exposure)
bg.monitor[monitors.subset$exposure > 0] <- rev(plasma(nrow(monitors.subset[exposure > 0,])))[bg.monitor[monitors.subset$exposure > 0]]
points(monitors.subset[ , .(receptor.longitude,receptor.latitude)] ,pch = 21, bg = bg.monitor, cex = 1.25)
dev.off()


pdf(file = "results/inmap_map.pdf", height = 4)
map("state", fill = FALSE, plot = TRUE)
bg.monitor.inmap <- rep(NA, nrow(monitors.subset))
bg.monitor.inmap[monitors.subset$exposure_inmap > 0] <- rank(monitors.subset[exposure_inmap > 0,]$exposure_inmap)
bg.monitor.inmap[monitors.subset$exposure_inmap > 0] <- rev(plasma(nrow(monitors.subset[exposure_inmap > 0,])))[bg.monitor.inmap[monitors.subset$exposure_inmap > 0]]
points(monitors.subset[ , .(receptor.longitude,receptor.latitude)] ,pch = 21, bg = bg.monitor.inmap, cex = 1.25)
dev.off()

hist(monitors.subset$exposure, breaks = 50)
hist(monitors.subset$exposure_inmap, breaks = 50)


## ------------------------------------------------------------------------------------ ##
##   rank comparisons for monitor exposure                                             ##
## ------------------------------------------------------------------------------------ ##
rankComp <- function(monitors, regions){
  data <- monitors[(receptor.region %in% regions) & (exposure > 0) & (exposure_inmap > 0), ]
  
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
  plot <- ggplot(data, aes(x = exposure_rank, y = inmap_rank, color = receptor.subregion)) + geom_point(size = 4) + 
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

p1 <- rankComp(monitors.subset, regions = "IndustrialMidwest")
#pdf(file = "results/comparisonIMW.pdf", height = 3, width = 4)
p1
dev.off()

#without Michigan and Ohio
rankComp(monitors.subset[!(receptor.state %in% c("MI","OH"))  ,], regions = "IndustrialMidwest")

p2 <- rankComp(monitors.subset, regions = "Northeast")
#pdf(file = "results/comparisonNE.pdf", height = 3, width = 4)
p2
dev.off()

#excluding western Penn.
rankComp(monitors.subset[!(receptor.state %in% c("PA") & receptor.longitude < -77.7),], regions = "Northeast")

p3 <- rankComp(monitors.subset, regions = "Southeast")
#pdf(file = "results/comparisonSE.pdf", height = 3, width = 4)
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
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.direction = "horizontal"
          )
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


regions <- c("IndustrialMidwest", "Northeast", "Southeast")
powerplant_cat <- c(1,2)
type = c("all.year","seasonal")

cases <- expand.grid(regions,powerplant_cat, type)
cases <- data.table(cases)
names(cases) <- c("PP.region", "powerplant_cat", "PP.type")
setkey(cases, PP.region, powerplant_cat, PP.type)

p = seq(0.02,0.98,0.02)

plots <- as.list(rep(0,12))
for(i in 1:nrow(cases)){
  plots[[i]] <- plotROC(edges[PP.region == cases[i,]$PP.region & powerplant_cat == cases[i,]$powerplant_cat & PP.type == cases[i,]$PP.type,
                              ], p)
}

for(i in 1:12){
  pdf(file = paste("results/roc",i,".pdf", sep = ""), height = 0.75, width = 0.75)
  print(plots[[i]])
  dev.off()
}

pdf(file = "results/roc_all.pdf", height = 3, width = 2.5)
plotROC(edges[receptor.region %in% c("IndustrialMidwest", "Northeast", "Southeast"),], title = "All regions")
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
##   Prob curve for edges and InMAP PM                                                   ##
## ------------------------------------------------------------------------------------ ##
#Compares edge probability for InMAP vs non-InMAP edges
getProbs <- function(edges, p = seq(0.02,0.98,0.02)){
  #function return false positive rate and true positive rate for different cutoffs
  probs <- Vectorize(function(edges, p){
    complete_edges <- edges[!is.na(edge) & !is.na(inmapPM),]
    
    
    complete_edges[ , edge.inmap := ifelse(complete_edges$inmapPM >= quantile(complete_edges$inmapPM,p),1,0)]
    
    
    percIn <- complete_edges[edge.inmap == 1, sum(edge)/length(edge) ]
    percOut <- complete_edges[edge.inmap == 0, sum(edge)/length(edge) ]
    
    
    return(c(percIn,percOut))
  }, vectorize.args = "p")
  
  rates <- probs(edges,p)
  #format output
  rates <- data.frame(`InMAP cutoff` = p,percIn = rates[1 ,], percOut = rates[2, ])
  return(rates)
}
plotProbs <- function(edges, p = seq(0.02,0.98,0.02), title = "", return.probs = FALSE){
  require(viridis)
  rates <- getProbs(edges, p)
  p1 <- ggplot(rates, aes(x = percIn, y = percOut, color = InMAP.cutoff)) + geom_point() +
    theme_bw() + geom_abline(slope = 1, intercept = 0) + scale_color_viridis(direction = -1) +
    theme(legend.position = "bottom", 
          #          axis.text = element_blank(),
          #          axis.title = element_blank(),
          legend.direction = "horizontal"
    ) + labs(title = title) + ylim(0,1) + xlim(0,1)
  ifelse(return.probs == TRUE, return(rates), return(p1))
}
#only do analysis for powerplants and monitors with sufficient linkages


probsIMW <-plotProbs(edges[PP.region == "IndustrialMidwest", ],
          return.probs = TRUE) 
probsIMW$PP.region <- "IndustrialMidwest"
probsNE <- plotProbs(edges[PP.region == "Northeast", ],
                     return.probs = TRUE)
probsNE$PP.region <- "Northeast"
probsSE <- plotProbs(edges[PP.region == "Southeast", ],
                     return.probs = TRUE)
probsSE$PP.region <- "Southeast"
probs <- rbind(probsIMW,probsNE, probsSE)

#pdf(file = "results/probPlots.pdf", height = 4)
ggplot(probs, aes(x = percIn, y = percOut, color = InMAP.cutoff, group = PP.region, shape = PP.region)) + geom_point() +
  theme_bw() + geom_abline(slope = 1, intercept = 0) + scale_color_viridis(direction = -1) +
  theme(
        #          axis.text = element_blank(),
        #          axis.title = element_blank(),
        
  ) + labs(title = "", x = "Edge % - InMAP edges", y = "Edge % - InMAP non-edges") + ylim(0,0.5) + xlim(0,0.5) +
  guides(shape=guide_legend(title="power plant region"))
#dev.off()



## ------------------------------------------------------------------------------------ ##
##  InMAP table from paper                                                              ##
## ------------------------------------------------------------------------------------ ##


#InMAP table from paper

InMAPsummary <- edges[!is.na(edge) & PP.region %in% c("IndustrialMidwest", "Northeast", "Southeast"), 
                      list(medianInMAP = signif(median(inmapPM, na.rm = TRUE)*1000,2),
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


## ------------------------------------------------------------------------------------ ##
##   Map of monitors and power plants with no edges                                     ##
## ------------------------------------------------------------------------------------ ##
regions = c("IndustrialMidwest","Northeast", "Southeast")

pdf(file = "results/degree_zero.pdf", height = 4)
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

points(monitors$receptor.longitude, monitors$receptor.latitude, pch = 21, 
       bg = ifelse(monitors$degree.zero == 1, "red","black"))
points(monitors[degree.zero == 1,]$receptor.longitude, monitors[degree.zero == 1,]$receptor.latitude,
       pch = 21, bg = "red")

pp.cex <- ifelse(powerplants[PP.region %in% regions,]$avgemissions < quantile(powerplants[PP.region %in% regions,]$avgemissions, 0.8), 0.6, 1.25)
pp.bg <- ifelse(powerplants[PP.region %in% regions,]$degree.zero == 1, "red", viridis(2)[2]) 
points(powerplants[PP.region %in% regions,]$PP.longitude, powerplants[PP.region %in% regions,]$PP.latitude,
       pch = 24, bg = pp.bg, col = "black", lwd = 1, cex = pp.cex)
legend(x = -78.5, y = 33, title = "Coal power\nplants",
       legend = c("linked" ,"unlinked"),
       pch = c(24, 24),
       pt.cex = c(1,1),
       pt.bg = c(viridis(2)[2],"red"),
       cex = 0.75, bty = "n", title.adj = 0, xjust = 0, inset = 0, y.intersp = 1)
legend(x = -78.5, y = 29, title = "AQS monitors",
       legend = c("linked" ,"unlinked"),
       pch = c(21, 21),
       pt.cex = c(1,1),
       pt.bg = c("black","red"),
       cex = 0.75, bty = "n", title.adj = 0, xjust = 0, inset = 0, y.intersp = 1)
dev.off()

## ------------------------------------------------------------------------------------ ##
##   why do some power plants not connect to anything - Appendix A                      ##
## ------------------------------------------------------------------------------------ ##
setkey(powerplants, PP)
setkey(monitors, Monitor)
regions <- c("IndustrialMidwest", "Northeast", "Southeast")

#emissions
emissions2005 <- fread(file = "data/emissions2005.csv")
names(emissions2005)[1] <- "PP"
setkey(emissions2005, PP)

emissions2005 <- emissions2005[powerplants[PP.region %in% regions,]$PP]
emissions.long <- melt(emissions2005, variable.name = "date", value.name = "SO2")
setkey(emissions.long, PP)
emissions.long <- emissions.long[powerplants[PP.region %in% regions ,.(PP,PP.region)]]
emissions.long$date <- as.Date(emissions.long$date)

emissions.daily <- emissions.long[ , list(dailySO2 = sum(SO2)), by = c("date","PP.region")]
#PP3136 (negative), #PP3149 (positive)
#PM
PM2005 <- fread("data/PM.daily.monitor2005_raw.csv")
names(PM2005)[1] <- "Monitor"
setkey(PM2005, Monitor)

PM2005 <- PM2005[monitors$Monitor]
PM2005.long <- melt(PM2005, variable.name = "date", value.name = "PM25")
setkey(PM2005.long, Monitor)
PM2005.long <- PM2005.long[monitors[ ,.(Monitor,receptor.region)]]
PM2005.long$date <- as.Date(PM2005.long$date)

PM2005.daily <- PM2005.long[ , list(avgPM = mean(PM25, na.rm = TRUE)), by = c("date", "receptor.region")]

p1 <- ggplot(emissions.daily, aes(x = date, y = dailySO2, group = PP.region, linetype = PP.region)) + geom_line() + 
  theme(legend.direction = "horizontal", legend.position = "bottom", axis.title.x = element_blank()) +
  guides(linetype = guide_legend(title = "region")) +
  labs(y = "SO2 (tons)")
p2 <- ggplot(PM2005.daily, aes(x = date, y = avgPM, group = receptor.region, linetype = receptor.region)) + geom_line() +
  theme(axis.title.x = element_blank()) + labs(y = "PM2.5")
mylegend<-g_legend(p1)
blank <- rectGrob(gp = gpar(col = "white"))

#pdf(file = "results/timeseries.pdf", width = 6.5, height = 5)
grid.arrange(p1 + theme(legend.position = "none"), 
             p2 + theme(legend.position = "none"), 
             mylegend, 
             #ncol = 1, 
             layout_matrix = rbind(c(1,1),c(4,2),c(3,3)),
             heights = c(0.45,0.45,0.1),
             widths = c(0.022,0.978))
#dev.off()


p1 <- ggplot(emissions.long[PP %in% c("PP3136", "PP3149")],
       aes(x = date, y = SO2, linetype = PP)) + geom_line() + 
  theme(legend.direction = "horizontal")
      

regions = c("IndustrialMidwest", "Northeast", "Southeast")

#summary table
PP.link.sum <- powerplants[ PP.region %in% regions , list(powerplants = .N, 
                                                          neg = median(linkedMonitors.neg), 
                                                          pos = median(linkedMonitors)), 
                            by = c("PP.region", "link.cat")]
setkey(PP.link.sum, PP.region, link.cat)
  sum1 <- dcast(PP.link.sum, link.cat~PP.region, value.var = "powerplants")
xtable(sum1)
  
txt.size = 10
theme2 <- theme(axis.title.x = element_blank(),
                legend.position = "none",
                axis.title = element_text(size = txt.size),
                axis.text = element_text(size = txt.size),
                legend.text = element_text(size = txt.size),
                legend.title = element_text(size = txt.size),
                plot.title = element_text(size = txt.size + 2, hjust = 0.5))
theme1 <- theme(axis.title.x = element_blank(),
                legend.position = "bottom",
                axis.title = element_text(size = txt.size),
                axis.text = element_text(size = txt.size),
                legend.text = element_text(size = txt.size),
                legend.title = element_text(size = txt.size),
                legend.key.size = unit(6, "line"),
                plot.title = element_text(size = txt.size + 2, hjust = 0.5)) 

pt.size <- 10

#pdf("monitor_networks/plots/emissions_analysis.pdf", height = 9, width = 22)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


p1 <- ggplot(powerplants[PP.region %in% regions,], aes(x = PP.region.abbr, y = avgemissions, fill = link.cat)) + 
  geom_boxplot(alpha = 0.75) +
  scale_fill_viridis(discrete = TRUE, direction = -1) + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.direction = "horizontal", 
        legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10)) +
  guides(fill = guide_legend(title = "power plant")) + 
  labs(title = "average daily SO2 (tons)")
p2 <- ggplot(powerplants[PP.region %in% regions,], aes(x = PP.region.abbr, y = sdemissions, fill = link.cat)) + 
  geom_boxplot(alpha = 0.75) + 
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10)) +
  labs(title = "average daily stdev in SO2")
p3 <- ggplot(powerplants[PP.region %in% regions,], aes(x = PP.region.abbr, y = 365 - emissions.ZeroDays, fill = link.cat)) +
  geom_boxplot(alpha = 0.75) +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  theme(text = element_text(size = 10),
        legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10)) + 
  labs(title = "operating days")
mylegend<-g_legend(p1)
blank <- rectGrob(gp = gpar(col = "white"))

pdf(file = "results/link.pdf", height = 3, width = 6.5)
grid.arrange(p1 + theme(legend.position = "none"),p2,p3, mylegend, ncol = 3,
             layout_matrix = rbind(c(1,2,3),c(4,4,4)),
             heights = c(0.90,0.10))
dev.off()

ggplot(powerplants[PP.region %in% regions], aes(x = linkedMonitors.negPerc, y = linkedMonitorsPerc, color = PP.region)) + geom_point()


## ------------------------------------------------------------------------------------ ##
##   Other degree zero analysis                                                          ##
## ------------------------------------------------------------------------------------ ##

#PA power plants
PP.PA <- powerplants[PP.state == "PA" & powerplant_cat == 2,]
setkey(PP.PA, PP.longitude)
PP.PA

plotEmissionsNetwork(edges[PP.state == "PA" & powerplant_cat == 2,], xlim = c(-80.5,-74.5), ylim = c(39.5,42.1))


PP.WI <- powerplants[PP.state == "WI",]
setkey(PP.WI, degree.zero,PP.longitude)
PP.WI

plotEmissionsNetwork(edges[PP.state == "WI",], xlim = c(-93.2,-86.5), ylim = c(42.5,47))


## ------------------------------------------------------------------------------------ ##
##   Map of power plants by state                                                        ##
## ------------------------------------------------------------------------------------ ##
pdf(file = "results/bystateSE.pdf", width = 6.5, height = 6.5)
par(mai = c(0,0,0,0))
par(mfrow = c(3,3))
regions <- c("Southeast")
lapply(unique(edges[PP.region %in% regions,]$PP.state), function(state){
  state = "MD"
  pairs.subset <- edges[PP.state == state, ]
  edges.subset <- edges[PP.state == state & edge == 1, ]
  edges.subset <- edges.subset[order(distance, decreasing = TRUE),]
  colors <- viridis(4)
  color.index <- 5 - edges.subset$distance_cat
  
  if(unique(pairs.subset$PP.region) == "Northeast") {
    xlim = c(-89, -70)
    ylim = c(33,46)
  }
  if(unique(pairs.subset$PP.region) == "IndustrialMidwest") {
    xlim = c(-95, -75)
    ylim = c(35,48)
  }
  if(unique(pairs.subset$PP.region) == "Southeast") {
    xlim = c(-95, -74)
    ylim = c(27,39)
  }
  map("state",fill=FALSE, plot=TRUE, xlim = xlim, ylim = ylim)
  points(pairs.subset$receptor.longitude,
         pairs.subset$receptor.latitude,
         pch = 21, bg = "black")
  segments(edges.subset$receptor.longitude,
           edges.subset$receptor.latitude,
           edges.subset$PP.longitude,
           edges.subset$PP.latitude,
           col = colors[color.index],
           lwd = 0.3)
  points(pairs.subset$PP.longitude,
         pairs.subset$PP.latitude,
         pch = 24,
         cex = ifelse(pairs.subset$powerplant_cat == 2, 1.25,0.6),
         bg = viridis(2)[2], lwd = 1)
})
dev.off()