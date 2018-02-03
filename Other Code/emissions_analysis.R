

library(gridExtra)
library(grid)

PP.summary <- edges[ , list(edges = sum(edge, na.rm = TRUE),
                         avglag = mean(lag, na.rm = TRUE),
                         edges.possible = sum(!is.na(lag)),
                         avgemissions = unique(avgemissions),
                         emissions.NAdays = unique(emissions.NAdays),
                         PP.region = unique(PP.region),
                         PP.longitude = unique(PP.longitude),
                         PP.latitude = unique(PP.latitude)),
                          by = "PP"]

setkey(PP.summary, PP)

#remove powerplants with no possible edges
PP.summary <- subset(PP.summary, !is.na(avglag))

#edge probability
PP.summary[ , edge.probability := edges/edges.possible]

#does the power plant have any edges
PP.summary[ , has.edges := ifelse(edges > 0, 1, 0)]

#plot power plants with and without edges
dft <- par("mar")
par(mar = c(0,0,0,0))
US <- map("state",fill=TRUE, plot=FALSE)
US.names <- US$names
US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
US_poly_sp <- map2SpatialPolygons(US,IDs=US.IDs,proj4string=CRS("+proj=longlat + datum=wgs84"))
plot(US_poly_sp)
title("Power plants with and without linked monitors", line = -3, adj = 0.75, cex.main = 2)
pp.cex <- ifelse(PP.summary$avgemissions < quantile(PP.summary$avgemissions, 0.8), 1.5, 3)
pp.bg <- ifelse(PP.summary$has.edges == 1, viridis(2)[1], viridis(2)[2])
points(PP.summary$PP.longitude, PP.summary$PP.latitude,pch = 24, 
       bg = pp.bg, col = "black", lwd = 2, cex = pp.cex)
legend(x = -78.8, y = 32.4, 
       title = "Power plant:",
       legend = c("with linked monitors","without linked monitors"),
       pch = c(24,24),
       pt.cex = 1.25,
       pt.bg = c(viridis(2)[1],viridis(2)[2]),
       cex = 1.5)


# add standard deviation of emissions
emissions <- fread("data/daily.emissions.csv")
power.plants <- emissions$V1
emissions[ , V1 := NULL]
emissions <- as.matrix(emissions)
rownames(emissions) <- power.plants
emissions <- emissions[ , as.Date(colnames(emissions)) >= as.Date("2005-06-01") &
                         as.Date(colnames(emissions)) <= as.Date("2005-08-31")]
emissions <- emissions[PP.summary$PP, ]

PP.summary$stdev.emissions <- apply(emissions,1,sd,na.rm = TRUE)
PP.summary <- subset(PP.summary, PP.region %in% c("IndustrialMidwest", "Northeast", "Southeast"))

#Is there a difference between power plants with and without edges
has.edge.summary <- PP.summary[ , list(n.powerplants = .N,
                  median.edges.count = as.integer(median(edges)),                     
                  median.edges.possible = as.integer(median(edges.possible)),
                  avgemissions = round(mean(avgemissions),1),
                  median.NAdays = as.integer(median(emissions.NAdays)),
                  avglag = round(mean(avglag),1),
                  mean.stdev = round(mean(stdev.emissions, na.rm = TRUE),1)),
        by = c("PP.region","has.edges")]
setkey(has.edge.summary, PP.region, has.edges)
has.edge.summary <- subset(has.edge.summary, PP.region %in% c("IndustrialMidwest", "Northeast", "Southeast"))

has.edge.summary


summary(glm(has.edges ~ avglag + avgemissions + edges.possible + emissions.NAdays,
            family = binomial, data = PP.summary))

txt.size = 16
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
                           legend.text = element_text(size = 20),
                           legend.title = element_text(size = 20),
                           legend.key.size = unit(6, "line"),
                           plot.title = element_text(size = txt.size + 2, hjust = 0.5)) 

pt.size <- 10

#pdf("monitor_networks/plots/emissions_analysis.pdf", height = 9, width = 22)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

num.PP <- ggplot(has.edge.summary, aes(x = PP.region, y = n.powerplants, color = as.factor(has.edges))) +
    labs(title = "Number of powerplants", y = "Powerplants" ) + geom_point(size = pt.size, shape = 17) + theme_bw() + 
    scale_color_manual(values = c(viridis(2)[2],viridis(2)[1]),name = "Powerplants: ",
                       labels = c("without edges","with edges"))+ ylim(0,125)+theme2

p1 <- ggplot(PP.summary, aes(x = PP.region, y = edges, fill = as.factor(has.edges))) +
  labs(title = "Number of linked monitors", y = "monitors") + geom_boxplot(alpha = 0.7) + theme_bw() + 
  scale_fill_manual(values = c(viridis(2)[2],viridis(2)[1]),
                    labels = c("without edges","with edges"),
                    guide = guide_legend(title.position = "left", title = "Powerplants:",
                                         label.position = "left")) + theme1

p2 <- ggplot(PP.summary, aes(x = PP.region, y = edges.possible, fill = as.factor(has.edges))) +
  labs(title = "Number of possible linked monitors", y = "possible monitors") +  geom_boxplot(alpha = 0.7) + theme_bw() + 
  scale_fill_manual(values = c(viridis(2)[2],viridis(2)[1]),name = "Powerplants: ", labels = c("without edges","with edges")) + theme2

p3 <- ggplot(PP.summary, aes(x = PP.region, y = avgemissions, fill = as.factor(has.edges))) +
  labs(title = "Average daily emissions", y = "SO2") +  geom_boxplot(alpha = 0.7) + theme_bw() + 
  scale_fill_manual(values = c(viridis(2)[2],viridis(2)[1]),name = "Powerplants: ", labels = c("without edges","with edges")) + theme2

p4 <- ggplot(PP.summary, aes(x = PP.region, y = stdev.emissions,  fill = as.factor(has.edges))) +
  labs(title = "Standard deviation in daily emissions", y = "SO2") +  geom_boxplot(alpha = 0.7) + theme_bw() + 
  scale_fill_manual(values = c(viridis(2)[2],viridis(2)[1]),name = "Powerplants: ", labels = c("without edges","with edges")) + theme2

p5 <- ggplot(PP.summary, aes(x = PP.region, y = emissions.NAdays, fill = as.factor(has.edges))) +
  labs(title = "Number of days with missing emissions data", y = "number of days") +
  geom_boxplot(alpha = 0.7) + theme_bw() + 
  scale_fill_manual(values = c(viridis(2)[2],viridis(2)[1]),name = "Powerplants: ", labels = c("without edges","with edges")) + theme2


mylegend<-g_legend(p1)

blank <- rectGrob(gp = gpar(col = "white"))


grid.arrange(num.PP, p1 + theme(legend.position = "none"),p2,mylegend, blank, ncol = 3, 
             top = textGrob("Why do some power plants have linked monitors and others do not?", 
                            gp = gpar(fontsize = 30)), 
             layout_matrix = rbind(c(5,5,5),c(1,2,3),c(4,4,4)),
             heights = c(0.05,0.75, 0.20))

grid.arrange(p3,p4,p5,mylegend, blank, ncol = 3,  
             top = textGrob("Why do some power plants have linked monitors and others do not?",
                            gp = gpar(fontsize = 30)),
             layout_matrix = rbind(c(5,5,5),c(1,2,3),c(4,4,4)),
             heights = c(0.05,0.75, 0.20))


# p1 <- ggplot(has.edge.summary, aes(x = PP.region, y = n.powerplants, shape = as.factor(has.edges))) +
#   labs(title = "Number of powerplants", y = "Powerplants" ) + geom_point(size = pt.size) + theme_bw() + 
#   scale_shape_manual(values = c(4,17),name = "Powerplants: ", labels = c("without edges","with edges")) + theme1
# 
# 
# p2 <- ggplot(has.edge.summary, aes(x = PP.region, y = median.edges.count, shape = as.factor(has.edges))) +
#   labs(title = "Median number of edges", y = "Edges") + geom_point(size = pt.size) + theme_bw() + 
#   scale_shape_manual(values = c(4,17),name = "Powerplants: ", labels = c("without edges","with edges")) + theme2
# 
# p3 <- ggplot(has.edge.summary, aes(x = PP.region, y = median.edges.possible, shape = as.factor(has.edges))) +
#   labs(title = "Median number of possible edges", y = "Possible edges") +  geom_point(size = pt.size) + theme_bw() + 
#   scale_shape_manual(values = c(4,17),name = "Powerplants: ", labels = c("without edges","with edges")) + theme2
# 
# p4 <- ggplot(has.edge.summary, aes(x = PP.region, y = avgemissions, shape = as.factor(has.edges))) +
#   labs(title = "Average emissions", y = "SO2") +  geom_point(size = pt.size) + theme_bw() + 
#   scale_shape_manual(values = c(4,17),name = "Powerplants: ", labels = c("without edges","with edges")) + theme2
# 
# p5 <- ggplot(has.edge.summary, aes(x = PP.region, y = mean.stdev,  shape = as.factor(has.edges))) +
#   labs(title = "Standard deviation in emissions", y = "SO2") +  geom_point(size = pt.size) + theme_bw() + 
#   scale_shape_manual(values = c(4,17),name = "Powerplants: ", labels = c("without edges","with edges")) + theme2
# 
# p6 <- ggplot(has.edge.summary, aes(x = PP.region, y = median.NAdays, shape = as.factor(has.edges))) +
#   labs(title = "Median number of days with missing emissions data", y = "Number of days")  + geom_point(size = pt.size) + theme_bw() + 
#   scale_shape_manual(values = c(4,17),name = "Powerplants: ", labels = c("without edges","with edges")) + theme2

# mylegend<-g_legend(p1)
# 
# grid.arrange(p1 + theme(legend.position = "none"),p2,p3,p4,p5,p6, mylegend, ncol = 3, 
#              top = textGrob("Why do some power plants have edges and others do not?", 
#                             gp = gpar(fontsize = 20)), layout_matrix = rbind(c(1,2,3),c(4,5,6),c(7,7,7)))


#dev.off()
