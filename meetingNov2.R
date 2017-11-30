rm(list = ls())

library(data.table)
library(imputeTS)
library(WGCNA)

source(file = "R/functions_emissions_networks.R")
load(file = "PM.imputed.RData")
edges <- fread(file = "edges2005.csv")[ , V1 := NULL]
edges_PMimputed <- fread(file = "edges2005_PMimputed.csv")[ , V1 := NULL]
edges_PMlowfreq <- fread(file = "edges2005_PMimputed_lowfreq.csv")[ , V1 := NULL]
setkey(edges,PP)

####DISCUSS AGENDA

#Back to basics, here is what we are doing for each monitor/PP pair
model <- get_gams_model("PP1733", "M26163-0016", emissions, PM, M_locations, PP_locations, 
               start.day = "06-01", end.day = "08-31", year = 2005, return.summary = TRUE, 
               return.plots = TRUE, wind.speed = 13, k1 = 5) 

#But, in 2005 many of the monitors look like this:
model <- get_gams_model("PP1733", "M26163-0038", emissions, PM, M_locations, PP_locations, 
                        start.day = "06-01", end.day = "08-31", year = 2005, return.summary = TRUE, 
                        return.plots = TRUE, wind.speed = 13, k1 = 5) 

# This is what it looks like after running on all
# Discuss comp time
head(edges, n = 10)

#Plots of the edges
plotEmissionsNetwork(edges, plot.diagnostics = FALSE, main = "Network plot (summer 2005)")
edge_analysis(edges)

#Two issues:
# 1. PM observations only recorded every 3 days for most monitors
# 2. Correcting for multiple comparisons 
#hist(edges$gams.coeff, breaks = 100)
hist(edges$p.value, main = "Raw p-values")
hist(edges$p.value_adj, main = "Adjusted p-values")


#Ok, so let's try to impute some of the missing PM

#Imputed PM examples: my imputation method is really boneheaded...need to redo
plotNA.imputations(PM["M26163-0016",883:974], PM.imputed["M26163-0016",883:974], 
                   ylab = "PM", xlab = "Index", main = "Monitor: M26163-0016", ylim = c(0,60))
#not so good for monitors like this:
plotNA.imputations(PM["M26163-0038",883:974], PM.imputed["M26163-0038",883:974], 
                   ylab = "PM", xlab = "Index", main = "Monitor: M26163-0038", ylim = c(0,60))

#Plots of edges using imputed PM
plotEmissionsNetwork(edges_PMimputed, plot.diagnostics = FALSE, 
                     main = "Network plot (summer 2005) - Imputed PM")
edge_analysis(edges_PMimputed)

#OK, now let's try spatial decomposition
# Performed wavelet decomposition daily using Antonelli et al 

#Plots of low frequency PM, average over the period 
dft <- par("mar")
par(mfrow = c(2,1), mar = rep(1,4))
plotEmissionsNetwork(edges, exposure.type = "continuous", exposure.var = "avgPM", 
                     plot.diagnostics = FALSE, plot.edges = FALSE, main = "PM (avg over period)")
plotEmissionsNetwork(edges_PMlowfreq, exposure.type = "continuous", exposure.var = "avgPM", 
                     plot.diagnostics = FALSE, plot.edges = FALSE, main = "low freq PM (avg over period)")
par(mfrow = c(1,1), mar = dft)


#Plot of the edges using low freq and imputed PM
#THIS IS COOL
plotEmissionsNetwork(edges_PMlowfreq, plot.diagnostics = TRUE, 
                     main = "Network plot (summer 2005) - PM imputed and decomposed")
edge_analysis(edges_PMlowfreq)

#NOTE: wavelet decomposition does funny things on boundaries

#Improvements
# 1. FIX imputation method for monitors with obs every 3rd day;
#    try imputing just over the time period of interest and not all 13 years
# 2. try fitting entire year with more knots
# 3. scale up and perform on QD's daily data (might help with the missingness issues)


#TRANSITION

#Now, let's try comparing to Christine/Lucas's rinmap output
# The point of this discussion is more about 
# the framework then the specific results at this point.

#Plot of inmap ~ continuous and binary
dft <- par("mar")
par(mfrow = c(2,1), mar = rep(1,4))
plotEmissionsNetwork(edges_PMimputed, exposure.type = "continuous", exposure.var = "inmapPM", 
                     plot.diagnostics = FALSE, plot.edges = FALSE,
                     main = "InMAP (2005)")
plotEmissionsNetwork(edges_PMimputed, exposure.type = "binary", exposure.var = "inmapPM",
                     exposure.binary.cutoff = 0.70,
                     plot.diagnostics = FALSE, plot.edges = FALSE,
                     main = "")
par(mfrow = c(1,1), mar = dft)

# Now, here is our output, metric is sum of gams.coeff
dft <- par("mar")
par(mfrow = c(2,1), mar = rep(1,4))
plotEmissionsNetwork(edges_PMlowfreq, exposure.type = "continuous", exposure.var = "gams.coeff", 
                     plot.diagnostics = FALSE, plot.edges = FALSE,
                     main = "Sum of gams coefficients")
plotEmissionsNetwork(edges_PMlowfreq, exposure.type = "binary", exposure.var = "gams.coeff",
                     exposure.binary.cutoff = 0.70,
                     plot.diagnostics = FALSE, plot.edges = FALSE,
                     main = "")
par(mfrow = c(1,1), mar = dft)


# Now, the task becomes quantifying the similarity between our gams "network" and the inmap "network".

# This workflow motivated by gene co-expression networks.

#Compare monitor ranks for total exposure by each metric
setkey(edges_PMimputed, Monitor)
gams.monitor <- edges_PMimputed[ , sum(gams.coeff, na.rm = TRUE), by = "Monitor"]$V1
inmap.monitor <- edges_PMimputed[ , sum(inmapPM, na.rm = TRUE), by = "Monitor"]$V1
verboseScatterplot(rank(gams.monitor),rank(inmap.monitor), xlab="Ranked Exposure GAMS Coeff", 
                   ylab="Ranked Exposure InMAP")

#Compare connectivity (basically a similarity in their adjacency matrices)
#create adjacency matrix
gams_matrix <- acast(edges_PMimputed, Monitor ~ PP, value.var = "gams.coeff")
inmap_matrix <- acast(edges_PMimputed, Monitor ~ PP, value.var = "inmapPM")

softPower = 10
rankConn.gams = rank(softConnectivity(t(gams_matrix),type="signed",power=softPower))
rankConn.inmap = rank(softConnectivity(t(inmap_matrix),type="signed",power=softPower))
verboseScatterplot(rankConn.gams,rankConn.inmap, xlab="Ranked Connectivity GAMS", 
                   ylab="Ranked Connectivity InMAP")

