#function imports the output from fitDailyPMmodels and adds additional data for follow
#on analyses
import_edges <- function(season = "summer", PM.type = "raw"){
  #get edge output from fitDailyPMmodels.R
  folder <- paste("output_",season,"_",PM.type, sep = "") 
  output_files <- list.files(folder,
                             pattern = "^edges2005_zipcode[0-9]+\\.csv$",
                             full.names=TRUE)
  print(length(output_files))
  ## read each file and append them
  edges <- do.call(rbind, lapply(output_files, fread))[ ,V1 := NULL]
  
  print("Finished Uploading.  Adding additional edge info...")
  
  #Calculate 
  edges$p.value_adj <- p.adjust(edges$p.value, method = "BH")
  edges$edge <- ifelse(edges$p.value_adj <= 0.05 & edges$gams.coeff > 0, 1, 0)
  
  #add monitor state and region
  zipcode_locations <- fread("../data/zipcode.locations.csv")[ ,V1:=NULL]
  setkey(zipcode_locations,ID)
  setkey(edges,Monitor, PP)
  edges$Monitor.state <- zipcode_locations[edges$Monitor,]$zip.state
  edges$Monitor.region <- getRegion(edges$Monitor.state)
  
  #only use these regions
  edges <- subset(edges, Monitor.region %in% c("Northeast","IndustrialMidwest","Southeast")) 
  
  #add powerplant state and region
  facility.state <- unique(fread("../data/unitlevel.csv",
                                 select = c("Facility.ID..ORISPL.","State.x")))
  colnames(facility.state) <- c("PP","State")
  facility.state$PP <- paste("PP",facility.state$PP,sep = "")
  setkey(facility.state, PP)
  setkey(edges,PP,Monitor)
  edges$PP.state <- facility.state[edges$PP,]$State
  edges$PP.region <- getRegion(edges$PP.state)
  
  inmap <- fread(file = "../data/inmap_zipcodePM.csv")
  zips <- inmap$V1
  inmap <- as.matrix(inmap[ ,-1])
  rownames(inmap) <- zips
  inmap_long <- data.table(melt(inmap))
  colnames(inmap_long) <- c("Monitor", "PP", "inmapPM")
  setkey(inmap_long, Monitor,PP)
  setkey(edges, Monitor, PP)
  edges <- inmap_long[edges]
  
  #add direction to edge data
  angle_breaks <- seq(22.5,337.5, by = 45)
  direction_ints <- data.table(PP = edges$PP, Monitor = edges$Monitor, interval = findInterval(edges$bearing, angle_breaks), key = "interval")
  direction.table <- data.table(interval = 0:8, direction = c("N","NE","E","SE", "S","SW","W","NW","N"), key = "interval")
  directions <- direction_ints[direction.table]
  setkey(directions, PP, Monitor)
  setkey(edges, PP, Monitor)
  edges <- merge(edges,directions[ ,.(PP,Monitor,direction)])
  
  return(edges)
}