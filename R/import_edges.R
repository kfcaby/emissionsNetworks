#function imports the output from fitDailyPMmodels and adds additional data for follow
#on analyses
import_edges <- function(unit.type = "monitor", season = "summer", PM.type = "raw"){
  #get edge output from fitDailyPMmodels.R
  folder <- paste(unit.type,"_networks/output_",season,"_",PM.type, sep = "") 
  output_files <- list.files(folder,
                             pattern = "^edges[0-9]+\\.csv$",
                             full.names=TRUE)
  print(length(output_files))
  ## read each file and append them
  edges <- do.call(rbind, lapply(output_files, fread))[ ,V1 := NULL]
  
  print("Finished Uploading.  Adding additional edge info...")
  
  #Calculate 
  edges$p.value_adj <- p.adjust(edges$p.value, method = "BH")
  edges$edge <- ifelse(edges$p.value_adj <= 0.05 & edges$gams.coeff > 0, 1, 0)
  
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
  

  
