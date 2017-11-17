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
  
  #add inmap
  inmap.file <- paste("data/inmap_",unit.type,"PM.csv", sep = "")
  inmap <- fread(inmap.file)[ ,V1 := NULL]
  setkey(inmap, PP, Monitor)
  setkey(edges, PP, Monitor)
  edges <- inmap[edges]
    
  return(edges)
}
  

  
