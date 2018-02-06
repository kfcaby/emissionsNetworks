plotLagProbs <- function(edges, region){
  probs <- edges[ receptor.region %in% region , 
                  list(sum(edge == 1,na.rm = TRUE)/sum(!is.na(edge))), 
                  by = "distance_cat"]
  probs <- probs[complete.cases(probs),]
  setkey(probs,distance_cat)
  par(mar = c(5.1,4.5,4.1,2.1))
  barplot(height = probs$V1, space = 0, main = paste(region,": Edge Probability by Distance",sep = ""),
          xlab = "Distance", ylab = "Edge Probability", cex.lab = 2, cex.main = 2, ylim = c(0,0.40), cex.lab = 2, cex.axis = 2)
  axis(1, labels = c("0-250km", "250-500km", "500-750km", "750-1000km"), at = 1:4 - 0.5, cex.lab = 2, cex.axis = 2)
}