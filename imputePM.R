imputePM <- function(monitorPM, plot.imputed = FALSE){
  require(imputeTS)
  monitorPM <- ts(monitorPM)
  first_nonNA <- min(which(!is.na(monitorPM)))
  first_nonNA <- ifelse(first_nonNA == 3, 1, first_nonNA)
  last_nonNA <-max(which(!is.na(monitorPM)))
  imp <- tryCatch({
    na.interpolation(monitorPM[first_nonNA:last_nonNA])
    }, error = function(err) monitorPM[first_nonNA:last_nonNA]
  )
  imp <- c(rep(NA, first_nonNA-1), imp, rep(NA,length(monitorPM) - last_nonNA))
  if(plot.imputed == TRUE){
    plotNA.imputations(monitorPM,imp)
  }
  return(imp)
}
