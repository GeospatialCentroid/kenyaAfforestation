generatePalettes <- function(rasters){
  
  pr_rast <- rasters[[grep("pr", names(rasters))]]
  tmin_rast <- rasters[[grep("tmin", names(rasters))]]
  tmax_rast <- rasters[[grep("tmax", names(rasters))]]
  
  pals <- vector("list", length = 3)
  names(pals) <- c("pr", "tmin", "tmax")
  
  
  #precipitation palette
  pals[["pr"]]$palette <-  colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(pr_rast),
                             na.color = "transparent")
  
  pals[["pr"]]$title <- "Precipitation (mm)"
  
  pals[["pr"]]$values <- values(pr_rast)
  
  #tmin palette
  pals[["tmin"]]$palette <-  colorNumeric(palette = "RdYlBu", reverse = TRUE, values(tmin_rast),
                             na.color = "transparent")
  
  pals[["tmin"]]$title <- paste("Minimum Temperature C", intToUtf8(176))
  
  pals[["tmin"]]$values <- values(tmin_rast)
  
  #tmax palette
  pals[["tmax"]]$palette <-  colorNumeric(palette = "RdYlBu", reverse = TRUE, values(tmax_rast),
                             na.color = "transparent")
  
  pals[["tmax"]]$title <- paste("Maximum Temperature C", intToUtf8(176))
  
  pals[["tmax"]]$values <- values(tmax_rast)
  
  
  return(pals)
  
}
