generatePalettes <- function(rasters){
  
  pr_rast <- rasters[[grep("pr", names(rasters))]]
  tmin_rast <- rasters[[grep("tmin", names(rasters))]]
  tmax_rast <- rasters[[grep("tmax", names(rasters))]]
  
  pals <- vector("list", length = 3)
  names(pals) <- c("pr", "tmin", "tmax")
  
  
  #precipitation palette
  pals[[1]] <-  colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(pr_rast),
                             na.color = "transparent")
  
  pals[[2]] <-  colorNumeric(palette = "RdYlBu", reverse = TRUE, values(tmin_rast),
                             na.color = "transparent")
  
  
  pals[[3]] <-  colorNumeric(palette = "RdYlBu", reverse = TRUE, values(tmax_rast),
                             na.color = "transparent")
  
  
  return(pals)
  
}
