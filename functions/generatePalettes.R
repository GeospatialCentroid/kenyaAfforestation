generatePalettes <- function(rasters, type){
  
  pr_rast <- rasters[[grep("pr", names(rasters))]]
  tmin_rast <- rasters[[grep("tmin", names(rasters))]]
  tmax_rast <- rasters[[grep("tmax", names(rasters))]]
  
  pals <- vector("list", length = 3)
  names(pals) <- c("pr", "tmin", "tmax")
  
  if (type == "abs"){
    #precipitation palette
    pals[["pr"]]$palette <-  colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(pr_rast),
                                          na.color = "transparent")
    
    pals[["pr"]]$title <- "Mean Monthly </br> Precipitation (mm)"
    
    pals[["pr"]]$values <- values(pr_rast)
    
    #tmin palette
    pals[["tmin"]]$palette <-  colorNumeric(palette = "RdYlBu", reverse = TRUE, values(tmin_rast),
                                            na.color = "transparent")
    
    pals[["tmin"]]$title <- paste("Mean Monthly </br> Minimum Temperature C", intToUtf8(176))
    
    pals[["tmin"]]$values <- values(tmin_rast)
    
    #tmax palette
    pals[["tmax"]]$palette <-  colorNumeric(palette = "RdYlBu", reverse = TRUE, values(tmax_rast),
                                            na.color = "transparent")
    
    pals[["tmax"]]$title <- paste("Mean Monthly </br> Maximum Temperature C", intToUtf8(176))
    
    pals[["tmax"]]$values <- values(tmax_rast)
    
    
  } else {
    #if type == change
    #precipitation palette (no negative percent, so continuous palette)
    
    colorPr <-  c(colorRampPalette(colors = c("#e86413", "#faf8d4"),space = "Lab")(abs(min(values(pr_rast), na.rm = TRUE))),
                    colorRampPalette(colors = c("#faf8d4", "#042163"),space = "Lab")(max(values(pr_rast), na.rm = TRUE)))
    
    
    pals[["pr"]]$palette <-  colorNumeric(palette = colorPr, values(pr_rast),
                                          na.color = "transparent")
    
    pals[["pr"]]$title <- "Percent Change in </br> Precipitation (mm)"
    
    pals[["pr"]]$values <- values(pr_rast)
    
    #temperature has - and + values, so create diverging palette

    #tmin palette
    
    colorTmin <-  c(colorRampPalette(colors = c("#330000", "white"),space = "Lab")(abs(min(values(tmin_rast), na.rm = TRUE))),
                   colorRampPalette(colors = c("white", "#003300"),space = "Lab")(max(values(tmin_rast), na.rm = TRUE)))
    
    pals[["tmin"]]$palette <-  colorNumeric(palette = colorTmin, values(tmin_rast),
                                            na.color = "transparent")
    
    pals[["tmin"]]$title <- paste("Percent change in </br> Minimum Temperature C", intToUtf8(176))
    
    pals[["tmin"]]$values <- values(tmin_rast)
    
    #tmax palette
    
    colorTmax <-  c(colorRampPalette(colors = c("#330000", "white"),space = "Lab")(abs(min(values(tmax_rast), na.rm = TRUE))),
                    colorRampPalette(colors = c("white", "#003300"),space = "Lab")(max(values(tmax_rast), na.rm = TRUE)))
    
  
    pals[["tmax"]]$palette <-  colorNumeric(palette = colorTmax, values(tmax_rast),
                                            na.color = "transparent")
    
    pals[["tmax"]]$title <- paste("Percent Change in </br> Maximum Temperature C", intToUtf8(176))
    
    pals[["tmax"]]$values <- values(tmax_rast)
    
    
    
  }
  
  
  return(pals)
  
}
