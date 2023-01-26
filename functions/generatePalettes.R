



# function for climate data -----------------------------------------------
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


#function for forest cover data 
genPalettes_forestCover <- function(data){
  
  # subset out change rasters 
  changeRasters <- data$forestChangeRasters
  
  
  doNothing <- changeRasters[grepl(pattern = "DoNothing", x = names(changeRasters))]
  noFires <-changeRasters[grepl(pattern = "NoFires", x = names(changeRasters))]
  histForest <- data$existingForest
  expForest <- data$expandedForest
  
  pals <- vector("list", length = 4)
  names(pals) <- c("nothing","fire", "hf", "ef")

  #historic forest cover palette
  pals[["hf"]]$palette <- colorNumeric(c("#edf8fb","#b2e2e2","#66c2a4","#2ca25f","#006d2c"), values(histForest),
                                       na.color = "transparent")
  
  pals[["hf"]]$title <- "Historic Forest Cover (%)"
  
  pals[["hf"]]$values <- values(histForest)
  
  #Expected base line forest cover palette
  pals[["ef"]]$palette <- colorNumeric(c("#edf8fb","#b2e2e2","#66c2a4","#2ca25f","#006d2c"), values(expForest),
                                       na.color = "transparent")
  
  pals[["ef"]]$title <- "Expected 2030 Forest Cover (%)"
  
  pals[["ef"]]$values <- values(expForest)
  
  ### these should be combined... Trouble is we are passing them in as a list of raster object, will need to reduce down to a stack I think. 
  #forest cover change - do nothing
  nothingStack <- raster::stack(doNothing)
  ###! ITS NOT CLEAR HOW palette is handling a dataframe of values... might need to collapse all values into a single list.  
  
  colorForest <-  c(colorRampPalette(colors = c("#a6611a", "#d46c02",  "white"),space = "Lab")(abs(min(values(nothingStack), na.rm = TRUE))),
                  colorRampPalette(colors = c("white", "#32CD32", "#003300"),space = "Lab")(max(values(nothingStack), na.rm = TRUE)))
  
  
  pals[["nothing"]]$palette <- colorNumeric(palette = colorForest, as.numeric(values(nothingStack)),
                                        na.color = "transparent")
  
  
  # pals[["nothing"]]$palette <- colorNumeric(c("#a6611a","#dfc27d","#f5f5f5","#66c2a4","#006d2c"), as.numeric(values(nothingStack)),
  #                                      na.color = "transparent")
  
  pals[["nothing"]]$title <- "Change in % Forest Area </br> No Management Action"
  
  pals[["nothing"]]$values <- as.numeric(values(nothingStack))
  
  #forest cover change - stop fires 
  ## no fire data to work with at the moment. 
  fireStack <- raster::stack(noFires)
  ###! ITS NOT CLEAR HOW palette is handling a dataframe of values... might need to collapse all values into a single list.  
  
  colorForest <-  c(colorRampPalette(colors = c("#a6611a", "#d46c02",  "white"),space = "Lab")(abs(min(values(fireStack), na.rm = TRUE))),
                    colorRampPalette(colors = c("white", "#32CD32", "#003300"),space = "Lab")(max(values(fireStack), na.rm = TRUE)))
  
  
  pals[["fire"]]$palette <- colorNumeric(palette = colorForest, as.numeric(values(fireStack)),
                                            na.color = "transparent")
  
  
  # pals[["nothing"]]$palette <- colorNumeric(c("#a6611a","#dfc27d","#f5f5f5","#66c2a4","#006d2c"), as.numeric(values(nothingStack)),
  #                                      na.color = "transparent")
  
  pals[["fire"]]$title <- "Change in % Forest Area </br> No Fires"
  
  pals[["fire"]]$values <- as.numeric(values(fireStack))
  
  
  
  
  
  # pals[["fire"]]$palette <- colorNumeric(c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"), values(stopFires),
  #                                    na.color = "transparent")
  
  # pals[["fire"]]$title <- "Expected Forest Cover Change Stop Fires (%)"
  
  # pals[["fire"]]$values <- values(stopFires)
  
  return(pals)
    
}
