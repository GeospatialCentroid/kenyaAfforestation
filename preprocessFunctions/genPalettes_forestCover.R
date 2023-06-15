###
# Generate palette object for displaying change in forest cover on map elements map2 module
# 20230217
###


#function for forest cover data 
genPalettes_forestCover <- function(data){
  
  # subset out change rasters 
  changeRasters <- data$forestChangeRasters
  
  
  doNothing <- changeRasters[grepl(pattern = "DoNothing", x = names(changeRasters))]
  less_fire <-changeRasters[grepl(pattern = "NoFires", x = names(changeRasters))]
  more_fire <-changeRasters[grepl(pattern = "DoubleFires", x = names(changeRasters))]

  histForest <- data$existingForest
  expForest <- data$expandedForest
  
  pals <- vector("list", length = 5)
  names(pals) <- c("nothing","less_fire","more_fire", "hf", "ef")
  
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
  
  colorForest <-  c(colorRampPalette(colors = c("#a6611a", "#d46c02",  "white"),space = "Lab")(abs(min(values(nothingStack), na.rm = TRUE))),
                    colorRampPalette(colors = c("white", "#32CD32", "#003300"),space = "Lab")(max(values(nothingStack), na.rm = TRUE)))
  
  
  pals[["nothing"]]$palette <- colorNumeric(palette = colorForest, as.numeric(values(nothingStack)),
                                            na.color = "transparent")
  
  
  pals[["nothing"]]$title <- "Change in % Forest Area </br> No Management Action"
  
  pals[["nothing"]]$values <- as.numeric(values(nothingStack))
  
  #forest cover change - stop fires 
  fireStack <- raster::stack(less_fire)
  
  colorForest <-  c(colorRampPalette(colors = c("#a6611a", "#d46c02",  "white"),space = "Lab")(abs(min(values(fireStack), na.rm = TRUE))),
                    colorRampPalette(colors = c("white", "#32CD32", "#003300"),space = "Lab")(max(values(fireStack), na.rm = TRUE)))
  
  pals[["less_fire"]]$palette <- colorNumeric(palette = colorForest, as.numeric(values(fireStack)),
                                         na.color = "transparent")
  
  pals[["less_fire"]]$title <- "Change in % Forest Area </br> Decrease Fire"
  
  pals[["less_fire"]]$values <- as.numeric(values(fireStack))
  
  #forest cover change - no grazing
  doubleFireStack <- raster::stack(more_fire)
  
  colorForest <-  c(colorRampPalette(colors = c("#a6611a", "#d46c02",  "white"),space = "Lab")(abs(min(values(doubleFireStack), na.rm = TRUE))),
                    colorRampPalette(colors = c("white", "#32CD32", "#003300"),space = "Lab")(max(values(doubleFireStack), na.rm = TRUE)))
  
  pals[["more_fire"]]$palette <- colorNumeric(palette = colorForest, as.numeric(values(doubleFireStack)),
                                         na.color = "transparent")
  
  pals[["more_fire"]]$title <- "Change in % Forest Area </br> Increase fire"
  
  pals[["more_fire"]]$values <- as.numeric(values(doubleFireStack))
  
  
  return(pals)
  
}
