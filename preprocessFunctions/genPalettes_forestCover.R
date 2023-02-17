###
# Generate palette object for displaying change in forest cover on map elements map2 module
# 20230217
###


#function for forest cover data 
genPalettes_forestCover <- function(data){
  
  # subset out change rasters 
  changeRasters <- data$forestChangeRasters
  
  
  doNothing <- changeRasters[grepl(pattern = "DoNothing", x = names(changeRasters))]
  noFires <-changeRasters[grepl(pattern = "NoFires", x = names(changeRasters))]
  noGraze <-changeRasters[grepl(pattern = "NoGrazing", x = names(changeRasters))]

  histForest <- data$existingForest
  expForest <- data$expandedForest
  
  pals <- vector("list", length = 5)
  names(pals) <- c("nothing","fire","graze", "hf", "ef")
  
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
  fireStack <- raster::stack(noFires)
  
  colorForest <-  c(colorRampPalette(colors = c("#a6611a", "#d46c02",  "white"),space = "Lab")(abs(min(values(fireStack), na.rm = TRUE))),
                    colorRampPalette(colors = c("white", "#32CD32", "#003300"),space = "Lab")(max(values(fireStack), na.rm = TRUE)))
  
  pals[["fire"]]$palette <- colorNumeric(palette = colorForest, as.numeric(values(fireStack)),
                                         na.color = "transparent")
  
  pals[["fire"]]$title <- "Change in % Forest Area </br> No Fires"
  
  pals[["fire"]]$values <- as.numeric(values(fireStack))
  
  #forest cover change - no grazing
  grazeStack <- raster::stack(noGraze)
  
  colorForest <-  c(colorRampPalette(colors = c("#a6611a", "#d46c02",  "white"),space = "Lab")(abs(min(values(grazeStack), na.rm = TRUE))),
                    colorRampPalette(colors = c("white", "#32CD32", "#003300"),space = "Lab")(max(values(grazeStack), na.rm = TRUE)))
  
  pals[["graze"]]$palette <- colorNumeric(palette = colorForest, as.numeric(values(grazeStack)),
                                         na.color = "transparent")
  
  pals[["graze"]]$title <- "Change in % Forest Area </br> No grazing"
  
  pals[["graze"]]$values <- as.numeric(values(grazeStack))
  
  
  return(pals)
  
}
