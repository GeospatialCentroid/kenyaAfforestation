# generate legend values --------------------------------------------------
### might be easier to declare of of these before hand and index them similar
### to how the rasters are being brought together.

# climate data  -----------------------------------------------------------
getPallette <- function(rasters){
  palList <- vector("list", length(rasters))
  for(i in seq_along(rasters)){
    rasts <- rasters[[i]]
    names <- names(rasts)
    vals <- vector("list", nlyr(rasts))
    for(j in 1:nlyr(rasts)){
      r1 <- rasts[[j]]
      minval <-  r1@ptr@.xData$range_min
      maxval <-  r1@ptr@.xData$range_max
      dom<- c(minval, maxval)
      val<- seq(minval,maxval)
      colorPal <-  c(colorRampPalette(colors = c("#330000", "white"),space = "Lab")(abs(minval)),
                     colorRampPalette(colors = c("white", "#003300"),space = "Lab")(abs(maxval)))
      
      pal <- colorNumeric(colorPal, domain = dom)
 
      vals[[j]] <- pal
      
      # Color platte think is odd,
      # pal <- ifelse(
      #   test = minval < 0 & maxval > 0,
      #   yes = colorNumeric(colorPal, domain = dom),
      #   ## something wierd is going on here with the domain argument. Need to look at this in the original code base 
      #   no = colorNumeric(colorRampPalette(colors = c("white", "#003300"),space = "Lab"),c(abs(maxval), dom))
    }
    palList[[i]] <- setNames(object = vals, nm = names) 
  }
  return(palList)
}
