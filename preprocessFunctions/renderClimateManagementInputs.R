###
# Generates spatial features to be used in the map2 module
# 20230217
###

renderClimateManagementInputs <- function(county, countyBuff, files){

  # create stand alone rasters 
  f1 <- files[grepl(pattern = ".tif" , x = files)]
  
  existingForest <- raster(f1[grepl(pattern = "existing", f1)])
  crs(existingForest) <- "+proj=sinu +lon_0=36.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  existingForest <- projClipCrop(raster = existingForest, county = county, countyBuff = countyBuff)
  
  
  
  # single file with project 2030 forest cover 
  expandedForest <- raster(f1[grepl(pattern = "expanded", f1)])
  expandedForest <- projClipCrop(raster = expandedForest, county = county, countyBuff = countyBuff)
  
  
  
  # list containers for combining 
  rasters <- list()
  areaCountry <- list()
  areaCounty <- list()
  listNames <- c()
  
  # grab all .rds files 
  f2 <- files[grepl(pattern = ".rds" , x = files)]
  
  for(i in seq_along(f2)){
    # read in objects
    r1 <- readRDS(f2[i])
    # gather name from the file name 
    n1 <- stringr::str_split(f2[i], pattern = "/") %>% unlist()
    name <- substr(x = n1[3], start = 1, stop = nchar(n1[3])-4)
    listNames[i] <- name
    # define list element
    rasters[[i]] <- r1$forest_change_rasters
    areaCountry[[i]] <- r1$areas_change_df 
    areaCounty[[i]] <- r1$county_change_df
    
  }
  #name list objects 
  names(rasters) <- listNames
  names(areaCountry) <- listNames
  names(areaCounty) <- listNames
  
  
  
  # restructure the county data ---------------------------------------------
  areaCounty <- lapply(X = areaCounty, FUN = gatherCounty)
  
  # reprojects rasters 
  rasters <- lapply(X = rasters, FUN = projClipCrop, county = county, countyBuff = countyBuff)
  
  # generate export object 
  inputs <- list(areaCountry = areaCountry,
                 areaCounty = areaCounty,
                 forestChangeRasters = rasters, 
                 expandedForest = expandedForest,
                 existingForest = existingForest)
  return(inputs)
}
