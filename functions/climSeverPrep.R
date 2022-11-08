###
# prep raster subsets for modules 
#
#
###



# climate data  -----------------------------------------------------------
prepClim <- function(rasters, ssps){
  ## spilts rasters into groups based on ssp value provided 
  ## rasters: all climate raster data as multilayered rasters
  ## ssps : character vector to spilt the raster data on. strings must be in the 
  ## name of the raster feature. 
  ## return: a list of multi layered rasters. 

  # filter function
  grep2 <- function(index, rasters){
    r1 <- rasters[[grep(index,names(rasters))]]
    return(r1)
  }
  
  # ordered list based on the ssps
  rasts <- lapply(ssps, grep2,rasters=rasters)
  names(rasts) <- ssps
  return(rasts)
}

prepClim2 <- function(rasters, ssps){
  ## spilts rasters into groups based on ssp value provided 
  ## rasters: all climate raster data as multilayered rasters
  ## ssps : character vector to spilt the raster data on. strings must be in the 
  ## name of the raster feature. 
  ## return: a list of multi layered rasters. 
  
  # filter function
  grep2 <- function(index, rasters){
    r1 <- rasters[[grep(index,names(rasters))]]
    return(r1)
  }
  
  # ordered list based on the ssps
  rasts <- lapply(ssps, grep2,rasters=rasters)
  names(rasts) <- ssps
  return(rasts)
}

