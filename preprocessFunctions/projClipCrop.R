###
# raster processing function
# 20230217
###


# processing function
projClipCrop <- function(raster, county, countyBuff){
  #reproject
  crs(raster) <- "+proj=sinu +lon_0=36.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  
  # reprojects rasters 
  raster <- raster %>% 
    terra::rast()%>%
    terra::project(county)%>%
    terra::crop(countyBuff) %>% 
    terra::mask(countyBuff)%>%
    #this has to be 'brick', otherwise 'raster()' only returns the first layer
    raster::brick() %>% 
    # project to leaflet crs to avoid need for reprojecting
    raster::projectRaster(crs = 3857)
  
  return(raster)
}