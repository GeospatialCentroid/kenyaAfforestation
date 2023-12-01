###
# Generate the spatial features for map1 modules --- some features are also used in 
# map 2 
# 20230217
###


renderClimateChangeInputs <- function(county, countyBuff, climateRasters){
  # vector of county names
  county_names <- county$ADMIN1
  
  #pull out absolute and change maps
  clim_abs <- climateRasters[["absolute_rasters"]] %>% 
    terra::rast()|>
    terra::project(county)|>
    terra::crop(countyBuff) |>
    terra::mask(countyBuff)|> 
    terra::project('EPSG:3857')|>
    terra::wrap()
  
  
  clim_change <- climateRasters[["change_rasters"]] |> 
    terra::rast()|>
    terra::project(county)|>
    terra::crop(countyBuff) |> 
    terra::mask(countyBuff)|>
    terra::project('EPSG:3857')|>
    terra::wrap()
  
    # raster::stack() %>% 
    # # project to leaflet crs to avoid need for reprojecting
    # raster::projectRaster(crs = 3857)
    
  
  # generate export object 
  inputs <- list(abs_rasters = clim_abs,
                 change_rasters = clim_change,
                 countyNames = county_names,
                 county = county)
  
  return(inputs)
}
