###
# Stand alone script for generating content for the kenya afforestation shiny app 
# carverd@colostate.edu
# 20230104 
###

renderClimateChangeInputs <- function(county,countyBuff, climateRasters){
  # vector of county names
  county_names <- county$ADMIN1
  
  #pull out absolute and change maps
  clim_abs <- climateRasters[["absolute_rasters"]] %>% 
    terra::rast()%>%
    terra::project(county)%>%
    terra::crop(countyBuff) %>%
    terra::mask(countyBuff)%>% 
    # change to raster, terra objects don't save as .rds and need raster for leaflet anyways
    raster::stack()
  
  
  clim_change <- climateRasters[["change_rasters"]] %>% 
    terra::rast()%>%
    terra::project(county)%>%
    terra::crop(countyBuff) %>% 
    terra::mask(countyBuff)%>%
    raster::stack()
  
  # generate export object 
  inputs <- list(abs_rasters = clim_abs,
                 change_rasters = clim_change,
                 countyNames = county_names,
                 county = county)
  
  return(inputs)
}

renderClimateManagementInputs <- function(county, countyBuff, files){
  # rasters and df 
  doNothing <- readRDS(files[4])
  stopFires<- readRDS(files[5])
  names(stopFires$areas_change_df) <- names(doNothing$areas_change_df)
  # reproject and crop rasters 
  doNothing$forest_change_rasters <- doNothing$forest_change_rasters %>%
    terra::rast()%>%
    terra::project(county)%>%
    terra::crop(countyBuff) %>% 
    terra::mask(countyBuff)%>%
    raster::stack()
  
  stopFires$forest_change_rasters <- stopFires$forest_change_rasters %>%
    terra::rast()%>%
    terra::project(county)%>%
    terra::crop(countyBuff) %>% 
    terra::mask(countyBuff)%>%
    raster::stack()
  
  # single file of historic forest cover 
  existingForest <- raster(files[1])
  crs(existingForest) <- "+proj=sinu +lon_0=36.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  existingForest <- existingForest %>%
    terra::rast()%>%
    terra::project(county)%>%
    terra::crop(countyBuff) %>% 
    terra::mask(countyBuff)%>%
    raster::raster()
  
  # single file with project 2030 forest cover 
  expandedForest <- raster(files[2])
  crs(expandedForest) <- "+proj=sinu +lon_0=36.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  expandedForest <- expandedForest %>%
    terra::rast()%>%
    terra::project(county)%>%
    terra::crop(countyBuff) %>% 
    terra::mask(countyBuff)%>%
    raster::raster()
  
  # generate export object 
  inputs <- list(doNothing = doNothing,
                 stopFires = stopFires, 
                 expandedForest = expandedForest,
                 existingForest = existingForest,
                 county = county)
  return(inputs)
}

# read in input data  -----------------------------------------------------
# ### projection raster template  -- required for maintaining the cell size of the input features.
# pro_template <- rast("data/referenceSpatialData/wgs_ext_res_temp.asc")
# ### test to see if this is needed.
# crs(pro_template) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# # mask features
# mask <- rast("data/referenceSpatialData/ken_mask.tif") %>%
#   terra::project(pro_template) # reprotion assing some decimal values

# county spatial feature
county <- sf::st_read("data/referenceSpatialData/KE_Admin1_pro.shp", stringsAsFactors = F)
county <- st_transform(county, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# buffer of the county features used for cropping -- may need to adjust buffer dist
countyBuff <- sf::st_buffer(x = county, dist = 0.8)

# climate change datasets 
### this includes tmin_series"      "tmax_series"   "pr_series" dataframes that we are not currently using 
### not sure what the area reference is. It might be the whole country. 
climCRS <- readRDS("data/climate_change_files.rds")

##** note: content to be added based on previous feedback 
### evaluate the implementation of the second map page 

### structure will change once we have full dataset 
files <- list.files(path = "data/Forest_Cover_layers_for_SSP126", 
                    full.names = TRUE)





# generate content for the climate change page  -----------------------------------------------
climateChangeInputs <- renderClimateChangeInputs(county = county, countyBuff = countyBuff, climateRasters = climCRS)
saveRDS(object = climateChangeInputs,
        file = "data/climateChangeInputs.RDS")

# generate content for the climate management page ------------------------
climateManagementInputs <- renderClimateManagementInputs(
  county = county,
  countyBuff = countyBuff,
  files = files
)
saveRDS(object = climateManagementInputs,
        file = "data/climateManagementInputs.RDS")