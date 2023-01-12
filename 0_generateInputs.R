###
# Stand alone script for generating content for the kenya afforestation shiny app 
# carverd@colostate.edu
# 20230104 
###
pacman::p_load(tidyr, dplyr, raster,terra)

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
  

  # processing function, mostly for lappy -----------------------------------
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
      raster::brick()
    return(raster)
  }
  

  # Gather funtion for county level data ------------------------------------
  gatherCounty <- function(data){
    data <- data %>%
      tidyr::gather(key = Year, value = Change, -County, -areas )
    names(data) <- c("County", "Areas","Year", "Change")
    return(data)
  }
  
  
  
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
files <- list.files(path = "data/managementLayers", 
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
