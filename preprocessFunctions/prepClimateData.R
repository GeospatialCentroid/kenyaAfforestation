# ###
# # read in and evaluate all input datasets 
# # carverd@colostate.edu
# # 20221117 
# ###
# 
# renderInputOld <- function(){
#   
#   ### projection raster template  -- required for maintaining the cell size of the input features.
#   pro_template <- rast("data/wgs_ext_res_temp.asc")
#   ### test to see if this is needed.
#   crs(pro_template) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#   # mask features
#   mask <- rast("data/ken_mask.tif") %>%
#     terra::project(pro_template) # reprotion assing some decimal values
#   # replace 0 qith NA
#   mask[which(mask[] == 0)] <- NA
#   # replace all non NA with 1
#   mask[which(!is.na(mask[]))] <- 1
#   
#   # county spatial feature
#   county <- sf::st_read("./data/KE_Admin1_pro.shp", stringsAsFactors = F)
#   county <- st_transform(county, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#   
#   # vector of county names
#   county_names <- county$ADMIN1
#   
#   # primary dataset for  the clim
#   clim2 <- readRDS("data/temp_pr_change.rds") %>%
#     map(rast)%>%
#     map(terra::project, pro_template)%>%
#     #map(terra::mask, mask) %>% 
#     map(terra::crop, county) %>%
#     rast() # reduce to a layered raster that makes indexing easier 
# 
#   inputs <- list(rasters = clim2,
#                  countyNames = county_names,
#                  county = county)
# 
#   
#   return(inputs)
# }
# 
# renderInputs <- function(){
#   
#   ### projection raster template  -- required for maintaining the cell size of the input features.
#   pro_template <- rast("data/wgs_ext_res_temp.asc")
#   ### test to see if this is needed.
#   crs(pro_template) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#   # mask features
#   mask <- rast("data/ken_mask.tif") %>%
#     terra::project(pro_template) # reprotion assing some decimal values
#   # replace 0 qith NA
#   mask[which(mask[] == 0)] <- NA
#   # replace all non NA with 1
#   mask[which(!is.na(mask[]))] <- 1
#   
#   # county spatial feature
#   county <- sf::st_read("./data/KE_Admin1_pro.shp", stringsAsFactors = F)
#   county <- st_transform(county, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#   
#   # vector of county names
#   county_names <- county$ADMIN1
#   
#   # new climate data -- unprojected. 
#   # clim <- readRDS("data/climData_112022.rds")
#   ### projected climate data 
#   
# 
# # Updated data --------------------------------------------------  
# climCRS <- readRDS("data/climate_change_files(2).rds")
#   
#   #pull out absolute and change maps
#   clim_abs <- climCRS[["absolute_rasters"]] %>% 
#     terra::rast()%>%
#     terra::project(county)%>%
#     terra::crop(county) %>% 
#     #change to raster, terra objects don't save as .rds and need raster for leaflet anyways
#     # raster::stack()
#     # replacing with the terra wrap function 
#     terra::wrap()
# 
#   clim_change <- climCRS[["change_rasters"]] %>% 
#     terra::rast()%>%
#     terra::project(county)%>%
#     terra::crop(county) %>% 
#     #raster::stack()
#     # replacing with the terra wrap function 
#     terra::wrap()
#   
#   # generate export object 
#   inputs <- list(abs_rasters = clim_abs,
#                  change_rasters = clim_change,
#                  countyNames = county_names,
#                  county = county)
#   
#   
#   return(inputs)
# }
# 
# renderClimateChangeInputs <- function(){
#   
# }
# 
# 
# renderForestChangeInputs <- function(){
#   # new climate data -- unprojected. 
#   # clim <- readRDS("data/climData_112022.rds")
#   ### projected climate data 
#   climCRS <- readRDS("data/climate_change_files(2).rds")
#   # list of three rasterbrick, 17 layers each. 
#   rastNames <- lapply(climCRS, FUN = names) %>% unlist()
#   # 
#   clim <- climCRS %>%
#     map(rast)%>%
#     map(terra::project, county)%>%
#     map(terra::crop, county) %>%
#     # rast() # reduce to a layered raster that makes indexing easier 
#     # replacing with the terra wrap function 
#     terra::wrap()
#     ### 
#   names(clim) <- rastNames
#   
#   # generate export object 
#   inputs <- list(rasters = clim,
#                  countyNames = county_names,
#                  county = county)
# 
# }
# 
