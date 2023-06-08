###
# Stand alone script for generating content for the kenya afforestation shiny app 
# carverd@colostate.edu
# 20230104 
###
pacman::p_load(tidyr, dplyr, raster, terra, sf)

# source preprocessing functions ------------------------------------
lapply(list.files(
  path = "preprocessFunctions/",
  pattern = ".R",
  full.names = TRUE
),
source)

# read in input data  -----------------------------------------------------

## county spatial feature
county <- sf::st_read("dataToPreprocess/referenceSpatialData/KE_Admin1_pro.shp", stringsAsFactors = F)
county <- sf::st_transform(county, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# buffer of the county features used for cropping -- may need to adjust buffer dist
countyBuff <- sf::st_buffer(x = county, dist = 0.8)

## climate change datasets 
### this includes tmin_series"      "tmax_series"   "pr_series" dataframes that we are not currently using 
### not sure what the area reference is. It might be the whole country. 
climCRS <- readRDS("dataToPreprocess/climateLayers/climate_change_files.rds")

### structure will change once we have full dataset 
files <- list.files(path = "dataToPreprocess/managementLayers", 
                    full.names = TRUE)

# generate content for the climate change page  -----------------------------------------------
climateChangeInputs <- renderClimateChangeInputs(county = county,
                                                 countyBuff = countyBuff,
                                                 climateRasters = climCRS)

# generate content for the climate management page ------------------------
climateManagementInputs <- renderClimateManagementInputs(
  county = county,
  countyBuff = countyBuff,
  files = files
)


# generate palettes for all rasters -----------------------------------------------
## subset to absolute val and percent change features 
clim_abs <- climateChangeInputs$abs_rasters
clim_change <- climateChangeInputs$change_rasters
## generate specific palettes 
pal_abs <- generatePalettes(rasters = clim_abs, type = "abs")
pal_change <- generatePalettes(clim_change, type = "change")

## forest management palettes
pal_management <- genPalettes_forestCover(climateManagementInputs)

## combine all three palette sets as list object
paletteList <- list(pal_abs, pal_change, pal_management)
names(paletteList) <- c("pal_abs", "pal_change", "pal_management")


# process model validation rasters ---------------------------------------
npp_val <- nppVals(path = "dataToPreprocess/validationLayers/NPP_valid.RDS",
                   county = county,
                   countyBuff = countyBuff)

carbon_val <- carbonVals(path1 = "dataToPreprocess/validationLayers/aboveC_valid.RDS",
                         path2 = "dataToPreprocess/validationLayers/belowC_valid.RDS",
                         county = county,
                         countyBuff = countyBuff )



# output generated objects ------------------------------------------------
## climate change inputs
saveRDS(object = climateChangeInputs,
        file = "appData/climateChangeInputs.RDS")
## climate management inputs
saveRDS(object = climateManagementInputs,
        file = "appData/climateManagementInputs.RDS")
## palette objects 
saveRDS(object = paletteList,
        file = "appData/palettes.RDS")

## validation inputs npp
saveRDS(object = npp_val,
        file = "appData/validationInputs_npp.RDS")

## validation inputs carbon
saveRDS(object = carbon_val,
        file = "appData/validationInputs_carbon.RDS")


