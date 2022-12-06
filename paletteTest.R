# test creating palettes for map and legends

# separate rasters by variable
pr_rast <- clim_new[[grep("pr", names(clim_new))]]

#get min and max pr values
min <- min(values(pr_rast), na.rm = TRUE)
max <- max(values(pr_rast), na.rm = TRUE)


#create palette
pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(pr_rast),
                    na.color = "transparent")


# test map
leaflet(options = leafletOptions(minZoom = 4)) %>%
  #set zoom levels
  setView(lng = 37.826119933082545
          , lat = 0.3347526538983459
          , zoom = 6) %>%
  # add z levels ------------------------------------------------------------
addMapPane("histData", zIndex = 407) %>%
  addMapPane("data", zIndex = 408) %>%
  addMapPane("county", zIndex = 409) %>%
  # tile providers ----------------------------------------------------------
addProviderTiles("Stamen.Toner", group = "Light") %>%
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  leaflet.extras::addResetMapButton() %>%
  # add ssp raster features -----------------------------------------------------
addRasterImage(
  raster(allRasters_new$`126`[[1]]),
  colors = pals[[3]],
  group = "Data",
  opacity = 0.9
) %>%
  # add historic raster -----------------------------------------------------
addRasterImage(raster(tmin_rast[[1]]),
               #colors = pal0(),
               group = "histData",
               opacity = 0.9) %>%
  # add county features -----------------------------------------------------
addPolygons(
  data = county,
  fillColor = "",
  fillOpacity = 0,
  color = "black",
  layerId = ~ ADMIN1,
  weight = 1.5,
  group = "County"
) %>%
  # add control groups ------------------------------------------------------
addLayersControl(
  baseGroups = c("OpenStreetMap", "Light"),
  overlayGroups = c("histData",
                    "Data",
                    "County"),
  position = "topleft",
  options = layersControlOptions(collapsed = TRUE)
) %>%
  # add legend --------------------------------------------------------------
###
# Need to figure out assigning a color palette before I can create a legend
###

addLegend(
  "bottomright",
  pal = pals[[3]],
  values = values(tmin_rast[[1]]),
  title = "Average Annual Temperature (Celcius)",
  #labels = c("Low Change", "", "", "", "High Change"),
  opacity = 1,
  layerId = "firstLegend",
  group = "Data",
  #na.label = "No Data"
  
  # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
)

generatePalettes <- function(rasters){
  
  pr_rast <- rasters[[grep("pr", names(rasters))]]
  tmin_rast <- rasters[[grep("tmin", names(rasters))]]
  tmax_rast <- rasters[[grep("tmax", names(rasters))]]
  
  pals <- vector("list", length = 3)
  names(pals) <- c("pr", "tmin", "tmax")
  
  
  #precipitation palette
  pals[[1]] <-  colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(pr_rast),
                             na.color = "transparent")
  
  pals[[2]] <-  colorNumeric(palette = "RdYlBu", values(tmin_rast),
                             na.color = "transparent")
  
  
  pals[[3]] <-  colorNumeric(palette = "RdYlBu", reverse = TRUE, values(tmax_rast),
                             na.color = "transparent")
  
  
  return(pals)
  
}
