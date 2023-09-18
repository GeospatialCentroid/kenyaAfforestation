selectedCounty <- county[1]
processedRasters <- clim_abs



#' renderCountyAverages
#'
#' @param selectedCounty individual county, used to pass into the map function
#' @param processedRasters raster brick for a
#'
#' @return
#' @export
#'
#' @examples
renderCountyAverages = function(selectedCounty,counties, processedRasters){
  # crop the rasters to the county bbox
  ## transform to crs and vect object 
  c1 <- counties %>% 
    filter(ADMIN1 == selectedCounty) %>%
    sf::st_transform(crs = 3857)%>%
    vect()
  # convert from raster to rast
  r1 <- rast(processedRasters)
  # crop and mask 
  r2 <- terra::crop(x = r1, y = c1, mask = TRUE)
  # list all values within each specific layer 
  vals <- values(r2)
  # get the mean of each column and organize into the dataframe of interest
  df <- colMeans(vals, na.rm =TRUE) %>%
    t() %>%
    as.data.frame() %>%
    mutate(county = c1$ADMIN1)%>%
    dplyr::select(county, everything())
  
  # construct the df --- will probably want to transform to long df rather then wide for easier filtering.  
  return(df)
}
