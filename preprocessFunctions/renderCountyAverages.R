#' renderCountyAverages
#'
#' @param selectedCounty individual county name, used to pass into the map function
#' @param counties the county shapefile
#' @param processedRasters raster brick of climate rasters
#'
#' @return
#' @export
#'
#' @examples
renderCountyAverages = function(selectedCounty, counties, processedRasters){
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
    as_tibble() %>%
    mutate(County = c1$ADMIN1) %>%
    dplyr::select(County, contains("tmin"), contains("tmax"), contains("pr")) %>% 
    #rename columns to be user friendly
    rename_at(vars(starts_with("tmin")), ~ str_replace(., "tmin", "Min_Temp")) %>% 
    rename_at(vars(starts_with("tmax")), ~ str_replace(., "tmax", "Max_Temp")) %>% 
    rename_at(vars(starts_with("pr")), ~ str_replace(., "pr", "Precipitation")) %>% 
    rename_at(vars(ends_with("hist")), ~ str_replace(., "hist", "Historic"))
  
  return(df)
}
