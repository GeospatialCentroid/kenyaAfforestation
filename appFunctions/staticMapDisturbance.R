#' function to produce the static maps for the 'Download Map' button
#'
#' @param r_hist the historic forest raster
#' @param r_base the baseline forest raster
#' @param r_change the percent change raster layer currently mapped for selected disturbance
#' @param county the county shapefile
#' @param disturbance the user-selected disturbance scenario
#' @param scenario the user-selected climate
#' @param year the user-selected time period
#' 
#' @return A list of tmap objects
staticMapDisturbance <-
  function(r_hist,
           r_base,
           r_change,
           county,
           disturbance,
           scenario,
           year) {
    
  # choice value pair to retrieve scenario name instead of input value
    dist_options <- c(
      "Historic Fire Severity" = "nothing",
      "Decreased Fire Severity" = "less_fire",
      "Increased Fire Severity" = "more_fire"
    )
    

  
  # create the 3 maps
  tm1 <- 
    tm_shape(r_hist) +
      tm_raster(style = "cont",
                palette = "Greens",
                title = "Historic Tree Cover",
                legend.format = list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) +
      tm_shape(county) +
      tm_borders() +
    tm_compass(position = "left") +
    tm_scale_bar(position = "left") +
    tm_credits(paste("Maps generated on", Sys.Date(), " \nfrom the Kenya Afforestation Decision Support Tool (ka-dst.com)"),
               position = c("left", "bottom")) +
    tm_layout(title = "Historic",
              title.size = 2,
              frame = FALSE,
              legend.outside = TRUE,
              legend.outside.size = 0.25,
              legend.title.size = 1,
              inner.margins = 0.05
    ) 
    
    

  
  tm2 <- 
    tm_shape(r_base) +
      tm_raster(style = "cont",
                palette = "Greens",
                title = "Baseline Tree Cover",
                legend.format = list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) +
      tm_shape(county) +
      tm_borders() +
      tm_compass(position = "left") +
      tm_scale_bar(position = "left") +
      tm_credits(paste("Maps generated on", Sys.Date(), " \nfrom the Kenya Afforestation Decision Support Tool (ka-dst.com)"),
                 position = c("left", "bottom")) +
      tm_layout(title = "Baseline",
                title.size = 2,
                frame = FALSE,
                legend.outside = TRUE,
                legend.outside.size = 0.25,
                legend.title.size = 1,
                inner.margins = 0.05
      ) 
    

  tm3 <- 
    tm_shape(r_change) +
      tm_raster(style = "cont",
                palette = "BrBG",
                title = "Predicted Tree Cover Change",
                legend.format = list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) +
      tm_shape(county) +
      tm_borders() +
      tm_compass(position = "left") +
      tm_scale_bar(position = "left") +
      tm_credits(paste("Maps generated on", Sys.Date(), " \nfrom the Kenya Afforestation Decision Support Tool (ka-dst.com)"),
                 position = c("left", "bottom")) +
      tm_layout(title = paste0(scenario, "\nPercent Change \n", "(from Baseline to 20", year, ") \n",
                               "with ", names(which(dist_options == disturbance))),
                title.size = 3,
                frame = FALSE,
                legend.outside = TRUE,
                legend.outside.size = 0.25,
                legend.title.size = 1,
                inner.margins = 0.05
      ) 
  
  return(list(tm1, tm2, tm3))
    

}

