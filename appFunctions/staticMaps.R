#' function to produce the static maps for the 'Download Map' button
#'
#' @param r_hist the historic raster layer currently mapped
#' @param r_proj the projected raster layer currently mapped
#' @param r_change the percent change raster layer currently mapped
#' @param county the county shapefile
#' @param variable the user-selected variable (from input$Layer)
#' @param title_abs title of the absolute value maps, pulled from the palettes object
#' @param title_change title of the percent change maps, pulled from the palettes object
#' 
#' @return A list of tmap objects
staticMaps <- function(r_hist, r_proj, r_change, county, variable, title_abs, title_change){
  
  
  # generate palettes
  ## abs palettes
  tmap_pal1 <- 
    if(variable == "pr") {
      "YlGnBu"
    } else {
      "-RdYlBu"
    }

  
  ## change palettes
  tmap_pal2 <- 
    if(variable == "pr") {
      "PuOr"
    } else {
      "-BrBG"
    }
    

  
  # create the 3 maps
  tm1 <- 
    tm_shape(r_hist) +
      tm_raster(style = "cont",
                palette = tmap_pal1,
                title = str_replace(title_abs, "</br>", "\n")) +
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
    tm_shape(r_proj) +
      tm_raster(style = "cont",
                palette = tmap_pal1,
                title = str_replace(title_abs, "</br>", "\n")) +
      tm_shape(county) +
      tm_borders() +
      tm_compass(position = "left") +
      tm_scale_bar(position = "left") +
      tm_credits(paste("Maps generated on", Sys.Date(), " \nfrom the Kenya Afforestation Decision Support Tool (ka-dst.com)"),
                 position = c("left", "bottom")) +
      tm_layout(title = "Projected",
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
                palette = tmap_pal2,
                title = str_replace(title_change, "</br>", "\n"),
                legend.format = list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) +
      tm_shape(county) +
      tm_borders() +
      tm_compass(position = "left") +
      tm_scale_bar(position = "left") +
      tm_credits(paste("Maps generated on", Sys.Date(), " \nfrom the Kenya Afforestation Decision Support Tool (ka-dst.com)"),
                 position = c("left", "bottom")) +
      tm_layout(title = "Percent Change",
                title.size = 2,
                frame = FALSE,
                legend.outside = TRUE,
                legend.outside.size = 0.25,
                legend.title.size = 1,
                inner.margins = 0.05
      ) 
  
  return(list(tm1, tm2, tm3))
    

}