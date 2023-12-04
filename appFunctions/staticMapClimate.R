#' function to produce the static maps for the 'Download Map' button
#'
#' @param r_hist the historic raster layer currently mapped
#' @param r_proj the projected raster layer currently mapped
#' @param r_change the percent change raster layer currently mapped
#' @param county the county shapefile
#' @param variable the user-selected variable (from input$Layer)
#' @param scenario the user-selected climate
#' @param year the user-selected time period
#' @param title_abs title of the absolute value maps, pulled from the palettes object
#' @param title_change title of the percent change maps, pulled from the palettes object
#' 
#' @return A list of tmap objects
staticMapClimate <-
  function(r_hist,
           r_proj,
           r_change,
           county,
           variable,
           scenario,
           year,
           title_abs,
           title_change) {
    
  
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
      "Blues"
    } else {
      "-BrBG"
    }
    

  
  # create the 3 maps
  tm1 <- 
    tm_shape(r_hist) +
      tm_raster(style = "cont",
                palette = tmap_pal1,
                legend.reverse = T,
                title = str_replace(title_abs, "</br> ", "\n")) +
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
                legend.reverse = T,
                title = str_replace(title_abs, "</br> ", "\n")) +
      tm_shape(county) +
      tm_borders() +
      tm_compass(position = "left") +
      tm_scale_bar(position = "left") +
      tm_credits(paste("Maps generated on", Sys.Date(), " \nfrom the Kenya Afforestation Decision Support Tool (ka-dst.com)"),
                 position = c("left", "bottom")) +
      tm_layout(title = paste0(scenario, "\nProjected ", "20", year),
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
                legend.reverse = T,
                title = str_replace(title_change, "</br> ", "\n"),
                legend.format = list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %"))) +
      tm_shape(county) +
      tm_borders() +
      tm_compass(position = "left") +
      tm_scale_bar(position = "left") +
      tm_credits(paste("Maps generated on", Sys.Date(), " \nfrom the Kenya Afforestation Decision Support Tool (ka-dst.com)"),
                 position = c("left", "bottom")) +
      tm_layout(title = paste0(scenario, "\nPercent Change \n", "(Historic - 20", year, ")"),
                title.size = 2,
                frame = FALSE,
                legend.outside = TRUE,
                legend.outside.size = 0.25,
                legend.title.size = 1,
                inner.margins = 0.05
      ) 
  
  return(list(tm1, tm2, tm3))
    

}


# test maps

# climateChangeInputs <- readRDS("appData/climateChangeInputs.RDS")
# ## define specific raster sets ----
# clim_abs <- climateChangeInputs$abs_rasters
# clim_change <- climateChangeInputs$change_rasters
# ## county shp----
# county <- climateChangeInputs$county
# 
# 
# r_hist <- clim_abs$pr_hist
# 
# r_proj <- clim_abs$pr_126_30
# 
# r_change <- clim_change$pr_126_30
# 
# # 
# staticMaps(r_hist, r_proj, r_change, county, variable = "pr",
#            year = "50",
#            scenario = "Optimistic Scenario",
#            title_abs = "Test",
#            title_change = "Test")
