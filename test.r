leaflet() %>%
  # tile providers ----------------------------------------------------------
addProviderTiles("Stamen.Toner", group = "Light")%>%
addRasterImage(raster(allRasters_new[[1]]$tmax_hist),
              colors = palettes[[1]]$tmax_hist, 
               group = "Data")
