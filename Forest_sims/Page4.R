#########################################################################
###########Page4: DOWNLOAD PAGE##################################
#########################################################

##County maps of forest cover

library(shiny)
library(shinyWidgets)
library(dplyr)
library(forcats)
library(leaflet)
library(rgdal)
library(Rcpp)
library(sp)
library(raster)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(sf)


###Data
mask<-raster("./data/ken_mask.tif")
fac<-stack(readRDS("./data/facet_change_raster.rds"))
manage<-readRDS("./data/manage_change.rds")
template<-raster("./data/top_carbon_AOI.tif")
temp_crs<-crs(template)
pro<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

decid<-raster("./data/decid_final.asc")
crs(decid)<-temp_crs
decid_pro<-projectRaster(decid, crs= pro)

ever<-raster("./data/egreen_final.asc")
crs(ever)<-temp_crs
ever_pro<-projectRaster(ever, crs= pro)

pop<-readRDS("./data/pop_change.rds")

eco<-stack(readRDS("./data/eco_change.rds"))

county<- sf::st_read("./data/KE_Admin1_pro.shp",stringsAsFactors = F)
county_names<-county$ADMIN1
county<-st_transform(county, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#############################################################################################################
############################################################################################################

reportresults<-reactive({
  
  temp_time <- ifelse(input$Timeline4 == "Near term (2030)","30", 
                      ifelse(input$Timeline == "Medium term (2050)", "50", "100"))
  
  time_scen<-input$Timeline4
  
  climate_scen<-input$Climate 
   
  temp_clime <- ifelse(input$Climate == "Optimistic","126", 
                       ifelse(input$Climate == "Pessimistic", "370", "245"))
  temp_variable<- ifelse(input$Parameter == "Population", "pop",
                         ifelse(input$Parameter == "Forest cover", "forest", "eco"))
  
  temp_county<-input$County1

###########################################################################################################
  ## Row1: Deciduous and  Evergreen forests
  ## Row2: Change in forests under different management
  ## Row3: Change in ecosystem services
  ######################################################################################################
  
  ## Row 1
  
  county_shape<-county[county$ADMIN1 == temp_county, "ADMIN1"]
  
  decid_plot<-raster::crop(decid_pro, county_shape)

  ever_plot<-raster::crop(ever_pro, county_shape) 
  
######################################################################################################  
  ##Row 2
    dat_gr<-manage$graze
    dat_wa<-manage$water
    dat_fi<-manage$fire
    
    ##Gr
    dat_gr_clim<-dat_gr[[grep(temp_clime, names(dat_gr))]]
    dat_gr_time<-dat_gr_clim[[grep(temp_time, names(dat_gr_clim))]]
    dat_gr_time<-dat_gr_time*mask
    names(dat_gr_time)<-"C"
    
    ##Fi
    dat_fi_clim<-dat_fi[[grep(temp_clime, names(dat_fi))]]
    dat_fi_time<-dat_fi_clim[[grep(temp_time, names(dat_fi_clim))]]
    dat_fi_time<-dat_fi_time*mask
    names(dat_fi_time)<-"B"
  
    ##wa
    dat_wa_clim<-dat_wa[[grep(temp_clime, names(dat_wa))]]
    dat_wa_time<-dat_wa_clim[[grep(temp_time, names(dat_wa_clim))]]
    dat_wa_time<-dat_wa_time*mask
    names(dat_wa_time)<-"D"
  
    #do nothing
    dat<-fac[[grep(temp_clime, names(fac))]]
    dat_time<-dat[[grep(temp_time, names(dat))]]
    dat_var<-dat_time[[grep("tree",names(dat_time))]]
    dat_var<-dat_var*mask
    names(dat_var)<-"A"
    
    
    changes<-stack(dat_wa_time, dat_gr_time, dat_fi_time, dat_var)
    changes_pro<-projectRaster(changes, crs = pro)
    
    changes_county<-raster::extract(changes_pro,county_shape, sum, na.rm = TRUE)
    
 #####################################################################################################
    ##POPULATION
    
    pop_r<-pop$popchange_rural
    pop_u<-pop$popchange_urban
    
    pop_r_clim<-pop_r[[grep(temp_clime, names(pop_r))]]
    pop_r_time<-pop_r_clim[[grep(temp_time, names(pop_r_clim))]]
    
    pop_u_clim<-pop_u[[grep(temp_clime, names(pop_u))]]
    pop_u_time<-pop_u_clim[[grep(temp_time, names(pop_u_clim))]]
    
    popchange<-stack(pop_r_time, pop_u_time)
    popchange_pro<-projectRaster(popchange, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    pop_county<-raster::crop(popchange_pro, county_shape)
    
######################################################################################################
    ### Ecosystem services
    
    eco_change_clime<-eco[[grep(temp_clime, names(eco))]]
    eco_change_time<-eco_change_clime[[grep(temp_time, names(eco_change_clime))]]
    eco_ken<-eco_change_time
    names(eco_ken)<-c("Soil total carbon" , "Annual evapotranspiration")
    eco_pro<-projectRaster(eco_ken, crs = pro)
    eco_county<-raster::crop(eco_pro,county_shape)
    
########################################################################################################
  
  return(list(county_shape, decid_plot, ever_plot, changes_county, 
              temp_variable, pop_county, eco_county, temp_county, time_scen, climate_scen ))

})
##########################################################################################################
##########################################################################################################
output$varchange6<-renderPlot({
  
  par(mfrow = c(1,2))
  plot(reportresults()[[2]], main = "Deciduous forests (sq.km)")
  plot(reportresults()[[1]]$geometry,add = TRUE)
  
  plot(reportresults()[[3]], main = "Evergreen forests (sq.km)")
  plot(reportresults()[[1]]$geometry, add= TRUE)
  
})  
  
output$varchange7<-renderPlot({
  
if (reportresults()[[5]]== "forest") 
  
{
  changes_county<-reportresults()[[4]] * 100
  barplot(changes_county, horiz = TRUE, main = "Forest cover change (sq.km) under climate change and management")
  legend("topright", c("A - Do nothing", "B - Stop fires" , "C - Stop grazing", "D - Stop drought impacts"),
           bty = "n" )
  }
  
if (reportresults()[[5]]== "pop") 
{
  par(mfrow = c(1,2))
  plot(reportresults()[[6]][[1]], main = "Rural population change")
  plot(reportresults()[[1]]$geometry,add = TRUE)
  
  plot(reportresults()[[6]][[2]], main = "Urban population change")
  plot(reportresults()[[1]]$geometry,add = TRUE)
} 

  if (reportresults()[[5]]== "eco") 
  {
    par(mfrow = c(1,2))
    plot(reportresults()[[7]][[1]], main = "Change in soil total carbon")
    plot(reportresults()[[1]]$geometry,add = TRUE)
    
    plot(reportresults()[[7]][[2]], main = "Change in annual evapotranspiration")
    plot(reportresults()[[1]]$geometry,add = TRUE)
    
  }   
  
  })


output$report <- downloadHandler(
  
  filename = "report.html",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    
    tempcss <- file.path(tempdir(),"background_pic.css" )
    file.copy("background_pic.css",tempcss, overwrite=TRUE)
    
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
    
    # Set up parameters to pass to Rmd document
    params <- list(n = list(county_shape = reportresults()[[1]], decid = reportresults()[[2]],
                            ever = reportresults()[[3]], forest_change = reportresults()[[4]],
                            pop = reportresults()[[6]], eco = reportresults()[[7]], 
                            county_name = reportresults()[[8]], time = reportresults()[9],
                            climate = reportresults()[10]))
                            
                            
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
    
  })