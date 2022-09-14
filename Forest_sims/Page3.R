#####################################################################################################
####Script loads rasters and results for Page 3: Pessimistic scenario
#######################################################################################################
##load libraries

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
fac<-stack(readRDS("./data/facet_change_raster.rds"))
manage<-readRDS("./data/manage_change.rds")
incfor<-readRDS("./data/increased_changes.rds")
clim<-stack(readRDS("./data/temp_pr_change.rds"))
npp_bc<-stack(readRDS("./data/npp_bc_change.rds"))
pop<-readRDS("./data/pop_change.rds")

mask<-raster("./data/ken_mask.tif")
pro_template<-raster("./data/wgs_ext_res_temp.asc")
crs(pro_template)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


county<- sf::st_read("./data/KE_Admin1_pro.shp",stringsAsFactors = F)
county_names<-county$ADMIN1
county<-st_transform(county, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
############################################################################################################

###Page 3 tab 1

facresults4<-reactive({
  
  temp_time <- ifelse(input$Timeline3 == "Near term (2030)","30", 
                      ifelse(input$Timeline3 == "Medium term (2050)", "50", "100"))
  
  temp_var<-ifelse (input$Layer3 == "Min Temperature", "tmin", 
                    ifelse(input$Layer3 == "Max Temperature", "tmax",
                           ifelse(input$Layer3 == "Precipitation", "prcp","npp")))
 
   if (temp_var == "tmin"| temp_var== "tmax"| temp_var == "prcp"){
    dat<-clim[[grep("_370", names(clim))]]
    dat_time<-dat[[grep(temp_time, names(dat))]]
    dat_var<-dat_time[[grep(temp_var,names(dat_time))]]}
  
  
  if (temp_var == "npp"){
    dat<-npp_bc[[grep("_370", names(npp_bc))]]
    dat_time<-dat[[grep(temp_time, names(dat))]]
    dat_var<-dat_time} 
  
  
  
  temp_county<-input$County
  
   dat_var<-projectRaster(from = dat_var, to = pro_template)
  
 
  return(list(dat_var,temp_var))
})

#######################################################################################################
################################################################################################
output$varchange4<-renderLeaflet({
  library(leaflet)
  
  
    minval<-min(values(facresults4()[[1]]),na.rm=TRUE)
    maxval<-max(values(facresults4()[[1]]),na.rm=TRUE)
  
  

  dom<-c(minval, maxval)
  
  val<-seq(minval,maxval)
  
  colorPal <- c(colorRampPalette(colors = c("#330000", "white"),
                                 space = "Lab")(abs(minval)),
                colorRampPalette(colors = c("white", "#003300"), 
                                 space = "Lab")(abs(maxval))) 
  if(minval < 0 & maxval > 0){
    pal<-colorNumeric(colorPal, dom)}
  else{pal<-colorNumeric(colorRampPalette(colors = c("white", "#003300"), 
                                          space = "Lab")(abs(maxval)), dom)}
  
  
  if(facresults4()[2]=="npp"){legend_title<-"Change (gm / sqm)"}else{legend_title<-"% change"}
  
  
  
  leaflet()%>%
    addTiles(group = "BaseMap") %>%
    addRasterImage(facresults4()[[1]],
                   colors = pal(val), group = "Data")%>%
    addLegend(pal = pal, values = dom, title = legend_title)%>%
    addPolygons(data = county, 
                fillColor = "", 
                fillOpacity = 0,
                color = "black",
                layerId = ~ADMIN1, weight = 3, group = "County")%>%
    # Layers control
    addLayersControl(
      baseGroups = c("BaseMap"),
      overlayGroups = c("Data","County"),
      options = layersControlOptions(collapsed = FALSE)
    )
})
###########################################################################################  
##########################################################################################  

observe({ 
  event <- input$varchange4_shape_click
  output$cnty4 <- renderText(
    paste0("County:",county$ADMIN1[county$ADMIN1 == event$id]))
  
})

observe({
  
  event<-input$varchange4_click
  rast_df<-as.data.frame(facresults4()[[1]], xy = TRUE)
  colnames(rast_df)<-c("x", "y", "z")
  temp_x<-which(abs(rast_df$x-event$lng)==min(abs(rast_df$x-event$lng)))
  temp_y<-which(abs(rast_df$y-event$lat)==min(abs(rast_df$y-event$lat)))
  rowid<-intersect(temp_x, temp_y)
  
  
  ifelse(facresults4()[2]=="npp",obs_val<-round(rast_df[rowid,][1,3],3),
         ifelse(facresults4()[2]== "forest",obs_val<-round(rast_df[rowid,][1,3]*100,3),
                obs_val<- round(rast_df[rowid,][1,3],3)))
  
  
  ifelse((facresults4()[2] == "npp"),text_to_paste<-"Change (gm/sq.m)= ",
         ifelse(facresults4()[2] == "forest", text_to_paste<-"Change (sq.km) = ",
                text_to_paste<-"Change (%) = " ))
  
  
  text2<-"Rasters represent change in variable in the selected time period relative to variable value in 2014"
  
  
  output$facdat4 <- renderText({
    paste0(text_to_paste, obs_val) 
  })
  
  output$explain4 <- renderText({
    text2 
  })
  
})

######################################################################################################
##PAGE 3 tab 2


facresults5<-reactive({
  
  temp_time <- ifelse(input$Timeline3_1 == "Near term (2030)","30", 
                      ifelse(input$Timeline3_1 == "Medium term (2050)", "50", "100"))
  

  temp_var<-ifelse(input$Management3 == "Do nothing", "forest",
                   ifelse(input$Management3 == "Stop grazing", "graze",
                          ifelse(input$Management3 == "Stop fires", "fire", "water")))
  
  temp_county<-input$County
  
  dat_var<-NULL  
  
  if (temp_var == "forest"){
    dat<-fac[[grep("_370", names(fac))]]
    dat_time<-dat[[grep(temp_time, names(dat))]]
    dat_var<-dat_time[[grep("tree",names(dat_time))]] * mask} else(dat_var<-dat_var)
  
  if (temp_var == "graze"){
    dat<-manage$graze
    dat_tr<-dat[[grep("_370", names(dat))]]
    dat_var<-dat_tr[[grep(temp_time, names(dat_tr))]] * mask} else(dat_var<-dat_var)
  
  if (temp_var == "fire"){
    dat<-manage$fire
    dat_tr<-dat[[grep("_370", names(dat))]]
    dat_var<-dat_tr[[grep(temp_time, names(dat_tr))]] * mask} else(dat_var<-dat_var)
  
  if (temp_var == "water"){
    dat<-manage$water
    dat_tr<-dat[[grep("_370", names(dat))]]
    dat_var<-dat_tr[[grep(temp_time, names(dat_tr))]] * mask} else(dat_var<-dat_var)
  
  
  dat_var<-projectRaster(from = dat_var, to = pro_template)
  
  return(list(dat_var,temp_var))
})


output$varchange5<-renderLeaflet({
  library(leaflet)
  
  
  minval<-min(values(facresults5()[[1]]),na.rm=TRUE)*100
  maxval<-max(values(facresults5()[[1]]),na.rm=TRUE)*100
 
  
  dom<-c(minval, maxval)
  
  val<-seq(minval,maxval)
  
  colorPal <- c(colorRampPalette(colors = c("#330000", "white"),
                                 space = "Lab")(abs(minval)),
                colorRampPalette(colors = c("white", "#003300"), 
                                 space = "Lab")(abs(maxval))) 
  if(round(minval) < 0 & maxval > 0){
    pal<-colorNumeric(colorPal, dom)}
  else{pal<-colorNumeric(colorRampPalette(colors = c("white", "#003300"), 
                                          space = "Lab")(abs(maxval)), dom)}
  
  legend_title<-"Change (sq.km)"
  
  
  
  leaflet()%>%
    addTiles(group = "BaseMap") %>%
    addRasterImage(facresults5()[[1]],
                   colors = pal(val), group = "Data")%>%
    addLegend(pal = pal, values = dom, title = legend_title)%>%
    addPolygons(data = county, 
                fillColor = "", 
                fillOpacity = 0,
                color = "black",
                layerId = ~ADMIN1, weight = 3, group = "County")%>%
    # Layers control
    addLayersControl(
      baseGroups = c("BaseMap"),
      overlayGroups = c("Data","County"),
      options = layersControlOptions(collapsed = FALSE)
    )
})
###########################################################################################  
##########################################################################################  
observe({ 
  event <- input$varchange5_shape_click
  output$cnty5 <- renderText(
    paste0("County:",county$ADMIN1[county$ADMIN1 == event$id]))
  
})

observe({
  
  event2<-input$varchange5_click
  rast_df<-as.data.frame(facresults5()[[1]], xy = TRUE)
  colnames(rast_df)<-c("x", "y", "z")
  temp_x<-which(abs(rast_df$x-event2$lng)==min(abs(rast_df$x-event2$lng)))
  temp_y<-which(abs(rast_df$y-event2$lat)==min(abs(rast_df$y-event2$lat)))
  rowid<-intersect(temp_x, temp_y)
  
  
  obs_val<-round(rast_df[rowid,][1,3]*100,3)
  
  text_to_paste<-"Change (sq.km) = "
  
  text2<-"Rasters represent change in forest extent in the selected time period relative to variable value in 2014"
  
  
  output$facdat5 <- renderText({
    paste0(text_to_paste, obs_val) 
  })
  
  output$explain5 <- renderText({
    text2 
  })
  
})


##################################################################################################################  
######################################################################################################
##Page 3 tab 3


output$varchange331<-renderPlot({
  par(mfrow = c(1,2))
  original_cover<-(projectRaster(incfor$all, crs = pro)) * 100
  original_cover[which(original_cover[]>100)]<-100
  original_cover[which(original_cover[]<0)]<-0
  plot(original_cover, main= "Current forest cover (in 2014)")
  plot(county$geometry, add = TRUE)
  inc_cover<-(projectRaster(incfor$all_inc, crs = pro)) * 100
  inc_cover[which(inc_cover[]>100)]<-100
  inc_cover[which(inc_cover[]<0)]<-0
  plot(inc_cover, main = "Expanded forest cover (by 2030)")
  plot(county$geometry, add = TRUE)
  
})


#############################################################################################################################
#################################################################################################################################
##################################################################################################################  
######################################################################################################
##Page 3 tab 3

incresults3<-reactive({
  
  temp_var<-ifelse(input$Management32 == "Do nothing", "N","Y")
  
  #county_selected<-input$county13
  
  dat_var<-NULL 
  
  dat_30<- incfor$all_inc
  names(dat_30)<-"2030"
  
  
  
  if (temp_var == "N"){
    da<-incfor$inc_change
    dat<-da[[grep("_370", names(da))]]
    dat_50<-dat[[grep("_50", names(dat))]]
    dat_50[temp_na]<-NA
    dat_50<-dat_50*mask
    names(dat_50)<-"2050"
    dat_100<-dat[[grep("_100", names(dat))]]
    dat_100[temp_na]<-NA
    names(dat_100)<-"2100"} 
  
  if (temp_var == "Y"){
    da<-incfor$fire_change
    dat<-da[[grep("_370", names(da))]]
    dat_50<-dat[[grep("_50", names(dat))]]
    dat_50[temp_na]<-NA
    dat_50<-dat_50 * mask
    names(dat_50)<-"2050"
    dat_100<-dat[[grep("_100", names(dat))]]
    dat_100[temp_na]<-NA
    names(dat_100)<-"2100"} 
  
  dat_hist<-incfor$all 
  names(dat_hist)<-"2014"
  
  
  
  dat_var<-stack(dat_hist,dat_30, dat_50, dat_100)
  
  dat_var<-projectRaster(from = dat_var, to = pro_template, method = "ngb")
  
  ############################################################################################    
  
  ## Create a table showing summaries of forest cover for each county in Current, 2030, 2050, 2100
  
  if(input$County33 == "All"){
    county_s <- county
    dat_county<-raster::crop(dat_var, county_s)
    dat_sum<-as.data.frame(cellStats(dat_county, sum, na.rm = TRUE))
    dat_sum$Year<-rownames(dat_sum)
    colnames(dat_sum)[1]<-"Values"
    torender<-data.frame("TIme Period" = c("Current forest cover", "Expanded forest cover in 2030", 
                                           "Forest cover in 2050", 
                                           "Forest cover in 2100 "),
                         "Percent Forest Cover" = dat_sum$Values * 100 / 580367 * 100, check.names = FALSE)
    torender[3,2]<-torender[3,2] + torender[1,2]
    torender[4,2]<-torender[4,2] + torender[1,2]}
    
  
  if(input$County33 != "All"){
    county_s <- county[county$ADMIN1 == input$County33, "ADMIN1"]
    dat_county<-raster::crop(dat_var, county_s)
    dat_sum<-as.data.frame(cellStats(dat_county, sum, na.rm = TRUE))
    dat_sum$Year<-rownames(dat_sum)
    colnames(dat_sum)[1]<-"Values"
    torender<-data.frame("Time Period" = c("Current forest cover", "Expanded forest cover in 2030", 
                                           "Forest cover in 2050", 
                                           "Forest cover in 2100"),
                         "Percent Forest Cover" = dat_sum$Values * 100 / 580367 * 100, check.names = FALSE)
    torender[3,2]<-torender[3,2] + torender[1,2]
    torender[4,2]<-torender[4,2] + torender[1,2]}
  
  return(list (dat_var = dat_var, torender, county_s))
  
  
})

####################################################################################################
output$varchange331<-renderLeaflet({
  
  library(leaflet)
  f_hist<-incresults3()$dat_var[[1]]
  f_30<-incresults3()$dat_var[[2]]
  f_50<-incresults3()$dat_var[[3]]
  f_100<-incresults3()$dat_var[[4]]
  #################################################################################################
  ##################################################################################################  
  minval<-min(values(f_30),na.rm=TRUE)* 100
  maxval<-max(values(f_30),na.rm=TRUE)* 100
  dom<-c(minval, maxval)
  val<-seq(minval,maxval)
  
  if(minval < 0 & maxval > 0){
    colorPal <- c(colorRampPalette(colors = c("#330000", "white"),
                                   space = "Lab")(abs(minval)),
                  colorRampPalette(colors = c("white", "#003300"), 
                                   space = "Lab")(abs(maxval))) 
    pal<-colorNumeric(colorPal, dom, na.color = "NA")}
  
  else {pal<-colorNumeric(colorRampPalette(colors = c("white", "#003300"),space = "Lab")(abs(maxval)), na.color = "NA", dom)}
  
  legend_title<-"Forest cover (sq.km)"
  ############################################################################################ 
  leaflet()%>%
    addTiles(group = "BaseMap") %>%
    addRasterImage(f_hist,
                   colors = pal(val), group = "Current cover")%>%
    addRasterImage(f_30,
                   colors = pal(val), group = "Expanded cover by 2030")%>%
    addLegend(pal = pal, values = dom, title = legend_title)%>%
    addPolygons(data = county, 
                fillColor = "", 
                fillOpacity = 0,
                color = "black",
                layerId = ~ADMIN1, weight = 3, group = "County")%>%
    addPolygons(data = incresults3()[[3]], 
                fillColor = "", 
                fillOpacity = 0,
                color = "blue",
                layerId = ~ADMIN1, weight = 3, group = "County")%>%
    # Layers control
    addLayersControl(
      position = "bottomright",
      baseGroups = c("BaseMap"),
      overlayGroups = c("Current cover","Expanded cover by 2030", "County"),
      options = layersControlOptions(collapsed = FALSE)
    )
})  



#############################################################################################################################
#################################################################################################################################
observe({ 
  event <- input$varchange331_shape_click
  output$cnty33 <- renderText(
    paste0("County:",county$ADMIN1[county$ADMIN1 == event$id]))
  
})

observe({
  
  event13<-input$varchange331_click
  rast_df<-as.data.frame(incresults3()$dat_var[[1]], xy = TRUE)
  colnames(rast_df)<-c("x", "y", "z")
  temp_x<-which(abs(rast_df$x-event13$lng)==min(abs(rast_df$x-event13$lng)))
  temp_y<-which(abs(rast_df$y-event13$lat)==min(abs(rast_df$y-event13$lat)))
  rowid<-intersect(temp_x, temp_y)
  
  rast_df1<-as.data.frame(incresults3()$dat_var[[2]], xy = TRUE)
  
  
  obs_val1<-round(rast_df[rowid,][1,3]*100,2)
  obs_val2<-round(rast_df1[rowid,][1,3]*100,2)
  
  
  text_to_paste<-"Map shows potential distribution of forest cover 
                     in 2030 following successful afforestation efforts. See Simulation details page 
                      to learn about how this future forest cover scenario was developed"
  
  text1<- paste("Forest cover in 2005: ", obs_val1, "sq.km")
  text2<- paste("Forest cover in 2030: ", obs_val2, "sq.km")
  
  
  output$facdat33 <- renderText({
    paste(text1,";", text2) 
  })  
  
  
  output$explain33 <- renderText({
    text_to_paste 
  })
  
})

#################################################################################################################################################
###############################################################################################################################################
output$tab33<-renderTable({
  
  incresults3()[2]
  
})

