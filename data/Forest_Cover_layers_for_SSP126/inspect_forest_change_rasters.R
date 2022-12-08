####Read Forest Change files

setwd("C:/SOGES/Kenya_applied/RESULTS")

pack<-list ("rgdal","ggplot2", "raster", "sp", "stringr", "rasterVis", "RColorBrewer", "gridExtra")
lapply(pack, "library", character.only = TRUE)
## Read the forest cover rasters and plot them

## Existing cover

existing<-raster("data/existing_forest.tif")


##Expanded cover in 2030 (baseline)

expanded<-raster("data/expanded_forest.tif")


##Plot both layers to see changes

existing_df<-as.data.frame(existing, xy=TRUE)

ggplot2::ggplot(data=existing_df)+
  geom_raster(aes(x=x, y = y, fill = existing_forest))+
  scale_fill_viridis_c(name= "Current evergreen and deciduous cover",option="plasma")+
  theme_classic()


expanded_df<-as.data.frame(expanded, xy=TRUE)

ggplot(data=expanded_df)+
  geom_raster(aes(x=x, y = y, fill = expanded_forest))+
  scale_fill_viridis_c(name= "Expanded evergreen and deciduous cover",option="plasma")+
  theme_classic()

############################################################################################
## Inspect forest cover changes under two management scenarios

DoNothing<-readRDS("data/ssp126_DoNothing.rds")
StopFires<-readRDS("data/ssp126_StopFires.rds")

##Absolute Cover change across Kenya

forest_change_donothing<-DoNothing$forest_change_rasters

##Plotting forest cover changes in 2100 under SSP 126, when no managament actions are taken

f_change_df<-as.data.frame(forest_change_donothing, xy=TRUE)

ggplot(data=f_change_df)+
  geom_raster(aes(x=x, y = y, fill = X126_100_DoNothing))+
  scale_fill_gradient2(name= "SSP1-2.6 (2100)", na.value = "grey87")+
  theme_classic()
#######################################################################

forest_change_stopfires<-StopFires$forest_change_rasters

##Plotting forest cover changes in 2100 under SSP 126, when no managament actions are taken

f_change_df<-as.data.frame(forest_change_stopfires, xy=TRUE)

ggplot(data=f_change_df)+
  geom_raster(aes(x=x, y = y, fill = X126_100_StopFires))+
  scale_fill_gradient2(name= "SSP1-2.6 (2100)", na.value = "grey87")+
  theme_classic()


####################################################################################

## Plot changes in distinct cover areas

donothing_change<-DoNothing$areas_change_df
stopfires_change<-StopFires$areas_change_df

## % change in cover in each forest type is plotted

p1<-ggplot(data= donothing_change,aes(x= Year,y = `%Change`, fill = Areas))+
  geom_bar(stat='identity', position= position_dodge())


p2<-ggplot(data= stopfires_change,aes(x= Year,y = `Change`, fill = Areas))+
  geom_bar(stat='identity', position= position_dodge())

grid.arrange(p1,p2, nrow =1)

###############
