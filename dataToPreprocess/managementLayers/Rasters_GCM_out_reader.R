####Script to read processed rasters

dat<-readRDS("C:/SOGES/Kenya_applied/RESULTS/GCM_outs/ssp126_GCM_DN.rds")

### To plot rasters showing change in forest cover across the study area. 

change_rast<-dat$forest_change_rasters

## Plot forest cover change in 2050. This raster shows mean of changes based on 17 different GCMs
change_2050<-change_rast[[1]]

library(ggplot2)
f_change_df<-as.data.frame(change_2050, xy=TRUE)

ggplot(data=f_change_df)+
  geom_raster(aes(x=x, y = y, fill = ssp126_50_donothing))+
  scale_fill_gradient2(name= "SSP1-2.6 (2050)", na.value = "grey87")+
  theme_classic()

####Boxplots showing changes in forest cover in different forest areas

kenya_change<-dat$areas_change_df

##Plot shows % changes in tree cover across different areas for 2050, 2070, 2100. Boxplots show estimates based on 
##17 unique climate model runs (GCM)


ggplot(data= kenya_change, aes(x=Year, y = value, color=Areas))+
  geom_boxplot()+ylab("% Change in tree cover")

#######Boxplots showing changes in cover in different forest areas in each county

county_change<-dat$county_change_df

## Baringo county

bar<-county_change[which(county_change$County=="Baringo"),]

ggplot(data= bar, aes(x=name, y = value, color=areas))+
  geom_boxplot()+ylab("% Change in tree cover")

### For deciduous and evergreen areas, estimates are based on pixels with at least 30% cover
