#### Changes in ecosystem services


rm(list = ls())
setwd("C:/SOGES/L_Range_NoEdits/L_Range/Output/")

pack<-list ("rgdal", "raster", "sp", "stringr", "rasterVis", "RColorBrewer", "ggplot2", "rgeos")
lapply(pack, "library", character.only = TRUE)
template<-raster("C:/GIS/SOGES/H_Soils/top_carbon_AOI.tif")
pro<-crs(template)
##Create a function that coverts the asc files to raster

rast_maker<-function (asc){
  temp<-as.matrix(read.table(asc, skip = 6, header= FALSE, sep = ""))
  temp2<-raster(temp, template=template)
  temp2[temp2==-9999]<-NA
  #temp2[temp2==0]<-NA
  projection(temp2)<-pro
  return(temp2)
}
#######################################################################################################
############################################################################################

## Extract and plot ecosystem service changes
## Soil total carbon
stc_hist<-rast_maker("ssp126_nograzing/stc_2015_12_1.asc")

stc_30_126<-rast_maker("ssp126_out/stc_2030_12_1.asc")
stc_30_126_change<-stc_30_126 - stc_hist
names(stc_30_126_change)<-"stc_30_126"

stc_50_126<-rast_maker("ssp126_out/stc_2050_12_1.asc")
stc_50_126_change<-stc_50_126 - stc_hist
names(stc_50_126_change)<-"stc_50_126"

stc_100_126<-rast_maker("ssp126_out/stc_2100_12_1.asc")
stc_100_126_change<-stc_100_126 - stc_hist
names(stc_100_126_change)<-"stc_100_126"

stc_30_245<-rast_maker("ssp245_out/stc_2030_12_1.asc")
stc_30_245_change<-stc_30_245 - stc_hist
names(stc_30_245_change)<-"stc_30_245"

stc_50_245<-rast_maker("ssp245_out/stc_2050_12_1.asc")
stc_50_245_change<-stc_50_245 - stc_hist
names(stc_50_245_change)<-"stc_50_245"

stc_100_245<-rast_maker("ssp245_out/stc_2100_12_1.asc")
stc_100_245_change<-stc_100_245 - stc_hist
names(stc_100_245_change)<-"stc_100_245"

stc_30_370<-rast_maker("ssp370_nograzing/stc_2030_12_1.asc")
stc_30_370_change<-stc_30_370 - stc_hist
names(stc_30_370_change)<-"stc_30_370"

stc_50_370<-rast_maker("ssp370_nograzing/stc_2050_12_1.asc")
stc_50_370_change<-stc_50_370 - stc_hist
names(stc_50_370_change)<-"stc_50_370"

stc_100_370<-rast_maker("ssp370_nograzing/stc_2100_12_1.asc")
stc_100_370_change<-stc_100_370 - stc_hist
names(stc_100_370_change)<-"stc_100_370"

stc_change<-stack(stc_30_126_change, stc_30_245_change, stc_30_370_change,
                  stc_50_126_change, stc_50_245_change, stc_50_370_change,
                  stc_100_126_change, stc_100_245_change, stc_100_370_change)
#############################################################################################
#######################################################################################

## Evapotranspiration
ae_hist<-rast_maker("ssp126_nograzing/ae_2015_12_1.asc")

ae_30_126<-rast_maker("ssp126_out/ae_2030_12_1.asc")
ae_30_126_change<-ae_30_126 - ae_hist
names(ae_30_126_change)<-"ae_30_126"

ae_50_126<-rast_maker("ssp126_out/ae_2050_12_1.asc")
ae_50_126_change<-ae_50_126 - ae_hist
names(ae_50_126_change)<-"ae_50_126"

ae_100_126<-rast_maker("ssp126_out/ae_2100_12_1.asc")
ae_100_126_change<-ae_100_126 - ae_hist
names(ae_100_126_change)<-"ae_100_126"

ae_30_245<-rast_maker("ssp245_out/ae_2030_12_1.asc")
ae_30_245_change<-ae_30_245 - ae_hist
names(ae_30_245_change)<-"ae_30_245"

ae_50_245<-rast_maker("ssp245_out/ae_2050_12_1.asc")
ae_50_245_change<-ae_50_245 - ae_hist
names(ae_50_245_change)<-"ae_50_245"

ae_100_245<-rast_maker("ssp245_out/ae_2100_12_1.asc")
ae_100_245_change<-ae_100_245 - ae_hist
names(ae_100_245_change)<-"ae_100_245"

ae_30_370<-rast_maker("ssp370_nograzing/ae_2030_12_1.asc")
ae_30_370_change<-ae_30_370 - ae_hist
names(ae_30_370_change)<-"ae_30_370"

ae_50_370<-rast_maker("ssp370_nograzing/ae_2050_12_1.asc")
ae_50_370_change<-ae_50_370 - ae_hist
names(ae_50_370_change)<-"ae_50_370"

ae_100_370<-rast_maker("ssp370_nograzing/ae_2100_12_1.asc")
ae_100_370_change<-ae_100_370 - ae_hist
names(ae_100_370_change)<-"ae_100_370"

ae_change<-stack(ae_30_126_change, ae_30_245_change, ae_30_370_change,
                 ae_50_126_change, ae_50_245_change, ae_50_245_change,
                 ae_100_126_change, ae_100_245_change, ae_100_370_change)
############################################################################################################

eco_change<-list(stc = stc_change, ae = ae_change)

saveRDS(eco_change, "C:/SOGES/report/Forest_sims/data/eco_change.rds")
