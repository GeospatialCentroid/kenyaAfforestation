rm(list = ls())
setwd("C:/SOGES/L_Range_NoEdits/L_Range/Output/")

pack<-list ("rgdal", "raster", "sp", "stringr", "rasterVis", "RColorBrewer", "ggplot2")
lapply(pack, "library", character.only = TRUE)
template<-raster("C:/GIS/SOGES/H_Soils/top_carbon_AOI.tif")
temp_na<-which(is.na(template[])==TRUE)
pro<-crs(template)
ken<-readOGR("C:/GIS/SOGES/FEWS/KE_LHZ_2011/Kenya_pro.shp")
ken_mask<-raster("C:/SOGES/L_range/Scenario files/ken_mask.tif") 
##Create a function that coverts the asc files to raster

rast_maker<-function (asc){
  temp<-as.matrix(read.table(asc, skip = 6, header= FALSE, sep = ""))
  temp2<-raster(temp, template=template)
  temp2[temp2==-9999]<-NA
  #temp2[temp2==0]<-NA
  projection(temp2)<-pro
  return(temp2)
}

##############################################################################################################################################
##############################################################################################################################################
##Show a raster showing difference between original and expanded forest cover scen

##Original forest cover
dec_30<-rast_maker("C:/SOGES/L_Range_NoEdits/L_Range/Layers/decid_final.asc")
ev_30<-rast_maker("C:/SOGES/L_Range_NoEdits/L_Range/Layers/egreen_final.asc")
all_30<-dec_30 + ev_30
all_30_frac<-all_30/100
all_30_frac[temp_na]<-NA
##Increased forest cover
dec_inc_30<-rast_maker("C:/SOGES/L_Range_NoEdits/L_Range/Layers/decid_forest_sims.asc")
ev_inc_30<-rast_maker("C:/SOGES/L_Range_NoEdits/L_Range/Layers/ever_forest_sims.asc")
all_inc_30<-dec_inc_30 + ev_inc_30
all_inc_30[which(all_inc_30[] > 100)]<-100
##############################################################################################
all_inc_30[which(ken_mask[]==0)]<-all_30[which(ken_mask[]==0)]
all_inc_frac_30<-all_inc_30 / 100
all_inc_frac_30[temp_na]<-NA
###########################################################################################################################################
par(mfrow = c(1,2))

plot(all_30_frac)
plot(ken, add = TRUE)
plot(all_inc_frac_30)
plot(ken, add = TRUE)
#########################################################################################################################################

## Do nothing - show change in forest cover in 2050, 2100
all0_30_frac<-which(all_30_frac[] ==0)

### YEAR 30
##SSP126
tree_126_30_inc<-rast_maker("ssp126_incforest/fac_2030_12_3.asc")
tree0_126_30_inc<-which(tree_126_30_inc[]==0)

tree_126_30_change<-tree_126_30_inc - all_30_frac
tree_126_30_change[intersect(tree0_126_30_inc, all0_30_frac)]<-NA
tree_126_30_change[temp_na]<-NA
names(tree_126_30_change)<-"tree_126_30_forest_change"

##SSP245
tree_245_30_inc<-rast_maker("ssp245_incforest/fac_2030_12_3.asc")
tree0_245_30_inc<-which(tree_245_30_inc[]==0)

tree_245_30_change<-tree_245_30_inc - all_30_frac
tree_245_30_change[intersect(tree0_245_30_inc, all0_30_frac)]<-NA
tree_245_30_change[temp_na]<-NA
names(tree_245_30_change)<-"tree_245_30_forest_change"

##SSP370
tree_370_30_inc<-rast_maker("ssp370_incforest/fac_2030_12_3.asc")
tree0_370_30_inc<-which(tree_370_30_inc[]==0)

tree_370_30_change<-tree_370_30_inc - all_30_frac
tree_370_30_change[intersect(tree0_370_30_inc, all0_30_frac)]<-NA
tree_370_30_change[temp_na]<-NA
names(tree_370_30_change)<-"tree_370_30_forest_change"

###YEAR 50

##SSP126
tree_126_50_inc<-rast_maker("ssp126_incforest/fac_2050_12_3.asc")
tree0_126_50_inc<-which(tree_126_50_inc[]==0)

tree_126_50_change<-tree_126_50_inc - all_30_frac
tree_126_50_change[intersect(tree0_126_50_inc, all0_30_frac)]<-NA
tree_126_50_change[temp_na]<-NA
names(tree_126_50_change)<-"tree_126_50_forest_change"

##SSP245
tree_245_50_inc<-rast_maker("ssp245_incforest/fac_2050_12_3.asc")
tree0_245_50_inc<-which(tree_245_50_inc[]==0)

tree_245_50_change<-tree_245_50_inc - all_30_frac
tree_245_50_change[intersect(tree0_245_50_inc, all0_30_frac)]<-NA
tree_245_50_change[temp_na]<-NA
names(tree_245_50_change)<-"tree_245_50_forest_change"

##SSP370
tree_370_50_inc<-rast_maker("ssp370_incforest/fac_2050_12_3.asc")
tree0_370_50_inc<-which(tree_370_50_inc[]==0)

tree_370_50_change<-tree_370_50_inc - all_30_frac
tree_370_50_change[intersect(tree0_370_50_inc, all0_30_frac)]<-NA
tree_370_50_change[temp_na]<-NA
names(tree_370_50_change)<-"tree_370_50_forest_change"

#### YEAR 2100

##SSP126
tree_126_100_inc<-rast_maker("ssp126_incforest/fac_2100_12_3.asc")
tree0_126_100_inc<-which(tree_126_100_inc[]==0)

tree_126_100_change<-tree_126_100_inc - all_30_frac
tree_126_100_change[intersect(tree0_126_100_inc, all0_30_frac)]<-NA
tree_126_100_change[temp_na]<-NA
names(tree_126_100_change)<-"tree_126_100_forest_change"

##SSP245
tree_245_100_inc<-rast_maker("ssp245_incforest/fac_2100_12_3.asc")
tree0_245_100_inc<-which(tree_245_100_inc[]==0)

tree_245_100_change<-tree_245_100_inc - all_30_frac
tree_245_100_change[intersect(tree0_245_100_inc, all0_30_frac)]<-NA
tree_245_100_change[temp_na]<-NA
names(tree_245_100_change)<-"tree_245_100_forest_change"

##SSP370
tree_370_100_inc<-rast_maker("ssp370_incforest/fac_2100_12_3.asc")
tree0_370_100_inc<-which(tree_370_100_inc[]==0)

tree_370_100_change<-tree_370_100_inc - all_30_frac
tree_370_100_change[intersect(tree0_370_100_inc, all0_30_frac)]<-NA
tree_370_100_change[temp_na]<-NA
names(tree_370_100_change)<-"tree_370_100_forest_change"


## Fire suppressed - show change in forest cover in 2050 and 2100 when fires are suppressed

### YEAR 30
##SSP126
tree_126_30_inc_fire<-rast_maker("ssp126_incforest/fac_2030_12_3.asc")
tree0_126_30_inc_fire<-which(tree_126_30_inc_fire[]==0)

tree_126_30_firechange<-tree_126_30_inc_fire - all_30_frac
tree_126_30_firechange[intersect(tree0_126_30_inc_fire, all0_30_frac)]<-NA
tree_126_30_firechange[temp_na]<-NA
names(tree_126_30_firechange)<-"tree_126_30_firechange"

##SSP245
tree_245_30_inc_fire<-rast_maker("ssp245_incforest/fac_2030_12_3.asc")
tree0_245_30_inc_fire<-which(tree_245_30_inc_fire[]==0)

tree_245_30_firechange<-tree_245_30_inc_fire - all_30_frac
tree_245_30_firechange[intersect(tree0_245_30_inc_fire, all0_30_frac)]<-NA
tree_245_30_firechange[temp_na]<-NA
names(tree_245_30_firechange)<-"tree_245_30_firechange"

##SSP370
tree_370_30_inc_fire<-rast_maker("ssp370_incforest/fac_2030_12_3.asc")
tree0_370_30_inc_fire<-which(tree_370_30_inc_fire[]==0)

tree_370_30_firechange<-tree_370_30_inc_fire - all_30_frac
tree_370_30_firechange[intersect(tree0_370_30_inc_fire, all0_30_frac)]<-NA
tree_370_30_firechange[temp_na]<-NA
names(tree_370_30_firechange)<-"tree_370_30_firechange"

###YEAR 50

##SSP126
tree_126_50_inc_fire<-rast_maker("ssp126_incf_nof/fac_2050_12_3.asc")
tree0_126_50_inc_fire<-which(tree_126_50_inc_fire[]==0)

tree_126_50_firechange<-tree_126_50_inc_fire - all_30_frac
tree_126_50_firechange[intersect(tree0_126_50_inc_fire, all0_30_frac)]<-NA
tree_126_50_firechange[temp_na]<-NA
names(tree_126_50_firechange)<-"tree_126_50_firechange"

##SSP245
tree_245_50_inc_fire<-rast_maker("ssp245_incf_nof/fac_2050_12_3.asc")
tree0_245_50_inc_fire<-which(tree_245_50_inc_fire[]==0)

tree_245_50_firechange<-tree_245_50_inc_fire - all_30_frac
tree_245_50_firechange[intersect(tree0_245_50_inc_fire, all0_30_frac)]<-NA
tree_245_50_firechange[temp_na]<-NA
names(tree_245_50_firechange)<-"tree_245_50_firechange"

##SSP370
tree_370_50_inc_fire<-rast_maker("ssp370_incf_nof/fac_2050_12_3.asc")
tree0_370_50_inc_fire<-which(tree_370_50_inc_fire[]==0)

tree_370_50_firechange<-tree_370_50_inc_fire - all_30_frac
tree_370_50_firechange[intersect(tree0_370_50_inc_fire, all0_30_frac)]<-NA
tree_370_50_firechange[temp_na]<-NA
names(tree_370_50_firechange)<-"tree_370_50_firechange"

#### YEAR 2100

##SSP126
tree_126_100_inc_fire<-rast_maker("ssp126_incf_nof/fac_2100_12_3.asc")
tree0_126_100_inc_fire<-which(tree_126_100_inc_fire[]==0)

tree_126_100_firechange<-tree_126_100_inc_fire - all_30_frac
tree_126_100_firechange[intersect(tree0_126_100_inc_fire, all0_30_frac)]<-NA
tree_126_100_firechange[temp_na]<-NA
names(tree_126_100_firechange)<-"tree_126_100_firechange"

##SSP245
tree_245_100_inc_fire<-rast_maker("ssp245_incf_nof/fac_2100_12_3.asc")
tree0_245_100_inc_fire<-which(tree_245_100_inc_fire[]==0)

tree_245_100_firechange<-tree_245_100_inc_fire - all_30_frac
tree_245_100_firechange[intersect(tree0_245_100_inc_fire, all0_30_frac)]<-NA
tree_245_100_firechange[temp_na]<-NA
names(tree_245_100_firechange)<-"tree_245_100_firechange"

##SSP370
tree_370_100_inc_fire<-rast_maker("ssp370_incf_nof/fac_2100_12_3.asc")
tree0_370_100_inc_fire<-which(tree_370_100_inc_fire[]==0)

tree_370_100_firechange<-tree_370_100_inc_fire - all_30_frac
tree_370_100_firechange[intersect(tree0_370_100_inc_fire, all0_30_frac)]<-NA
tree_370_100_firechange[temp_na]<-NA
names(tree_370_100_firechange)<-"tree_370_100_firechange"

######################################################################################################################################################
####################################################################################################################################################

### Combined rasters

inc_change<-stack(tree_126_30_change, tree_245_30_change, tree_370_30_change,
                  tree_126_50_change, tree_245_50_change, tree_370_50_change,
                  tree_126_100_change, tree_245_100_change, tree_370_100_change)


fir_change<-stack(tree_126_30_firechange, tree_245_30_firechange, tree_370_30_firechange,
                  tree_126_50_firechange, tree_245_50_firechange, tree_370_50_firechange,
                  tree_126_100_firechange, tree_245_100_firechange, tree_370_100_firechange)



increased_sims<-list(all = all_30_frac,inc_change = inc_change, fire_change = fir_change,
                     all_inc = all_inc_frac_30)

saveRDS(increased_sims, "C:/SOGES/report/Forest_sims/data/increased_changes.RDS")
