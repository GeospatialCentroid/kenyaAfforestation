rm(list = ls())
setwd("C:/SOGES/L_Range_NoEdits/L_Range/Output/")

pack<-list ("rgdal", "raster", "sp", "stringr", "rasterVis", "RColorBrewer", "ggplot2")
lapply(pack, "library", character.only = TRUE)
template<-raster("C:/GIS/SOGES/H_Soils/top_carbon_AOI.tif")
pro<-crs(template)
ken<-readOGR("C:/GIS/SOGES/FEWS/KE_LHZ_2011/Kenya_pro.shp")
ken_mask<-raster("C:/SOGES/L_range/Scenario files/ken_mask.tif") # Read in a mask file for Kenya
##Create a function that coverts the asc files to raster

rast_maker<-function (asc){
  temp<-as.matrix(read.table(asc, skip = 6, header= FALSE, sep = ""))
  temp2<-raster(temp, template=template)
  temp2[temp2==-9999]<-NA
  #temp2[temp2==0]<-NA
  projection(temp2)<-pro
  return(temp2)
}
##############################################################################
##############################################################################
##Import datasets for the year 2050

##SSP245
herb_245_50<-rast_maker("ssp245_forest_scen1/fac_2050_12_1.asc")
shrub_245_50<-rast_maker("ssp245_forest_scen1/fac_2050_12_2.asc")
tree_245_50<-rast_maker("ssp245_forest_scen1/fac_2050_12_3.asc")
tree0_245_50<-which(tree_245_50[]==0)
##SSP370
herb_370_50<-rast_maker("ssp370_forest_scen1/fac_2050_12_1.asc")
shrub_370_50<-rast_maker("ssp370_forest_scen1/fac_2050_12_2.asc")
tree_370_50<-rast_maker("ssp370_forest_scen1/fac_2050_12_3.asc")
tree0_370_50<-which(tree_370_50[]==0)
##SSP126
herb_126_50<-rast_maker("ssp126_forest_scen1/fac_2050_12_1.asc")
shrub_126_50<-rast_maker("ssp126_forest_scen1/fac_2050_12_2.asc")
tree_126_50<-rast_maker("ssp126_forest_scen1/fac_2050_12_3.asc")
tree0_126_50<-which(tree_126_50[]==0)
##Import rasters for the year 2030

##SSP245
herb_245_30<-rast_maker("ssp245_forest_scen1/fac_2030_12_1.asc")
shrub_245_30<-rast_maker("ssp245_forest_scen1/fac_2030_12_2.asc")
tree_245_30<-rast_maker("ssp245_forest_scen1/fac_2030_12_3.asc")
tree0_245_30<-which(tree_245_30[]==0)
##SSP370
herb_370_30<-rast_maker("ssp370_forest_scen1/fac_2030_12_1.asc")
shrub_370_30<-rast_maker("ssp370_forest_scen1/fac_2030_12_2.asc")
tree_370_30<-rast_maker("ssp370_forest_scen1/fac_2030_12_3.asc")
tree0_370_30<-which(tree_370_30[]==0)
##SSP126
herb_126_30<-rast_maker("ssp126_forest_scen1/fac_2030_12_1.asc")
shrub_126_30<-rast_maker("ssp126_forest_scen1/fac_2030_12_2.asc")
tree_126_30<-rast_maker("ssp126_forest_scen1/fac_2030_12_3.asc")
tree0_126_30<-which(tree_126_30[]==0)

##Import rasters for the year 2100

##SSP245
herb_245_100<-rast_maker("ssp245_forest_scen1/fac_2100_12_1.asc")
shrub_245_100<-rast_maker("ssp245_forest_scen1/fac_2100_12_2.asc")
tree_245_100<-rast_maker("ssp245_forest_scen1/fac_2100_12_3.asc")
tree0_245_100<-which(tree_245_100[]==0)
##SSP370
herb_370_100<-rast_maker("ssp370_forest_scen1/fac_2100_12_1.asc")
shrub_370_100<-rast_maker("ssp370_forest_scen1/fac_2100_12_2.asc")
tree_370_100<-rast_maker("ssp370_forest_scen1/fac_2100_12_3.asc")
tree0_370_100<-which(tree_370_100[]==0)
##SSP126
herb_126_100<-rast_maker("ssp126_forest_scen1/fac_2100_12_1.asc")
shrub_126_100<-rast_maker("ssp126_forest_scen1/fac_2100_12_2.asc")
tree_126_100<-rast_maker("ssp126_forest_scen1/fac_2100_12_3.asc")
tree0_126_100<-which(tree_126_100[]==0)

##TREE 2020
tree_126_20<-rast_maker("ssp126_forest_scen1/fac_2020_12_3.asc")
tree0_126_20<-which(tree_126_20[]==0)
tree_245_20<-rast_maker("ssp245_forest_scen1/fac_2020_12_3.asc")
tree0_245_20<-which(tree_245_20[]==0)
tree_370_20<-rast_maker("ssp370_forest_scen1/fac_2020_12_3.asc")
tree0_370_20<-which(tree_370_20[]==0)

##Hist2104
herb_2014<-rast_maker("ssphist_out/fac_2014_12_1.asc")
shrub_2014<-rast_maker("ssphist_out/fac_2014_12_2.asc")
tree_2014<-rast_maker("ssphist_out/fac_2014_12_3.asc")
tree0_2014<-which(tree_2014[]==0)
###################################


#################################################################################

##Create rasters that represent fractional change for each facet
tree_change_50_370<-(tree_370_50 -tree_370_20)
tree_change_50_370[intersect(tree0_370_20, tree0_370_50)]<-NA
names(tree_change_50_370)<-"tree_50_370"

shrub_change_50_370<-(shrub_370_50 -shrub_2014)
names(shrub_change_50_370)<-"shrub_50_370"

herb_change_50_370<-(herb_370_50 -herb_2014)
names(herb_change_50_370)<-"herb_50_370"

tree_change_50_245<-(tree_245_50 -tree_245_20)
tree_change_50_245[intersect(tree0_245_20, tree0_245_50)]<-NA
names(tree_change_50_245)<-"tree_50_245"

shrub_change_50_245<-(shrub_245_50 -shrub_2014)
names(shrub_change_50_245)<-"shrub_50_245"

herb_change_50_245<-(herb_245_50 -herb_2014)
names(herb_change_50_245)<-"herb_50_245"

tree_change_50_126<-(tree_126_50 -tree_126_20)
tree_change_50_126[intersect(tree0_126_20, tree0_126_50)]<-NA
names(tree_change_50_126)<-"tree_50_126"

shrub_change_50_126<-(shrub_126_50 -shrub_2014)
names(shrub_change_50_126)<-"shrub_50_126"

herb_change_50_126<-(herb_126_50 -herb_2014)
names(herb_change_50_126)<-"herb_50_126"

#For year 2030

tree_change_30_370<-(tree_370_30 -tree_370_20)
tree_change_30_370[intersect(tree0_370_20, tree0_370_30)]<-NA
names(tree_change_30_370)<-"tree_30_370"

shrub_change_30_370<-(shrub_370_30 -shrub_2014)
names(shrub_change_30_370)<-"shrub_30_370"

herb_change_30_370<-(herb_370_30 -herb_2014)
names(herb_change_30_370)<-"herb_30_370"

tree_change_30_245<-(tree_245_30 -tree_245_20)
tree_change_30_245[intersect(tree0_245_20, tree0_245_30)]<-NA
names(tree_change_30_245)<-"tree_30_245"

shrub_change_30_245<-(shrub_245_30 -shrub_2014)
names(shrub_change_30_245)<-"shrub_30_245"
herb_change_30_245<-(herb_245_30 -herb_2014)
names(herb_change_30_245)<-"herb_30_245"


tree_change_30_126<-(tree_126_30 -tree_126_20)
tree_change_30_126[intersect(tree0_126_20, tree0_126_30)]<-NA
names(tree_change_30_126)<-"tree_30_126"

shrub_change_30_126<-(shrub_126_30 -shrub_2014)
names(shrub_change_30_126)<-"shrub_30_126"
herb_change_30_126<-(herb_126_30 -herb_2014)
names(herb_change_30_126)<-"herb_30_126"

#For year 2100

tree_change_100_370<-(tree_370_100 -tree_370_20)
tree_change_100_370[intersect(tree0_370_20, tree0_370_100)]<-NA
names(tree_change_100_370)<-"tree_100_370"

shrub_change_100_370<-(shrub_370_100 -shrub_2014)
names(shrub_change_100_370)<-"shrub_100_370"
herb_change_100_370<-(herb_370_100 -herb_2014)
names(herb_change_100_370)<-"herb_100_370"


tree_change_100_245<-(tree_245_100 -tree_245_20)
tree_change_100_245[intersect(tree0_245_20, tree0_245_100)]<-NA
names(tree_change_100_245)<-"tree_100_245"

shrub_change_100_245<-(shrub_245_100 -shrub_2014)
names(shrub_change_100_245)<-"shrub_100_245"
herb_change_100_245<-(herb_245_100 -herb_2014)
names(herb_change_100_245)<-"herb_100_245"

tree_change_100_126<-(tree_126_100 -tree_126_20)
tree_change_100_126[intersect(tree0_126_20, tree0_126_100)]<-NA
names(tree_change_100_126)<-"tree_100_126"

shrub_change_100_126<-(shrub_126_100 -shrub_2014)
names(shrub_change_100_126)<-"shrub_100_126"
herb_change_100_126<-(herb_126_100 -herb_2014)
names(herb_change_100_126)<-"herb_100_126"
##############################################################################

change_rasters<-list(tree_change_100_370, tree_change_50_370, tree_change_30_370,
                     tree_change_100_245, tree_change_50_245, tree_change_30_245, 
                     tree_change_100_126, tree_change_50_126, tree_change_30_126,
                     shrub_change_100_370, shrub_change_50_370, shrub_change_30_370,
                     shrub_change_100_245, shrub_change_50_245, shrub_change_30_245, 
                     shrub_change_100_126, shrub_change_50_126, shrub_change_30_126,
                     herb_change_100_370, herb_change_50_370, herb_change_30_370,
                     herb_change_100_245, herb_change_50_245, herb_change_30_245, 
                     herb_change_100_126, herb_change_50_126, herb_change_30_126
)

facet_changes<-saveRDS(change_rasters,"C:/SOGES/report/Forest/Forest_optimistic/data/scen_change_raster.rds")

#####################################################################################################
####################################################################################################
####Calculate the difference between grazing and no grazing for all three ssps for 2030, 2050 and 2100
##Read in results from the climate only simulations for all ssps for 2030, 2050, 2100
##2030
tree_126_30<-rast_maker("ssp126_out/fac_2030_12_3.asc")
tree0_126_30<-which(tree_126_30[]==0)

tree_245_30<-rast_maker("ssp245_out/fac_2030_12_3.asc")
tree0_245_30<-which(tree_245_30[]==0)

tree_370_30<-rast_maker("ssp370_out/fac_2030_12_3.asc")
tree0_370_30<-which(tree_370_30[]==0)


##2050
tree_126_50<-rast_maker("ssp126_out/fac_2050_12_3.asc")
tree0_126_50<-which(tree_126_50[]==0)

tree_245_50<-rast_maker("ssp245_out/fac_2050_12_3.asc")
tree0_245_50<-which(tree_245_50[]==0)

tree_370_50<-rast_maker("ssp370_out/fac_2050_12_3.asc")
tree0_370_50<-which(tree_370_50[]==0)

##2100
tree_126_100<-rast_maker("ssp126_out/fac_2100_12_3.asc")
tree0_126_100<-which(tree_126_100[]==0)

tree_245_100<-rast_maker("ssp245_out/fac_2100_12_3.asc")
tree0_245_100<-which(tree_245_100[]==0)

tree_370_100<-rast_maker("ssp370_out/fac_2100_12_3.asc")
tree0_370_100<-which(tree_370_100[]==0)


## YEAR 2030
##SSP245
tree_245_30_ngr<-rast_maker("ssp245_nograzing/fac_2030_12_3.asc")
tree0_245_30_ngr<-which(tree_245_30_ngr[]==0)
tree_245_30_change<-tree_245_30_ngr - tree_245_30
tree_245_30_change[intersect(tree0_245_30_ngr,tree0_245_30)]<-NA
names(tree_245_30_change)<-"tree_245_30_ngr"

##SSP370
tree_370_30_ngr<-rast_maker("ssp370_nograzing/fac_2030_12_3.asc")
tree0_370_30_ngr<-which(tree_370_30_ngr[]==0)
tree_370_30_change<-tree_370_30_ngr - tree_370_30
tree_370_30_change[intersect(tree0_370_30_ngr,tree0_370_30)]<-NA
names(tree_370_30_change)<-"tree_370_30_ngr"

##SSP126
tree_126_30_ngr<-rast_maker("ssp126_nograzing/fac_2030_12_3.asc")
tree0_126_30_ngr<-which(tree_126_30_ngr[]==0)
tree_126_30_change<-tree_126_30_ngr - tree_126_30
tree_126_30_change[intersect(tree0_126_30_ngr,tree0_126_30)]<-NA
names(tree_126_30_change)<-"tree_126_30_ngr"

##YEAR 2050
##SSP245
tree_245_50_ngr<-rast_maker("ssp245_nograzing/fac_2050_12_3.asc")
tree0_245_50_ngr<-which(tree_245_50_ngr[]==0)
tree_245_50_change<-tree_245_50_ngr - tree_245_50
tree_245_50_change[intersect(tree0_245_50_ngr,tree0_245_50)]<-NA
names(tree_245_50_change)<-"tree_245_50_ngr"

##SSP370
tree_370_50_ngr<-rast_maker("ssp370_nograzing/fac_2050_12_3.asc")
tree0_370_50_ngr<-which(tree_370_50_ngr[]==0)
tree_370_50_change<-tree_370_50_ngr - tree_370_50
tree_370_50_change[intersect(tree0_370_50_ngr,tree0_370_50)]<-NA
names(tree_370_50_change)<-"tree_370_50_ngr"

##SSP126
tree_126_50_ngr<-rast_maker("ssp126_nograzing/fac_2050_12_3.asc")
tree0_126_50_ngr<-which(tree_126_50_ngr[]==0)
tree_126_50_change<-tree_126_50_ngr - tree_126_50
tree_126_50_change[intersect(tree0_126_50_ngr,tree0_126_50)]<-NA
names(tree_126_50_change)<-"tree_126_50_ngr"


###############################################################################################################

##YEAR 2100
##SSP245
tree_245_100_ngr<-rast_maker("ssp245_nograzing/fac_2100_12_3.asc")
tree0_245_100_ngr<-which(tree_245_100_ngr[]==0)
tree_245_100_change<-tree_245_100_ngr - tree_245_100
tree_245_100_change[intersect(tree0_245_100_ngr,tree0_245_100)]<-NA
names(tree_245_100_change)<-"tree_245_100_ngr"
##SSP370
tree_370_100_ngr<-rast_maker("ssp370_nograzing/fac_2100_12_3.asc")
tree0_370_100_ngr<-which(tree_370_100_ngr[]==0)
tree_370_100_change<-tree_370_100_ngr - tree_370_100
tree_370_100_change[intersect(tree0_370_100_ngr,tree0_370_100)]<-NA
names(tree_370_100_change)<-"tree_370_100_ngr"

##SSP126
tree_126_100_ngr<-rast_maker("ssp126_nograzing/fac_2100_12_3.asc")
tree0_126_100_ngr<-which(tree_126_100_ngr[]==0)
tree_126_100_change<-tree_126_100_ngr - tree_126_100
tree_126_100_change[intersect(tree0_126_100_ngr,tree0_126_100)]<-NA
names(tree_126_100_change)<-"tree_126_100_ngr"

###############################################################################################################

ngr_gr<-stack(tree_126_30_change, tree_245_30_change, tree_370_30_change,
                 tree_126_50_change, tree_245_50_change, tree_370_50_change,
                 tree_126_100_change, tree_245_100_change, tree_370_100_change)

#####################################################################################################
######################################################################################################
## Fire effects
## YEAR 2030
##SSP245
tree_245_30_fir<-rast_maker("ssp245_nofire/fac_2030_12_3.asc")
tree0_245_30_fir<-which(tree_245_30_fir[]==0)
tree_245_30_fchange<-tree_245_30_fir - tree_245_30
tree_245_30_fchange[intersect(tree0_245_30_fir,tree0_245_30)]<-NA
names(tree_245_30_fchange)<-"tree_245_30_nfir"

##SSP370
tree_370_30_fir<-rast_maker("ssp370_nofire/fac_2030_12_3.asc")
tree0_370_30_fir<-which(tree_370_30_fir[]==0)
tree_370_30_fchange<-tree_370_30_fir - tree_370_30
tree_370_30_fchange[intersect(tree0_370_30_fir,tree0_370_30)]<-NA
names(tree_370_30_fchange)<-"tree_370_30_nfir"

##SSP126
tree_126_30_fir<-rast_maker("ssp126_nofire/fac_2030_12_3.asc")
tree0_126_30_fir<-which(tree_126_30_fir[]==0)
tree_126_30_fchange<-tree_126_30_fir - tree_126_30
tree_126_30_fchange[intersect(tree0_126_30_fir,tree0_126_30)]<-NA
names(tree_126_30_fchange)<-"tree_126_30_nfir"

##YEAR 2050
##SSP245
tree_245_50_fir<-rast_maker("ssp245_nofire/fac_2050_12_3.asc")
tree0_245_50_fir<-which(tree_245_50_fir[]==0)
tree_245_50_fchange<-tree_245_50_fir - tree_245_50
tree_245_50_fchange[intersect(tree0_245_50_fir,tree0_245_50)]<-NA
names(tree_245_50_fchange)<-"tree_245_50_nfir"

##SSP370
tree_370_50_fir<-rast_maker("ssp370_nofire/fac_2050_12_3.asc")
tree0_370_50_fir<-which(tree_370_50_fir[]==0)
tree_370_50_fchange<-tree_370_50_fir - tree_370_50
tree_370_50_fchange[intersect(tree0_370_50_fir,tree0_370_50)]<-NA
names(tree_370_50_fchange)<-"tree_370_50_nfir"


##SSP126
tree_126_50_fir<-rast_maker("ssp126_nofire/fac_2050_12_3.asc")
tree0_126_50_fir<-which(tree_126_50_fir[]==0)
tree_126_50_fchange<-tree_126_50_fir - tree_126_50
tree_126_50_fchange[intersect(tree0_126_50_fir,tree0_126_50)]<-NA
names(tree_126_50_fchange)<-"tree_126_50_nfir"


###############################################################################################################

##YEAR 2100
##SSP245
tree_245_100_fir<-rast_maker("ssp245_nofire/fac_2100_12_3.asc")
tree0_245_100_fir<-which(tree_245_100_fir[]==0)
tree_245_100_fchange<-tree_245_100_fir - tree_245_100
tree_245_100_fchange[intersect(tree0_245_100_fir,tree0_245_100)]<-NA
names(tree_245_100_fchange)<-"tree_245_100_nfir"

##SSP370
tree_370_100_fir<-rast_maker("ssp370_nofire/fac_2100_12_3.asc")
tree0_370_100_fir<-which(tree_370_100_fir[]==0)
tree_370_100_fchange<-tree_370_100_fir - tree_370_100
tree_370_100_fchange[intersect(tree0_370_100_fir,tree0_370_100)]<-NA
names(tree_370_100_fchange)<-"tree_370_100_nfir"

##SSP126
tree_126_100_fir<-rast_maker("ssp126_nofire/fac_2100_12_3.asc")
tree0_126_100_fir<-which(tree_126_100_fir[]==0)
tree_126_100_fchange<-tree_126_100_fir - tree_126_100
tree_126_100_fchange[intersect(tree0_126_100_fir,tree0_126_100)]<-NA
names(tree_126_100_fchange)<-"tree_126_100_nfir"
##########################################################################################
nfir_fir<-stack(tree_126_30_fchange, tree_245_30_fchange, tree_370_30_fchange,
              tree_126_50_fchange, tree_245_50_fchange, tree_370_50_fchange,
              tree_126_100_fchange, tree_245_100_fchange, tree_370_100_fchange)


###############################################################################################################
#########################################################################################################
########################################################################################################
### Calculate difference between water effect and no water effect

## YEAR 2030
##SSP245
tree_245_30_wat<-rast_maker("ssp245_nowaterDREst/fac_2030_12_3.asc")
tree0_245_30_wat<-which(tree_245_30_wat[]==0)
tree_245_30_wchange<-tree_245_30_wat - tree_245_30
tree_245_30_wchange[intersect(tree0_245_30_wat,tree0_245_30)]<-NA
names(tree_245_30_wchange)<-"tree_245_30_nwat"

##SSP370
tree_370_30_wat<-rast_maker("ssp370_nowaterDREst/fac_2030_12_3.asc")
tree0_370_30_wat<-which(tree_370_30_wat[]==0)
tree_370_30_wchange<-tree_370_30_wat - tree_370_30
tree_370_30_wchange[intersect(tree0_370_30_wat,tree0_370_30)]<-NA
names(tree_370_30_wchange)<-"tree_370_30_nwat"

##SSP126
tree_126_30_wat<-rast_maker("ssp126_nowaterDREst/fac_2030_12_3.asc")
tree0_126_30_wat<-which(tree_126_30_wat[]==0)
tree_126_30_wchange<-tree_126_30_wat - tree_126_30
tree_126_30_wchange[intersect(tree0_126_30_wat,tree0_126_30)]<-NA
names(tree_126_30_wchange)<-"tree_126_30_nwat"
##############################################################################################################
##YEAR 2050
##SSP245
tree_245_50_wat<-rast_maker("ssp245_nowaterDREst/fac_2050_12_3.asc")
tree0_245_50_wat<-which(tree_245_50_wat[]==0)
tree_245_50_wchange<-tree_245_50_wat - tree_245_50
tree_245_50_wchange[intersect(tree0_245_50_wat,tree0_245_50)]<-NA
names(tree_245_50_wchange)<-"tree_245_50_nwat"

##SSP370
tree_370_50_wat<-rast_maker("ssp370_nowaterDREst/fac_2050_12_3.asc")
tree0_370_50_wat<-which(tree_370_50_wat[]==0)
tree_370_50_wchange<-tree_370_50_wat - tree_370_50
tree_370_50_wchange[intersect(tree0_370_50_wat,tree0_370_50)]<-NA
names(tree_370_50_wchange)<-"tree_370_50_nwat"


##SSP126
tree_126_50_wat<-rast_maker("ssp126_nowaterDREst/fac_2050_12_3.asc")
tree0_126_50_wat<-which(tree_126_50_wat[]==0)
tree_126_50_wchange<-tree_126_50_wat - tree_126_50
tree_126_50_wchange[intersect(tree0_126_50_wat,tree0_126_50)]<-NA
names(tree_126_50_wchange)<-"tree_126_50_nwat"


###############################################################################################################

##YEAR 2100
##SSP245
tree_245_100_wat<-rast_maker("ssp245_nowaterDREst/fac_2100_12_3.asc")
tree0_245_100_wat<-which(tree_245_100_wat[]==0)
tree_245_100_wchange<-tree_245_100_wat - tree_245_100
tree_245_100_wchange[intersect(tree0_245_100_wat,tree0_245_100)]<-NA
names(tree_245_100_wchange)<-"tree_245_100_nwat"

##SSP370
tree_370_100_wat<-rast_maker("ssp370_nowaterDREst/fac_2100_12_3.asc")
tree0_370_100_wat<-which(tree_370_100_wat[]==0)
tree_370_100_wchange<-tree_370_100_wat - tree_370_100
tree_370_100_wchange[intersect(tree0_370_100_wat,tree0_370_100)]<-NA
names(tree_370_100_wchange)<-"tree_370_100_nwat"

##SSP126
tree_126_100_wat<-rast_maker("ssp126_nowaterDREst/fac_2100_12_3.asc")
tree0_126_100_wat<-which(tree_126_100_wat[]==0)
tree_126_100_wchange<-tree_126_100_wat - tree_126_100
tree_126_100_wchange[intersect(tree0_126_100_wat,tree0_126_100)]<-NA
names(tree_126_100_wchange)<-"tree_126_100_nwat"

nwat_wat<-stack(tree_126_30_wchange, tree_245_30_wchange, tree_370_30_wchange,
                tree_126_50_wchange, tree_245_50_wchange, tree_370_50_wchange,
                tree_126_100_wchange, tree_245_100_wchange, tree_370_100_wchange)



exp_out<-list(graze = ngr_gr, fire = nfir_fir, water = nwat_wat)

saveRDS(exp_out,"C:/SOGES/report/Forest_sims/data/manage_change.rds")

