setwd("C:/GIS/SOGES/Pop/ssp_processed")

rural_30_126<-raster("ssp1_30_rural.tif")
names(rural_30_126)<-"126_30_rural"

rural_50_126<-raster("ssp1_50_rural.tif")
names(rural_50_126)<-"126_50_rural"

rural_100_126<-raster("ssp1_100_rural.tif")
names(rural_100_126)<-"126_100_rural"


urban_30_126<-raster("ssp1_30_urban.tif")
names(urban_30_126)<-"126_30_urban"

urban_50_126<-raster("ssp1_50_urban.tif")
names(urban_50_126)<-"126_50_urban"

urban_100_126<-raster("ssp1_100_urban.tif")
names(urban_100_126)<-"126_100_urban"

###############################################################################################################

rural_30_245<-raster("ssp2_30_rural.tif")
names(rural_30_245)<-"245_30_rural"

rural_50_245<-raster("ssp2_50_rural.tif")
names(rural_50_245)<-"245_50_rural"

rural_100_245<-raster("ssp2_100_rural.tif")
names(rural_100_245)<-"245_100_rural"


urban_30_245<-raster("ssp2_30_urban.tif")
names(urban_30_245)<-"245_30_urban"

urban_50_245<-raster("ssp2_50_urban.tif")
names(urban_50_245)<-"245_50_urban"

urban_100_245<-raster("ssp2_100_urban.tif")
names(urban_100_245)<-"245_100_urban"

###########################################################################################################

rural_30_370<-raster("ssp3_30_rural.tif")
names(rural_30_370)<-"370_30_rural"

rural_50_370<-raster("ssp3_50_rural.tif")
names(rural_50_370)<-"370_50_rural"

rural_100_370<-raster("ssp3_100_rural.tif")
names(rural_100_370)<-"370_100_rural"


urban_30_370<-raster("ssp3_30_urban.tif")
names(urban_30_370)<-"370_30_urban"

urban_50_370<-raster("ssp3_50_urban.tif")
names(urban_50_370)<-"370_50_urban"

urban_100_370<-raster("ssp3_100_urban.tif")
names(urban_100_370)<-"370_100_urban"


pop<-stack(rural_30_126, rural_50_126, rural_100_126,
           urban_30_126, urban_50_126, urban_100_126,
           rural_30_245, rural_50_245, rural_100_245,
           urban_30_245, urban_50_245, urban_100_245,
           rural_30_370, rural_50_370, rural_100_370,
           urban_30_370, urban_50_370, urban_100_370)



pop_hist_rural<-raster("base_rural.tif")
pop_hist_urban<-raster("base_urban.tif")


pop_change_rural<-stack(rural_30_126, rural_50_126, rural_100_126,
                        rural_30_245, rural_50_245, rural_100_245,
                        rural_30_370, rural_50_370, rural_100_370) - pop_hist_rural

names(pop_change_rural)<-c("126_30_rural", "126_50_rural", "126_100_rural",
                           "245_30_rural", "245_50_rural", "245_100_rural",
                           "370_30_rural", "370_50_rural", "370_100_rural")


pop_change_urban<-stack(urban_30_126, urban_50_126, urban_100_126,
                        urban_30_245, urban_50_245, urban_100_245,
                        urban_30_370, urban_50_370, urban_100_370) - pop_hist_urban

names(pop_change_urban)<-c("126_30_urban", "126_50_urban", "126_100_urban",
                           "245_30_urban", "245_50_urban", "245_100_urban",
                           "370_30_urban", "370_50_urban", "370_100_urban")

population<-list(pop=pop, popchange_urban = pop_change_urban, popchange_rural = pop_change_rural)

saveRDS(population, "C:/SOGES/report/Forest_sims/data/pop_change.rds")
