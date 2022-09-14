#####################################################################################
#################FOREST COVER SCENARIOS#######################################
####################################################################################
##Set workign directory and load libraries
rm(list=ls())
#setwd("C:/GIS/SOGES/concensus_lc")
pack<-list("raster", "rgdal", "ggplot2", "sp", "RColorBrewer", "colorspace")
lapply(pack, "require", character.only = TRUE)

## Read in base data for evergreen and deciduous forest layers

ever<-raster("C:/GIS/SOGES/Everdecid/evergreen_pro_aoi.tif")
decid<-raster("C:/GIS/SOGES/Everdecid/deciduous_pro_aoi.tif")
urban<- raster("C:/GIS/SOGES/Concensus_lc/urban_res_aoi.tif")
ag <- raster("C:/GIS/SOGES/Concensus_lc/ag_res_aoi.tif")
herb<-raster("C:/GIS/SOGES/Concensus_lc/herb_res_aoi.tif")
plot(ag + ever + decid + urban)

## Create a raster of total ever and decid cover
alltrees<-sum(ever, decid)
plot(alltrees)

writeRaster(alltrees, "C:/SOGES/L_Range/Scenario files/Ori_tot_100.tif", 
            datatype = "INT4S",overwrite= TRUE)

###Estimate forest cover at a 30% threshold across Kenya
##read Kenya boundary
ken<-readOGR("C:/GIS/SOGES/FEWS/KE_LHZ_2011/Kenya_pro.shp")
ken_mask<-raster("C:/SOGES/L_range/Scenario files/ken_mask.tif") # Read in a mask file for Kenya

# Extract alltree raster for Kenya

tree_ken<-alltrees * ken_mask
plot(tree_ken)
plot(ken,add=TRUE)


temp_cover<-sum(tree_ken[])  ## total area in sq km
temp_cover/area(ken) * (10^6)  # Divide by area of Kenya 
# Forested areas thresholded at greater than 30% together account for 4 % of land area. 

## % evergreen cover in Kenya

ev_ken<-ever*ken_mask
plot(ev_ken)
plot(ken, add= TRUE)
ev_cover<-sum(ev_ken []) 
ev_cover / area(ken) * (10^6)

## % deciduous cover in Kenya
decid_ken<-decid*ken_mask
plot(decid_ken)
decid_cover<-sum(decid_ken[]) 
decid_cover/ area(ken) * (10^6)

###Create a raster template showing areas with < 25% cover, 25%-50% cover, 50-75% cover and greater 
# than 75% cover classified based on whther deciduous cover greater or less than evergreen cover
##############################################################################################
############################################################################################
#################################################################################################

## Classify tree cover areas based on extent
alt25<-which(alltrees[]<25)
a2550<-which(alltrees[]>=25 & alltrees[] < 50)
a5075<-which(alltrees[]>=50 & alltrees[] <75)
agt75<-which(alltrees[] >= 75)

## Identify evergreen and deciduous domianted areas
everdom<-which(ever[] > decid[])
decidom<-which(decid[] > ever[])

##Identify areas with no urban cover
urb0<-which(urban[] == 0)

##Identify areas within Kenya
ken1<-which(ken_mask[] == 1)

##Identify areas with ag >=30 

ag30<-which(ag[] >=30)

## Ag greater than equal to 25
ag25<-which(ag[] >=25)

## Ag cover is less than equal to 25%
aglt25<-which(ag[]>0 & ag[]<=25)

## Ag cover is greater that 0%
aggt0<-which(ag[]>0)

##Grass cover areas

#Grass cover is 50 - 75 % 

h5075<-which(herb[] >= 50 & herb [] < 75)

###############################################################################################
## Scenario 1: Add 30 % to all areas with 25-50%, 25 % to all areas with 50-75% 

#Identify cells where evergreen cover dominates and total forest is 25 to 50 %, where urban cover = 0 and
## Where agricultural cover is >=30%

scen1a<-intersect(intersect(intersect(intersect (a2550,ken1),everdom),ag30),urb0)

#Identify cells where evergreen cover dominates and total forest is 50 to 75%, where urban cover = 0 and
## Where agricultural cover is >=25%
scen1b<-intersect(intersect(intersect(intersect (a5075,ken1),everdom),ag25),urb0)

#Identify cells where evergreen cover dominates and total forest is greater than 75%, where urban cover = 0 and
## Where agricultural cover is <=25%
scen1c<-intersect(intersect(intersect(intersect (agt75,ken1),everdom),aglt25),urb0)


ever_cover<-ever
ever_cover[scen1a]<- ever_cover[scen1a]+30
ever_cover[scen1b]<-ever_cover[scen1b]+25
ever_cover[scen1c]<-ever_cover[scen1c]+ag[scen1c]
#plot(ever_cover)

ever_cover_ken<-ever_cover * ken_mask
ever_cover_ken[which(ever_cover_ken[] > 100)]<-100
plot(ever_cover_ken)
plot(ken,add=TRUE)

temp_cover<-sum(ever_cover_ken[ever_cover_ken > 0])
temp_cover/area(ken) * (10^6)

#writeRaster(ever_cover, "C:/SOGES/L_Range/Scenario files/Scen1incever100.tif", overwrite= TRUE)

################################################################################################################
############################################################################################
##############################################################################################
### NEW SCENARIO TESTED FOR FOREST_SIMS SHINY APP
## Dramatically afforest areas where evergreen forests are dominant by converting ag to forest

new_scen<-intersect(everdom,aggt0)

ever_cover[new_scen]<-ever_cover[new_scen] + ag[new_scen]
ever_cover[which(ever_cover[] > 100)]<-100

ever_cover_ken<-ever_cover * ken_mask
ever_cover_ken[which(ever_cover_ken[] > 100)]<-100
plot(ever_cover_ken)
plot(ken,add=TRUE)

temp_cover<-sum(ever_cover_ken[ever_cover_ken > 0])
temp_cover/area(ken) * (10^6)

writeRaster(ever_cover, "C:/SOGES/L_Range_NoEdits/L_Range/Layers/ever_forest_sims.asc", overwrite= TRUE)

#####################################################################################################
## Dramatically afforest areas where deciduous forests are dominant by converting ag to forest
new_scen_d<-intersect(decidom, aggt0)

decid_cover<-decid
decid_cover[new_scen_d]<-decid_cover[new_scen_d]+ag[new_scen_d]

decid_cover[which(decid_cover[]>100)]<-100
decid_cover_ken<-decid_cover * ken_mask
plot(decid_cover_ken)
plot(ken,add=TRUE)

temp_cover<-sum(decid_cover_ken[], na.rm= TRUE)
temp_cover/area(ken) * (10^6)

writeRaster(decid_cover, "C:/SOGES/L_Range_NoEdits/L_Range/Layers/decid_forest_sims.asc", overwrite= TRUE)


#############################################################################
#############################################################################
# Scenarios for deciduous cover

## Scenario 1: Add 30 % to all areas with 25-50%, 25 % to all areas with 50-75% and areas
## with greater than 75 % cover maximized to 100 % where ag lands are available

#Identify cells where decid cover domiantes and total forest is 25 to 50 %, where urban cover = 0 and
## Where agricultural cover is >=30%

scen1d<-intersect(intersect(intersect(intersect (a2550,ken1),decidom),ag30),urb0)

#Identify cells where decid cover domiantes and total forest is 50 to 75%, where urban cover = 0 and
## Where agricultural cover is >=25%
scen1e<-intersect(intersect(intersect(intersect (a5075,ken1),decidom),ag25),urb0)

#Identify cells wher decid cover domiantes and total forest is greater than 75%, where urban cover = 0 and
## Where agricultural cover is <=25%
scen1f<-intersect(intersect(intersect(intersect (agt75,ken1),decidom),aglt25),urb0)

#Identify cells wher decid cover domiantes and total forest is greater than 75%, where urban cover = 0 and
## Where herb cover is 50-75%

scen1g<-intersect(intersect(intersect(intersect (alt25,ken1),decidom),h5075),urb0)

##Increase deciduous cover
decid_cover<-decid

decid_cover[scen1d]<-decid_cover[scen1d]+50 # + 30
decid_cover[scen1e]<-decid_cover[scen1e]+25
decid_cover[scen1f]<-decid_cover[scen1f]+ag[scen1f]
decid_cover[scen1g]<-decid_cover[scen1g]+30 ## Add for Scenario 2
#plot(decid_cover)

decid_cover_ken<-decid_cover * ken_mask
plot(decid_cover_ken)
plot(ken,add=TRUE)

temp_cover<-sum(decid_cover_ken[decid_cover_ken > 30])
temp_cover/area(ken) * (10^6)

#writeRaster(decid_cover, "C:/SOGES/L_Range/Scenario files/Scen2incdecid100.tif", overwrite= TRUE)


## Scenario 3: All areas with 25-75 % cover lose 10 % cover in evergreen and deciduous dominated
## forests

ever_cover<-ever

scen3a<-(intersect(intersect (a2550,ken1),everdom))
scen3b<-(intersect(intersect (a5075,ken1),everdom))
scen3e<-(intersect(intersect (agt75,ken1),everdom))

ever_cover[scen3a]<-ever_cover[scen3a]-5
ever_cover[scen3b]<-ever_cover[scen3b]-5
ever_cover[scen3e]<-ever_cover[scen3e]-5

ever_cover_ken<-ever_cover * ken_mask
plot(ever_cover_ken)
plot(ken,add=TRUE)

temp_cover<-sum(ever_cover_ken[ever_cover_ken > 30])
temp_cover/area(ken) * (10^6)


#writeRaster(ever_cover, "C:/SOGES/L_Range/Scenario files/Scen3decev100.tif", overwrite= TRUE)

######################################################################
##Deciduous

decid_cover<-decid

scen3c<-(intersect(intersect (a2550,ken1),decidom))
scen3d<-(intersect(intersect (a5075,ken1),decidom))
scen3f<-(intersect(intersect (agt75,ken1),decidom))

decid_cover[scen3c]<-decid_cover[scen3c]-5
decid_cover[scen3d]<-decid_cover[scen3d]-5
decid_cover[scen3f]<-decid_cover[scen3f]-5

decid_cover_ken<-decid_cover * ken_mask
plot(decid_cover_ken)
plot(ken,add=TRUE)

temp_cover<-sum(decid_cover_ken[decid_cover_ken > 30])
temp_cover/area(ken) * (10^6)

#writeRaster(decid_cover, "C:/SOGES/L_Range/Scenario files/Scen3decdec100.tif", overwrite= TRUE)
#######################################################################
#######################################################################

##create a very optimistic forest cover scenario where evergreen and 
##deciduous forest cover all over kenya is doubled

###Evergreencover
##Identify evergreen forest patches in Kenya without deciduous cover

evercov<-which(ever[]>0)
nodecov<-which(decid[]==0)
everken<-intersect(intersect(nodecov, ken1),evercov)

ever_cover<-ever
ever_cover[everken]<- 100 #ever_cover[everken]*2
ever_cover[ever_cover >100]<-100
plot(ever_cover)

ever_cover_ken<-ever_cover * ken_mask
plot(ever_cover_ken)
plot(ken,add=TRUE)

temp_cover<-sum(ever_cover_ken[ever_cover_ken > 0])
temp_cover/area(ken) * (10^6)

#writeRaster(ever_cover, "C:/SOGES/L_Range/Scenario files/Optimincever100.tif",
            datatype = "INT4S",overwrite= TRUE)
#writeRaster(test, "C:/SOGES/L_Range/Scenario files/Optincev100_diff.tif",
            datatype = "INT4S",overwrite= TRUE)

#####################################################################
decov<-which(decid[]>0)
noev<-which(ever[]==0)
decken<-intersect(intersect(noev, ken1),decov)

decid_cover<-decid
decid_cover[decken]<- decid_cover[decken]*2
decid_cover[decid_cover >100]<-100
plot(decid_cover)

decid_cover_ken<-decid_cover * ken_mask
plot(decid_cover_ken)
plot(ken,add=TRUE)

temp_cover<-sum(decid_cover_ken[decid_cover_ken > 0])
temp_cover/area(ken) * (10^6)


writeRaster(decid_cover, "C:/SOGES/L_Range/Scenario files/Optimincdec100.tif", 
            datatype = "INT4S",overwrite= TRUE)

test_dec<-decid_cover-decid

writeRaster(test_dec, "C:/SOGES/L_Range/Scenario files/Optincdec_diff.tif", 
            datatype = "INT4S",overwrite= TRUE)

###########################################################################
##Total forest cover
tot_new<-ever_cover + decid_cover

writeRaster(tot_new, "C:/SOGES/L_Range/Scenario files/Opt_tot_100.tif", 
            datatype = "INT4S",overwrite= TRUE)

tot_new_ken<-tot_new * ken_mask
plot(tree_ken)
plot(ken,add=TRUE)


temp_new_cover<-sum(tot_new_ken[])  ## total area in sq km
temp_new_cover/area(ken) * (10^6) 


############################################################################
#############################################################################
#########################################################################
### Read in the Hansen dataet of forest loss to determine if
##forest gains are occuring in areas of loss between 2000 and 2020

loss<-raster("C:/SOGES/L_Range/Scenario files/H_loss_re_AOI.tif")
loss_1k<-raster("C:/SOGES/L_Range/Scenario files/Hansen_loss_pro_AOI.tif")

losstk_ken<-crop(loss_1k,ken)
sum(losstk_ken[])/area(ken) * (10^6)
plot(losstk_ken)
plot(ken,add= TRUE)


############################################################################
##########################################################################
## Identifying areas where increase in forest cover occurs across Kenya under the scenarios

## Read in Kenya county dataset

county<-readOGR("C:/GIS/SOGES/FEWS/County/KE_Admin1_pro.shp")

county_names<-county@data$ADMIN1

ever_county<-extract(ever, county, fun = sum, sp = TRUE)
ever_dat_county<-ever_county@data$ever_pro_aoi

ever_change_county<-extract(ever_cover, county, fun= sum, sp =TRUE)
everchange_dat_county<-ever_change_county@data$ever_pro_aoi

ever_change1<-((everchange_dat_county - ever_dat_county)/ever_dat_county)* 100


## Deciduous
decid_county<-extract(decid, county, fun = sum, sp = TRUE)
decid_dat_county<-decid_county@data$decid_pro_aoi

##Scenario 1
decid_change_county<-extract (decid_cover, county, fun=sum, sp =TRUE)
decidchange_dat_county<-decid_change_county@data$decid_pro_aoi

decid_change1<-((decidchange_dat_county - decid_dat_county)/decid_dat_county)* 100

##Scenario 2

decid_change_county2<-extract (decid_cover, county, fun=sum, sp =TRUE)
decidchange_dat_county2<-decid_change_county2@data$decid_pro_aoi

decid_change2<-((decidchange_dat_county2 - decid_dat_county)/decid_dat_county)* 100

##Data table

change_table<-data.frame(County = county_names, Ever_change1 = ever_change1,
                         Decid_Change1 = decid_change1,
                         Decid_Change2 = decid_change2)

change_table$Decid_Change2<-decid_change_county

#################################################################################
###Total forest cover 

totforest_scenario1<-ever_cover + decid_cover
totforest_scenario1[which(totforest_scenario1[] > 100)]<-100

totforest_scenario2<-ever_cover + decid_cover
totforest_scenario2[which(totforest_scenario2[] > 100)]<-100

totforest_scenario3<-ever_cover+ decid_cover
totforest_scenario3[which(totforest_scenario3[] > 100)]<-100

totbaseforest<-ever+decid
totbaseforest[which(totbaseforest[] > 100)]<-100


#writeRaster(totforest_scenario2, "C:/SOGES/L_Range/Scenario files/totforest2.tif", overwrite= TRUE)
##################################################################################################################################

### Plot forest cover scenarios

x<-sequential_hcl(9, palette = "Emrld")
#x<-c("khaki" , "tan1", "sienna4", "mediumseagreen","turquoise4", "seagreen4")

jpeg("C:/SOGES/L_Range/Scenario files/scenario_maps.jpg", quality = 100, width=800, height = 600)

par(mfrow = c(2,2))
par (mar= c(5,4,4,2))
#Base
scenb<-crop(totbaseforest, ken)
plot(scenb, col = rev(x), main = "Baseline Scenario")
plot(county, add= TRUE, border = "black")

#1
scen1<-crop(totforest_scenario1, ken)
plot(scen1, col = rev(x) , main = "Scenario 1")
plot(county, add= TRUE, border = "black")

#2
scen2<-crop(totforest_scenario2, ken)
plot(scen2, col = rev(x), main = "Scenario 2")
plot(county, add= TRUE, border = "black")

#3

scen3<-crop(totforest_scenario3, ken)
plot(scen3, col = rev(x), main = "Scenario 3")
plot(county, add= TRUE, border = "black")


dev.off()
