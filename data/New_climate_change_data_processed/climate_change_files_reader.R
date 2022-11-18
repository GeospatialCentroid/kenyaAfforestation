###Read and inspect future climate change files.


# 20221108 
# carverd@colostate.edu
# doing some direct edits to alter the inputs for specifics features 

##Load libraries
pacman::p_load("rgdal", "raster", "sp", "stringr", "rasterVis", "RColorBrewer", "ggplot2")

##Read in .rds file with climate rasters and timeseries

c_files<-readRDS("data/New_climate_change_data_processed/climate_change_files.rds")

##Inspect climate_files

### spilt out content in specific inputs 
c_rasts <- c_files[[1]]
#project rasters 
crs(c_rasts) <- "crs value"
saveRDS(c_rasts, file = "data/climData_112022.rds")

##Change_rasters: Rasters show % change in pr, tmin and tmax for 4 scenarios over 4 time periods
##Scenarios: SSP1-2.6, SSP2-4.5, SSp3-7.0, SSP5-8.5
##Time periods: 2030: % change in pr(or tmin or tmax) from 2021 to 2040 relative to historic time period (1950 - 2014)
## Similarly for 2050 (2041 - 2060); 2070 (2061-2080); 2090 (2081-2100)

names(climate_files[[1]])

###############################################################################################

##Timeseries for tmin, tmax and pr

#tmin (mean annual minimum temperature)
tmin_series<-climate_files[[2]]
ggplot(data = tmin_series, aes(x=Year, y = Tmin, color = Scenario))+
  geom_line()+geom_point()

##tmax (mean annual maximum temperature)
tmax_series<-climate_files[[3]]
ggplot(data = tmax_series, aes(x=Year, y = Tmax, color = Scenario))+
  geom_line()+geom_point()


##Precip (total annual precipitation)
pr_series<-climate_files[[4]]
ggplot(data = pr_series, aes(x=Year, y = Precip, color = Scenario))+
  geom_line()+geom_point()

