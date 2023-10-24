# tied the function and implementation into a single file



# testing  ----------------------------------------------------------------
# file = "~/Documents/kenyaAfforestation/appData/climateChangeInputs.RDS"
# # fivle = "~/Documents/kenyaAfforestation/appData/validationInputs_carbon.RDS" # stored as nested list 
# exportPath = "exportData"
# 
# exportRaster(file = rdsFiles,
#              exportPath = exportPath)
# 


# implementing ------------------------------------------------------------
files <- list.files("~/Documents/kenyaAfforestation/appData",
                    pattern = ".RDS",
                    full.names = TRUE)
purrr::map(.x = files, .f = exportRaster, .progress = TRUE, exportPath = exportPath)

#this doesnt work for features that are stored within nested lists. doing direclty here
file1 <- readRDS("~/Documents/kenyaAfforestation/appData/validationInputs_carbon.RDS")
file2 <- readRDS("~/Documents/kenyaAfforestation/appData/validationInputs_npp.RDS")
files <- c(file1$above, file1$below, file2$rasters)
# call agian with specieis files. 
exportRaster(file = files, exportPath = exportPath)



#' Title
#'
#' @param exportPath : folder location to save outputs 
#' @param file : path to rds file of interest 
#'
#' @description
#' grabs an RDS or list from the appDate file and writes about a series of .tifs that can be share on a non R environment
#' 
#' @export A series of tifs
exportRaster <- function(file, exportPath){
  # condition to account for the nested RDS files 
  if(class(file)=="character"){
    r1 <- readRDS(file)  
  }else{
    r1 <- file
  }
  # grab names for export file name reference 
  names <- names(r1)
  # test if features inside the rds are tif files 
  ### this will change if we need to rework everything to a terra object 
  index <- c()
  for(i in seq_along(names)){
    r2 <- r1[[i]]
    # raster bricks
    if(class(r2)[1]=="RasterBrick"){
      index <- c(index,i)
    }
  }
  print(paste0("export data for ",length(index), " rds files"))
  # itorate over raster objects and export
  for(i in index){
    print(paste0("export data from ", names[i]))
    # grab first element 
    r2 <- r1[[i]]
    n1 <- names(r2) 
    for(j in seq_along(n1)){
      # select first layer
      r3 <- r2[[j]]
      # contrust the file name 
      fname <- paste0(exportPath,"/",names[i],"_",names(r3),".tif")
      raster::writeRaster(x = r3, filename = fname, overwrite = TRUE)
    }
  }
}


#' export file names 
#'
#' @param exportPath : folder path of the exported datasets 
#'
#' @return : csv writen to folder path 
gatherFilesNames <- function(exportPath){
  files <- list.files(path = exportPath, pattern = ".tif")
  df <- data.frame(names = files)
  write.csv(df, paste0(exportPath,"/filenames.csv"))
}




