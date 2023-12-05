# script to export and organize rasters for Zenodo data publish



# testing  ----------------------------------------------------------------
# file = "~/Documents/kenyaAfforestation/appData/climateChangeInputs.RDS"
# # fivle = "~/Documents/kenyaAfforestation/appData/validationInputs_carbon.RDS" # stored as nested list 
# exportPath = "exportData"
# 
# exportRaster(file = rdsFiles,
#              exportPath = exportPath)
# 


#' Export rasters
#'
#' @param exportPath : folder location to save outputs 
#' @param file : path to rds file of interest 
#'
#' @description
#' grabs an RDS or list from the appDate file and writes about a series of .tifs that can be share on a non R environment
#' 
#' @export A series of tifs
exportClimateRasters <- function(file, exportPath) {
  #argument to create export path if does not already exist
  if (!dir.exists(exportPath)) {
    dir.create(exportPath)
  }
  
  # create sub folders
  
  ## strings of folder names
  sub <-
    c("Historic",
      "Optimistic",
      "MiddleOfTheRoad",
      "Pessimistic",
      "Extreme")
  
  walk(sub,
       ~ if (!dir.exists(paste0(exportPath, "/", .x))) {
         dir.create(paste0(exportPath, "/", .x))
       })
  
  walk(sub[-1],
       ~ if (!dir.exists(paste0(exportPath, "/", .x, "/Climate"))) {
         dir.create(paste0(exportPath, "/", .x, "/Climate"))
       })
  
  
  
  #read in .RDS file
  x <- readRDS(file)
  
  # return all elements that are raster bricks
  r1 <- x %>% keep( ~ is(.x, "RasterBrick"))
  
  # if empty, pluck from second level (for validation sets)
  if (is_empty(r1)) {
    r1 <-   pluck(x, 2) %>% keep( ~ is(.x, "RasterBrick"))
  }
  
  
  # grab names for export file name reference
  r1_names <- names(r1)
  
  
  #iterate over raster brick to save each layer with appropriate name
  for (i in seq_along(r1_names)) {
    print(paste0("export data from ", r1_names[i]))
    # grab first element and all layer names
    r2 <- r1[[i]]
    n1 <- names(r2)
    
    #save each individual layer
    for (j in seq_along(n1)) {
      # select first layer
      r3 <- r2[[j]]
      
      # write raster to appropriate sub folder destination
      
      ## Historic
      if (str_detect(names(r3), "hist")) {
        writeRaster(
          r3,
          filename = paste0(
            exportPath,
            "/",
            "Historic/",
            r1_names[i],
            "_",
            names(r3),
            ".tif"
          ),
          overwrite = TRUE
        )
        
      }
      
      ## Optimistic
      else if (str_detect(names(r3), "126")) {
        writeRaster(
          r3,
          filename = paste0(
            exportPath,
            "/",
            "Optimistic/Climate/",
            r1_names[i],
            "_",
            names(r3),
            ".tif"
          ),
          overwrite = TRUE
        )
        
      }
      
      ## Middle of the Road
      else if (str_detect(names(r3), "245")) {
        writeRaster(
          r3,
          filename = paste0(
            exportPath,
            "/",
            "MiddleOfTheRoad/Climate/",
            r1_names[i],
            "_",
            names(r3),
            ".tif"
          ),
          overwrite = TRUE
        )
        
      }
      
      ## Pessimistic
      else if (str_detect(names(r3), "370")) {
        writeRaster(
          r3,
          filename = paste0(
            exportPath,
            "/",
            "Pessimistic/Climate/",
            r1_names[i],
            "_",
            names(r3),
            ".tif"
          ),
          overwrite = TRUE
        )
        
      }
      
      ## Extreme
      else if (str_detect(names(r3), "585")) {
        writeRaster(
          r3,
          filename = paste0(
            exportPath,
            "/",
            "Extreme/Climate/",
            r1_names[i],
            "_",
            names(r3),
            ".tif"
          ),
          overwrite = TRUE
        )
        
      }
      
    }
  }
}


#' Export filenames 
#'
#' @param exportPath : folder path of the exported datasets 
#'
#' @return : csv writen to folder path 
gatherFilesNames <- function(exportPath){
  files <- list.files(path = exportPath, pattern = ".tif")
  df <- data.frame(names = files)
  write.csv(df, paste0(exportPath,"/filenames.csv"))
}


# implement ------------------------------------------------------------
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

# test new workflow
exportClimateRasters(file = "appData/climateChangeInputs.RDS", exportPath = "test/")



