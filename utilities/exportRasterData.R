# script to export and organize rasters for Zenodo data publish
# All functions for exporting are written first, implementation found in last section

#' Export Climate Rasters
#' @param file : path to rds file of climate rasters
#' @param exportPath : folder location to save outputs 
#' @description Pulls the rasters in the given .RDS files and writes a series of .tifs saved to specified subfolders
exportClimateRasters <- function(file, exportPath) {
  
  # argument to create export path if does not already exist
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

  # read in .RDS file
  x <- readRDS(file)
  
  # return all elements that are raster bricks
  r1 <- list_flatten(x) %>% keep( ~ is(.x, "RasterBrick"))
  
  # grab names for export file name reference
  n1 <- names(r1)
  
  #iterate over raster brick to save each layer with appropriate name
  for (i in seq_along(n1)) {
    
    print(paste("export", n1[i]))
    
    # grab first element and all layer names
    r2 <- r1[[i]]
    n2 <- names(r2)
    
    #save each individual layer
    for (j in seq_along(n2)) {
      
      # select first layer
      r3 <- r2[[j]]
      
      # write raster to appropriate sub folder destination
      
      ## Historic
      if (str_detect(names(r3), "hist")) {
        writeRaster(
          r3,
          filename = paste0(
            exportPath,
            "/Historic/",
            n1[i],
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
            "/Optimistic/Climate/",
            n1[i],
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
            "/MiddleOfTheRoad/Climate/",
            n1[i],
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
            "/Pessimistic/Climate/",
            n1[i],
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
            "/Extreme/Climate/",
            n1[i],
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


#' Export Disturbance Rasters
#' @param file : path to rds file of disturbance/management rasters
#' @param exportPath : folder location to save outputs 
#' @description Pulls the rasters in the given .RDS files and writes a series of .tifs saved to specified subfolders
exportDisturbanceRasters <- function(file, exportPath) {
  
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
       ~ if (!dir.exists(paste0(exportPath, "/", .x, "/Disturbance"))) {
         dir.create(paste0(exportPath, "/", .x, "/Disturbance"))
       })
  
  #read in .RDS file
  x <- readRDS(file)
  
  # return all elements that are raster layers (historic and expanding tree cover)
  r_hist <- list_flatten(x) %>% keep( ~ is(.x, "RasterLayer"))
  
  # write to Historic? folder
  writeRaster(r_hist$expandedForest,
              filename = paste0(exportPath, "/Historic/Baseline_TreeCover.tif"))
  
  writeRaster(r_hist$existingForest,
              filename = paste0(exportPath, "/Historic/Historic_TreeCover.tif"))
  
  # return all raster bricks of change rasters
  r_change <- list_flatten(x) %>% keep( ~ is(.x, "RasterBrick"))
  
  # grab names to iterate over
  n1 <- names(r_change)
  
  #iterate over raster brick to save each layer with appropriate name
  for (i in seq_along(n1)) {
    
    print(paste("export",n1[i]))
    
    #grab first element and all layer names
    r2 <- r_change[[i]]
    n2 <- names(r2)
    
    #save each individual layer
    for (j in seq_along(n2)) {
      
      #select layer
      r3 <- r2[[j]]
      
      #change file names to be more accurate
      if (str_detect(names(r3), "nothing")) {
        n3 <- str_replace(names(r3), "donothing",  "currentFireSeverity")
      } else if (str_detect(names(r3), "stopfires")) {
        n3 <- str_replace(names(r3), "stopfires",  "decreaseFireSeverity")
      } else if (str_detect(names(r3), "doublefires")) {
        n3 <- str_replace(names(r3), "doublefires",  "increaseFireSeverity")
      }
      
      # write raster to appropriate sub folder destination

      ## Optimistic
      if (str_detect(n3, "126")) {
        writeRaster(
          r3,
          filename = paste0(
            exportPath,
            "/Optimistic/Disturbance/TreeCoverChange_",
            n3,
            ".tif"
          ),
          overwrite = TRUE
        )
      }
      
      ## Middle of the Road
      else if (str_detect(n3, "245")) {
        writeRaster(
          r3,
          filename = paste0(
            exportPath,
            "/MiddleOfTheRoad/Disturbance/TreeCoverChange_",
            n3,
            ".tif"
          ),
          overwrite = TRUE
        )
      }
      
      ## Pessimistic
      else if (str_detect(n3, "370")) {
        writeRaster(
          r3,
          filename = paste0(
            exportPath,
            "/Pessimistic/Disturbance/TreeCoverChange_",
            n3,
            ".tif"
          ),
          overwrite = TRUE
        )
      }
      
      ## Extreme
      else if (str_detect(n3, "585")) {
        writeRaster(
          r3,
          filename = paste0(
            exportPath,
            "/Extreme/Disturbance/TreeCoverChange_",
            n3,
            ".tif"
          ),
          overwrite = TRUE
        )
      }
    }
  }
}


#' Export Validation Rasters
#' @param file : path to rds file of validation rasters
#' @param exportPath : folder location to save outputs 
#' @description Pulls the rasters in the given .RDS files and writes about a series of .tifs saved to specified subfolders
exportValidationRasters <- function(file, exportPath) {
  
  #argument to create export path if does not already exist
  if (!dir.exists(exportPath)) {
    dir.create(exportPath)
  }
  
  # create sub folder
  if (!dir.exists(paste0(exportPath, "/Validation"))) {
    dir.create(paste0(exportPath, "/Validation"))
  }
  
  #read in .RDS file
  x <- readRDS(file)
  
  # pull all raster bricks objects
  r <- list_flatten(x) %>% keep( ~ is(.x, "RasterBrick"))
  
  # grab names to iterate over
  n1 <- names(r)
  
  #iterate over raster brick to save each layer with appropriate name
  for (i in seq_along(n1)) {
    
    print(paste("export",n1[i]))
    
    #grab first element and all layer names
    r2 <- r[[i]]
    n2 <- names(r2)
    
    #save each individual layer
    for (j in seq_along(n2)) {
      
      #select layer
      r3 <- r2[[j]]
      
      # write raster
      writeRaster(
        r3,
        filename = paste0(
          exportPath,
          "/Validation/",
          names(r3),
          ".tif"
        ),
        overwrite = TRUE
      )
    }
  }
}


#' Export filenames 
#' @param exportPath : folder path of the exported datasets 
#' @return : csv writen to folder path 
gatherFilesNames <- function(exportPath){
  files <- list.files(path = exportPath, pattern = ".tif", recursive = TRUE)
  df <- data.frame(names = files)
  write.csv(df, paste0(exportPath,"/filenames.csv"))
}


# implement ------------------------------------------------------------
exportClimateRasters(file = "appData/climateChangeInputs.RDS", exportPath = "exportData/")
exportDisturbanceRasters(file = "appData/climateManagementInputs.RDS", exportPath = "exportData/")
exportValidationRasters(file = "appData/validationInputs_carbon.RDS", exportPath = "exportData/")
exportValidationRasters(file = "appData/validationInputs_npp.RDS", exportPath = "exportData/")

gatherFilesNames(exportPath = "exportData/")
