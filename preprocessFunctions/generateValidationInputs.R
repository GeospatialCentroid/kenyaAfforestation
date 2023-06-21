###
# processes data and produces palette object for the validation steps 
# as there is only max 2 images per validation category, data and palette object
# are delivered in the same RDS object 
# carverd@colostate.edu
# 20230324
###


# process model validation rasters ---------------------------------------
#' nppVals
#'
#' @param path : location of the npp rds object 
#' @param county : sf object of Kenya Counties 
#' @param countyBuff : sf object of Kenya Counties used for bbox 
#'
#' @return List object with raster data palette and vals for measures and reference data
nppVals <- function(path, county, countyBuff){
  # read in and crop data
  npp_val <- readRDS(path) %>% 
    raster::stack() %>% 
    projClipCrop(county, countyBuff)
  # alter names of features 
  names(npp_val) <- c("npp_dif", "npp_ref")
  
  # generate Palette features 
  ## set up palette info for maps
  # colorNPP_dif <-  c(colorRampPalette(colors = c("#ca0020", "white"),space = "Lab")(abs(min(values(npp_val$npp_dif), na.rm = TRUE))),
  #                 colorRampPalette(colors = c("white","#0571b0" ),space = "Lab")(max(values(npp_val$npp_dif), na.rm = TRUE)))
  # 
  npp_dif_pal <- colorNumeric(palette = "RdBu", c(-1.9, 1.9),
                              na.color = "transparent")
  
  npp_dif_values <- values(npp_val$npp_dif)
  
  
  npp_ref_pal <- colorNumeric(palette = "Greens", values(npp_val$npp_ref),
                              na.color = "transparent")
  
  npp_ref_values <- values(npp_val$npp_ref)
  
  
  npp_val <- list(npp_val, npp_dif_pal, npp_dif_values, npp_ref_pal, npp_ref_values)
  
  names(npp_val) <- c("rasters", "npp_dif_pal", "npp_dif_values", "npp_ref_pal", "npp_ref_values")
  return(npp_val)
}


##
#' carbonVals
#'
#' @param path1 : path to the above ground carbon rds
#' @param path2 : path to the above ground carbon rds
#' @param county : sf object of Kenya Counties 
#' @param countyBuff : sf object of Kenya Counties used for bbox 
#'
#' @return : nested list of above/below values (rastes, palettes, values)
carbonVals <- function(path1, path2, county, countyBuff ){
  
  # read in and crop above ground layers 
  cAbove_val <- readRDS(path1) %>% 
    raster::stack() %>% 
    projClipCrop(county, countyBuff)
  # alter names of features 
  names(cAbove_val) <- c("cAbove_dif", "cAbove_ref")
  
  # generate Palette features 
  ## set up palette info for maps
  color_cabove_dif <-  c(colorRampPalette(colors = c("#ca0020", "#f4a582", "white"),space = "Lab")(abs(min(values(cAbove_val$cAbove_dif), na.rm = TRUE))),
                     colorRampPalette(colors = c("white", "#92c5de","#0571b0" ),space = "Lab")(max(values(cAbove_val$cAbove_dif), na.rm = TRUE)))
  
  cAbove_dif_pal <- colorNumeric(palette = color_cabove_dif, values(cAbove_val$cAbove_dif),
                              na.color = "transparent")
  
  cAbove_dif_values <- values(cAbove_val$cAbove_dif)
  
  cAbove_ref_pal <- colorNumeric(palette = "Greens", values(cAbove_val$cAbove_ref),
                              na.color = "transparent")
  
  cAbove_ref_values <- values(cAbove_val$cAbove_ref)
  
  
  cAbove_val <- list(cAbove_val, cAbove_dif_pal, cAbove_dif_values, cAbove_ref_pal, cAbove_ref_values)
  names(cAbove_val) <- c("cAbove_rasters", "cAbove_dif_pal", "cAbove_dif_values", "cAbove_ref_pal", "cAbove_ref_values")
  ### repeat the process with the below ground layers 
  # read in and crop above ground layers 
  cBelow_val <- readRDS(path2) %>% 
    raster::stack() %>% 
    projClipCrop(county, countyBuff)
  # alter names of features 
  names(cBelow_val) <- c("cBelow_dif", "cBelow_ref")
  
  # generate Palette features 
  ## set up palette info for maps
  color_cbelow_dif <-  c(colorRampPalette(colors = c("#ca0020", "#f4a582", "white"),space = "Lab")(abs(min(values(cBelow_val$cBelow_dif), na.rm = TRUE))),
                         colorRampPalette(colors = c("white", "#92c5de","#0571b0" ),space = "Lab")(max(values(cBelow_val$cBelow_dif), na.rm = TRUE)))
  
  
  cBelow_dif_pal <- colorNumeric(palette = color_cbelow_dif, values(cBelow_val$cBelow_dif),
                                 na.color = "transparent")
  
  cBelow_dif_values <- values(cBelow_val$cBelow_dif)
  
  cBelow_ref_pal <- colorNumeric(palette = "Greens", values(cBelow_val$cBelow_ref),
                                 na.color = "transparent")
  
  cBelow_ref_values <- values(cBelow_val$cBelow_ref)
  
  
  cBelow_val <- list(cBelow_val, cBelow_dif_pal, cBelow_dif_values, cBelow_ref_pal, cBelow_ref_values)
  names(cBelow_val) <- c("cBelow_rasters", "cBelow_dif_pal", "cBelow_dif_values", "cBelow_ref_pal", "cBelow_ref_values")
  
  # combine to return single object 
  carbonVals <- list(
    above = cAbove_val,
    below = cBelow_val
  )
  return(carbonVals)
}
  
  
  
  