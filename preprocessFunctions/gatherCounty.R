###
# helper function for some county data filtering
# 20230217
###

# Gather function for county level data ------------------------------------
gatherCounty <- function(data){
  data <- data %>%
    tidyr::gather(key = Year, value = Change, -County, -areas )
  names(data) <- c("County", "Areas","Year", "Change")
  return(data)
}

