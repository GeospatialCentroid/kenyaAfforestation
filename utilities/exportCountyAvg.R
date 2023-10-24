# script to export county climate averages to single Excle file, with a sheet for each climate scenario


library(writexl)


# read in full county averages table
county_avg <- county_avg <- readRDS("appData/countyClimAverages.RDS")


# create string of scenarios and names to use in map()
scenarios <- c("hist", "126", "245", "370", "585")

names <- c("Historic", "Optimistic", "Middle of the Road", "Pessimistic", "Extreme")


# for each scenario add as a sheet to final excel output applying the name to each sheet
walk2(
  scenarios,
  names,
  ~ county_avg %>%
    select(County, contains(.x)) %>%
    as.data.frame() %>%
    write.xlsx(
      file = "exportData/Kenya_county_climates.xlsx",
      sheetName = .y,
      append = TRUE,
      row.names = FALSE
    )
)
