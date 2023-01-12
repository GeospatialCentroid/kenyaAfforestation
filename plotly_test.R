# test out plotly
library(plotly)

country_df <- climateManagementInputs$areaCountry$ssp126_DoNothing

county_df <- climateManagementInputs$areaCounty$ssp126_DoNothing
# filter to test county
county_baringo <- county_df %>% filter(County == "Baringo")

range <- list(min(county_df$Change, na.rm = TRUE), max(county_df$Change, na.rm = TRUE))


pal <- c("#2d4221","#32a850", "#87c7cd",  "#d8cb39")
pal <- setNames(pal, c("All", "Evergreen", "Deciduous", "New"))

plot_ly(data = country_df, y = ~Change, x = ~Year, type = "bar",
        color = ~Areas, name = ~Areas, colors = pal) %>% 
  layout(yaxis = list(title = "<b>% Change</b>", range = range),
         xaxis = list(title = "", tickfont = list(size = 22), side = "top"),
         #margin=list(l=20, r=20, t=0, b=-200),
         legend = list( x = 0.95, y = 0,
                       font = list(size = 15)))




plot_ly(data = county_baringo, y = ~Change, x = ~Year, type = "bar",
        color = ~Areas, name = ~Areas, colors = pal)  %>% 
  layout(yaxis = list(range = range))

#counties w/o data
county_df %>% filter(is.na(Change)) %>% 
  group_by(County, Year) %>% 
  dplyr::count() %>%
  filter(n > 3)

# No data for Busia, Isiolo, Mombasa, Vihiga
  
  
county_df %>% filter(County == "Busia")
