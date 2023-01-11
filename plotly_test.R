# test out plotly

country_df <- climateManagementInputs$areaCountry$ssp126_DoNothing

county_df <- climateManagementInputs$areaCounty$ssp126_DoNothing
# filter to test county
county_baringo <- county_df %>% filter(County == "Baringo")



pal <- c("#2d4221","#32a850", "#87c7cd",  "#d8cb39")
pal <- setNames(pal, c("All", "Evergreen", "Deciduous", "New"))

plot_ly(data = country_df, y = ~Change, x = ~Year, type = "bar",
        color = ~Areas, name = ~Areas, colors = pal) %>% 
  layout(yaxis = list(range = range))


range <- list(min(county_df$Change, na.rm = TRUE), max(county_df$Change, na.rm = TRUE))


plot_ly(data = county_baringo, y = ~Change, x = ~Year, type = "bar",
        color = ~Areas, name = ~Areas, colors = pal)  %>% 
  layout(yaxis = list(range = range))
