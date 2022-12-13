### Maps for climate change management 

# define ui ---------------------------------------------------------------
map2_UI <- function(id, panelName, county_names){
  ns <- NS(id)
  tagList( # ensures that correct html returns. Functions as a list. 
    h3(panelName),
    sidebarLayout(
      sidebarPanel(width = 3,
                   radioButtons(
                     inputId=ns("Timeline"),
                     label= tags$strong("Pick a future timeperiod:"),
                     choices = list("2041-2060" = "50",
                                    "2061-2080" = "70",
                                    "2081-2100" = "100"),
                     selected = "50"
                   ),
                   # select mapped variable 
                   selectInput(
                     inputId=ns("Layer"),
                     label=tags$strong("Pick a variable to visualize on the map:"),
                     choices = list("No Management Action" = "nothing",  ## this will need to change to match the new dataset convention
                                    "Stop Fires" = "fire"),
                     selected = "nothing"
                   ),
                   em("You can view current and near future forest Cover on via the map controls"),
                   # select County features 
                   selectInput(
                     inputId=ns("County23"), label=tags$strong("Pick a county to visualize forest cover changes:"),
                     choices = c("All", county_names), multiple = F
                   ),
                   # tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                   # tags$p(tags$strong("Click")," on a pixel within Kenya to see the county name and pixel value.")
                   ),
      mainPanel(width = 9,
        leafletOutput(ns("varchange1")),
        h3("County wide changes in tree cover"),
        plotlyOutput(ns("percentChange")),
        p("This plot summarizes the total change in tree cover throughout the country."),
        textOutput(ns("cnty3")),
        textOutput(ns("facdat3")),
        textOutput(ns("explain3"))
      
      )
    )
  )
}


# define server  ---------------------------------------------------------- 
map2_server <- function(id, histRaster, futureRaster, noChangeRasters, 
                        stopFireRasters, countyFeat
                        # ssp, will need to add once all data is present. 
                        ){
  moduleServer(id,function(input,output,session){
    

  # define raster features  -------------------------------------------------
   hist1 <- histRaster
   base1 <- futureRaster
  ## reactive elements to select the project data to show 
  #  r1 <-noChangeRasters$forest_change_rasters$X126_50_DoNothing
  
   
  # bind features to single list  
   r1 <- list(
     nothing = noChangeRasters$forest_change_rasters,
     fire = stopFireRasters$forest_change_rasters
   )
   # select object based on set name, then drop list to get raster brick
  r2 <-  reactive({r1[grep(pattern = input$Layer, x = names(r1))][[1]]})
  # select raster layer based on the selected timeline --- double brackets on raster bricks 
  r3 <-  reactive({r2()[[grep(pattern = input$Timeline, x = names(r2()))]]})
  ### this becomes the raster object to gather 

   #  generarte the map object 
    map <-reactive({ 
      leaflet(options = leafletOptions(minZoom = 5)) %>%
      setView( lng = 37.826119933082545
               , lat = 0.3347526538983459
               , zoom = 2 )%>%
      # tile providers ----------------------------------------------------------
      addProviderTiles("Stamen.Toner", group = "Light") %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap")%>%
      leaflet.extras::addResetMapButton() %>%
      # add z levels ------------------------------------------------------------
      addMapPane("Historic Data", zIndex = 406) %>%
      addMapPane("Baseline Data", zIndex = 407) %>%
      addMapPane("Projected Data", zIndex = 408) %>%
      addMapPane("Counties", zIndex = 409) %>%
  #   # add historic raster -----------------------------------------------------
      addRasterImage(hist1,
                   #colors = pal1(),
                   group = "Historic Data",
                   opacity = 1) %>%
      # add baseline raster features -----------------------------------------------------
    addRasterImage(base1,
                   #colors = pal0(),
                   # colors = pal(),
                   group = "Baseline Data",
                   opacity = 1) %>%
    # %>%
  #     # add percent change layer ------------------------------------------------
    addRasterImage(
      #r2()[[1]][[1]],
      r3(),
      layerId = "change",
      #colors = pal1(),
      # colors = pal2(),
      group = "Projected Data",
      opacity = 1
    ) %>%
      # add county features -----------------------------------------------------
    addPolygons(
      data = countyFeat,
      fillColor = "",
      fillOpacity = 0,
      color = "black",
      layerId = ~ ADMIN1,
      weight = 1.5,
      group = "Counties"
    ) %>%
      # add legend --------------------------------------------------------------
  #   # Including it as it's own control group because it applies to both temp map objects. 
  #   addLegend(
  #     "bottomright",
  #     pal = pal(),
  #     values = vals(),
  #     title = title(),
  #     #   labels = c("Low Change", "", "", "", "High Change"),
  #     opacity = 1,
  #     layerId = "firstLegend",
  #     group =  "Temperature Legend",  ### c("Historic Data","Projected Data") did not work as expected 
  #     #   na.label = "No Data"
  #     #
  #     #   # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  #   ) %>%
  #     addLegend(
  #       "bottomleft",
  #       pal = pal2(),
  #       values = vals2(),
  #       title = title2(),
  #       #   labels = c("Low Change", "", "", "", "High Change"),
  #       opacity = 1,
  #       layerId = "secondLegend",
  #       group = "Percent Change",
  #     ) %>% 
      # add control groups ------------------------------------------------------
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Light"),
      overlayGroups = c(
        "Historic Data",
        "Baseline Data",
        "Projected Data",
        "Counties"
      ),
      position = "topleft",
      options = layersControlOptions(collapsed = TRUE)
    ) 
    # %>%
  #     # Keep projected layer off by default
  #     hideGroup("Percent Change")
  #   
  })
  

  # generate the count plots  -----------------------------------------------
    # bind features to single list  
    df1 <- list(
      nothing = noChangeRasters$areas_change_df,
      fire = stopFireRasters$areas_change_df
    )
    # select df based on name 
    df2 <-  reactive({df1[grep(pattern = input$Layer, x = names(df1))][[1]]})

    p1 <-  reactive({plot_ly(data = df2(), y = ~`%Change`, x = ~Year, type = "bar",
                   color = ~Areas, name = ~Areas)})
      
  
    
    output$varchange1 <- renderLeaflet({map()})
    output$percentChange <- renderPlotly({p1()})
    output$cnty3 <- renderText("")
    }
  )
}


