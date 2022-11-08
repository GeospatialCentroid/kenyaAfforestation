### Maps for climate change in Kenya selection 

# define ui ---------------------------------------------------------------
map_UI <- function(id, panelName){
  ns <- NS(id)
  tagList( # ensures that correct html returns. Functions as a list. 
    h3(panelName),
    sidebarLayout(
                # sidebar panel -----------------------------------------------------------
                 sidebarPanel(width = 3,
                   # select future time frame 
                   radioButtons(
                     inputId=ns("Timeline"),
                     label= tags$strong("Pick a future timeperiod:"),
                     choices = list("Near term (2030)" = "2030",
                                    "Medium term (2050)" = "2050",
                                    "Long term (2100)" = "2100"),
                     selected = "2030"
                   ),
                   # select mapped variable 
                   selectInput(
                     inputId=ns("Layer"),
                     label=tags$strong("Pick a variable that you would like to visualize:"),
                     choices = list("Min Temperature" = "tmin",
                                    "Max Temperature" = "tmax",
                                    "Precipitation" = "prcp"),
                     selected = "tmin"
                     ),
                   tags$p(tags$strong("Click"), "on a pixel within Kenya to see the county name and pixel value."),
                 ),
          # main panel -------------------------------------------------------------- 
          mainPanel(width = 8,
                   leafletOutput(ns("map1")),
                   textOutput(ns("cnty")),
      )
    )
  )
}


# define server  ---------------------------------------------------------- 
map_server <- function(id, rasters, countyFeat){
  moduleServer(id,function(input,output,session){
    
    
    # not the most efficent process but it works. Tricky to get all the reactive
    # call posistions lined up so use this as a model for changes 
    n1 <- reactive({paste0(input$Layer,"_",input$Timeline)})
    index <- reactive({grep(pattern = n1(), x = names(rasters))})
    r1 <- reactive({raster(rasters[[index()]])})

    # generate legend values --------------------------------------------------
    ### might be easier to declare of of these before hand and index them similar 
    ### to how the rasters are being brought together. 

    # minval <-  reactive({r1()@data@min})
    # maxval <-  reactive({r1()@data@max})
    # dom<- reactive({c(minval(), maxval())})
    # val<- reactive({seq(minval(),maxval())})
    # colorPal <-  reactive({c(colorRampPalette(colors = c("#330000", "white"),space = "Lab")(abs(minval())),
    #                 colorRampPalette(colors = c("white", "#003300"),space = "Lab")(abs(maxval())))})
    # # Color platte think is odd,
    # pal <- reactive({ifelse(
    #   test = minval() < 0 & maxval() > 0,
    #   yes = colorNumeric(colorPal(), dom()),
    #   no = colorNumeric(colorRampPalette(colors = c("white", "#003300"),space = "Lab")(abs(maxval())), dom())
    # )})

    # this works with a direct index on raster input object. 
      map1 <- reactive({
        leaflet(options = leafletOptions(minZoom = 2)) %>%
      #set zoom levels 
        setView( lng = 37.826119933082545
                 , lat = 0.3347526538983459
                 , zoom = 4 )%>%
      # add z levels ------------------------------------------------------------
        addMapPane("data", zIndex = 408) %>%
        addMapPane("county", zIndex = 409) %>%
      # tile providers ----------------------------------------------------------
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap")%>%
        addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
        addProviderTiles("Stamen.Toner", group = "Light")%>%
        leaflet.extras::addResetMapButton() %>%
      # add raster features -----------------------------------------------------
        addRasterImage(r1(),
                       # colors = pal, 
                       group = "Data",
                       opacity = 0.9)%>%
      # add county features -----------------------------------------------------
        addPolygons(data = countyFeat, 
                    fillColor = "", 
                    fillOpacity = 0,
                    color = "black",
                    layerId = ~ADMIN1,
                    weight = 1.5,
                    group = "County")%>%
      # add control groups ------------------------------------------------------
        addLayersControl(
          baseGroups = c("Light","Dark", "OpenStreetMap"),
          overlayGroups = c(
            "Data",
            "County"
          ),
          position = "topleft", 
          options = layersControlOptions(collapsed = TRUE)
        )
      # add legend --------------------------------------------------------------
        ###
        # Need to figure out assigning a color palette before I can create a legend
        ### 
        
        # addLegend(
        #   "topright",
        #   colors = pal,
        #   title = "% change",
        #   labels = c("Low Change", "", "", "", "High Change"),
        #   opacity = 1,
        #   layerId = "firstLegend",
        #   group = "Data",
        #   na.label = "No Data"
        # 
        #   # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
        # )
        
      })
      
      
      output$map1 <- renderLeaflet({map1()})
      
      #map click
      observeEvent(input$map1_click, {
        click <- input$map1_click
        clat <- click$lat
        clon <- click$lng
        # filter data
        point <- as(st_point(x = c(clon, clat)), "Spatial")
        extractVal <- raster::extract(r1(), point)%>%
          round(digits = 2)

        output$cnty <- renderText(paste0("the value at the selected location is ",as.character(extractVal), "%")) #n1()
      })
    }
  )
}
