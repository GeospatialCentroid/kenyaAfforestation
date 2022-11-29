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
                     choices = list("2021-2040" = "30",  # these will need to change to match the new input dataset naming convention
                                    "2041-2060" = "50",
                                    "2061-2080" = "70",
                                    "2081-2100" = "90"),
                     selected = "30"
                   ),
                   # select mapped variable 
                   selectInput(
                     inputId=ns("Layer"),
                     label=tags$strong("Pick a variable that you would like to visualize:"),
                     choices = list("Min Temperature" = "tmin",  ## this will need to change to match the new dataset convention
                                    "Max Temperature" = "tmax",
                                    "Precipitation" = "pr"),
                     selected = "tmin"
                     ),
                   tags$p(tags$strong("Click"), "on a pixel within Kenya to see the county name and pixel value."),
                 ),
          # main panel -------------------------------------------------------------- 
          mainPanel(width = 8,
                   leafletOutput(ns("map1")),
                   h3(textOutput(ns("cnty"))),
      )
    )
  )
}


# define server  ---------------------------------------------------------- 
map_server <- function(id,histRasters,sspRasters,ssp,countyFeat,histPal, sspPal){
  moduleServer(id,function(input,output,session){
    # filter for the historic data
    index0 <- reactive({grep(pattern = input$Layer, x = names(histRasters))})
    r0 <-reactive({raster(histRasters[[index0()]])})
    # historic palette
    index0p <-reactive({grep(pattern = input$Layer, x = names(histPal))})
    pal0 <-reactive({histPal[[index0p()]]})
    # not the most efficent process but it works. Tricky to get all the reactive
    # call posistions lined up so use this as a model for changes 
    n1 <- reactive({paste0(input$Layer,"_",ssp,"_",input$Timeline)})
    index1 <- reactive({grep(pattern = n1(), x = names(sspRasters))})
    r1 <- reactive({raster(sspRasters[[index1()]])})
    # ssp palette
    index1p <-reactive({grep(pattern = n1(), x = names(sspPal))})
    pal1 <-reactive({sspPal[[index1p()]]})
    
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
        leaflet(options = leafletOptions(minZoom = 4)) %>%
      #set zoom levels 
        setView( lng = 37.826119933082545
                 , lat = 0.3347526538983459
                 , zoom = 5 )%>%
      # add z levels ------------------------------------------------------------
        addMapPane("histData", zIndex = 407) %>%
        addMapPane("data", zIndex = 408) %>%
        addMapPane("county", zIndex = 409) %>%
      # tile providers ----------------------------------------------------------
        addProviderTiles("Stamen.Toner", group = "Light")%>%
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap")%>%
        leaflet.extras::addResetMapButton() %>%
      # add ssp raster features -----------------------------------------------------
        addRasterImage(r1(),
                       colors = pal1(), 
                       group = "Data",
                       opacity = 0.9)%>%
      # add historic raster -----------------------------------------------------
        addRasterImage(r0(),
                       colors = pal0(), 
                       group = "histData",
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
          baseGroups = c("OpenStreetMap","Light"),
          overlayGroups = c(
            "histData",
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
        # I need a historic and current value
        # current
        extractVal1 <- raster::extract(r1(), point)%>%
          round(digits = 2)
        #historic
        extractVal0 <- raster::extract(r0(), point)%>%
          round(digits = 2)
        
        # condition for setting the label based on input value 
        label1 <- reactive({
          if(input$Layer == "pr"){
            "mm"
          }else{
            "c"
          }
        })
        
        output$cnty <- renderText(paste0("The historic value at the selected location is "
                                         ,as.character(extractVal0)," ",label1(),
                                  ". The future projected value is "
                                  ,as.character(extractVal1)," ",label1()))#n1()
      })
    }
  )
}
