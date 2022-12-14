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
                     label=tags$strong("Pick a variable to visualize on the map:"),
                     choices = list("Min Temperature" = "tmin",  ## this will need to change to match the new dataset convention
                                    "Max Temperature" = "tmax",
                                    "Precipitation" = "pr"),
                     selected = "tmin"
                     ),
                   em("You can view Percent Change by turning the layer on via the map controls"),
                   # choose between actual value and percent change
                   ## can't get switch to function correctly
                   # materialSwitch(
                   #   inputId = ns("percentLayer"),
                   #   label = "View Percent Change",
                   #   status = "danger"
                   # ),
                   hr(),
                   tags$p(tags$strong("Click"), "on a pixel within Kenya to see value:"),
                   h5(htmlOutput(ns("cnty")))
                 ),
          # main panel -------------------------------------------------------------- 
          mainPanel(width = 9,
                   leafletOutput(ns("map1"), width="100%",height="500px")
      )
    )
  )
}


# define server  ---------------------------------------------------------- 
map_server <- function(id, histRasters, sspRasters, changeRasters, ssp,
                       countyFeat, 
                       #histPal, sspPal, 
                       pals1, pals2){
  moduleServer(id,function(input,output,session){
    # filter for the historic data
    index0 <- reactive({grep(pattern = input$Layer, x = names(histRasters))})
    r0 <-reactive({histRasters[[index0()]]})
    # historic palette
    # index0p <-reactive({grep(pattern = input$Layer, x = names(histPal))})
    # pal0 <-reactive({histPal[[index0p()]]})
    # not the most efficent process but it works. Tricky to get all the reactive
    # call posistions lined up so use this as a model for changes 
    n1 <- reactive({paste0(input$Layer,"_",ssp,"_",input$Timeline)})
    index1 <- reactive({grep(pattern = n1(), x = names(sspRasters))})
    r1 <- reactive({sspRasters[[index1()]]})
    # ssp palette
    # index1p <-reactive({grep(pattern = n1(), x = names(sspPal))})
    # pal1 <-reactive({sspPal[[index1p()]]})
    
    
    pal <- reactive(pals1[[input$Layer]]$palette)
    vals <- reactive(pals1[[input$Layer]]$values)
    title <- reactive(pals1[[input$Layer]]$title)
    
    
    #percent change layer and palette
    r2 <- reactive({changeRasters[[index1()]]})
    
    pal2 <- reactive(pals2[[input$Layer]]$palette)
    vals2 <- reactive(pals2[[input$Layer]]$values)
    title2 <- reactive(pals2[[input$Layer]]$title) 
    
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
        setView(lng = 37.826119933082545
                , lat = 0.3347526538983459
                , zoom = 6) %>%
        # add z levels ------------------------------------------------------------
      addMapPane("Historic Data", zIndex = 406) %>%
        addMapPane("Projected Data", zIndex = 407) %>%
        addMapPane("Percent Change", zIndex = 408) %>%
        addMapPane("Counties", zIndex = 409) %>%
        # tile providers ----------------------------------------------------------
      addProviderTiles("Stamen.Toner", group = "Light") %>%
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
        leaflet.extras::addResetMapButton() %>%
        # add ssp raster features -----------------------------------------------------
      addRasterImage(r1(),
                     #colors = pal1(),
                     colors = pal(),
                     group = "Projected Data",
                     opacity = 1) %>%
        # add historic raster -----------------------------------------------------
      addRasterImage(r0(),
                     #colors = pal0(),
                     colors = pal(),
                     group = "Historic Data",
                     opacity = 1) %>%
        # add percent change layer ------------------------------------------------
      addRasterImage(
        r2(),
        layerId = "change",
        #colors = pal1(),
        colors = pal2(),
        group = "Percent Change",
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
      # Including it as it's own control group because it applies to both temp map objects. 
      addLegend(
        "bottomright",
        pal = pal(),
        values = vals(),
        title = title(),
        #   labels = c("Low Change", "", "", "", "High Change"),
        opacity = 1,
        layerId = "firstLegend",
        group =  "Temperature Legend",  ### c("Historic Data","Projected Data") did not work as expected 
        #   na.label = "No Data"
        #
        #   # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
      ) %>%
        addLegend(
          "bottomleft",
          pal = pal2(),
          values = vals2(),
          title = title2(),
          #   labels = c("Low Change", "", "", "", "High Change"),
          opacity = 1,
          layerId = "secondLegend",
          group = "Percent Change",
        ) %>% 
        # add control groups ------------------------------------------------------
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Light"),
        overlayGroups = c(
          "Historic Data",
          "Projected Data",
          "Percent Change",
          "Temperature Legend",
          "Counties"
        ),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
        # Keep projected layer off by default
        hideGroup("Percent Change")
      
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
            paste("C", intToUtf8(176))
          }
        })
        
        output$cnty <- renderText(paste("Historic value:",
                                         "<b>", as.character(extractVal0), "</b>",label1(), "<br>",
                                  "Projected value:",
                                  "<b>", as.character(extractVal1),"</b>",label1()))
      })
      
      
    }
  )
}
