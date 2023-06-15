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
                     label= tags$strong("Pick a future time period:"),
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
    n1 <- reactive({paste0(input$Layer,"_",ssp,"_",input$Timeline)})
    index1 <- reactive({grep(pattern = n1(), x = names(sspRasters))})
    r1 <- reactive({sspRasters[[index1()]]})
    
    # ssp palette
    pal <- reactive(pals1[[input$Layer]]$palette)
    vals <- reactive(pals1[[input$Layer]]$values)
    title <- reactive(pals1[[input$Layer]]$title)
    
    
    #percent change layer and palette
    r2 <- reactive({changeRasters[[index1()]]})
    
    pal2 <- reactive(pals2[[input$Layer]]$palette)
    vals2 <- reactive(pals2[[input$Layer]]$values)
    title2 <- reactive(pals2[[input$Layer]]$title) 
      
    
    output$map1 <- leaflet::renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 4)) %>%
          #set zoom levels
          setView(lng = 37.826119933082545
                  , lat = 0.3347526538983459
                  , zoom = 6) %>%
          # add z levels ------------------------------------------------------------
        addMapPane("BaseMap", zIndex = 410) %>%
        addMapPane("HistoricData", zIndex = 420) %>%
        addMapPane("ProjectedData", zIndex = 430) %>%
        addMapPane("PercentChange", zIndex = 440) %>%
        addMapPane("Counties", zIndex = 450) %>%
        # tile providers ----------------------------------------------------------
        # addProviderTiles("Stamen.Toner", group = "Light", options = pathOptions(pane = "BaseMap")) %>%
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap")%>%
          #leaflet.extras::addResetMapButton() %>%
          # add county features -----------------------------------------------------
        addPolygons(
          data = countyFeat,
          fillColor = "",
          fillOpacity = 0,
          color = "black",
          layerId = ~ ADMIN1,
          weight = 1.5,
          group = "Counties",
          options = pathOptions(pane = "Counties"),
          # add hover over lables 
          label= ~ ADMIN1,
          labelOptions = labelOptions(noHide = F,
                                      style = list("font-weight" = "bold"),
                                      textsize = "15px"),
          # add highlight options to make labels a bit more intuitive 
          highlightOptions = highlightOptions(
            color = "yellow",
            opacity = 1,
            weight = 3,
            bringToFront = TRUE
          )
        ) %>%
          # add percent change legend
          addLegend_decreasing(
            "topright",
            pal = pal2(),
            values = vals2(),
            title = title2(),
            #   labels = c("Low Change", "", "", "", "High Change"),
            opacity = 1,
            layerId = "secondLegend",
            group = "Percent Change",
            decreasing = TRUE
          ) %>% 
        # add control groups ------------------------------------------------------
        addLayersControl(
          # baseGroups = c("OpenStreetMap", "Light"),
          overlayGroups = c(
            "Historic Data",
            "Projected Data",
            "Percent Change",
            "Counties"
          ),
          position = "topleft",
          options = layersControlOptions(collapsed = TRUE)
        )   %>%         # Keep projected layer off by default
        hideGroup("Percent Change")

      })
      
      
      # this makes it so the proxy map is rendered in the background, otherwise the map is empty when you first navigate to this page
      outputOptions(output, "map1", suspendWhenHidden=FALSE)
      
      # add rasters to proxy map
      observe({
        leafletProxy("map1") %>%
          # add historic raster -----------------------------------------------------
        addRasterImage(r0(),
                       #colors = pal0(),
                       colors = pal(),
                       group = "Historic Data",
                       opacity = 1,
                       project = FALSE)%>%
          # add ssp raster features -----------------------------------------------------
        addRasterImage(r1(),
                       #colors = pal1(),
                       colors = pal(),
                       group = "Projected Data",
                       opacity = 1,
                       project = FALSE) %>%
          
          # add percent change layer ------------------------------------------------
        addRasterImage(
          r2(),
          layerId = "change",
          #colors = pal1(),
          colors = pal2(),
          group = "Percent Change",
          opacity = 1,
          project = FALSE
        ) 
        
      })
          
         
      # add shared temperature legend  ------------------------------------------------------------
      #observeEvent(input$map1_groups,{
      observe({
        leafletProxy("map1") %>%
          removeControl(layerId = "firstLegend")
        
        if ('Historic Data' %in% input$map1_groups | 'Projected Data' %in% input$map1_groups){
          leafletProxy("map1") %>%
            addLegend_decreasing(
              "bottomright",
              pal = pal(),
              values = vals(),
              title = title(),
              opacity = 1,
              layerId = "firstLegend",
              decreasing = TRUE
            )
            
        }

      })
      
          
          
      
      #map click
      observeEvent(input$map1_click, {
        click <- input$map1_click
        clat <- click$lat
        clon <- click$lng
        # filter data
        #point <- as(st_point(x = c(clon, clat)), "Spatial")
        #need coordinates in same CRS as rasters
        point <- st_sfc(st_point(x = c(clon,clat)),crs = 4326) %>% 
          st_transform(3857) %>%
          as("Spatial")
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
