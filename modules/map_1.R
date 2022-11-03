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
                   tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                   tags$p(tags$strong("Click"), "on a pixel within Kenya to see the county name and pixel value."),
                 ),
          # main panel -------------------------------------------------------------- 
          mainPanel(width = 8,
                   leafletOutput(ns("varchange")),
                   textOutput(ns("cnty")),
                   textOutput(ns("facdat")),
                   textOutput(ns("explain")),
                   "I think we can remove this. Or find a better spot.",
                   tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                 )
    )
  )
}


# define server  ---------------------------------------------------------- 
map_server <- function(id, rasters){
  moduleServer(id,function(input,output,session){
    # generate our display raster
    
    
    ### effective call of the input features from the ui 
    n1 <- reactive({paste0(input$Layer,"_",input$Timeline)})
    
    ### I haven't figure out how to correctly index these features yet 
    facresults<-reactive({
      # compile inputs from the radio buttons
      n1 <- reactive({paste0(input$Layer,"_",input$Timeline)})
      # test for the presence of values in names and subset
      r1 <- rasters()[[grep(pattern = n1, x = names(rasters()))]]
      return(raster::raster(r1))
    })
    
    
    # this works with a direct index on raster input object. 
      map <- leaflet(options = leafletOptions(minZoom = 4)) %>%
        setView( lng = 37.826119933082545
                 , lat = 0.3347526538983459
                 , zoom = 2 )%>%
        addProviderTiles("OpenStreetMap", group = "OpenStreetMap")%>%
        leaflet::addRasterImage(rasters[[1]]) 
        #addRasterImage(x = facresults)

      output$varchange <- renderLeaflet({map})
      
      # test place for checking reactively results 
      output$cnty <- renderText(n1())
    }
  )
}
