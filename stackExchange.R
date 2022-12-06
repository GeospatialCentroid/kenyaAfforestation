map_UI <- function(id, panelName){
  ns <- NS(id)
  tagList(
      leafletOutput(ns("map1")),
      textOutput(ns("print"))
      )
}

map_server <- function(id){
  moduleServer(id,function(input,output,session){
    map <- reactive({
      leaflet() %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap")
    })
    #render 
    output$map1 <- renderLeaflet({map()})
    
    # observer map click
    observeEvent(input$map1_click, {
      click <- input$map1_click
      clat <- click$lat
      clon <- click$lng
      # print click value to test 
      output$print <- renderText(as.character(clat))
    })
  })
}


  
  
  # UI section --------------------------------------------------------------
ui <- fluidPage(
    map_UI(id = "example")
  )
  
  
  
# Server ------------------------------------------------------------------
server <- function(input, output, session) {
    map_server(id = "example")
  }
  

shinyApp(ui, server)

  