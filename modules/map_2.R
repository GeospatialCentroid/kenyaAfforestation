### Maps for climate change management 

# define ui ---------------------------------------------------------------
map2_UI <- function(id, panelName, county_names){
  ns <- NS(id)
  tagList( # ensures that correct html returns. Functions as a list. 
    h3(panelName),
    sidebarLayout(
      sidebarPanel(width = 3,
                   radioButtons(
                     inputId = ns("Timeline2"),
                     label = tags$strong("Pick a future timeperiod:"),
                     choices = c("Near term (2030)", "Medium term (2050)", "Long term (2100)") 
                   ),
                   selectInput(
                     inputId = ns("Management"),
                     label = tags$strong("Pick a management scenario:"),
                     choices = c("Do nothing", "Stop grazing", "Stop fires", "Reduce water stress"), multiple = F
                   ),
                   hr(style = "border-all: 1px solid #000000;"),
                   radioButtons(
                     inputId=ns("Visualize"),label= tags$strong("Visualize an expanded forest cover scenario"),
                     choices = c("Forest cover in 2030")
                   ),
                   h4("How does this cover change over time?"),
                   selectInput(
                     inputId=ns("Management22"), label=tags$strong("Pick a management scenario:"),
                     choices = c( "Do nothing", "Stop fires"), multiple = F
                   ),
                   selectInput(
                     inputId=ns("County23"), label=tags$strong("Pick a county to visualize forest cover changes:"),
                     choices = c("All", county_names), multiple = F
                   ),
                   tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                   tags$p(tags$strong("Click")," on a pixel within Kenya to see the county name and pixel value.")),
      mainPanel(width = 8,
        leafletOutput(ns("varchange1")),
        textOutput(ns("cnty3")),
        textOutput(ns("facdat3")),
        textOutput(ns("explain3"))
      )
    )
  )
}


# define server  ---------------------------------------------------------- 
map2_server <- function(id){
  moduleServer(id,function(input,output,session){
    map <- leaflet(options = leafletOptions(minZoom = 5)) %>%
      setView( lng = 37.826119933082545
               , lat = 0.3347526538983459
               , zoom = 2 )%>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap")

    output$varchange1 <- renderLeaflet({map})

    output$cnty3 <- renderText("This is printed content ")
    }
  )
}


