### current testing the kenya afforestation revamp 
### carverd@colostate.edu 
### 20221014

library(shiny)
# source modules
lapply(list.files(path = "modules/", pattern = ".R", full.names = TRUE), source)
# source UI or Server only functions 
lapply(list.files(path = "functions/", pattern = ".R", full.names = TRUE), source)


# UI section --------------------------------------------------------------
ui <- fluidPage(
  
  landingPage_UI(),
  
  tags$h2("Climate Futures"),
  map_UI(
    id = "map_1"
  ),
  tags$h2("Management Options"),
  map2_UI(
    id = "map_2"
  ),
  # tags$h2("Expanded Management Options"),
  # map3_UI(
  #   id = "map_3",
  #   county_names = county_names
  # ),
)

server <- function(input,output, session){
  map_server(
    id = "map_1"
  )
  map2_server(
    id = "map_2"
  )

  # passing reactive elements to module functions  --------------------------
  # from  https://www.youtube.com/watch?v=oOYaHsPXLvs&ab_channel=RConsortium 40min
  test <- reactiveVal(1)
  morevals <- reactiveValues(test = 1)
  
  module_server(id = "id_1", results = test )# this works
  module_server(id = "id_2", results = test() )# this will only return the initial value
  
  module_server(id - "id_3", results = morevals) # this works 
  module_server(id = "id_4", results = morevals$test) # this does not work
  ## passing a object defined in the ui 
  module_server(id - "id_5", results = reactive({input$test})) # this works 
  module_server(id - "id_5", results = input$test) # this does not work. 
  
  
  # map3_server(
  #   id = "map_3"
  # )
}

shinyApp(ui,server)
