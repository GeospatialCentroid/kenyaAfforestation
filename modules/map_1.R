### Map selection and plot module 

# define ui ---------------------------------------------------------------
map_UI <- function(id){
  ns <- NS(id)
  tagList( # ensures that correct html returns. Functions as a list. 
    tabsetPanel(
      tabPanel(
        tabPanel("Climate change in Kenya",
                 sidebarPanel(
                   # select climate future
                   radioButtons(
                     inputId=ns("climateFuture"),
                     label= "Pick Climate Future:",
                     choices = c("Optmistic", "Middle of the road", "Pessimistic")
                   ),
                   # select future time frame 
                   radioButtons(
                     inputId=ns("Timeline"),
                     label= "Pick a future timeperiod:",
                     choices = c("Near term (2030)", "Medium term (2050)", "Long term (2100)") 
                   ),
                   # select mapped variable 
                   selectInput(
                     inputId=ns("Layer"),
                     label="Pick a variable that you would like to visualize:",
                     choices = c("Min Temperature", "Max Temperature", "Precipitation", "Net primary productivity"), multiple = F
                   ),
                   tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                   tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                   tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                 ),
                 mainPanel(
                   leafletOutput(ns("varchange")),
                   textOutput(ns("cnty")),
                   textOutput(ns("facdat")),
                   textOutput(ns("explain"))
                 )
        )
      )
    )
  )
}


# define server  ---------------------------------------------------------- 
map_server <- function(id){
  moduleServer(
    id,
    function(input,output,session){
      
    }
  )
}

