### Map selection and plot module 

# define ui ---------------------------------------------------------------
map2_UI <- function(id){
  ns <- NS(id)
  tagList( # ensures that correct html returns. Functions as a list. 
    tabsetPanel(
      tabPanel(
        tabPanel("Climate and management effects on existing forests",
                 sidebarPanel(
                   # select climate future
                   radioButtons(
                     inputId=ns("climateFuture"),
                     label= "Pick Climate Future:",
                     choices = c("Optmistic", "Middle of the road", "Pessimistic")
                   ),
                   # select future time frame 
                   radioButtons(
                     inputId=ns("Timeline_1"),
                     label= "Pick a future timeperiod:",
                     choices = c("Near term (2030)", "Medium term (2050)", "Long term (2100)") 
                   ),
                   # select mapped variable 
                   selectInput(
                     inputId=ns("Management"),
                     label="Pick a management scenario:",
                     choices = c("Do nothing", "Stop grazing", "Stop fires", "Reduce water stress"), multiple = F)
                   ),
                   tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                   tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                   tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
                 ),
                 mainPanel(
                   leafletOutput(ns("varchange1")),
                   textOutput(ns("cnty1")),
                   textOutput(ns("facdat1")),
                   textOutput(ns("explain1"))
                 )
        )
      )
    )
}


# define server  ---------------------------------------------------------- 
map2_server <- function(id){
  moduleServer(
    id,
    function(input,output,session){
      
    }
  )
}

