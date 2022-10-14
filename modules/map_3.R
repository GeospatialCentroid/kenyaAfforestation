### Map selection and plot module 

# define ui ---------------------------------------------------------------
map3_UI <- function(id, county_names){
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
                   h4("How does this cover change over time?"), 
                   selectInput(
                     inputId=ns("Management12"),
                     label= "Pick a management scenario:",
                     choices = c("Do nothing", "Stop fires"), multiple = F
                   ),
                   # select mapped variable 
                   selectInput(
                     inputId=ns("County13"),
                     label="Pick a county to visualize forest cover changes:",
                     choices = c("All", county_names), multiple = F)
                 ),
                 tags$p(span("Large maps may take a few seconds to render.", style = "color:red")),
                 tags$p(HTML("<b>Click</b> on a pixel within Kenya to see the county name and pixel value.")),
                 tags$p(HTML("<b> Click here </b> to see how these data were generated and to learn more about caveats (under construction)"))
        ),
        mainPanel(
          leafletOutput(ns("varchange131")),
          textOutput(ns("cnty13")),
          textOutput(ns("facdat13")),
          textOutput(ns("explain13")),
          tableOutput("tab13")
        )
      )
    )
  )
}


# define server  ---------------------------------------------------------- 
map3_server <- function(id){
  moduleServer(
    id,
    function(input,output,session){
      
    }
  )
}

